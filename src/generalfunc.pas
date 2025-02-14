{
This file is part of OvoM3U
Copyright (C) 2020 Marco Caselli

OvoM3U is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

}
{$I codegen.inc}
unit GeneralFunc;

interface

uses
  Classes, SysUtils, LazLoggerBase;

type
  TStopDownloadFunc = function(): boolean of object;

function TimeToMSec(Time: double): int64;
function FormatTimeRange(const Time: TDateTime; ShortMode: boolean = False): string; overload;
function FormatTimeRange(StartTime, EndTime: TDateTime; TimeOnly: boolean = False): string; overload;
function CompareBoolean(a, b: boolean): integer;
function DownloadFromUrl(AFrom: string; ATo: string; StopDownloadFunc: TStopDownloadFunc = nil): boolean;
function EpgDateToDate(const iDateStr: string): TDateTime;
function CleanupFileName(AString: string): string;

type
  TByteStringFormat = (bsfDefault, bsfBytes, bsfKB, bsfMB, bsfGB, bsfTB);

function FormatByteString(Bytes: uint64; Format: TByteStringFormat = bsfDefault): string;

implementation

uses
  opensslsockets, fphttpclient, DateUtils;

const
  OneKB = 1024;
  OneMB = OneKB * OneKB;
  OneGB = OneKB * OneMB;
  OneTB = OneKB * OneGB;

  OneHour = MinsPerHour * SecsPerMin * MSecsPerSec;

type

  { TDownloadStream }

  TOnWriteStream = procedure(Sender: TObject; APos: int64) of object;

  TDownloadStream = class(TStream)
  private
    FOnWriteStream: TOnWriteStream;
    FStream: TStream;
    fStopDownloadFunc: TStopDownloadFunc;
    FHTTPClient: TFPHTTPClient;
  public
    constructor Create(AStream: TStream; StopDownloadFunc: TStopDownloadFunc);
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    procedure DoProgress;
  published
    property OnWriteStream: TOnWriteStream read FOnWriteStream write FOnWriteStream;
  end;
  { TDownloadStream }

constructor TDownloadStream.Create(AStream: TStream; StopDownloadFunc: TStopDownloadFunc);
begin
  inherited Create;
  FStream := AStream;
  FStream.Position := 0;
  fStopDownloadFunc := StopDownloadFunc;
end;

destructor TDownloadStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TDownloadStream.Read(var Buffer; Count: longint): longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TDownloadStream.Write(const Buffer; Count: longint): longint;
begin
  Result := FStream.Write(Buffer, Count);
  DoProgress;
end;

function TDownloadStream.Seek(Offset: longint; Origin: word): longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

procedure TDownloadStream.DoProgress;
begin
  if Assigned(fStopDownloadFunc) and fStopDownloadFunc then
    FHTTPClient.Terminate;

  if Assigned(FOnWriteStream) then
    FOnWriteStream(Self, Self.Position);
end;

function DownloadFromUrl(AFrom: string; ATo: string; StopDownloadFunc: TStopDownloadFunc): boolean;
var
  DS: TDownloadStream;
begin
  Result := False;
  DS := TDownloadStream.Create(TFileStream.Create(ATo, fmCreate), StopDownloadFunc);
  try
    try
      DS.FHTTPClient := TFPHTTPClient.Create(nil);
      DS.FHTTPClient.AllowRedirect := True;
      DS.FHTTPClient.HTTPMethod('GET', AFrom, DS, [200]);
      Result := True;
    except
      on E: Exception do
      begin
        Result := False;
        ds.FStream.Free;
        ds.FStream := nil;
        DeleteFile(Ato);
      end;
    end;
  finally
    DS.FHTTPClient.Free;
    DS.Free
  end;
end;

function CleanupFileName(AString: string): string;
var
  x: integer;
const
  IllegalCharSet: set of char =
    ['|', '<', '>', '\', '^', '+', '=', '?', '/', '[', ']', '"', ';', ',', '*'];
begin
  for x := 1 to Length(AString) do
    if AString[x] in IllegalCharSet then
      AString[x] := '_';
  Result := AString;
end;

function EpgDateToDate(const iDateStr: string): TDateTime;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: word;
  TimeOffset: integer;
  DatePart, OffsetPart: string;
begin
  AMilliSecond := 0;
  TimeOffset := Pos(' ', iDateStr);
  if TimeOffset > 0 then
  begin
    DatePart := Copy(iDateStr, 1, TimeOffset - 1);
    OffsetPart := Copy(iDateStr, TimeOffset + 1, 5);
  end
  else
    DatePart := iDateStr;

  AYear := StrToIntDef(copy(DatePart, 1, 4), 0);
  AMonth := StrToIntDef(copy(DatePart, 5, 2), 1);
  ADay := StrToIntDef(copy(DatePart, 7, 2), 1);
  AHour := StrToIntDef(copy(DatePart, 9, 2), 0);
  AMinute := StrToIntDef(copy(DatePart, 11, 2), 0);
  ASecond := StrToIntDef(copy(DatePart, 13, 2), 0);

  Result := EncodeDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond,
    AMilliSecond);

  if TimeOffset > 0 then
  begin
    if TryStrToInt(OffsetPart, TimeOffset) then
      TimeOffset := -1 * Trunc((TimeOffset div 100) * 60 + (TimeOffset mod 100))
    else
      TimeOffset := 0;
  end;
  Result := incMinute(Result, TimeOffset - GetLocalTimeOffset({Result, True}));
end;

function FormatTimeRange(StartTime, EndTime: TDateTime; TimeOnly: boolean): string;
begin
  if (StartTime = 0) and (EndTime = 0) then
    Result := EmptyStr
  else
  begin
    if TimeOnly then
      Result := FormatDateTime('t', StartTime) + ' - ' + FormatDateTime('t', EndTime)
    else
      Result := FormatDateTime('ddddd t', StartTime) + ' - ' + FormatDateTime('ddddd t', EndTime);
  end;
end;

function FormatTimeRange(const Time: TDateTime; ShortMode: boolean = False): string;
begin
  if ShortMode and (Time < OneHour) then
    Result := FormatDateTime('[mm]:ss', Time / MSecsPerDay, [fdoInterval])
  else
    Result := FormatDateTime('[hh]:mm:ss', Time / MSecsPerDay, [fdoInterval]);

end;

function CompareBoolean(a, b: boolean): integer;
const
  BoolOrder: array [False..True] of integer = (0, 1); // o 1,0 se si desidera ordinare il contrario
begin
  Result := BoolOrder[a] - BoolOrder[b];
end;


// code from David Heffernan, from http://stackoverflow.com/questions/30548940/correct-way-to-convert-size-in-bytes-to-kb-mb-gb-delphi
function FormatByteString(Bytes: uint64; Format: TByteStringFormat = bsfDefault): string;
begin
  if Format = bsfDefault then if Bytes < OneKB then Format := bsfBytes
    else if Bytes < OneMB then Format := bsfKB
    else if Bytes < OneGB then Format := bsfMB
    else if Bytes < OneTB then Format := bsfGB
    else
      Format := bsfTB;

  case Format of
    bsfKB:
      Result := SysUtils.Format('%.1n KB', [Bytes / OneKB]);
    bsfMB:
      Result := SysUtils.Format('%.1n MB', [Bytes / OneMB]);
    bsfGB:
      Result := SysUtils.Format('%.1n GB', [Bytes / OneGB]);
    bsfTB:
      Result := SysUtils.Format('%.1n TB', [Bytes / OneTB]);
    else  // bsfBytes:
      Result := SysUtils.Format('%d bytes', [Bytes]);

  end;
end;

function TimeToMSec(Time: double): int64;
const
  transform = 1 / (24 * 60 * 60);
begin
  Result := trunc(Time * 1000 / transform);
end;

end.
