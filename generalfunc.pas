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
  Classes, SysUtils,  LazLoggerBase;
type
TStopDownloadFunc = function ():boolean of object;

function TimeToMSec(Time: double): int64;
Function FormatTimeRange(const Time: TDateTime; ShortMode:boolean=false): string;   overload;
Function FormatTimeRange(StartTime, EndTime:TDateTime; TimeOnly: boolean=false): string;  overload;
function CompareBoolean (a, b: Boolean): Integer;
FUNCTION DownloadFromUrl(AFrom: String; ATo: String; StopDownloadFunc:TStopDownloadFunc=nil):boolean;
function EpgDateToDate(const iDateStr: string): TDateTime;
function CleanupFileName(AString: string): string;

 type
  TByteStringFormat = (bsfDefault, bsfBytes, bsfKB, bsfMB, bsfGB, bsfTB);
  function FormatByteString(Bytes: UInt64; Format: TByteStringFormat = bsfDefault): string;

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

  TOnWriteStream = procedure(Sender: TObject; APos: Int64) of object;
  TDownloadStream = class(TStream)
  private
    FOnWriteStream: TOnWriteStream;
    FStream: TStream;
    fStopDownloadFunc: TStopDownloadFunc;
    FHTTPClient : TFPHTTPClient;
  public
    constructor Create(AStream: TStream; StopDownloadFunc: TStopDownloadFunc);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
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

function TDownloadStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TDownloadStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := FStream.Write(Buffer, Count);
  DoProgress;
end;

function TDownloadStream.Seek(Offset: LongInt; Origin: Word): LongInt;
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

function DownloadFromUrl(AFrom: String; ATo: String; StopDownloadFunc: TStopDownloadFunc): boolean;
var
  DS: TDownloadStream;
begin
  Result := false;
  DS := TDownloadStream.Create(TFileStream.Create(ATo, fmCreate), StopDownloadFunc);
  try
    try
      DS.FHTTPClient := TFPHTTPClient.Create(nil);
      DS.FHTTPClient.AllowRedirect:=true;
      DS.FHTTPClient.HTTPMethod('GET', AFrom, DS, [200]);
      Result := true;
    except
      on E: Exception do
      begin
        Result := false;
        ds.FStream.Free;
        ds.FStream := nil;
        DeleteFile(Ato);
      end;
    end;
  finally
    DS.FHTTPClient.free;
    DS.Free
  end;
end;

function CleanupFileName(AString: string): string;
var
  x: integer;
const
  IllegalCharSet: set of char =
    ['|','<','>','\','^','+','=','?','/','[',']','"',';',',','*'];
begin
  for x := 1 to Length(AString) do
    if AString[x] in IllegalCharSet then
      AString[x] := '_';
  Result := AString;
end;

function EpgDateToDate(const iDateStr: string): TDateTime;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
   AMilliSecond:=0;
   AYear:=StrToIntDef(copy(iDateStr,1,4),0);
   AMonth:=StrToIntDef(copy(iDateStr,5,2),1);
   ADay:=StrToIntDef(copy(iDateStr,7,2),1);
   AHour:=StrToIntDef(copy(iDateStr,9,2),0);
   AMinute:=StrToIntDef(copy(iDateStr,11,2),0);
   ASecond:=StrToIntDef(copy(iDateStr,13,2),0);

  Result := EncodeDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond,
    AMilliSecond);
//  W
end;

function FormatTimeRange(StartTime, EndTime: TDateTime; TimeOnly: boolean): string;
begin
  if TimeOnly then
    Result := FormatDateTime('t', StartTime)+ ' - '+ FormatDateTime('t', EndTime)
  else
    Result := FormatDateTime('ddddd t', StartTime)+ ' - ' +FormatDateTime('ddddd t', EndTime);
end;

function FormatTimeRange(const Time: TDateTime; ShortMode:boolean=false): string;
begin
  if ShortMode and (Time < OneHour) then
    result:=FormatDateTime('[mm]:ss', Time / MSecsPerDay,[fdoInterval])
  else
    result:=FormatDateTime('[hh]:mm:ss', Time / MSecsPerDay,[fdoInterval]);

end;

function CompareBoolean (a, b: Boolean): Integer;
const
   BoolOrder: Array [False..True] Of Integer = (0,1); // o 1,0 se si desidera ordinare il contrario
Begin
   result := BoolOrder [a] - BoolOrder [b];
End ;


// code from David Heffernan, from http://stackoverflow.com/questions/30548940/correct-way-to-convert-size-in-bytes-to-kb-mb-gb-delphi
function FormatByteString(Bytes: UInt64; Format: TByteStringFormat = bsfDefault): string;
begin
  if Format = bsfDefault then begin
    if Bytes < OneKB then begin
      Format := bsfBytes;
    end
    else if Bytes < OneMB then begin
      Format := bsfKB;
    end
    else if Bytes < OneGB then begin
      Format := bsfMB;
    end
    else if Bytes < OneTB then begin
      Format := bsfGB;
    end
    else begin
      Format := bsfTB;
    end;
  end;

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
