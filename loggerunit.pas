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
unit LoggerUnit;

interface

uses
  Classes, SysUtils, LazLogger;

Type
  TOvoLogLevel = (llTRACE, llDEBUG, llINFO, llWARN, llERROR, llFORCED, llNO_LOG);

  TOnLogMessage = procedure(Sender: TObject; const Message: string) of object;

  { TOvoLogger }

  TOvoLogger = class

  private
    FLevel: TOvoLogLevel;
    FOnLogMessage: TOnLogMessage;
    function GetLogName: string;
    procedure SetLevel(AValue: TOvoLogLevel);
    function DecodeLevel(ALevel:TOvoLogLevel): string;
    procedure SetLogName(AValue: string);
    procedure SetOnLogMessage(AValue: TOnLogMessage);
  public
    property Level : TOvoLogLevel read FLevel write SetLevel;
    Property LogName: string read GetLogName write SetLogName;
    property OnLogMessage: TOnLogMessage read FOnLogMessage write SetOnLogMessage;
    procedure LevelFromString(VerboseLevel:string);
    Procedure SaveOldLog;
    procedure Log(ALevel: TOvoLogLevel; const Msg: string);  overload;
    procedure Log(ALevel: TOvoLogLevel; const fmt: string; Args: array of const); overload;
    Constructor Create;
  end;

function OvoLogger: TOvoLogger;

implementation
Var
  FOvoLogger: TOvoLogger;

function OvoLogger: TOvoLogger;
begin
  Result := FOvoLogger;
  DebugLogger.CloseLogFileBetweenWrites := true;
end;

{ TOvoLogger }

procedure TOvoLogger.SetLevel(AValue: TOvoLogLevel);
begin
  if FLevel = AValue then Exit;
  FLevel := AValue;
end;

function TOvoLogger.GetLogName: string;
begin
  Result := DebugLogger.LogName;
end;

function TOvoLogger.DecodeLevel(ALevel: TOvoLogLevel): string;
begin
  Case ALevel of
    llTRACE: Result  := '   TRACE: ';
    llDEBUG: Result  := '   DEBUG: ';
    llINFO: Result   := '    INFO: ';
    llWARN: Result   := ' WARNING: ';
    llERROR: Result  := '   ERROR: ';
  else
    Result := '  NOTICE: '; // llFORCED
  end;
end;

procedure TOvoLogger.SetLogName(AValue: string);
begin
  DebugLogger.LogName := AValue;
end;

procedure TOvoLogger.SetOnLogMessage(AValue: TOnLogMessage);
begin
  FOnLogMessage := AValue;
end;

procedure TOvoLogger.LevelFromString(VerboseLevel: string);
begin
  if VerboseLevel = 'TRACE' then
    Level := llTRACE
  else
  if VerboseLevel = 'DEBUG' then
    Level := llDEBUG
  else
  if VerboseLevel = 'INFO' then
     Level := llINFO
  else
  if VerboseLevel = 'WARNING' then
     Level := llWARN
  else
  if VerboseLevel = 'NONE' then
     Level := llNO_LOG
  else
     Level := llERROR;

end;

procedure TOvoLogger.SaveOldLog;
var
  OldLogName : string;
begin
  try
  If not LogName.IsEmpty and FileExists(OvoLogger.LogName) then
    begin
      OldLogName := ChangeFileExt(LogName,'.old.log');
      if FileExists(OldLogName) then
         DeleteFile(OldLogName);
      RenameFile(LogName,  OldLogName);
    end;
  Except
    // no exception here, if rename fail try at least to write in existing file
  end;
end;

procedure TOvoLogger.Log(ALevel: TOvoLogLevel; const Msg: string);
begin
  if ALevel < FLevel then exit;
  DebugLogger.DebugLn(DateTimeToStr(now, true), DecodeLevel(ALevel),Msg);
  if Assigned(FOnLogMessage) then
    FOnLogMessage(self, DateTimeToStr(now, true)+' '+DecodeLevel(ALevel)+' '+Msg);
end;

procedure TOvoLogger.Log(ALevel: TOvoLogLevel; const fmt: string; Args: array of const);
begin
  Log(ALevel, format(fmt, Args));
end;

constructor TOvoLogger.Create;
begin
  FLevel := llWARN;
end;

Initialization
 FOvoLogger := TOvoLogger.Create;

Finalization
 If Assigned(FOvoLogger) then
   FOvoLogger.Free;
end.

