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
program ovom3u;

uses {$IFDEF UNIX}
  cthreads,
  {$ENDIF} {$IFDEF HASAMIGA}
  athreads, {$ENDIF}
  {$IF defined(LCLGTK2) or defined(LCLGTK3)}
    {$IF defined(Linux) or defined(FreeBSD)}
     unix_init_xlib,
    {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  LazLogger, lazmouseandkeyinput,
  Config,
  umain,
  sysutils,
  uEPGFOrm,
  LoggerUnit, appconsts, Renderer, uChannels, uBackEnd,
  {$IFDEF LINUX}mpris2,{$ENDIF}
  cec, CEC_intf, MultimediaKeys, uLogViewer;

{$R *.res}
var
  Verbose: string;

begin
  //setHeapTraceOutput('trace.log');
  // needed to output exception to a file
  Application.Flags := Application.Flags + [appNoExceptionMessages];

  OvoLogger.LogName := ConfigObj.ConfigDir+LogFileName;
  OvoLogger.SaveOldLog;
  Verbose := Application.GetOptionValue('v','verbose');
  OvoLogger.LevelFromString(Verbose);
  OvoLogger.Log(llFORCED, '----------------------------');
  OvoLogger.Log(llFORCED, DisplayAppName);
  OvoLogger.Log(llFORCED, format (rVersionString,[AppVersion, RevisionStr, BuildDate]));
  OvoLogger.Log(llFORCED, format (rBuildEnv,[lazVersion, fpcVersion]));
  OvoLogger.Log(llFORCED, format (rTarget,[TargetCPU, TargetOS]));
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfPlayer, fPlayer);
  Application.Run;
end.
