{
This file is part of OvoM3U
Copyright (C) 2020 Marco Caselli

OvoPlayer is free software; you can redistribute it and/or
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
  cthreads, {$ENDIF} {$IFDEF HASAMIGA}
  athreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  LazLogger,
  Config,
  umain,
  uEPGFOrm,
  LoggerUnit;

{$R *.res}

begin
  OvoLogger.LogName := Config.GetConfigDir+'ovom3u.log';
  OvoLogger.Level := TRACE;
  OvoLogger.Log(FORCED, '----------------------------');
  OvoLogger.Log(FORCED, 'OvoM3U ');
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfPlayer, fPlayer);
  Application.CreateForm(TEPGForm, EPGForm);
  Application.Run;
end.
