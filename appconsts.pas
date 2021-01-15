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
unit AppConsts;


interface

uses
  Classes, SysUtils, LCLVersion, versionresource;

{$i revision.inc}

const
  DisplayAppName = 'ovoM3U';
  AppVersion = {$i version.inc};
  BuildDate = {$I %DATE%};
  lazVersion  = lcl_version;         // Lazarus version (major.minor.micro)
//  lazRevision = RevisionStr;         // Lazarus SVN revision
  fpcVersion  = {$I %FPCVERSION%};   // FPC version (major.minor.micro)
  TargetCPU   = {$I %FPCTARGETCPU%}; // Target CPU of FPC
  TargetOS    = {$I %FPCTARGETOS%};  // Target Operating System of FPC


  AppName  = 'ovom3u';
  DefaultResourceDirectory = '/usr/share/' + AppName + '/';
  ResourceSubDirectory     = 'Resources';
  EPGLibraryName = 'epg.db';
  TempEPGFile = 'current-epg';
  TempEPGFileDecompressed = 'current-epg.xml';
  LogFileName:string = appname + '.log' ;


Resourcestring
  rVersionString = 'Version: %0:s (Date %2:s)';
  rBuildEnv      = 'Build environment: Lazarus %0:s  FPC %1:s';
  rTarget        = 'Target: CPU %0:s  OS %1:s';
implementation

Function GetAppName : String;
begin
  Result:=AppName;
end;

initialization
 OnGetApplicationName := @GetAppName;


end.

