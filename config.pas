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
unit Config;

interface

uses
  Classes, SysUtils, Graphics, JsonTools, typinfo, um3uloader;

type
  { TEnum }

  TEnum<T> = class(TObject)
  public
    class function ToString(const aEnumValue: T): string; reintroduce;
    class function FromString(const aEnumString: string; const aDefault: T): T;
  end;

{ TConfig }

  { TListProperties }

  TListProperties = record
    ChannelsDownloadLogo: boolean;
    ChannelsFileName: string;
    ChannelsKind: TProviderKind;
    ChannelsUrl: string;

    UseChno: boolean;

    EpgFileName: string;
    EpgKind: TProviderKind;
    EPGUrl: string;
  end;

  TGuiProperties = record
    fViewLogo:boolean;
    fViewCurrentProgram: boolean;
  public
    property ViewLogo:boolean read fViewLogo write fViewLogo;
    property ViewCurrentProgram: boolean read fViewCurrentProgram write fViewCurrentProgram;
  end;

  { TMPVProperties }

  TMPVProperties = record
  private
    fHardwareAcceleration: boolean;
  public
    Property HardwareAcceleration: boolean read fHardwareAcceleration write fHardwareAcceleration;
  end;

  TConfig = class
  private
    fCacheDir: string;
    FConfigFile: string;
    fConfigDir: string;
    FEPGChanged: boolean;
    FGuiProperties: TGuiProperties;
    FListChanged: boolean;
    fListProperties: TListProperties;
    FMPVProperties: TMPVProperties;
    FPortableMode: boolean;
    ResourcesPath: string;
    fConfigHolder: TJsonNode;
    fExecutableDir: string;
    function GetCacheDir: string;
    function GetConfigDir: string;
    procedure SetEPGChanged(AValue: boolean);
    procedure SetGuiProperties(AValue: TGuiProperties);
    procedure SetListChanged(AValue: boolean);
    procedure SetListProperties(AValue: TListProperties);
    procedure SetMPVProperties(AValue: TMPVProperties);
  public
    property ListProperties: TListProperties read fListProperties write SetListProperties;
    property GuiProperties: TGuiProperties read FGuiProperties write SetGuiProperties;
    property MPVProperties: TMPVProperties read FMPVProperties write SetMPVProperties;
    property PortableMode: boolean read FPortableMode;

    // Used to signal changes, not saved
    Property ListChanged: boolean read FListChanged write SetListChanged;
    property EPGChanged: boolean read FEPGChanged write SetEPGChanged;
    constructor Create;
    procedure ReadConfig;
    procedure SaveConfig;
    procedure WriteStrings(const APath: string; Values: TStrings);
    function ReadStrings(const APath: string; Values: TStrings): integer;
    procedure WriteString(const APath: string; const  Value: String);
    function ReadString(const APath: string; const  ADefault: String): string;
    function GetResourcesPath: string;
    procedure WriteBoolean(const APath: string; Value: Boolean);
    function ReadBoolean(const APath: string; ADefault: Boolean): Boolean;
    procedure WriteInteger(const APath: string; Value: Integer);
    function ReadInteger(const APath: string; ADefault: Integer): Integer;

    procedure Flush;
    destructor Destroy; override;
    // -- //
    property ConfigDir: string read fConfigDir;
    property CacheDir: string read fCacheDir;
    property ConfigFile: string read FConfigFile;
  end;

  { TSimpleHistory }

  TSimpleHistory = class
  private
    FMax: Integer;
    IntList: TStringList;
    function GetCount: integer;
    procedure SetMax(AValue: Integer);
  public
    function Add(const S: string): Integer;
    Constructor Create;
    Destructor Destroy; override;
    Procedure SetList(List: TStrings);

    Procedure LoadFromConfig(Config: TConfig; APath: string);
    Procedure WriteToConfig(Config: TConfig; APath: string);
    Property Max: Integer read FMax write SetMax;
    Property Count: integer read GetCount;
  end;


function ConfigObj: TConfig;

implementation

{ TConfig }
uses
  Fileutil
  // only for default font !
{$ifdef Darwin}
  , MacOSAll
{$endif}  ;

var
  FConfigObj: TConfig;

const
  SectionUnix = 'UNIX';
  IdentResourcesPath = 'ResourcesPath';
  ResourceSubDirectory = 'Resources';

 {$ifdef UNIX}
  DefaultDirectory = '/usr/share/ovom3u/';
  {$DEFINE NEEDCFGSUBDIR}
 {$endif}

 {$ifdef DARWIN}
  BundleResourcesDirectory = '/Contents/Resources/';
 {$endif}

function NextToken(const S: string; var SeekPos: Integer;
  const TokenDelim: Char): string;
var
  TokStart: Integer;
begin
  repeat
    if SeekPos > Length(s) then begin Result := ''; Exit end;
    if S[SeekPos] = TokenDelim then Inc(SeekPos) else Break;
  until false;
  TokStart := SeekPos; { TokStart := first character not in TokenDelims }

  while (SeekPos <= Length(s)) and not(S[SeekPos] = TokenDelim) do Inc(SeekPos);

  { Calculate result := s[TokStart, ... , SeekPos-1] }
  result := Copy(s, TokStart, SeekPos-TokStart);

  { We don't have to do Inc(seekPos) below. But it's obvious that searching
    for next token can skip SeekPos, since we know S[SeekPos] is TokenDelim. }
  Inc(SeekPos);
end;

function ConfigObj: TConfig;
begin
  if not Assigned(FConfigObj) then
    FConfigObj := TConfig.Create;
  Result := FConfigObj;
end;

{ TEnum }

class function TEnum<T>.ToString(const aEnumValue: T): string;
begin
  WriteStr(Result, aEnumValue);
end;

class function TEnum<T>.FromString(const aEnumString: string; const aDefault: T): T;
var
  OrdValue: Integer;
begin
  OrdValue := GetEnumValue(TypeInfo(T), aEnumString);
  if OrdValue < 0 then
    Result := aDefault
  else
    Result := T(OrdValue);
end;

{ TSimpleHistory }

procedure TSimpleHistory.SetMax(AValue: Integer);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;

  while IntList.Count > FMax do
    IntList.Delete(IntList.Count-1);           // -1 since its 0 indexed

end;

function TSimpleHistory.GetCount: integer;
begin
  Result := IntList.count;
end;

function TSimpleHistory.Add(const S: string): Integer;
var
   i : integer;
begin
   i := IntList.IndexOf(S);
   if i<>-1 then
     IntList.Delete(i);

   IntList.Insert(0, S);

   // Trim the oldest files if more than NumFiles
   while IntList.Count > FMax do
     IntList.Delete(IntList.Count-1);           // -1 since its 0 indexed
  Result := IntList.Count;
end;

constructor TSimpleHistory.Create;
begin
  IntList := TStringList.Create;
end;

destructor TSimpleHistory.Destroy;
begin
  FreeAndNil(IntList);
  inherited Destroy;
end;

procedure TSimpleHistory.SetList(List: TStrings);
begin
  List.Assign(IntList);
end;

procedure TSimpleHistory.LoadFromConfig(Config: TConfig; APath: string);
begin
  Config.ReadStrings(APath, IntList);
end;

procedure TSimpleHistory.WriteToConfig(Config: TConfig; APath: string);
begin
  Config.WriteStrings(APath, IntList);
end;

procedure TConfig.SetListProperties(AValue: TListProperties);
begin
   fListChanged :=
     (fListProperties.ChannelsKind <> AValue.ChannelsKind) or
     (fListProperties.ChannelsFileName <> AValue.ChannelsFileName) or
     (fListProperties.ChannelsUrl <> AValue.ChannelsUrl) or
     (fListProperties.UseChno <> AValue.UseChno) or
     (fListProperties.ChannelsDownloadLogo <> AValue.ChannelsDownloadLogo);

   fEPGChanged :=
      (fListProperties.EpgKind <> AValue.EpgKind) or
      (fListProperties.EpgFileName <> AValue.EpgFileName) or
      (fListProperties.EpgUrl <> AValue.EpgUrl) or
      (fListProperties.UseChno <> AValue.UseChno);

  fListProperties:=AValue;
end;

procedure TConfig.SetMPVProperties(AValue: TMPVProperties);
begin
  FMPVProperties := AValue;
end;

constructor TConfig.Create;
begin

  fExecutableDir:= IncludeTrailingPathDelimiter(ProgramDirectory);

   if FileExists(fExecutableDir+'portable.txt') then
    fPortableMode := True;

  fConfigDir := GetConfigDir;
  fCacheDir  := GetCacheDir;


  if FPortableMode then
     begin
       FConfigFile:=fConfigDir+ApplicationName+ConfigExtension
     end
  else
    begin
      FConfigFile := GetAppConfigFile(False
    {$ifdef NEEDCFGSUBDIR}
        , True
    {$ENDIF}
        );

    end;
  fConfigHolder := TJsonNode.Create;
  if NOT FileExists(FConfigFile) then
    SaveConfig;
  ReadConfig;

end;

destructor TConfig.Destroy;
begin
  SaveConfig;
  fConfigHolder.Free;
  Finalize(fListProperties);
  inherited Destroy;
end;


function TConfig.GetConfigDir: string;
var
  Path: string;
begin
  if fPortableMode then
    Path:= fExecutableDir + 'config'
  else
    Path := GetAppConfigDir(False);
  ForceDirectories(Path);
  Result := IncludeTrailingPathDelimiter(Path);

end;

procedure TConfig.SetEPGChanged(AValue: boolean);
begin
  if FEPGChanged = AValue then Exit;
  FEPGChanged := AValue;
end;

procedure TConfig.SetGuiProperties(AValue: TGuiProperties);
begin

  FGuiProperties := AValue;
end;

procedure TConfig.SetListChanged(AValue: boolean);
begin
  if FListChanged = AValue then Exit;
  FListChanged := AValue;
end;

function TConfig.GetCacheDir: string;
begin
  if FPortableMode then
    begin
      Result := fExecutableDir+'cache';
      Result:=IncludeTrailingPathDelimiter(Result);
    end
  else
  begin
  {$ifdef UNIX}
    Result:=GetEnvironmentVariable('XDG_CONFIG_HOME');
    if (Result='') then
      begin
        Result:= GetEnvironmentVariable('HOME');
        if result <> '' then
          result:=IncludeTrailingPathDelimiter(result)+ '.cache/'
      end
    else
      Result:=IncludeTrailingPathDelimiter(Result);

   Result:=IncludeTrailingPathDelimiter(Result+ApplicationName);
  {$endif}
  {$ifdef WINDOWS}
    Result:=GetEnvironmentVariable('LOCALAPPDATA');
    if result <> '' then
      result:=IncludeTrailingPathDelimiter(result)+ 'Caches\';

    Result:=IncludeTrailingPathDelimiter(Result+ApplicationName);
  {$endif}
  end;
  ForceDirectories(Result);
end;

procedure TConfig.SaveConfig;
begin
  WriteString(SectionUnix+'/'+IdentResourcesPath, ResourcesPath);
  WriteString('m3u/ProviderKind',TEnum<TProviderKind>.ToString(fListProperties.ChannelsKind));
  WriteString('m3u/FileName',fListProperties.ChannelsFileName);
  WriteString('m3u/Url',fListProperties.ChannelsUrl);

  WriteString('EPG/ProviderKind',TEnum<TProviderKind>.ToString(fListProperties.EPGKind));
  WriteString('EPG/FileName',fListProperties.EPGFileName);
  WriteString('EPG/Url',fListProperties.EPGUrl);

  WriteBoolean('m3u/UseChno', fListProperties.UseChno);
  WriteBoolean('m3u/DownloadLogo', fListProperties.ChannelsDownloadLogo);

  WriteBoolean('gui/ViewLogo', FGuiProperties.ViewLogo);
  WriteBoolean('gui/ViewCurrentProgram', FGuiProperties.ViewCurrentProgram);

  WriteBoolean('MPV/HardwareAcceleration', FMPVProperties.HardwareAcceleration);


  fConfigHolder.SaveToFile(FConfigFile, true);
end;

procedure TConfig.ReadConfig;
begin

  fConfigHolder.LoadFromFile(FConfigFile);
{$ifdef WINDOWS}
  ResourcesPath := ReadString(SectionUnix + '/' + IdentResourcesPath,
    ExtractFilePath(ExtractFilePath(ParamStr(0))));
{$else}
  {$ifndef DARWIN}
  ResourcesPath := ReadString(SectionUnix + '/' + IdentResourcesPath, DefaultDirectory);
  {$endif}
{$endif}

  fListProperties.ChannelsKind:= TEnum<TProviderKind>.FromString(ReadString('m3u/ProviderKind',''), Local);
  fListProperties.ChannelsFileName:= ReadString('m3u/FileName','');
  fListProperties.ChannelsUrl:= ReadString('m3u/Url','');

  fListProperties.EpgKind:= TEnum<TProviderKind>.FromString(ReadString('EPG/ProviderKind',''), Local);
  fListProperties.EpgFileName:= ReadString('EPG/FileName','');
  fListProperties.EpgUrl:= ReadString('EPG/Url','');


  fListProperties.UseChno := ReadBoolean('m3u/UseChno', false);
  fListProperties.ChannelsDownloadLogo := ReadBoolean('m3u/DownloadLogo', false);

  FGuiProperties.ViewLogo := ReadBoolean('gui/ViewLogo', false);
  FGuiProperties.ViewCurrentProgram := ReadBoolean('gui/ViewCurrentProgram', false);

  FMPVProperties.HardwareAcceleration := ReadBoolean('MPV/HardwareAcceleration', true);

  FListChanged := False;
  FEPGChanged := False;
end;

procedure TConfig.WriteStrings(const APath: string; Values: TStrings);
var
  Node: TJsonNode;
  i: Integer;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
    begin
     Node.Clear;
     for i := 0 to Values.Count -1 do
       node.Add('',Values[i]);
    end
  else
    begin
      Node := fConfigHolder.find(APath, true);  // fConfigHolder.Add(APath, nkArray);
      node.Kind:=nkArray;
      for i := 0 to Values.Count -1 do
        node.Add('',Values[i]);

    end;
end;

function TConfig.ReadStrings(const APath: string; Values: TStrings): integer;
var
  Node: TJsonNode;
  i: Integer;
begin
  Values.Clear;
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
    begin
      for i := 0 to node.Count -1 do
       Values.Add(Node.Child(i).AsString);
    end;

  Result := Values.Count;
end;

procedure TConfig.WriteString(const APath: string; const Value: String);
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Node.AsString := Value
  else
     begin
       fConfigHolder.find(APath, true).AsString := Value;
     end;

end;

function TConfig.ReadString(const APath: string; const ADefault: String): string;
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Result :=  Node.AsString
  else
     Result :=  ADefault;
end;

procedure TConfig.WriteBoolean(const APath: string; Value: Boolean);
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Node.AsBoolean := Value
  else
     fConfigHolder.find(APath, true).AsBoolean := Value;

end;

function TConfig.ReadBoolean(const APath: string; ADefault: Boolean): Boolean;
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath, true);
  if Assigned(Node) then
     Result :=  Node.AsBoolean
  else
    Result :=  ADefault;
end;

procedure TConfig.WriteInteger(const APath: string; Value: Integer);
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Node.AsInteger := Value
  else
     begin
       fConfigHolder.find(APath, true).AsInteger := Value;
     end;

end;

function TConfig.ReadInteger(const APath: string; ADefault: Integer): Integer;
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Result :=  Node.AsInteger
  else
     Result :=  ADefault;
end;

procedure TConfig.Flush;
begin
  fConfigHolder.SaveToFile(FConfigFile, true);
end;

function TConfig.GetResourcesPath: string;
{$ifdef DARWIN}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef DARWIN}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := ResourcesPath;
{$endif}
{$endif}

{$ifdef WINDOWS}
  Result := ExtractFilePath(ExtractFilePath(ParamStr(0))) + ResourceSubDirectory + PathDelim;
{$endif}

end;

initialization
  FConfigObj := nil;

finalization
  if Assigned(FConfigObj) then
  begin
    FConfigObj.SaveConfig;
    FConfigObj.Free;
  end;


end.
