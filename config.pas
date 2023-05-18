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
unit Config;

interface

uses
  Classes, SysUtils, Graphics, JsonTools, typinfo, Generics.collections;

type
  { TEnum }

  TEnum<T> = class(TObject)
  public
    class function ToString(const aEnumValue: T): string; reintroduce;
    class function FromString(const aEnumString: string; const aDefault: T): T;
  end;

{ TConfig }
  TConfig = class;

  { TConfigParam }
  TConfigParam = class(TObject)
  private
    FDirty: boolean;
    fOwner: TConfig;
    procedure SetDirty(AValue: boolean);
  Protected
    Procedure InternalSave; virtual; abstract;
  public
    property Dirty: boolean read FDirty write SetDirty;
    property Owner: TConfig read fOwner;
    Constructor Create(aOwner:TConfig); virtual;
    Destructor Destroy; override;
    Procedure Save;
    Procedure Load; virtual; abstract;
  end;

  TConfigList= TObjectList<TConfigParam>;

  TConfig = class
  private
    fConfigList:  TConfigList;
    fDirty: boolean;
    fCacheDir: string;
    FConfigFile: string;
    fConfigDir: string;
    FPortableMode: boolean;
    ResourcesPath: string;
    fConfigHolder: TJsonNode;
    fExecutableDir: string;
    function GetCacheDir: string;
    function GetConfigDir: string;
    procedure SetDirty(AValue: boolean);
    procedure Attach(cfgobject: TConfigParam);
    procedure Remove(cfgobject: TConfigParam);

  public
    property PortableMode: boolean read FPortableMode;

    Property Dirty: boolean read FDirty write SetDirty;

    // Used to signal changes, not saved
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
    procedure WriteRect(const APath: string; Value: TRect);
    function ReadRect(const APath: string; ADefault: TRect): TRect;

    procedure Flush;
    constructor Create;
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

{ TConfigParam }

procedure TConfigParam.SetDirty(AValue: boolean);
begin
  if FDirty=AValue then Exit;
  FDirty:=AValue;
  if FDirty then
    fOwner.Dirty:=true;
end;

constructor TConfigParam.Create(aOwner: TConfig);
begin
  fOwner := AOwner;
  fOwner.Attach(Self);
  FDirty:=False;
end;

destructor TConfigParam.Destroy;
begin
  Save;
  fOwner.Remove(Self);

  inherited Destroy;
end;

procedure TConfigParam.Save;
begin
  if FDirty then
    InternalSave;

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

procedure TConfig.Attach(cfgobject: TConfigParam);
begin
  fConfigList.Add(cfgobject);
  cfgobject.Load;
end;

procedure TConfig.Remove(cfgobject: TConfigParam);
begin
  cfgobject.Save;
  fConfigList.Remove(cfgobject);
end;

constructor TConfig.Create;
begin
  fDirty:= False;
  fConfigList:= TConfigList.Create(True);

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
  fConfigList.Free;
  fConfigHolder.Free;
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

procedure TConfig.SetDirty(AValue: boolean);
begin
  if FDirty=AValue then Exit;
  FDirty:=AValue;

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
var
  i: integer;
begin
  fDirty:= false;
  for i := 0 to Pred(fConfigList.Count) do
    if fConfigList[i].Dirty then
       begin
         fConfigList[i].Save;
         fConfigList[i].Dirty:=false;
         FDirty:= true;
       end;
  if fDirty then
    begin
      WriteString(SectionUnix+'/'+IdentResourcesPath, ResourcesPath);
      fConfigHolder.SaveToFile(FConfigFile, true);
    end;

  fDirty := false;

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

procedure TConfig.WriteRect(const APath: string; Value: TRect);
begin
  WriteInteger(APath+'/Top',Value.Top);
  WriteInteger(APath+'/Left',Value.Left);
  WriteInteger(APath+'/Heigth',Value.Height);
  WriteInteger(APath+'/Width',Value.Width);
end;

function TConfig.ReadRect(const APath: string; ADefault: TRect): TRect;
begin
  Result.Top:= ReadInteger(APath+'/Top',ADefault.Top);
  Result.Left:= ReadInteger(APath+'/Left',ADefault.Left);
  Result.Height:= ReadInteger(APath+'/Heigth',ADefault.Height);
  Result.Width:= ReadInteger(APath+'/Width',ADefault.Width);
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
