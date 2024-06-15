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
  Classes, SysUtils, Graphics, JsonTools, typinfo, sqlite3dyn, sqlite3conn, sqldb, Generics.collections;

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
  protected
    procedure InternalSave; virtual; abstract;
  public
    property Dirty: boolean read FDirty write SetDirty;
    property Owner: TConfig read fOwner;
    constructor Create(aOwner: TConfig); virtual;
    destructor Destroy; override;
    procedure Save;
    procedure Load; virtual; abstract;
  end;

  TConfigList = TObjectList<TConfigParam>;


  TProviderKind = (Local, URL);

  { TM3UList }

  TM3UList = class
  private
    FChannelsDownloadLogo: boolean;
    FChannelsFileName: string;
    FChannelsUrl: string;
    FEPGUrl: string;
    FListID: integer;
    FName: string;
    FUseChno: boolean;
    procedure SetChannelsDownloadLogo(AValue: boolean);
    procedure SetChannelsFileName(AValue: string);
    procedure SetChannelsUrl(AValue: string);
    procedure SetEPGUrl(AValue: string);
    procedure SetListID(AValue: integer);
    procedure SetName(AValue: string);
    procedure SetUseChno(AValue: boolean);
  public
    property ListID: integer read FListID write SetListID;
    property Name: string read FName write SetName;
    property EPGUrl: string read FEPGUrl write SetEPGUrl;
    property ChannelsDownloadLogo: boolean read FChannelsDownloadLogo write SetChannelsDownloadLogo;
    property ChannelsUrl: string read FChannelsUrl write SetChannelsUrl;
    property UseChno: boolean read FUseChno write SetUseChno;
    function ChannelKind: TProviderKind;
    function EpgKind: TProviderKind;
    procedure Load(List: integer); overload;
    procedure Load; overload;
  end;

  { TListsManager }


  { TListProperties }

  TListProperties = class(TConfigParam)
  private
    FCurrentList: integer;
    procedure SetCurrentList(AValue: integer);
  protected
    procedure InternalSave; override;
  public
    property CurrentList: integer read FCurrentList write SetCurrentList;
    procedure Load; override;
  end;

  TListsManager = class(TObjectList<TM3UList>)
  private
    fOwner: TConfig;
    fListProperties: TListProperties;
  public
    procedure Load;
    procedure Save;
    constructor Create(Owner: TConfig); overload;
    destructor Destroy; override;
    function LastChannelMd5(ListID: int64): string;
    function LastScan(ListID: int64; const ScanType: string): TDateTime;
    procedure SetLastChannelMd5(ListID: int64; const ComputedMD5: string);
    procedure SetLastScan(ListID: int64; ScanType: string; Date: TdateTime);
    procedure ListAdd(var List: TM3UList);
    function ListDelete(List: TM3UList): boolean;


  end;

  TConfig = class
  private
    fConfigList: TConfigList;
    fDirty: boolean;
    fCacheDir: string;
    FConfigFile: string;
    fConfigDir: string;
    FListManager: TListsManager;
    FPortableMode: boolean;
    ResourcesPath: string;
    fConfigHolder: TJsonNode;
    fExecutableDir: string;
    fDB: TSQLite3Connection;
    fTR: TSQLTransaction;

    procedure CheckDBStructure;
    function GetCacheDir: string;
    function GetConfigDir: string;
    function GetDbVersion: integer;
    procedure SetDirty(AValue: boolean);
    procedure Attach(cfgobject: TConfigParam);
    procedure Remove(cfgobject: TConfigParam);
    procedure SetupDBConnection;
    procedure UpgradeDBStructure(LoadedDBVersion: integer);

  public
    property PortableMode: boolean read FPortableMode;

    property Dirty: boolean read FDirty write SetDirty;

    // Used to signal changes, not saved
    procedure ReadConfig;
    procedure SaveConfig;
    procedure WriteStrings(const APath: string; Values: TStrings);
    function ReadStrings(const APath: string; Values: TStrings): integer;
    procedure WriteString(const APath: string; const Value: string);
    function ReadString(const APath: string; const ADefault: string): string;
    function GetResourcesPath: string;
    procedure WriteBoolean(const APath: string; Value: boolean);
    function ReadBoolean(const APath: string; ADefault: boolean): boolean;
    procedure WriteInteger(const APath: string; Value: integer);
    function ReadInteger(const APath: string; ADefault: integer): integer;
    procedure WriteRect(const APath: string; Value: TRect);
    function ReadRect(const APath: string; ADefault: TRect): TRect;

    property ListManager: TListsManager read FListManager write FListManager;

    procedure Flush;
    constructor Create;
    destructor Destroy; override;
    // -- //
    property ConfigDir: string read fConfigDir;
    property CacheDir: string read fCacheDir;
    property ConfigFile: string read FConfigFile;
    property DB: TSQLite3Connection read fDB;
    property TR: TSQLTransaction read fTR;
  end;

  { TSimpleHistory }

  TSimpleHistory = class
  private
    FMax: integer;
    IntList: TStringList;
    function GetCount: integer;
    procedure SetMax(AValue: integer);
  public
    function Add(const S: string): integer;
    constructor Create;
    destructor Destroy; override;
    procedure SetList(List: TStrings);

    procedure LoadFromConfig(Config: TConfig; APath: string);
    procedure WriteToConfig(Config: TConfig; APath: string);
    property Max: integer read FMax write SetMax;
    property Count: integer read GetCount;
  end;


function ConfigObj: TConfig;

implementation

{ TConfig }
uses
  Fileutil, AppConsts, LoggerUnit
  // only for default font !
  {$ifdef Darwin}
  , MacOSAll
  {$endif}  ;

var
  FConfigObj: TConfig;

const
  PRAGMAS_COUNT = 3;
  PRAGMAS: array [1..PRAGMAS_COUNT] of string =
    (
    //            'PRAGMA locking_mode = EXCLUSIVE;',
    'PRAGMA temp_store = MEMORY;',
    'PRAGMA count_changes = 0;',
    'PRAGMA encoding = "UTF-8";'
    );
  CURRENTDBVERSION = 3;

  CREATECONFIGTABLE1 =
    'CREATE TABLE config ('
    + 'Version INTEGER COLLATE NOCASE'
    + ');';

  CREATELISTTABLE =
    'CREATE TABLE "m3ulists" ('
    + 'ID INTEGER'
    + ',Name VARCHAR'
    + ',Position VARCHAR'
    + ',UseNumber INTEGER'
    + ',GetLogo INTEGER'
    + ',EPG VARCHAR'
    + ',PRIMARY KEY("ID" AUTOINCREMENT))';
  CREATECONFIGTABLE2 =
    ' INSERT INTO config (Version) VALUES(1);';
  UPDATECONFIG =
    'UPDATE config SET Version = %d;';

  CREATESCANTABLE1 =
    'CREATE TABLE scans ('
    + ' List Integer'
    + ' ,Epg DATETIME'
    + ' ,Channels DATETIME'
    + ',ChannelsMd5 VARCHAR  '
    + ',PRIMARY KEY("List"))';
  CREATESCANTABLE2 =
    'insert into  scans select 0,0,0,null where not EXISTS (select * from scans);';

  CREATECHANNELTABLE =
    'CREATE TABLE channels ('
    + ' List Integer '
    + ',ID INTEGER'
    + ',Name VARCHAR COLLATE NOCASE'
    + ',ChannelNo VARCHAR COLLATE NOCASE'
    + ',EpgName VARCHAR COLLATE NOCASE'
    + ', primary key (ID AUTOINCREMENT) '
    + ')';
  CREATECHANNELINDEX1 =
    'CREATE INDEX "idx_Channels_Name" on channels (Name ASC);';
  CREATECHANNELINDEX2 =
    'CREATE INDEX "idx_Channels_EpgName" on channels (EpgName ASC);';
  CREATECHANNELINDEX3 =
    '  CREATE UNIQUE INDEX idx_Channels_List ON channels (List, ID);';


  CREATEPROGRAMMETABLE =
    'CREATE TABLE programme ('
    + ' List Integer '
    + ',idProgram    integer '
    + ',idChannel    integer'
    + ',sTitle       VARCHAR(128)'
    + ',sPlot        VARCHAR'
    + ',dStartTime   DATETIME'
    + ',dEndTime     DATETIME'
    + ', primary key (idProgram AUTOINCREMENT) '
    + ');';
  CREATEPROGRAMMEINDEX1 =
    'CREATE INDEX "idx_programme_Channel" on programme (idChannel, dStartTime ASC);';
  CREATEPROGRAMMEINDEX2 =
    'CREATE INDEX "idx_programme_iStartTime" on programme (dStartTime ASC);';
  CREATEPROGRAMMEINDEX3 =
    '  CREATE UNIQUE INDEX idx_programme_List ON programme (List, idProgram);';

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

function NextToken(const S: string; var SeekPos: integer;
  const TokenDelim: char): string;
var
  TokStart: integer;
begin
  repeat
    if SeekPos > Length(s) then
    begin
      Result := '';
      Exit;
    end;
    if S[SeekPos] = TokenDelim then Inc(SeekPos)
    else
      Break;
  until False;
  TokStart := SeekPos; { TokStart := first character not in TokenDelims }

  while (SeekPos <= Length(s)) and not (S[SeekPos] = TokenDelim) do Inc(SeekPos);

  { Calculate result := s[TokStart, ... , SeekPos-1] }
  Result := Copy(s, TokStart, SeekPos - TokStart);

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
  OrdValue: integer;
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
  if FDirty = AValue then Exit;
  FDirty := AValue;
  if FDirty then
    fOwner.Dirty := True;
end;

constructor TConfigParam.Create(aOwner: TConfig);
begin
  fOwner := AOwner;
  fOwner.Attach(Self);
  FDirty := False;
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

procedure TSimpleHistory.SetMax(AValue: integer);
begin
  if FMax = AValue then Exit;
  FMax := AValue;

  while IntList.Count > FMax do
    IntList.Delete(IntList.Count - 1);           // -1 since its 0 indexed

end;

function TSimpleHistory.GetCount: integer;
begin
  Result := IntList.Count;
end;

function TSimpleHistory.Add(const S: string): integer;
var
  i: integer;
begin
  i := IntList.IndexOf(S);
  if i <> -1 then
    IntList.Delete(i);

  IntList.Insert(0, S);

  // Trim the oldest files if more than NumFiles
  while IntList.Count > FMax do
    IntList.Delete(IntList.Count - 1);           // -1 since its 0 indexed
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
  fDirty := False;
  fConfigList := TConfigList.Create(True);

  fExecutableDir := IncludeTrailingPathDelimiter(ProgramDirectory);

  if FileExists(fExecutableDir + 'portable.txt') then
    fPortableMode := True;

  fConfigDir := GetConfigDir;
  fCacheDir := GetCacheDir;


  if FPortableMode then
  begin
    FConfigFile := fConfigDir + ApplicationName + ConfigExtension;
  end
  else
  begin
    FConfigFile := GetAppConfigFile(False
      {$ifdef NEEDCFGSUBDIR}
      , True
      {$ENDIF}
      );

  end;

  SetupDBConnection;
  CheckDBStructure;
  fConfigHolder := TJsonNode.Create;

  FListManager := TListsManager.Create(Self);
  FListManager.Load;


  if not FileExists(FConfigFile) then
    SaveConfig;
  ReadConfig;
end;

destructor TConfig.Destroy;
begin
  SaveConfig;
  fConfigList.Free;
  fConfigHolder.Free;
  FListManager.Free;
  fTR.Commit;
  fDB.Transaction := nil;
  fDB.Connected := False;
  fTR.Free;
  fDB.Free;
  inherited Destroy;
end;


function TConfig.GetConfigDir: string;
var
  Path: string;
begin
  if fPortableMode then
    Path := fExecutableDir + 'config'
  else
    Path := GetAppConfigDir(False);
  ForceDirectories(Path);
  Result := IncludeTrailingPathDelimiter(Path);

end;

procedure TConfig.SetDirty(AValue: boolean);
begin
  if FDirty = AValue then Exit;
  FDirty := AValue;

end;

function TConfig.GetCacheDir: string;
begin
  if FPortableMode then
  begin
    Result := fExecutableDir + 'cache';
    Result := IncludeTrailingPathDelimiter(Result);
  end
  else
  begin
    {$ifdef UNIX}
    Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
    if (Result = '') then
    begin
      Result := GetEnvironmentVariable('HOME');
      if Result <> '' then
        Result := IncludeTrailingPathDelimiter(Result) + '.cache/';
    end
    else
      Result := IncludeTrailingPathDelimiter(Result);

    Result := IncludeTrailingPathDelimiter(Result + ApplicationName);
    {$endif}
    {$ifdef WINDOWS}
    Result := GetEnvironmentVariable('LOCALAPPDATA');
    if Result <> '' then
      Result := IncludeTrailingPathDelimiter(Result) + 'Caches\';

    Result := IncludeTrailingPathDelimiter(Result + ApplicationName);
    {$endif}
  end;
  ForceDirectories(Result);
end;

procedure TConfig.SaveConfig;
var
  i: integer;
begin
  fDirty := False;
  for i := 0 to Pred(fConfigList.Count) do
    if fConfigList[i].Dirty then
    begin
      fConfigList[i].Save;
      fConfigList[i].Dirty := False;
      FDirty := True;
    end;
  if fDirty then
  begin
    WriteString(SectionUnix + '/' + IdentResourcesPath, ResourcesPath);
    fConfigHolder.SaveToFile(FConfigFile, True);
  end;

  ListManager.Save;
  fDirty := False;
  fTR.CommitRetaining;

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

  ListManager.Load;

end;

procedure TConfig.WriteStrings(const APath: string; Values: TStrings);
var
  Node: TJsonNode;
  i: integer;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
  begin
    Node.Clear;
    for i := 0 to Values.Count - 1 do
      node.Add('', Values[i]);
  end
  else
  begin
    Node := fConfigHolder.find(APath, True);  // fConfigHolder.Add(APath, nkArray);
    node.Kind := nkArray;
    for i := 0 to Values.Count - 1 do
      node.Add('', Values[i]);

  end;
end;

function TConfig.ReadStrings(const APath: string; Values: TStrings): integer;
var
  Node: TJsonNode;
  i: integer;
begin
  Values.Clear;
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
  begin
    for i := 0 to node.Count - 1 do
      Values.Add(Node.Child(i).AsString);
  end;

  Result := Values.Count;
end;

procedure TConfig.WriteString(const APath: string; const Value: string);
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
    Node.AsString := Value
  else
  begin
    fConfigHolder.find(APath, True).AsString := Value;
  end;

end;

function TConfig.ReadString(const APath: string; const ADefault: string): string;
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
    Result := Node.AsString
  else
    Result := ADefault;
end;

procedure TConfig.WriteBoolean(const APath: string; Value: boolean);
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
    Node.AsBoolean := Value
  else
    fConfigHolder.find(APath, True).AsBoolean := Value;

end;

function TConfig.ReadBoolean(const APath: string; ADefault: boolean): boolean;
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath, True);
  if Assigned(Node) then
    Result := Node.AsBoolean
  else
    Result := ADefault;
end;

procedure TConfig.WriteInteger(const APath: string; Value: integer);
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
    Node.AsInteger := Value
  else
  begin
    fConfigHolder.find(APath, True).AsInteger := Value;
  end;

end;

function TConfig.ReadInteger(const APath: string; ADefault: integer): integer;
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
    Result := Node.AsInteger
  else
    Result := ADefault;
end;

procedure TConfig.WriteRect(const APath: string; Value: TRect);
begin
  WriteInteger(APath + '/Top', Value.Top);
  WriteInteger(APath + '/Left', Value.Left);
  WriteInteger(APath + '/Heigth', Value.Height);
  WriteInteger(APath + '/Width', Value.Width);
end;

function TConfig.ReadRect(const APath: string; ADefault: TRect): TRect;
begin
  Result.Top := ReadInteger(APath + '/Top', ADefault.Top);
  Result.Left := ReadInteger(APath + '/Left', ADefault.Left);
  Result.Height := ReadInteger(APath + '/Heigth', ADefault.Height);
  Result.Width := ReadInteger(APath + '/Width', ADefault.Width);
end;

procedure TConfig.Flush;
begin
  fConfigHolder.SaveToFile(FConfigFile, True);
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


procedure TConfig.SetupDBConnection;
var
  i: integer;
begin
  OvoLogger.Log(llINFO, 'Setup EPG database');
  fDB := TSQLite3Connection.Create(nil);
  fDB.OpenFlags := [sofReadWrite, sofCreate, sofFullMutex, sofSharedCache];
  fDB.DatabaseName := FConfigDir + EPGLibraryName;

  ftr := TSQLTransaction.Create(nil);

  fTR.DataBase := fDB;

  for i := 1 to PRAGMAS_COUNT do
    fdb.ExecuteDirect(PRAGMAS[i]);

  fdb.Connected := True;

  fTR.Active := True;

end;

function TConfig.GetDbVersion: integer;
var
  TableList: TStringList;
  tmpQuery: TSQLQuery;
begin
  TableList := TStringList.Create;
  try
    fDB.GetTableNames(TableList, False);
    if TableList.IndexOf('Config') < 0 then
    begin
      Result := 1;
      fDB.ExecuteDirect(CREATECONFIGTABLE1);
      fDB.ExecuteDirect(CREATECONFIGTABLE2);
      ftr.CommitRetaining;
    end
    else
    begin
      tmpQuery := TSQLQuery.Create(fDB);
      tmpQuery.DataBase := fDB;
      tmpQuery.Transaction := fTR;
      tmpQuery.SQL.Text := 'SELECT Version FROM Config';
      tmpQuery.Open;
      Result := tmpQuery.Fields[0].AsInteger;
      tmpQuery.Free;
    end;
  finally
    TableList.Free;
  end;

end;

procedure TConfig.CheckDBStructure;
var
  TableList: TStringList;
  LoadedDBVersion: integer;
begin
  OvoLogger.Log(llINFO, 'Check EPG database');
  try
    TableList := TStringList.Create;
    try
      fDB.GetTableNames(TableList, False);
      if TableList.IndexOf('config') < 0 then
      begin
        OvoLogger.Log(llDEBUG, 'Creating config table');
        fDB.ExecuteDirect(CREATECONFIGTABLE1);
        fDB.ExecuteDirect(CREATECONFIGTABLE2);
        fDB.ExecuteDirect(format(UPDATECONFIG, [CURRENTDBVERSION]));
        fTR.CommitRetaining;
      end;
      if TableList.IndexOf('m3ulists') < 0 then
      begin
        OvoLogger.Log(llDEBUG, 'Creating m3ulists table');
        fDB.ExecuteDirect(CREATELISTTABLE);
        fTR.CommitRetaining;
      end;
      if TableList.IndexOf('scans') < 0 then
      begin
        OvoLogger.Log(llDEBUG, 'Creating scans table');
        fDB.ExecuteDirect(CREATESCANTABLE1);
        fTR.CommitRetaining;
      end;
      // Make sure table contains a row
      fDB.ExecuteDirect(CREATESCANTABLE2);
      fTR.CommitRetaining;
      if TableList.IndexOf('channels') < 0 then
      begin
        OvoLogger.Log(llDEBUG, 'Creating channel table');
        fDB.ExecuteDirect(CREATECHANNELTABLE);
        fDB.ExecuteDirect(CREATECHANNELINDEX1);
        fDB.ExecuteDirect(CREATECHANNELINDEX2);
        fDB.ExecuteDirect(CREATECHANNELINDEX3);
        fTR.CommitRetaining;
      end;
      if TableList.IndexOf('programme') < 0 then
      begin
        OvoLogger.Log(llDEBUG, 'Creating programme table');
        fDB.ExecuteDirect(CREATEPROGRAMMETABLE);
        fDB.ExecuteDirect(CREATEPROGRAMMEINDEX1);
        fDB.ExecuteDirect(CREATEPROGRAMMEINDEX2);
        fDB.ExecuteDirect(CREATEPROGRAMMEINDEX3);
        fTR.CommitRetaining;
      end;

    finally
      TableList.Free;
    end;

  except
    on e: Exception do
      OvoLogger.Log(llERROR, 'Error initializing EPG Database : %s', [e.Message]);
  end;

  LoadedDBVersion := GetDbVersion;
  if LoadedDBVersion < CURRENTDBVERSION then
    UpgradeDBStructure(LoadedDBVersion);

end;

procedure TConfig.UpgradeDBStructure(LoadedDBVersion: integer);
const
  ToV2_1 = 'ALTER TABLE "channels" add COLUMN "epgName" varchar NULL;';
  UPDATESTATUS = 'UPDATE confid SET Version = %d;';
var
  MustUpdate: boolean;
begin
  MustUpdate := False;
  OvoLogger.Log(llINFO, 'Upgrading db version from %d to %d:', [LoadedDBVersion, CURRENTDBVERSION]);
  if LoadedDBVersion < 2 then
  begin
    fDB.ExecuteDirect(ToV2_1);
    MustUpdate := True;
  end;

  if LoadedDBVersion < 3 then
  begin
    fDB.ExecuteDirect('Drop table channels');
    fDB.ExecuteDirect('Drop table programme');
    fDB.ExecuteDirect('Drop table scans');
    FDB.ExecuteDirect(format(UPDATECONFIG, [CURRENTDBVERSION]));
    CheckDBStructure;

    MustUpdate := True;
  end;


  if MustUpdate then
    FDB.ExecuteDirect(format(UPDATECONFIG, [CURRENTDBVERSION]));

end;

{ TListsManager }

procedure TListsManager.Load;
var
  tmpQuery: TSQLQuery;
  wItem: TM3UList;
begin
  Clear;
  tmpQuery := TSQLQuery.Create(fOwner.fDB);
  try
    tmpQuery.DataBase := fOwner.fDB;
    tmpQuery.Transaction := fOwner.fTR;
    tmpQuery.SQL.Text := 'SELECT ID, Name, Position, UseNumber, GetLogo, EPG FROM m3ulists;';
    tmpQuery.Open;
    while not tmpQuery.EOF do
    begin
      wItem := TM3UList.Create;
      wItem.ListID := tmpQuery.FieldByName('ID').AsInteger;
      wItem.Name := tmpQuery.FieldByName('Name').AsString;
      wItem.ChannelsUrl := tmpQuery.FieldByName('Position').AsString;
      wItem.UseChno := tmpQuery.FieldByName('UseNumber').AsBoolean;
      wItem.ChannelsDownloadLogo := tmpQuery.FieldByName('GetLogo').AsBoolean;
      wItem.EPGUrl := tmpQuery.FieldByName('EPG').AsString;
      Add(wItem);
      tmpQuery.Next;
    end;

  finally
    tmpQuery.Free;
  end;

end;


procedure TListsManager.Save;
var
  tmpQuery: TSQLQuery;
  wItem: TM3UList;
begin
  tmpQuery := TSQLQuery.Create(fOwner.fDB);
  try
    tmpQuery.DataBase := fOwner.fDB;
    tmpQuery.Transaction := fOwner.fTR;
    tmpQuery.SQL.Text := 'INSERT OR REPLACE INTO m3ulists (ID, Name, Position, UseNumber, GetLogo, EPG) ' +
      'VALUES (:ID, :Name, :Position, :UseNumber, :GetLogo, :EPG);';
    for wItem in self do
    begin
      if wItem.ListID = 0 then
        tmpQuery.ParamByName('ID').Value := Null
      else
        tmpQuery.ParamByName('ID').AsInteger := wItem.ListID;

      tmpQuery.ParamByName('Name').AsString := wItem.Name;
      tmpQuery.ParamByName('Position').AsString := wItem.ChannelsUrl;
      tmpQuery.ParamByName('UseNumber').AsBoolean := wItem.UseChno;
      tmpQuery.ParamByName('GetLogo').AsBoolean := wItem.ChannelsDownloadLogo;
      tmpQuery.ParamByName('EPG').AsString := wItem.EPGUrl;
      tmpQuery.ExecSQL;
    end;

  finally
    tmpQuery.Free;
  end;

end;

constructor TListsManager.Create(Owner: TConfig);
begin
  inherited Create(True);
  FOwner := Owner;
  fListProperties := TListProperties.Create(FOwner);
end;

destructor TListsManager.Destroy;
begin
  //  fListProperties.Free;
  inherited Destroy;
end;


function TListsManager.LastChannelMd5(ListID: int64): string;
var
  tmpQuery: TSQLQuery;
begin
  try
    tmpQuery := TSQLQuery.Create(FOwner.DB);
    tmpQuery.DataBase := FOwner.DB;
    tmpQuery.Transaction := FOwner.TR;
    tmpQuery.ParamByName('list').AsInteger := ListID;
    tmpQuery.SQL.Text := 'SELECT ChannelsMd5  FROM Scans  where list =:list';
    tmpQuery.Open;
    if not tmpQuery.EOF then
      Result := tmpQuery.Fields[0].AsString
    else
      Result := '';

  finally
    tmpQuery.Free;
  end;
end;

procedure TListsManager.SetLastChannelMd5(ListID: int64; const ComputedMD5: string);
begin
  FOwner.DB.ExecuteDirect('update scans set ChannelsMd5 = ' + QuotedStr(ComputedMD5) + ' where list = ' + IntToStr(ListID));
end;

function TListsManager.LastScan(ListID: int64; const ScanType: string): TDateTime;
var
  tmpQuery: TSQLQuery;
begin
  try
    tmpQuery := TSQLQuery.Create(FOwner.DB);
    tmpQuery.DataBase := FOwner.DB;
    tmpQuery.Transaction := FOwner.TR;
    tmpQuery.SQL.Text := 'SELECT ' + ScanType + ' FROM Scans where list =:list';
    tmpQuery.ParamByName('list').AsInteger := ListID;
    tmpQuery.Open;
    if not tmpQuery.EOF then
      Result := tmpQuery.Fields[0].AsDateTime
    else
      Result := 0;

  finally
    tmpQuery.Free;
  end;

end;

procedure TListsManager.SetLastScan(ListID: int64; ScanType: string; Date: TdateTime);
var
  tmpQuery: TSQLQuery;
begin
  try
    tmpQuery := TSQLQuery.Create(ConfigObj.DB);
    tmpQuery.DataBase := ConfigObj.DB;
    tmpQuery.Transaction := ConfigObj.TR;
    tmpQuery.SQL.Text := 'UPDATE scans set ' + ScanType + ' =:date  where list =:list';
    tmpQuery.parambyname('date').AsDateTime := Date;
    tmpQuery.ParamByName('list').AsInteger := ListID;
    tmpQuery.ExecSQL;
    ConfigObj.TR.CommitRetaining;
  finally
    tmpQuery.Free;
  end;

end;

procedure TListsManager.ListAdd(var List: TM3UList);
var
  tmpQuery: TSQLQuery;
  wItem: TM3UList;
begin
  tmpQuery := TSQLQuery.Create(fOwner.fDB);
  try
    tmpQuery.DataBase := fOwner.fDB;
    tmpQuery.Transaction := fOwner.fTR;
    tmpQuery.SQL.Text := 'INSERT OR REPLACE INTO m3ulists (ID, Name, Position, UseNumber, GetLogo, EPG) ' +
      'VALUES (:ID, :Name, :Position, :UseNumber, :GetLogo, :EPG);';
    if List.ListID = 0 then
      tmpQuery.ParamByName('ID').Value := Null
    else
      tmpQuery.ParamByName('ID').AsInteger := List.ListID;

    tmpQuery.ParamByName('Name').AsString := List.Name;
    tmpQuery.ParamByName('Position').AsString := List.ChannelsUrl;
    tmpQuery.ParamByName('UseNumber').AsBoolean := List.UseChno;
    tmpQuery.ParamByName('GetLogo').AsBoolean := List.ChannelsDownloadLogo;
    tmpQuery.ParamByName('EPG').AsString := List.EPGUrl;
    tmpQuery.ExecSQL;
    List.ListID:= fOwner.fDB.GetInsertID;

  finally
    tmpQuery.Free;
  end;

end;


function TListsManager.ListDelete(List: TM3UList): boolean;
var
  q: TSQLQuery;
begin
  Result := False;
  q := TSQLQuery.Create(fOwner.fDB);
  try
    q.DataBase := fOwner.fDB;
    q.Transaction := fOwner.fTR;
    try
      q.SQL.Text := 'Delete from M3ULists where ID = :ID ';
      q.ParamByName('ID').AsLargeInt := List.ListID;
      q.ExecSQL;
      q.SQL.Text := 'Delete from channels where list = :ID ';
      q.ParamByName('ID').AsLargeInt := List.ListID;
      q.ExecSQL;
      q.SQL.Text := 'Delete from programme where list = :ID ';
      q.ParamByName('ID').AsLargeInt := List.ListID;
      q.ExecSQL;
      q.SQL.Text := 'Delete from scans where list = :ID ';
      q.ParamByName('ID').AsLargeInt := List.ListID;
      q.ExecSQL;
      Delete(IndexOf(List));

      fOwner.fTR.CommitRetaining;
      Result := True;
    except
      fowner.fTR.RollbackRetaining;
    end;


  finally
    q.Free;

  end;

end;

{ TM3UList }

procedure TM3UList.SetChannelsDownloadLogo(AValue: boolean);
begin
  if FChannelsDownloadLogo = AValue then Exit;
  FChannelsDownloadLogo := AValue;
end;

procedure TM3UList.SetChannelsFileName(AValue: string);
begin
  if FChannelsFileName = AValue then Exit;
  FChannelsFileName := AValue;
end;

procedure TM3UList.SetChannelsUrl(AValue: string);
begin
  if FChannelsUrl = AValue then Exit;
  FChannelsUrl := AValue;
end;

procedure TM3UList.SetEPGUrl(AValue: string);
begin
  if FEPGUrl = AValue then Exit;
  FEPGUrl := AValue;
end;

procedure TM3UList.SetListID(AValue: integer);
begin
  if FListID = AValue then Exit;
  FListID := AValue;
end;

procedure TM3UList.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TM3UList.SetUseChno(AValue: boolean);
begin
  if FUseChno = AValue then Exit;
  FUseChno := AValue;
end;

function TM3UList.ChannelKind: TProviderKind;
begin
  if FChannelsUrl.ToLower.StartsWith('http://') or FChannelsUrl.ToLower.StartsWith('https://') then
    Result := URL
  else
    Result := Local;
end;

function TM3UList.EpgKind: TProviderKind;
begin
  if FEPGUrl.ToLower.StartsWith('http://') or FEPGUrl.ToLower.StartsWith('https://') then
    Result := URL
  else
    Result := Local;
end;

procedure TM3UList.Load(List: integer);
var
  qList: TSQLQuery;
begin
  FListId := List;
  qList := TSQLQuery.Create(ConfigObj.DB);
  try
    qList.Transaction := ConfigObj.TR;
    qList.SQL.Text := 'SELECT ID,Name,Position,UseNumber,GetLogo,EPG from m3ulists where ID = :list;';
    qList.ParamByName('list').AsInteger := List;
    qList.Open;
    if not qList.EOF then
    begin
      FChannelsUrl := qList.FieldByName('Position').AsString;
      FChannelsDownloadLogo := qList.FieldByName('GetLogo').AsBoolean;
      FUseChno := qList.FieldByName('UseNumber').AsBoolean;
      FName := qList.FieldByName('Name').AsString;
      FEPGUrl := qList.FieldByName('EPG').AsString;
    end;
  finally
    qList.Free;
  end;
end;

procedure TM3UList.Load;
begin
  Load(FListID);
end;

{ TListProperties }


procedure TListProperties.SetCurrentList(AValue: integer);
begin
  if FCurrentList = AValue then Exit;
  FCurrentList := AValue;
  Dirty := True;
end;


procedure TListProperties.InternalSave;
begin
  Owner.WriteInteger('m3u/CurrentList', CurrentList);
end;

procedure TListProperties.Load;
begin
  CurrentList := Owner.ReadInteger('m3u/CurrentList', 0);
  Dirty := False;
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
