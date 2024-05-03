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
unit epg;

interface

uses
  Classes, SysUtils, DateUtils, laz2_XMLRead, Laz2_DOM, sqldb, um3uloader, BaseTypes, AppConsts, Config, Generics.Collections;

type
  TEpg = class;

  TChannelData = class
    EpgId: string;
    Id: integer;
  end;

  { TEpgScanner }

  TEpgScanner = class(TThread)
  private
    FCurrentList: integer;
    FOnEndWork: TNotifyEvent;
    FScanEvent: PRTLEvent;
    XMLDoc: TXMLDocument;
    Root: tdomNode;
    fOwner: TEpg;
    Success: boolean;
    FStopped: boolean;
    function FindChannelIdByEPGName(Channel: string): integer;
    function FindChannelIdByName(Channel: string): integer;
    procedure SetCurrentList(AValue: integer);
    procedure SetOnEndWork(AValue: TNotifyEvent);

  protected
    procedure Execute; override;
    function Load(EPGFile: string): integer;
    procedure DoEndWork;
    procedure TerminatedSet; override;
    function StoppedCheck: boolean;
  public
    procedure Stop;
    property OnEndWork: TNotifyEvent read FOnEndWork write SetOnEndWork;
    property ScanEvent: PRTLEvent read FScanEvent;
    property CurrentList: integer read FCurrentList write SetCurrentList;
    constructor Create(Owner: TEpg); reintroduce;
    destructor Destroy; override;
  end;

  { TListProperties }

  { TEpgProperties }

  TEpgProperties = class(TConfigParam)
  private
    FEpgFileName: string;
    FEpgKind: TProviderKind;
    FEPGUrl: string;
    procedure SetEpgFileName(AValue: string);
    procedure SetEpgKind(AValue: TProviderKind);
    procedure SetEPGUrl(AValue: string);
  protected
    procedure InternalSave; override;
  public
    property EpgFileName: string read FEpgFileName write SetEpgFileName;
    property EpgKind: TProviderKind read FEpgKind write SetEpgKind;
    property EPGUrl: string read FEPGUrl write SetEPGUrl;
    procedure Load; override;
  end;

  TEpg = class
  private
    FCurrentList: integer;
    fEpgAvailable: boolean;
    fEpgProperties: TEpgProperties;
    FOnScanComplete: TNotifyEvent;
    FOnScanStart: TNotifyEvent;
    fScanning: boolean;
    Scanner: TEpgScanner;
    procedure AfterScan;
    procedure EndScan(AObject: TObject);
    procedure SetCurrentList(AValue: integer);
  public
    property EpgProperties: TEpgProperties read fEpgProperties;
    property OnScanComplete: TNotifyEvent read FOnScanComplete write FOnScanComplete;
    property OnScanStart: TNotifyEvent read FOnScanStart write FOnScanStart;
    property EpgAvailable: boolean read fEpgAvailable;
    property Scanning: boolean read fScanning;
    property CurrentList: integer read FCurrentList write SetCurrentList;
    constructor Create;
    destructor Destroy; override;
    function LastScan(const ScanType: string): TDateTime;
    function LastChannelMd5: string;
    procedure LoadChannelList(List: TM3ULoader);
    procedure Scan;
    procedure SetLastScan(ScanType: string; Date: TdateTime);
    procedure SetLastChannelMd5(const ComputedMD5: string);

    function GetEpgInfo(Channel: integer; CurrTime: TDateTime): REpgInfo; overload;
    function GetEpgInfo(Channel: integer; StartTime: TDateTime; EndTime: TDateTime): AREpgInfo; overload;
    function GetEpgInfo(const SearchTerm: string): AREpgInfo; overload;
  end;


implementation

uses GeneralFunc, LoggerUnit, ZStream;

const
  INSERTPROGRAMME = 'INSERT INTO "programme"("List","idProgram","idChannel","sTitle","sPlot","dStartTime","dEndTime")' + ' values  (:list, NULL,:idChannel,:sTitle,:sPlot,:dStartTime,:dEndTime);';

  { TEpgProperties }

procedure TEpgProperties.SetEpgFileName(AValue: string);
begin
  if FEpgFileName = AValue then Exit;
  FEpgFileName := AValue;
  Dirty := True;

end;

procedure TEpgProperties.SetEpgKind(AValue: TProviderKind);
begin
  if FEpgKind = AValue then Exit;
  FEpgKind := AValue;
  Dirty := True;

end;

procedure TEpgProperties.SetEPGUrl(AValue: string);
begin
  if FEPGUrl = AValue then Exit;
  FEPGUrl := AValue;
  Dirty := True;

end;


procedure TEpgProperties.InternalSave;
begin
  Owner.WriteString('EPG/ProviderKind', TEnum<TProviderKind>.ToString(EPGKind));
  Owner.WriteString('EPG/FileName', EPGFileName);
  Owner.WriteString('EPG/Url', EPGUrl);
end;

procedure TEpgProperties.Load;
begin
  EpgKind := TEnum<TProviderKind>.FromString(Owner.ReadString('EPG/ProviderKind', ''), Local);
  EpgFileName := Owner.ReadString('EPG/FileName', '');
  EpgUrl := Owner.ReadString('EPG/Url', '');
  Dirty := False;
end;

procedure TEpg.SetCurrentList(AValue: integer);
begin
  if FCurrentList = AValue then Exit;
  FCurrentList := AValue;
end;

function TEpg.LastChannelMd5: string;
var
  tmpQuery: TSQLQuery;
begin
  try
    tmpQuery := TSQLQuery.Create(ConfigObj.DB);
    tmpQuery.DataBase := ConfigObj.DB;
    tmpQuery.Transaction := ConfigObj.TR;
    tmpQuery.SQL.Text := 'SELECT ChannelsMd5  FROM Scans';
    tmpQuery.Open;
    if not tmpQuery.EOF then
      Result := tmpQuery.Fields[0].AsString
    else
      Result := '';

  finally
    tmpQuery.Free;
  end;
end;

procedure TEpg.SetLastChannelMd5(const ComputedMD5: string);
begin
  ConfigObj.DB.ExecuteDirect('update scans set ChannelsMd5 = ' + QuotedStr(ComputedMD5) + ' where list = ' + IntToStr(FCurrentList));
end;

function TEpg.LastScan(const ScanType: string): TDateTime;
var
  tmpQuery: TSQLQuery;
begin
  try
    tmpQuery := TSQLQuery.Create(ConfigObj.DB);
    tmpQuery.DataBase := ConfigObj.DB;
    tmpQuery.Transaction := ConfigObj.TR;
    tmpQuery.SQL.Text := 'SELECT ' + ScanType + ' FROM Scans where list =:list';
    tmpQuery.ParamByName('list').AsInteger := FCurrentList;
    tmpQuery.Open;
    if not tmpQuery.EOF then
      Result := tmpQuery.Fields[0].AsDateTime
    else
      Result := 0;

  finally
    tmpQuery.Free;
  end;

end;

procedure TEpg.SetLastScan(ScanType: string; Date: TdateTime);
var
  tmpQuery: TSQLQuery;
begin
  try
    tmpQuery := TSQLQuery.Create(ConfigObj.DB);
    tmpQuery.DataBase := ConfigObj.DB;
    tmpQuery.Transaction := ConfigObj.TR;
    tmpQuery.SQL.Text := 'UPDATE scans set ' + ScanType + ' =:date';
    tmpQuery.parambyname('date').AsDateTime := Date;
    tmpQuery.ExecSQL;
    ConfigObj.TR.CommitRetaining;
  finally
    tmpQuery.Free;
  end;

end;

procedure TEpg.EndScan(AObject: TObject);
begin
  if not TEpgScanner(AObject).Success then
    OvoLogger.Log(llINFO, 'EPG update Not completed');
  OvoLogger.Log(llINFO, 'EPG update thread stopped');
  AfterScan;

  if Assigned(FOnScanComplete) then
    FOnScanComplete(Self);

  fScanning := False;

end;


procedure TEpg.AfterScan;
begin
  ConfigObj.TR.CommitRetaining;
  fEpgAvailable := True;
  fScanning := False;
end;


procedure TEpg.Scan;
begin
  if Assigned(Scanner) then
  begin
    Scanner.Stop;
  end;

  if LastScan('epg') + 12 / 24 > now then
  begin
    OvoLogger.Log(llINFO, 'Skipping EPG update, used cache');
    AfterScan;
    exit;
  end;
  RTLEventSetEvent(Scanner.FScanEvent);

  fEpgAvailable := True;
  if Assigned(FOnScanStart) then
    FOnScanStart(self);

end;

function TEpg.GetEpgInfo(Channel: integer; CurrTime: TDateTime): REpgInfo;
var
  qSearch: TSQLQuery;
begin
  qSearch := TSQLQuery.Create(ConfigObj.DB);
  try
    qSearch.Transaction := ConfigObj.TR;
    qSearch.SQL.Text := 'select * from programme p where p.idChannel = :id  ' + ' and dStartTime < :time and dEndTime > :time and list =:list ';
    qSearch.ParamByName('id').AsInteger := Channel;
    qSearch.ParamByName('time').AsDateTime := CurrTime;
    qSearch.ParamByName('list').AsInteger := FCurrentList;
    qSearch.Open;
    if qSearch.EOF then
      Result := Default(REpgInfo)
    else
    begin
      Result.Title := qSearch.FieldByName('sTitle').AsString;
      Result.Plot := qSearch.FieldByName('sPlot').AsString;
      Result.StartTime := qSearch.FieldByName('dStartTime').AsDateTime;
      Result.EndTime := qSearch.FieldByName('dEndTime').AsDateTime;
      Result.HaveData := True;
    end;
  finally
    qSearch.Free;
  end;

end;

function TEpg.GetEpgInfo(const SearchTerm: string): AREpgInfo;
var
  qSearch: TSQLQuery;
  i: longint;
begin
  qSearch := TSQLQuery.Create(ConfigObj.DB);
  try
    qSearch.Transaction := ConfigObj.TR;
    qSearch.SQL.Text := 'select c.name as sChannelName, p.Stitle, p.sPlot, p.dStartTime, p.dEndTime from programme p'
      + ' JOIN channels c on c.ID = p.idChannel'
      + ' where stitle like :search or sPlot like :search and list = :list'
      + ' order by p.dStartTime';
    qSearch.ParamByName('search').AsString := '%' + SearchTerm + '%';
    qSearch.ParamByName('list').AsInteger := FCurrentList;
    qSearch.PacketRecords := -1;
    qSearch.Open;
    i := qSearch.RecordCount;
    SetLength(Result, qSearch.RecordCount);
    i := 0;
    while not qSearch.EOF do
    begin
      Result[i].Channel := qSearch.FieldByName('sChannelName').AsString;
      Result[i].Title := qSearch.FieldByName('sTitle').AsString;
      Result[i].Plot := qSearch.FieldByName('sPlot').AsString;
      Result[i].StartTime := qSearch.FieldByName('dStartTime').AsDateTime;
      Result[i].EndTime := qSearch.FieldByName('dEndTime').AsDateTime;
      Result[i].HaveData := True;
      Inc(i);
      qsearch.Next;
    end;
  finally
    qSearch.Free;
  end;

end;


function TEpg.GetEpgInfo(Channel: integer; StartTime: TDateTime; EndTime: TDateTime): AREpgInfo;
var
  qSearch: TSQLQuery;
  i: integer;
begin
  qSearch := TSQLQuery.Create(ConfigObj.DB);
  try
    qSearch.Transaction := ConfigObj.TR;
    qSearch.SQL.Text := 'select distinct p.Stitle, p.sPlot, p.dStartTime, p.dEndTime from programme p where p.idChannel = :id  ' +
      ' and ((dStartTime >= :stime and dEndTime <= :etime) ' +
      '  or  (dStartTime <= :stime and dEndTime >= :stime) ' +
      '  or  (dStartTime <= :etime and dEndTime >= :etime)) ' +
      ' and  list =:list ' +
      ' order by dStartTime';
    qSearch.ParamByName('id').AsInteger := Channel;
    qSearch.ParamByName('stime').AsDateTime := StartTime;
    qSearch.ParamByName('etime').AsDateTime := EndTime;
    qSearch.ParamByName('list').AsInteger := FCurrentList;
    qSearch.PacketRecords := -1;
    qSearch.Open;
    i := qSearch.RecordCount;
    SetLength(Result, qSearch.RecordCount);
    i := 0;
    while not qSearch.EOF do
    begin
      Result[i].Title := qSearch.FieldByName('sTitle').AsString;
      Result[i].Plot := qSearch.FieldByName('sPlot').AsString;
      Result[i].StartTime := qSearch.FieldByName('dStartTime').AsDateTime;
      Result[i].EndTime := qSearch.FieldByName('dEndTime').AsDateTime;
      Result[i].HaveData := True;
      Inc(i);
      qsearch.Next;
    end;
  finally
    qSearch.Free;
  end;

end;


constructor TEpg.Create;
begin
  fEpgProperties := TEpgProperties.Create(ConfigObj);
  fEpgAvailable := False;
  Scanner := TEpgScanner.Create(self);
  Scanner.OnEndWork := EndScan;
  Scanner.Start;
end;

procedure TEpg.LoadChannelList(List: TM3ULoader);
var
  item: TM3UItem;
  qinsert: TSQLQuery;
  i: integer;
begin
  OvoLogger.Log(llINFO, 'Updating EPG channels list');
  qinsert := TSQLQuery.Create(ConfigObj.DB);
  try
    ConfigObj.DB.ExecuteDirect('delete from channels;');
    qinsert.Transaction := ConfigObj.TR;
    qinsert.SQL.Text := 'insert into channels values (:list, :id, :name, :ChannelNo, :EpgName);';
    i := 0;
    for i := 0 to List.Count - 1 do
    begin
      Item := List[i];
      qinsert.ParamByName('list').AsInteger := FCurrentList;
      qinsert.ParamByName('id').AsInteger := i;
      qinsert.ParamByName('name').AsString := item.Title;
      qinsert.ParamByName('EpgName').AsString := item.tvg_name;
      qinsert.ParamByName('ChannelNo').AsInteger := item.Number;
      qinsert.ExecSQL;
    end;
  finally
    qinsert.Free;
  end;
  ConfigObj.TR.CommitRetaining;

end;

destructor TEpg.Destroy;
begin
  if Assigned(Scanner) then
  begin
    Scanner.OnTerminate := nil;
    Scanner.Terminate;
    Scanner.Free;
  end;
  ConfigObj.TR.Commit;
  ConfigObj.DB.Transaction := nil;
  ConfigObj.DB.Connected := False;
  ConfigObj.TR.Free;
  ConfigObj.DB.Free;
  inherited Destroy;
end;

{ TEpgScanner }

procedure TEpgScanner.Execute;
var
  CacheDir: string;
  SourceEpg, EpgFile: string;
  Decompress: TGZFileStream;
  FcacheFile: TFileStream;
  GzHeader: word;
begin
  while not Terminated do
  begin
    RTLEventWaitFor(FScanEvent);
    if StoppedCheck then
      Continue;
    RTLEventResetEvent(FScanEvent);
    CacheDir := ConfigObj.CacheDir;
    if StoppedCheck then
      Continue;
    try
      Success := False;
      fOwner.fScanning := True;
      ConfigObj.DB.ExecuteDirect('delete from programme');
      OvoLogger.Log(llINFO, 'EPG update thread started');
      if fOwner.EpgProperties.EpgKind = Url then
      begin
        OvoLogger.Log(llINFO, 'Downloading EPG from %s', [fOwner.EpgProperties.EPGUrl]);
        SourceEpg := CacheDir + TempEPGFile;
        DownloadFromUrl(fOwner.EpgProperties.EPGUrl, SourceEpg, StoppedCheck);
      end
      else
      begin
        SourceEpg := fOwner.EpgProperties.EpgFileName;
        OvoLogger.Log(llINFO, 'Load EPG from local file %s', [fOwner.EpgProperties.EpgFileName]);
      end;
      if StoppedCheck then
        Continue;
      FcacheFile := TFileStream.Create(SourceEpg, fmOpenRead);
      GzHeader := FcacheFile.ReadWord;
      FcacheFile.Free;
      if NtoBe(GzHeader) = $1f8b then
      begin
        EpgFile := CacheDir + TempEPGFileDecompressed;
        OvoLogger.Log(llINFO, 'EPG is GZipped, inflating to %s', [EpgFile]);
        try
          Decompress := TGZFileStream.Create(SourceEpg, gzopenread);
          FcacheFile := TFileStream.Create(EpgFile, fmOpenWrite or fmcreate);
          Decompress.Position := 0;
          if StoppedCheck then
            Continue;
          FcacheFile.CopyFrom(Decompress, 0);
        finally
          FcacheFile.Free;
          Decompress.Free;
        end;
      end
      else
        EpgFile := SourceEpg;
      OvoLogger.Log(llINFO, 'Scanning EPG XML file %s', [EpgFile]);
      if StoppedCheck then
        Continue;
      if Load(EpgFile) > 0 then
      begin
        fOwner.setlastscan('epg', now);
        Success := True;
      end;
    except
      on e: Exception do
        OvoLogger.Log(llERROR, 'Error scanning EPG Data : %s', [e.Message]);
    end;
    Synchronize(DoEndWork);
  end;
end;

function TEpgScanner.FindChannelIdByName(Channel: string): integer;
var
  qFind: TSQLQuery;
begin
  qFind := TSQLQuery.Create(ConfigObj.DB);
  try
    qFind.Transaction := ConfigObj.TR;
    qFind.SQL.Text := 'select id from Channels where name = :name and list = :list;';
    qFind.ParamByName('name').AsString := Channel;
    qFind.ParamByName('list').AsInteger := FCurrentList;
    qFind.Open;
    if qFind.EOF then
      Result := -1
    else
      Result := qFind.Fields[0].AsInteger;

  finally
    qFind.Free;
  end;
end;

procedure TEpgScanner.SetCurrentList(AValue: integer);
begin
  if FCurrentList = AValue then Exit;
  FCurrentList := AValue;
end;

function TEpgScanner.FindChannelIdByEPGName(Channel: string): integer;
var
  qFind: TSQLQuery;
begin
  qFind := TSQLQuery.Create(ConfigObj.DB);
  try
    qFind.Transaction := ConfigObj.TR;
    qFind.SQL.Text := 'select id from Channels where epgname = :name and list = :list ;';
    qFind.ParamByName('name').AsString := Channel;
    qFind.ParamByName('list').AsInteger := FCurrentList;
    qFind.Open;
    if qFind.EOF then
      Result := -1
    else
      Result := qFind.Fields[0].AsInteger;

  finally
    qFind.Free;
  end;
end;


procedure TEpgScanner.SetOnEndWork(AValue: TNotifyEvent);
begin
  FOnEndWork := AValue;
end;


function TEpgScanner.Load(EPGFile: string): integer;
var
  i: integer;
  CurrNode: TDOMNode;
  fName: DOMString;
  OldName: string;
  s1, s2: TDateTime;
  idChannel: integer;
  qInsert: TSQLQuery;
  ChannelList: TObjectList<TChannelData>;

  function NodeValue(const ValueName: string): string; inline;
  var
    tmpnode: TDOMNode;
  begin
    tmpnode := CurrNode.FindNode(ValueName);
    if Assigned(tmpnode) then
      Result := tmpnode.TextContent
    else
      Result := EmptyStr;
  end;

  procedure AddChannel(const Node: TDOMNode); inline;
  var
    Channel: TChannelData;
    Child: TDomNode;
    j, k: integer;
  begin
    Channel := TChannelData.Create;
    Channel.EpgId := Node.Attributes.GetNamedItem('id').NodeValue;
    Channel.Id := FindChannelIdByEPGName(Channel.EpgId);
    if Channel.Id = -1 then
      for j := 0 to Node.ChildNodes.Count - 1 do
      begin
        Child := Node.ChildNodes[j];
        if Child.NodeName = 'display-name' then
        begin
          k := FindChannelIdByName(Child.TextContent);
          if k <> -1 then
          begin
            Channel.Id := k;
            Break;
          end;
        end;
      end;

    if Channel.Id <> -1 then
      ChannelList.Add(Channel)
    else
      Channel.Free;
  end;

  function FindChannelIdInList(ChannelEpgId: string): integer;
  var
    Channel: TChannelData;
  begin
    Result := -1;
    for channel in ChannelList do
    begin
      if Channel.EpgId = ChannelEpgId then
        Result := Channel.Id;
    end;
  end;

begin
  Result := 0;
  if StoppedCheck then exit;
  ChannelList := TObjectList<TChannelData>.Create;
  ReadXMLFile(XMLDoc, EPGFile);
  try

    if StoppedCheck then exit;
    Root := XMLDoc.FindNode('tv');
    //  writeln(Root.NodeName, '  ', Root.ChildNodes.Count);
    OldName := '';
    idChannel := -1;
    qInsert := TSQLQuery.Create(ConfigObj.DB);
    try
      qInsert.Transaction := ConfigObj.TR;
      qInsert.SQL.Text := INSERTPROGRAMME;

      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        CurrNode := Root.ChildNodes.Item[i];
        //      writeln(currnode.NodeName);
        if CurrNode.NodeName = 'channel' then
          AddChannel(CurrNode);

        if CurrNode.NodeName <> 'programme' then
          Continue;
        fName := CurrNode.Attributes.GetNamedItem('channel').NodeValue;
        if fName <> OldName then
        begin
          idChannel := FindChannelIdInList(fName);
          OldName := fName;
        end;

        if idChannel <> -1 then
        begin
          if StoppedCheck then
            exit;
          qInsert.ParamByName('idchannel').AsInteger := idChannel;
          qInsert.ParamByName('sTitle').AsString := NodeValue('title');
          qInsert.ParamByName('sPlot').AsString := NodeValue('desc');
          s1 := EpgDateToDate(CurrNode.Attributes.GetNamedItem('start').NodeValue);
          s2 := EpgDateToDate(CurrNode.Attributes.GetNamedItem('stop').NodeValue);
          qInsert.ParamByName('dStartTime').AsDateTime := s1;
          qInsert.ParamByName('dEndTime').AsDateTime := s2;
          qInsert.ParamByName('list').AsInteger := FCurrentList;
          qInsert.ExecSQL;
        end;
      end;

      Result := 1;
    finally
      qInsert.Free;
    end;

  finally
    XMLDoc.Free;
    ChannelList.Free;
  end;

end;

procedure TEpgScanner.DoEndWork;
begin
  if Assigned(FOnEndWork) then
    FOnEndWork(self);
end;

procedure TEpgScanner.TerminatedSet;
begin
  inherited TerminatedSet;
  RTLEventSetEvent(FScanEvent);
end;

function TEpgScanner.StoppedCheck: boolean;
begin
  Result := Terminated or FStopped;
  if Result then
    FStopped := False;
end;

procedure TEpgScanner.Stop;
begin
  FStopped := True;
  OvoLogger.Log(llINFO, 'Stopping EPG thread');
  RTLEventSetEvent(FScanEvent);
end;

constructor TEpgScanner.Create(Owner: TEpg);
begin
  inherited Create(True);
  fOwner := Owner;
  FScanEvent := RTLEventCreate;
end;

destructor TEpgScanner.Destroy;
begin
  RTLEventDestroy(FScanEvent);
  inherited Destroy;
end;


end.
