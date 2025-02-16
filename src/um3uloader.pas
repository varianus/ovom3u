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
unit um3uloader;

interface

uses
  Classes, SysUtils, Generics.Collections, SQLDB, StrUtils, Config, Nullable;

type

  TM3UItem = class
  public
    Number: integer;
    tvg_chno: integer;
    tvg_name: string;
    Title: string;
    Mrl: string;
    Id: string;
    Group: string;
    IconUrl: string;
    IconLocal: string;
    IconAvailable: boolean;
    CurrProgram: string;
  end;

  TLogMessage = procedure(Message: string) of object;

  TM3ULoader = class;
  { TFilteredList }

  TFilterParam = record
    Group: TNullable<string>;
    Title: TNullable<string>;
  end;

  TFilteredList = record
  private
    OriginalList: TM3uLoader;
    FFilterArray: array of integer;
    function GetItem(idx: integer): TM3UItem;
  public
    property Item[idx: integer]: TM3UItem read GetItem; default;
    function Count: integer;
    function Map(idx: integer): integer;
    function IndexOf(Value: integer): integer;
  end;

  { TM3ULoader }

  TM3ULoader = class(TObjectList<TM3UItem>)
  private
    fLastMessage: string;
    FOnListChanged: TNotifyEvent;
    FActiveList: TM3UList;
    function LoadList: boolean;
    procedure SetActiveList(AValue: TM3UList);
    procedure SetOnListChange(AValue: TNotifyEvent);
    function SortbyNumber(GENERIC_CONST Left, Right: TM3UItem): integer;
    procedure FixChannelNumbering;
    procedure UpdateLogo;
  protected
    procedure DoListChanged;
    function Load(const ListName: string): boolean;
  public
    ListMd5: string;
    EPGURL: string;
    Groups: TStringList;
    property ActiveList: TM3UList read FActiveList write SetActiveList;
    property LastMessage: string read fLastMessage;
    property OnListChanged: TNotifyEvent read FOnListChanged write SetOnListChange;
    constructor Create;
    destructor Destroy; override;
    function ItemByChno(chno: integer): integer;
    function Filter(aFilter: TFilterParam): TFilteredList;
    procedure LoadChannelList;
    function TestList(List: TM3UList; Logger: TLogMessage): boolean;


  end;

  { TLogoLoader }

  TLogoLoader = class(TThread)
  private
    fOwner: TM3ULoader;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TM3ULoader); reintroduce;
  end;

resourcestring
  RSEmpty = 'M3U file is empty';
  RSMissingHeader = 'Missing #EXTM3U Header';
  RSAnyGroup = '<all groups>';

implementation

uses Math, LoggerUnit, GeneralFunc, Generics.Defaults, md5;

const
  CountExt = 4;
  CoverExt: array [0..CountExt - 1] of string =
    ('.png', '.jpg', '.jpeg', '.gif');

  { TFilteredList }

function TFilteredList.Map(idx: integer): integer;
begin
  Result := FFilterArray[idx];
end;

function TFilteredList.IndexOf(Value: integer): integer;
var
  i: integer;
begin
  for i := 0 to Length(FFilterArray) - 1 do
    if FFilterArray[i] = Value then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TFilteredList.GetItem(idx: integer): TM3UItem;
begin
  Result := OriginalList[Map(idx)];
end;

function TFilteredList.Count: integer;
begin
  Result := Length(FFilterArray);
end;


{ TLogoLoader }

procedure TLogoLoader.Execute;
var
  i: integer;
  Item: TM3UItem;
begin
  if Terminated then
    exit;

  for item in fOwner do
  begin
    if Terminated then
      exit;

    if not Item.IconLocal.IsEmpty then
      if FileExists(Item.IconLocal) then
      begin
        Item.IconAvailable := True;
        Queue(fOwner.DoListChanged);
      end
      else
      if not DownloadFromUrl(Item.IconUrl, Item.IconLocal) then
        Item.IconLocal := ''
      else
      begin
        Item.IconAvailable := True;
        Queue(fOwner.DoListChanged);
      end;
  end;
end;

constructor TLogoLoader.Create(Owner: TM3ULoader);
begin
  inherited Create(False);
  fOwner := Owner;
end;

{ TM3ULoader }

constructor TM3ULoader.Create;
begin
  inherited Create(True);
  Groups := TStringList.Create;
  Groups.Sorted := True;
end;

destructor TM3ULoader.Destroy;
begin
  Groups.Free;
  inherited Destroy;
end;

function TM3ULoader.Load(const ListName: string): boolean;
var
  f: textfile;
  s: string;
  p, ext: string;
  Item: TM3UItem;
  fData: boolean;
  index, CurrGroup: integer;
  Context: TMD5Context;
  Digest: TMD5Digest;
  i: integer;
  Cachedir: string;

  function FindCommaOutsideQuotes(const S: string): integer;
  var
    i: integer;
    InQuotes: boolean;
  begin
    Result := -1;
    InQuotes := False;
    for i := 1 to Length(S) do
      if S[i] = '"' then
        InQuotes := not InQuotes
      else if (S[i] = ',') and not InQuotes then
      begin
        Result := i;
        Exit;
      end;
  end;


  function FindTag(const tag: string; const st: string): string;
  var
    tagpos: integer;
    TagStart: integer;
  begin
    TagPos := Pos(tag + '=', st);
    if TagPos > 0 then
    begin
      TagStart := PosEx('"', st, tagpos) + 1;
      Result := ExtractSubstr(St, TagStart, ['"']);
    end
    else
      Result := EmptyStr;
  end;

begin
  Clear;
  Groups.Clear;
  Result := False;
  Index := 1;

  if ListName.IsEmpty then
  begin
    OvoLogger.Log(llWARN, 'No list to load');
    exit;
  end;

  try
    Groups.BeginUpdate;
    OvoLogger.Log(llINFO, 'Loading list from %s', [ListName]);
    MD5Init(Context);
    Cachedir := IncludeTrailingPathDelimiter(ConfigObj.CacheDir + 'logo');
    ForceDirectories(Cachedir);
    p := ExtractFilePath(ListName);
    assignfile(f, ListName);
    reset(f);
    if EOF(f) then
    begin
      fLastMessage := RSEmpty;
      exit;
    end;

    readln(f, s);
    s := trim(s);
    if uppercase(copy(s, 1, 7)) <> '#EXTM3U' then
    begin
      fLastMessage := RSMissingHeader;
      exit;
    end;
    if FActiveList.EPGFromM3U then
    begin
      EPGURL := FindTag('url-tvg', S);
      if EPGURL = '' then
        OvoLogger.Log(llWARN, 'EPG Url from M3U missing or empty!');
      FActiveList.EPGUrl := EPGURL;
    end;

    fData := False;
    while EOF(f) <> True do
    begin
      readln(f, s);
      s := trim(s);
      if (s <> EmptyStr) then
      begin
        MD5Update(Context, s[1], Length(s));
        if (uppercase(copy(s, 1, 8)) = '#EXTINF:') then
        begin
          item := TM3UItem.Create;
          Item.Number := index;
          Item.Group := FindTag('group-title', s);
          if Groups.Find(Item.Group, CurrGroup) then
            Groups.Objects[CurrGroup] := TObject(PtrInt(Groups.Objects[CurrGroup]) + 1)
          else
            Groups.AddObject(Item.Group, TObject(1));
          item.Id := FindTag('tvg-id', s);
          item.IconUrl := FindTag('tvg-logo', s);
          item.tvg_name := FindTag('tvg-name', s);
          item.tvg_chno := StrToIntDef(FindTag('tvg-chno', s), 0);
          Item.Title := copy(s, FindCommaOutsideQuotes(S) + 1, Length(s));
          if not Trim(item.IconUrl).IsEmpty then
          begin
            ext := LowerCase(ExtractFileExt(Item.IconUrl));
            i := 0;
            while i < CountExt do
              if ext = CoverExt[i] then
                i := CountExt + 1
              else
                Inc(i);
            if i > CountExt then
              Item.IconLocal := CacheDir + CleanupFileName(Item.Title) + ext;
          end;
          item.IconAvailable := False;
          Inc(index);
          Add(Item);
          fData := True;
        end
        else
        if s[1] <> '#' then
          if fData then
          begin
            item.Mrl := s;
            fData := False;
          end;

      end;
    end;
    MD5Final(Context, Digest);
    ListMd5 := MD5Print(Digest);
    Result := True;
  finally
    Groups.EndUpdate;
    if not fLastMessage.IsEmpty then
      OvoLogger.Log(llWARN, fLastMessage);
    closefile(f);
  end;
end;

function TM3ULoader.ItemByChno(chno: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if items[i].tvg_chno = chno then
    begin
      Result := i;
      break;
    end;
end;

function TM3ULoader.Filter(aFilter: TFilterParam): TFilteredList;
var
  i, j: integer;
  Item: TM3UItem;
begin
  Result.OriginalList := self;
  SetLength(Result.FFilterArray, Count);
  i := 0;
  for j := 0 to Count - 1 do
  begin
    Item := FItems[j];
    if ((not aFilter.Group.HasValue) or (Item.Group = Afilter.Group.Value)) and
      ((not aFilter.Title.HasValue) or (AnsiContainsText(Item.Title, AFilter.Title.Value))) then
    begin
      Result.FFilterArray[i] := j;
      Inc(i);
    end;

  end;
  SetLength(Result.FFilterArray, i);

end;

function TM3ULoader.SortbyNumber(GENERIC_CONST Left, Right: TM3UItem): integer;
begin
  Result := CompareValue(left.Number, Right.Number);
end;

function TM3ULoader.LoadList: boolean;
var
  CacheDir, IPTVList: string;
  Kind: TProviderKind;
  ListName: string;
begin
  Result := False;
  Kind := FActiveList.ChannelKind;

  if Kind = URL then
  begin
    CacheDir := ConfigObj.CacheDir;
    IPTVList := FActiveList.ChannelsUrl;
    try
      ListName := CacheDir + format('list-%d-iptv.m3u', [FActiveList.ListID]);
      if (ConfigObj.ListManager.LastScan(FActiveList.ListID, 'channels') + 12 / 24 < now) {mcmcmcmcmcmc or List.ListProperties.Dirty } then
      try
        OvoLogger.Log(llINFO, 'Downloding channels list from ' + IPTVList);
        DownloadFromUrl(IPTVList, ListName);
        ConfigObj.ListManager.SetLastScan(FActiveList.ListID, 'channels', now);
      except
        on e: Exception do
          OvoLogger.Log(llERROR, 'Can''t download new list at: ' +
            IPTVList + ' error:' +
            E.Message);
      end
      else
        OvoLogger.Log(llINFO, 'Using cached channels list');

      IPTVList := ListName;
    finally
    end;
  end
  else
    IPTVList := FActiveList.ChannelsUrl;

  if FileExists(IPTVList) then
    if not Load(IPTVList) then
    begin
      OvoLogger.Log(llERROR, 'Can''t load %. Error: %s', [IPTVList, LastMessage]);
      exit;
    end;

  Result := True;

  OvoLogger.Log(llINFO, 'Found %d channels', [Count]);

  if FActiveList.UseChno then
  begin
    FixChannelNumbering;
    OvoLogger.Log(llINFO, 'Renumber channels using tvg-chno');
  end;

  if FActiveList.ChannelsDownloadLogo then
    UpdateLogo;

end;

procedure TM3ULoader.LoadChannelList;
var
  item: TM3UItem;
  qinsert: TSQLQuery;
  i: integer;
begin
  OvoLogger.Log(llINFO, 'Updating channels list for ID ' + IntToStr(FActiveList.ListID));
  qinsert := TSQLQuery.Create(ConfigObj.DB);
  try
    ConfigObj.DB.ExecuteDirect('delete from channels where list = ' + IntToStr(FActiveList.ListID));

    qinsert.Transaction := ConfigObj.TR;
    qinsert.SQL.Text := 'insert into channels values (:list, :id, :name, :ChannelNo, :EpgName);';
    i := 0;
    for i := 0 to Count - 1 do
    begin
      Item := Items[i];
      qinsert.ParamByName('list').AsInteger := FActiveList.ListID;
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

function TM3ULoader.TestList(List: TM3UList; Logger: TLogMessage): boolean;
var
  CacheDir, IPTVList: string;
  Kind: TProviderKind;
  ListName: string;
begin
  Result := False;
  FActiveList := List;
  Kind := List.ChannelKind;

  if Kind = URL then
  begin
    CacheDir := ConfigObj.CacheDir;
    IPTVList := List.ChannelsUrl;
    try
      ListName := CacheDir + 'list-TEST-iptv.m3u';
      try
        Logger('Downloding channels list from ' + IPTVList);
        DownloadFromUrl(IPTVList, ListName);
      except
        on e: Exception do
          Logger('Can''t download new list at: ' +
            IPTVList + ' error:' +
            E.Message);
      end;

      IPTVList := ListName;
    finally
    end;
  end
  else
  begin
    IPTVList := List.ChannelsUrl;
    Logger('Loading list from ' + IPTVList);
  end;

  if FileExists(IPTVList) then
  begin
    if not Load(IPTVList) then
    begin
      Logger(format('Can''t load %. Error: %s', [IPTVList, LastMessage]));
      exit;
    end;
  end
  else
  begin
    Logger(format('List %s not found', [IPTVList, LastMessage]));
    exit;
  end;


  Result := True;

  if List.EPGUrl <> '' then
    Logger('Contains EPG URL:' + List.EPGUrl);

  Logger(format('Found %d channels in %d groups', [Count, Groups.Count]));

end;


procedure TM3ULoader.DoListChanged;
begin
  LoadList;
  if ListMd5 <> ConfigObj.ListManager.LastChannelMd5(FactiveList.ListID) then
  begin
    OvoLogger.Log(llINFO, 'Channels list changed, reloading EPG');
    LoadChannelList;
    ConfigObj.ListManager.SetLastChannelMd5(FActiveList.ListID, ListMd5);

  end;

  if Assigned(FOnListChanged) then
    FOnListChanged(self);
end;

procedure TM3ULoader.SetOnListChange(AValue: TNotifyEvent);
begin
  FOnListChanged := AValue;
end;


procedure TM3ULoader.SetActiveList(AValue: TM3UList);
begin
  FActiveList := AValue;
  DoListChanged;
end;

procedure TM3ULoader.FixChannelNumbering;
var
  i: integer;
  MaxChno: integer;
begin
  Maxchno := 0;
  for i := 0 to Count - 1 do
  begin
    items[i].Number := items[i].tvg_chno;
    if items[i].tvg_chno > MaxChno then
      MaxChno := items[i].tvg_chno;
  end;
  for i := 0 to Count - 1 do
    if items[i].Number = 0 then
    begin
      Inc(MaxChno);
      items[i].tvg_chno := MaxChno;
      items[i].Number := MaxChno;
    end;
  Sort(TComparer<TM3UItem>.Construct(SortByNumber));
end;

procedure TM3ULoader.UpdateLogo;
begin
  with TLogoLoader.Create(self) do
  begin
    FreeOnTerminate := True;
    Start;
  end;
end;

end.
