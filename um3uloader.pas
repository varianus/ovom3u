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
  Classes, SysUtils, Generics.Collections, StrUtils, Config, Nullable;

type
  TProviderKind = (Local, URL);

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
    property Item[idx:integer]:TM3UItem read GetItem; default;
    Function Count: integer;
    function Map(idx:integer):integer;
    function IndexOf(value:integer):integer;
  end;


  { TListProperties }

  TListProperties = Class(TConfigParam)
  private
    FChannelsDownloadLogo: boolean;
    FChannelsFileName: string;
    FChannelsKind: TProviderKind;
    FChannelsUrl: string;
    FUseChno: boolean;
    procedure SetChannelsDownloadLogo(AValue: boolean);
    procedure SetChannelsFileName(AValue: string);
    procedure SetChannelsKind(AValue: TProviderKind);
    procedure SetChannelsUrl(AValue: string);
    procedure SetUseChno(AValue: boolean);
  Protected
    Procedure InternalSave; Override;
  public
    Property ChannelsDownloadLogo: boolean read FChannelsDownloadLogo write SetChannelsDownloadLogo;
    Property ChannelsFileName: string read FChannelsFileName write SetChannelsFileName;
    Property ChannelsKind: TProviderKind read FChannelsKind write SetChannelsKind;
    Property ChannelsUrl: string read FChannelsUrl write SetChannelsUrl;
    Property UseChno: boolean read FUseChno write SetUseChno;
    Procedure Load; Override;
  end;
  { TM3ULoader }

  TM3ULoader = class(TObjectList<TM3UItem>)
  private
    fLastMessage: string;
    fListProperties: TListProperties;
    FOnListChanged: TNotifyEvent;
    procedure SetOnListChange(AValue: TNotifyEvent);
    function SortbyNumber(const Left, Right: TM3UItem): integer;

  public

    ListMd5: string;
    Groups: TStringList;
    Property ListProperties: TListProperties read fListProperties;
    procedure DoListChanged;
    property LastMessage: string read fLastMessage;
    property OnListChanged: TNotifyEvent read FOnListChanged write SetOnListChange;
    constructor Create;
    destructor Destroy; override;
    function Load(const ListName: string): boolean;
    function ItemByChno(chno: integer): integer;
    Function Filter(aFilter:TFilterParam): TFilteredList;

    procedure FixChannelNumbering;
    procedure UpdateLogo;

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

{ TListProperties }

procedure TListProperties.SetChannelsDownloadLogo(AValue: boolean);
begin
  if FChannelsDownloadLogo=AValue then Exit;
  FChannelsDownloadLogo:=AValue;
  Dirty:=true;
end;

procedure TListProperties.SetChannelsFileName(AValue: string);
begin
  if FChannelsFileName=AValue then Exit;
  FChannelsFileName:=AValue;
  Dirty:=true;

end;

procedure TListProperties.SetChannelsKind(AValue: TProviderKind);
begin
  if FChannelsKind=AValue then Exit;
  FChannelsKind:=AValue;
  Dirty:=true;

end;

procedure TListProperties.SetChannelsUrl(AValue: string);
begin
  if FChannelsUrl=AValue then Exit;
  FChannelsUrl:=AValue;
  Dirty:=true;

end;

procedure TListProperties.SetUseChno(AValue: boolean);
begin
  if FUseChno=AValue then Exit;
  FUseChno:=AValue;
  Dirty:=true;

end;

procedure TListProperties.InternalSave;
begin
  Owner.WriteString('m3u/ProviderKind',TEnum<TProviderKind>.ToString(ChannelsKind));
  Owner.WriteString('m3u/FileName',ChannelsFileName);
  Owner.WriteString('m3u/Url',ChannelsUrl);
  Owner.WriteBoolean('m3u/UseChno', UseChno);
  Owner.WriteBoolean('m3u/DownloadLogo', ChannelsDownloadLogo);
end;

procedure TListProperties.Load;
begin
  ChannelsKind:= TEnum<TProviderKind>.FromString(Owner.ReadString('m3u/ProviderKind',''), Local);
  ChannelsFileName:= Owner.ReadString('m3u/FileName','');
  ChannelsUrl:= Owner.ReadString('m3u/Url','');
  UseChno := Owner.ReadBoolean('m3u/UseChno', false);
  ChannelsDownloadLogo := Owner.ReadBoolean('m3u/DownloadLogo', false);
  Dirty:=false;
end;

{ TFilteredList }

function TFilteredList.Map(idx: integer): integer;
begin
  Result := FFilterArray[idx];
end;

function TFilteredList.IndexOf(value: integer): integer;
var
  i: Integer;
begin
  for i :=0 to Length(FFilterArray) -1 do
    if FFilterArray[i] = value then
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
    begin
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
  fListProperties := TListProperties.Create(ConfigObj);
  Groups := TStringList.Create;
  Groups.Sorted:=true;
  Groups.Duplicates:=dupIgnore;
end;

destructor TM3ULoader.Destroy;
begin
  Groups.free;
  inherited Destroy;
end;

function TM3ULoader.Load(const ListName: string): boolean;
var
  f: textfile;
  s: string;
  p, ext: string;
  Item: TM3UItem;
  fData: boolean;
  index: integer;
  Context: TMD5Context;
  Digest: TMD5Digest;
  i: integer;
  Cachedir: string;


  function FindTag(const tag: string; const st: string): string;
  var
    tagpos: integer;
    TagStart: integer;
  begin
    TagPos := Pos(tag, st);
    if TagPos > 0 then
    begin
      TagStart := PosEx('"', st, tagpos) + 1;
      Result := ExtractSubstr(St, TagStart, ['"']);
    end
    else
      result := EmptyStr;
  end;

begin
  Clear;
  Result := False;
  Index := 1;

  if ListName.IsEmpty then
  begin
    OvoLogger.Log(llWARN, 'No list to load');
    exit;
  end;

  try
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
    fData := False;
    while EOF(f) <> True do
    begin
      readln(f, s);
      s := trim(s);
      if (s <> EmptyStr) then
      begin
        MD5Update(Context, s[1], Length(s));
        if (uppercase(copy(s, 1, 7)) = '#EXTINF') then
        begin
          item := TM3UItem.Create;
          Item.Number := index;
          Item.Group := FindTag('group-title', s);
          Groups.Add(Item.Group);
          item.Id := FindTag('tvg-id', s);
          item.IconUrl := FindTag('tvg-logo', s);
          item.tvg_name := FindTag('tvg-name', s);
          item.tvg_chno := StrToIntDef(FindTag('tvg-chno', s), 0);
          Item.Title := copy(s, RPos(',', S) + 1, Length(s));
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
    DoListChanged;
  finally
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
var i,j:Integer;
    Item: TM3UItem;
begin
  Result.OriginalList := self;
  SetLength(Result.FFilterArray, Count);
  i:= 0;
  for j:= 0 to count -1 do
    begin
      Item := FItems[j];
      if ((not aFilter.Group.HasValue) or (Item.Group = Afilter.Group.Value)) and
         ((not aFilter.Title.HasValue) or (AnsiContainsText(Item.Title, AFilter.Title.value))) then
        begin
          Result.FFilterArray[i]:=j;
          inc(i);
        end;

    end;
  SetLength(Result.FFilterArray,i);

end;

function TM3ULoader.SortbyNumber(const Left, Right: TM3UItem): integer;
begin
  Result := CompareValue(left.Number, Right.Number);
end;

procedure TM3ULoader.DoListChanged;
begin
  if Assigned(FOnListChanged) then
    FOnListChanged(self);
end;

procedure TM3ULoader.SetOnListChange(AValue: TNotifyEvent);
begin
  FOnListChanged := AValue;
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
