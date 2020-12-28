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
unit um3uloader;

interface

uses
  Classes, SysUtils, Generics.Collections, StrUtils;

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
    Icon: string;
    CurrProgram: string;
  end;

  { TM3ULoader }

  TM3ULoader = class(TObjectList<TM3UItem>)
  private
    fLastMessage: string;
    function SortbyNumber(constref Left, Right: TM3UItem): integer;
  public
    property LastMessage: string read fLastMessage;
    constructor Create;
    destructor Destroy; override;
    function Load(const ListName: string): boolean;
    function ItemByChno(chno: integer): integer;
    procedure FixChannelNumbering;

  end;


implementation

uses Math, Generics.Defaults;

resourcestring
  RSEmpty = 'M3U file is empty';
  RSMissingHeader = 'Missing #EXTM3U Header';

{ TM3ULoader }

constructor TM3ULoader.Create;
begin
  inherited Create(True);
end;

destructor TM3ULoader.Destroy;
begin
  inherited Destroy;
end;

function TM3ULoader.Load(const ListName: string): boolean;
var
  f: textfile;
  s: string;
  p: string;
  Item: TM3UItem;
  fData: boolean;
  index: integer;

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
    end;
  end;

begin
  Clear;
  Index := 1;
  p := ExtractFilePath(ListName);
  assignfile(f, ListName);
  reset(f);
  Result := True;
  if EOF(f) then
  begin
    Result := False;
    fLastMessage := RSEmpty;
    exit;
  end;

  readln(f, s);
  s := trim(s);
  if uppercase(copy(s, 1, 7)) <> '#EXTM3U' then
  begin
    Result := False;
    fLastMessage := RSMissingHeader;
    exit;
  end;
  fData := False;
  while EOF(f) <> True do
  begin
    readln(f, s);
    s := trim(s);
    if (s <> EmptyStr) then
      if (uppercase(copy(s, 1, 7)) = '#EXTINF') then
      begin
        if not fData then
        begin
          item := TM3UItem.Create;
          Item.Number := index;
          Item.Group := FindTag('tvg-group', s);
          item.Id := FindTag('tvg-id', s);
          item.Icon := FindTag('tvg-logo', s);
          item.tvg_name := FindTag('tvg-name', s);
          item.tvg_chno := StrToIntDef(FindTag('tvg-chno', s), 0);
          Item.Title := copy(s, RPos(',', S) + 1, Length(s));
          Inc(index);
          Add(Item);
          fData := True;
        end;
      end
      else
      if s[1] <> '#' then
        if fData then
        begin
          item.Mrl := s;
          fData := False;
        end;
  end;
  closefile(f);
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

function TM3ULoader.SortbyNumber(constref Left, Right: TM3UItem): integer;
begin
  Result := CompareValue(left.Number, Right.Number);
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

end.
