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
unit images_handler;

interface

uses
  SysUtils, classes, libmpv;

function OpenFn(user_data: Pointer; uri: pansichar; var info: mpv_stream_cb_info): integer;


implementation

function SizeFn(cookie: Pointer): int64;
begin
//  Result := TResourceStream(cookie).Size
  Result := MPV_ERROR_UNSUPPORTED ;
end;

function ReadFn(cookie: Pointer; buf: pansichar; nbytes: uint64): int64;
begin
  move(TResourceStream(cookie).Memory, buf^, TResourceStream(cookie).Size);
  Result := 0;
end;

function SeekFn(cookie: Pointer; offset: int64): int64;
begin
  Result := MPV_ERROR_UNSUPPORTED ;
end;

procedure CloseFn(cookie: Pointer);
begin
  TResourceStream(cookie).Free;
end;

function OpenFn(user_data: Pointer; uri: pansichar; var info: mpv_stream_cb_info): integer;
begin
  try
    info.cookie := TResourceStream.Create(HINSTANCE,uppercase(Uri),RT_RCDATA);
    info.size_fn := nil;//@SizeFn;
    info.read_fn := @ReadFn;
    info.seek_fn := nil;//@SeekFn;
    info.close_fn := @CloseFn;
    Result := 0;
  except
    Result := -1; // MPV_ERROR_LOADING_FAILED
  end;
end;

end.
