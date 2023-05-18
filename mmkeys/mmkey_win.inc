{%MainUnit ../multimediakeys.pas}
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
uses windows;

var
  WindowClassAtom: ATOM; // RegisterWindowClass yields an atom if successful.
  WindowClassInfo: WNDCLASSEX; // Class info Data structure for main window.
  WinClassName: string = 'Winamp v1.x';
  hwindow: HWND;

type

  { TkeyCaptureWin }

  TkeyCaptureWin = class (TKeyCapture)
  private
    function CreateAppWindow: boolean;
  public
    Procedure BeginGrab; Override;
    Procedure EndGrab; Override;
  end;

{ TkeyCaptureWin }

function WinProc(hw: HWND; uMsg: UINT; wp: WPARAM; lp: LPARAM): LRESULT;
  stdcall; export;
var
 obj : TKeyCaptureWin;
begin

  Result := 0;
  case uMsg of
    WM_HOTKEY : begin
                  obj := TKeyCaptureWin(GetWindowLongPtr(HWindow,GWL_USERDATA));
                  if Assigned(obj.owner.onMmKey) then
                     obj.Owner.onMmKey(Obj.Owner, FakeKey+ Wp);
                    end;
  else
      Result := DefWindowProc(hw, uMsg, wp, lp);
    end; // Case
end; // WinProc

function TkeyCaptureWin.CreateAppWindow: boolean;

  function RegisterWindowClass: boolean;
  begin
    WindowClassInfo.cbSize := sizeof(WindowClassInfo);
    WindowClassInfo.Style := 0;
    WindowClassInfo.lpfnWndProc := @WinProc;
    WindowClassInfo.cbClsExtra := 0;
    WindowClassInfo.cbWndExtra := 0;
    WindowClassInfo.hInstance := HInstance;
    WindowClassInfo.hIcon := 0;
    WindowClassInfo.hCursor := 0;
    WindowClassInfo.hbrBackground := 0;
    WindowClassInfo.lpszMenuName := nil;
    WindowClassInfo.lpszClassName := PChar(WinClassName);
    WindowClassInfo.hIconSm := 0;
    WindowClassAtom := RegisterClassEx(WindowClassInfo);
    Result := WindowClassAtom <> 0;
  end; // RegisterWindowClass - Nested Function

begin
  Result := false;

  if not RegisterWindowClass then
     exit;

  HWindow := CreateWindowEx(WS_EX_NOACTIVATE or WS_EX_TRANSPARENT,
    PChar(WinClassName), PChar(WinClassName), Ws_popup or WS_CLIPSIBLINGS, 0,
    0, 0, 0, 0, 0, HInstance, nil);

  if HWindow <> 0 then
    begin
    ShowWindow(HWindow, SW_HIDE);
    SetWindowLongPtr(HWindow,GWL_USERDATA,PtrInt(Self));
    UpdateWindow(HWindow);
    Result := True;
    end;
end; // CreateAppWindow

procedure TkeyCaptureWin.BeginGrab;
begin
  CreateAppWindow;
  RegisterHotKey(hwindow, Longint(VK_MEDIA_STOP),     0, VK_MEDIA_STOP);
  RegisterHotKey(hwindow, Longint(VK_MEDIA_PREV_TRACK), 0, VK_MEDIA_PREV_TRACK);
  RegisterHotKey(hwindow, Longint(VK_MEDIA_PLAY_PAUSE),     0, VK_MEDIA_PLAY_PAUSE);
  RegisterHotKey(hwindow, Longint(VK_MEDIA_NEXT_TRACK),     0, VK_MEDIA_NEXT_TRACK);

end;

procedure TkeyCaptureWin.EndGrab;
begin
  UnRegisterHotkey( hwindow, Longint(VK_MEDIA_STOP));
  UnRegisterHotkey( hwindow, Longint(VK_MEDIA_PREV_TRACK));
  UnRegisterHotkey( hwindow, Longint(VK_MEDIA_PLAY_PAUSE));
  UnRegisterHotkey( hwindow, Longint(VK_MEDIA_NEXT_TRACK));

  DestroyWindow(hwindow);
end;

constructor TMultimediaKeys.Create(Mode:Integer);
begin
  fMode := Mode;
  KeyCapture := TkeyCaptureWin.Create;
  KeyCapture.Owner:= Self;
  KeyCapture.BeginGrab;
end;

destructor TMultimediaKeys.Destroy;
begin
  KeyCapture.EndGrab;
  KeyCapture.Free;
end;


