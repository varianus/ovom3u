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
unit MultimediaKeys;

interface

uses
  Classes, SysUtils, LCLType, glib2;

type
  TMultimediaKeys = class;

  { TKeyCapture }
  MmKey = procedure(Sender: TObject; var Key: word) of Object;

  TKeyCapture = class
  protected
    procedure BeginGrab; virtual; abstract;
    procedure EndGrab; virtual; abstract;
    function GetGrabbed: Boolean; Virtual;
  public
    Owner: TMultimediaKeys;
    Property Grabbed: Boolean read GetGrabbed;
  end;

  { TMultimediaKeys }

  TMultimediaKeys = class
  private
    fMode: integer;
    KeyCapture: TKeyCapture;
    FOnMmKey : MMKey;
    Procedure  SetOnMmKey(aOnMmKey : MmKey);
  public
    constructor Create(Mode:Integer);
    destructor Destroy; override;
    property Mode:integer read fMode;
    property OnMmKey: MmKey read FOnMmKey write SetOnMmKey;

  end;
Const
  FakeKey = $200;

implementation

{ TMultimediaKeys }
{$IFDEF WINDOWS}
{$I mmkey_win.inc}
{$ENDIF}
{$IFDEF UNIX}
{$I mmkey_Linux.inc}
{$ENDIF}

function TKeyCapture.GetGrabbed: Boolean;
begin
  result:=False;
end;

procedure TMultimediaKeys.SetOnMmKey(aOnMmKey: MmKey);
begin
  FOnMmKey := AOnMmKey
end;

end.

