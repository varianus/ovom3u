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
unit cec_intf;
{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, cec, config;

Type

  CecKey = procedure(Sender: TObject; var Key: word) of Object;

  { THDMI_CEC }

  THDMI_CEC = class
  private
    Config: libcec_configuration;
    Callback :ICECCallbacks;
    Connection: libcec_connection_t;
    FOnCecKey: CecKey;
    Procedure DoCecKey(Key: Word);
    procedure SetOnCecKey(AValue: CecKey);
  public
    Constructor Create;
    Destructor Destroy; override;
    property OnCecKey: CecKey read FOnCecKey write SetOnCecKey;
  end;

implementation
uses LoggerUnit, LCLType;

{ THDMI_CEC }
procedure LogMessage(cbparam: Pointer; const &message: Pcec_log_message); cdecl;
begin
  OvoLogger.Log(llTRACE, &Message^.&message);
end;

procedure KeyPress(cbparam: Pointer; const key: Pcec_keypress); cdecl;
var
  cec: THDMI_CEC absolute cbparam;
const
  FakeKey = $200;
begin
 // if key^.duration > 0 then
    begin
    case Key^.keycode of
      CEC_USER_CONTROL_CODE_NUMBER0..CEC_USER_CONTROL_CODE_NUMBER9:    cec.DoCecKey(ord(Key^.keycode) +16+FakeKey);
      CEC_USER_CONTROL_CODE_DISPLAY_INFORMATION: cec.DoCecKey(FakeKey+VK_I);
      CEC_USER_CONTROL_CODE_ELECTRONIC_PROGRAM_GUIDE: cec.DoCecKey(FakeKey+VK_E);
      CEC_USER_CONTROL_CODE_SELECT: cec.DoCecKey(+VK_RETURN);
      CEC_USER_CONTROL_CODE_CHANNEL_UP: cec.DoCecKey(FakeKey+VK_MEDIA_NEXT_TRACK);
      CEC_USER_CONTROL_CODE_CHANNEL_DOWN: cec.DoCecKey(FakeKey+VK_MEDIA_PREV_TRACK);
      CEC_USER_CONTROL_CODE_LEFT: cec.DoCecKey(VK_LEFT);
      CEC_USER_CONTROL_CODE_RIGHT: cec.DoCecKey(VK_RIGHT);
      CEC_USER_CONTROL_CODE_UP: cec.DoCecKey(VK_UP);
      CEC_USER_CONTROL_CODE_DOWN: cec.DoCecKey(VK_DOWN);
      CEC_USER_CONTROL_CODE_ROOT_MENU : cec.DoCecKey(FakeKey+VK_C);

      CEC_USER_CONTROL_CODE_STOP: cec.DoCecKey(FakeKey+VK_MEDIA_STOP);
      CEC_USER_CONTROL_CODE_PAUSE: cec.DoCecKey(FakeKey+VK_P);
      CEC_USER_CONTROL_CODE_EXIT: cec.DoCecKey(VK_ESCAPE);
      CEC_USER_CONTROL_CODE_F4_YELLOW: cec.DoCecKey(FakeKey+VK_F);
      CEC_USER_CONTROL_CODE_F2_RED: cec.DoCecKey($300);
      CEC_USER_CONTROL_CODE_F3_GREEN: cec.DoCecKey(FakeKey+VK_B);
      CEC_USER_CONTROL_CODE_F1_BLUE: cec.DoCecKey(FakeKey+VK_TAB);
      CEC_USER_CONTROL_CODE_FAST_FORWARD: cec.DoCecKey(FakeKey+VK_RIGHT);
      CEC_USER_CONTROL_CODE_REWIND: cec.DoCecKey(FakeKey+VK_LEFT);
    end;
    OvoLogger.Log(llDEBUG, ' CEC Key %d',   [Key^.keycode]);
    end;

end;

procedure CommandReceived(cbparam: Pointer; const cmd: Pcec_command); cdecl;
begin
  OvoLogger.Log(llTRACE, 'Command %d>%d, %d ',[cmd^.initiator, cmd^.destination, cmd^.opcode]);
{  case cmd^.opcode of
   CEC_OPCODE_STANDBY:
     begin
       if(cmd^.initiator = CECDEVICE_TV)  then
         OvoLogger.Log(llFORCED, 'TV STANDBY');
     end;
  end;         }
end;

procedure THDMI_CEC.DoCecKey(Key: Word);
begin
  if Assigned(FOnCecKey) then
    FOnCecKey(self, Key);
end;

procedure THDMI_CEC.SetOnCecKey(AValue: CecKey);
begin
  FOnCecKey:=AValue;
end;

constructor THDMI_CEC.Create;
var
  ca: cec_adapter;
  num_adapters: Int8;
begin

  Load_LibCEC(cec.libcec);

  libcec_clear_configuration( @config);
  CallBack.commandReceived:= @CommandReceived;
  Callback.logMessage:=@LogMessage;
  Callback.keyPress:=@KeyPress;
  config.callbacks:= @Callback;
  config.deviceTypes.types[0] := CEC_DEVICE_TYPE_RECORDING_DEVICE;
  config.callbackParam:=self;
  config.iButtonRepeatRateMs:=300;
  Connection := libcec_initialise(@config);
  if not Assigned(Connection) then
    begin
      Raise Exception.Create('Unable to initialise libcec');
    end;

  libcec_init_video_standalone(Connection);
  num_adapters := libcec_find_adapters(Connection, @ca, 1, Nil);
  if num_adapters < 1 then
    begin
      libcec_destroy(Connection);
      Raise Exception.Create('No Cec adapters found');
    end;
 if not boolean(libcec_open(Connection, ca.comm, 5000)) then
   begin
     libcec_destroy(Connection);
     Raise Exception.CreateFmt('Cannot open connection to %s',[ca.comm]);
   end;

  libcec_set_active_source(Connection, CEC_DEVICE_TYPE_RESERVED);
end;

destructor THDMI_CEC.Destroy;
begin
  libcec_destroy(Connection);
  Free_LibCEC;
  inherited Destroy;
end;

end.

