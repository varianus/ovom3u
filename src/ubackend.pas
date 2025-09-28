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
unit uBackEnd;

interface

uses
  Classes, SysUtils, fptimer, um3uloader, epg, Config, MPV_Engine, LoggerUnit,
  GeneralFunc, BaseTypes, OpenGLContext
  {$IFDEF USE_LIBCEC}, cec_intf {$ENDIF}
  {$IFDEF USE_MMKEYS}, MultimediaKeys{$ENDIF}
  {$IFDEF USE_MPRIS2}, mpris2{$ENDIF}
  ;

resourcestring
  RS_NoChannel = 'No channel';
  RS_MissingAddress = 'Missing Channel Address';
  RS_LoadMessage = '{\s}Load: {\n}%3.3d %s';

type
  ExternalInput = procedure(Sender: TObject; var Key: word) of object;

  { TPluginsProperties }

  TPluginsProperties = class(TConfigParam)
  private
    FEnableCEC: boolean;
    FEnableMMKeys: boolean;
    FEnableMPRIS2: boolean;
    FMMKeysMode: integer;
    procedure SetEnableCEC(AValue: boolean);
    procedure SetEnableMMKeys(AValue: boolean);
    procedure SetEnableMPRIS2(AValue: boolean);
    procedure SetMMKeysMode(AValue: integer);
  protected
    procedure InternalSave; override;
  public
    property EnableCEC: boolean read FEnableCEC write SetEnableCEC;
    property EnableMPRIS2: boolean read FEnableMPRIS2 write SetEnableMPRIS2;
    property EnableMMKeys: boolean read FEnableMMKeys write SetEnableMMKeys;
    property MMKeysMode: integer read FMMKeysMode write SetMMKeysMode;
    procedure Load; override;

  end;

  { TBackend }

  TBackend = class
  private
    FOnExternalInput: ExternalInput;
    FOnListChanged: TNotifyEvent;
    FOnPlay: TNotifyEvent;
    procedure OnListChangedPlay(Sender: TObject);
    procedure OSDTimerTimer(Sender: TObject);
    procedure SetOnExternalInput(AValue: ExternalInput);
    procedure CecKey(Sender: TObject; var Key: word);
    procedure SetOnListChanged(AValue: TNotifyEvent);
    procedure SetOnPlay(AValue: TNotifyEvent);
    procedure LogLevelChange(Sender: TObject);
  public
    M3ULoader: TM3ULoader;
    EpgData: TEpg;
    M3UList: TM3UList;
    {$IFDEF USE_LIBCEC}HDMI_CEC: THDMI_CEC;{$ENDIF}
    {$IFDEF USE_MMKEYS}mmkey: TMultimediaKeys;{$ENDIF}
    {$IFDEF USE_MPRIS2} Mpris: TMpris2;
    {$ENDIF}
    MpvEngine: TMPVEngine;
    OSDTimer: TFPTimer;
    Loading: boolean;
    PreviousIndex, CurrentIndex: integer;
    ShowingInfo: boolean;

  public
    PluginsProperties: TPluginsProperties;
    procedure ShowEpg;
    function MapChannel(Index: integer): integer;
    function MapIndex(Index: integer): integer;
    procedure OsdMessage(Message: string; TimeOut: boolean = True);
    procedure LoadList(AList: TM3UList);
    function InitializeEngine(Renderer: TOpenGLControl): boolean;
    procedure Play(index: integer);
    procedure SwapChannel;
    procedure HardReset;
  public
    property OnExternalInput: ExternalInput read FOnExternalInput write SetOnExternalInput;
    property OnListChanged: TNotifyEvent read FOnListChanged write SetOnListChanged;
    property OnPlay: TNotifyEvent read FOnPlay write SetOnPlay;
    constructor Create;
    destructor Destroy; override;
  end;

function BackEnd: TBackend;

implementation

var
  fBackend: TBackend;

function BackEnd: TBackend;
begin
  if not Assigned(fBackend) then
    fBackend := TBackend.Create;
  Result     := fBackend;
end;
{ TPluginsProperties }

procedure TPluginsProperties.SetEnableCEC(AValue: boolean);
begin
  if FEnableCEC = AValue then Exit;
  FEnableCEC := AValue;
  Dirty      := True;
end;

procedure TPluginsProperties.SetEnableMMKeys(AValue: boolean);
begin
  if FEnableMMKeys = AValue then Exit;
  FEnableMMKeys := AValue;
end;

procedure TPluginsProperties.SetEnableMPRIS2(AValue: boolean);
begin
  if FEnableMPRIS2 = AValue then Exit;
  FEnableMPRIS2 := AValue;
end;

procedure TPluginsProperties.SetMMKeysMode(AValue: integer);
begin
  if FMMKeysMode = AValue then Exit;
  FMMKeysMode := AValue;
end;

procedure TPluginsProperties.InternalSave;
begin
  Owner.WriteBoolean('Plugins/HDMI-CEC/Enabled', EnableCEC);
  Owner.WriteBoolean('Plugins/MPRIS2/Enabled', EnableMPRIS2);
  Owner.WriteBoolean('Plugins/MMKeys/Enabled', EnableMMKeys);
  Owner.WriteInteger('Plugins/MMKeys/Mode', MMKeysMode);
end;

procedure TPluginsProperties.Load;
begin
  EnableCEC := Owner.ReadBoolean('Plugins/HDMI-CEC/Enabled', False);
  EnableMPRIS2 := Owner.ReadBoolean('Plugins/MPRIS2/Enabled', False);
  EnableMMKeys := Owner.ReadBoolean('Plugins/MMKeys/Enabled', False);
  MMKeysMode := Owner.ReadInteger('Plugins/MMKeys/Mode', 1);
  Dirty := False;
end;

{ TBackend }

procedure TBackend.LoadList(AList: TM3UList);
begin
  M3UList := AList;
  M3ULoader.ActiveList := M3UList;
  epgData.ActiveList := M3UList;

end;


function TBackend.InitializeEngine(Renderer: TOpenGLControl): boolean;
begin
  mpvengine := TMPVEngine.Create;
  Result    := MpvEngine.Initialize(Renderer);
  OvoLogger.OnLevelChange := LogLevelChange;
end;

procedure TBackend.OsdMessage(Message: string; TimeOut: boolean = True);
begin

  if MpvEngine.GLRenderControl.Visible then
  begin
    mpvengine.OsdMessage(message);
    OSDTimer.Enabled := TimeOut;
  end;

end;

procedure TBackend.Play(index: integer);
var
  fLastMessage: string;
  ChNum: integer;
begin

  if (Index > M3ULoader.Count) or (Index < 0) then
  begin
    OsdMessage(RS_NoChannel, True);
    exit;
  end;

  if (CurrentIndex = Index) and not mpvengine.IsIdle then
    exit;

  if M3ULoader[Index].Mrl.IsEmpty then
  begin
    OsdMessage(RS_MissingAddress, True);
    exit;
  end;

  OvoLogger.Log(llINFO, 'Tuning to %s', [M3ULoader[Index].Title]);

  PreviousIndex := CurrentIndex;
  CurrentIndex  := Index;
  mpvengine.Play(BackEnd.M3ULoader[CurrentIndex].Mrl);
  if BackEnd.M3ULoader.ActiveList.UseChno then
    ChNum := BackEnd.M3ULoader[CurrentIndex].tvg_chno
  else
    ChNum := index + 1;
  Loading := True;
  fLastMessage := format(RS_LoadMessage, [ChNum, BackEnd.M3ULoader[CurrentIndex].title]);
  OsdMessage(fLastMessage);
  if Assigned(FOnPlay) then
    FOnPlay(Self);
end;

procedure TBackend.SwapChannel;
begin
  if PreviousIndex <> -1 then
    Play(PreviousIndex);
end;

procedure TBackend.HardReset;
var
  Renderer: TOpenGLControl;
begin

  Renderer := MpvEngine.GLRenderControl;
  mpvengine.Stop;
  MpvEngine.isRenderActive := False;

  sleep(100);
  MpvEngine.Free;

  InitializeEngine(Renderer);
end;


procedure TBackend.ShowEpg;
var
  Info: REpgInfo;
begin
  if not ShowingInfo and (currentIndex <> -1) then
  begin
    Info := epgData.GetEpgInfo(CurrentIndex, now);
    mpvengine.OsdEpg(Format('%3.3d: %s', [M3ULoader[CurrentIndex].Number, BackEnd.M3ULoader[CurrentIndex].title]), info, True);
    ShowingInfo      := True;
    OSDTimer.Enabled := True;
  end
  else
    OSDTimerTimer(self);

end;

function TBackend.MapChannel(Index: integer): integer;
begin
  if M3ULoader.ActiveList.UseChno then
    Result := M3ULoader.ItemByChno(Index)
  else
  if index >= M3ULoader.Count then
    Result := -1
  else
    Result := Index - 1;

end;

function TBackend.MapIndex(Index: integer): integer;
begin
  if index >= M3ULoader.Count then
    Result := -1
  else
  if M3ULoader.ActiveList.UseChno then
    Result := M3ULoader.Items[Index].tvg_chno
  else
    Result := Index + 1;
end;

procedure TBackend.OSDTimerTimer(Sender: TObject);
begin

  if MpvEngine.GLRenderControl.Visible then
  begin
    mpvengine.OsdEpg('', Default(REpgInfo), False);
    mpvengine.OsdMessage();
    ShowingInfo := False;
  end;
  OSDTimer.Enabled := False;
end;

procedure TBackend.SetOnExternalInput(AValue: ExternalInput);
begin
  FOnExternalInput := AValue;
end;

procedure TBackend.CecKey(Sender: TObject; var Key: word);
begin
  if Assigned(FOnExternalInput) then
    FOnExternalInput(Sender, key);
end;

procedure TBackend.SetOnListChanged(AValue: TNotifyEvent);
begin
  FOnListChanged := AValue;
end;

procedure TBackend.SetOnPlay(AValue: TNotifyEvent);
begin
  FOnPlay := AValue;
end;

procedure TBackend.LogLevelChange(Sender: TObject);
begin
  if Assigned(MpvEngine) then
    MpvEngine.UpdateLogLevel;
end;

procedure TBackend.OnListChangedPlay(Sender: TObject);
begin

  if Assigned(FOnListChanged) then
    FOnListChanged(Sender);

end;


constructor TBackend.Create;
begin
  PluginsProperties := TPluginsProperties.Create(ConfigObj);
  M3ULoader := TM3ULoader.Create;
  EpgData   := TEpg.Create;

  M3ULoader.OnListChanged := OnListChangedPlay;
  {$IFDEF USE_LIBCEC}
  if PluginsProperties.EnableCEC then
  try
    HDMI_CEC := THDMI_CEC.Create;
    HDMI_CEC.OnCecKey := CecKey;
  except
    on e: Exception do
    begin
      OvoLogger.Log(llERROR, 'CEC ->' + e.Message);
      HDMI_CEC := nil;
    end;
  end
  else
    HDMI_CEC := nil;
  {$ENDIF}


  {$IFDEF USE_MMKEYS}
  if PluginsProperties.EnableMMKeys then
  try
    mmkey := TMultimediaKeys.Create(PluginsProperties.MMKeysMode);
    mmkey.OnMmKey := CecKey;
  except
    on e: Exception do
    begin
      OvoLogger.Log(llERROR, 'MultimediaKeys ->' + e.Message);
      mmkey := nil;
    end;
  end
  else
    mmkey := nil;
  {$ENDIF}


  {$IFDEF USE_MPRIS2}
  if PluginsProperties.EnableMPRIS2 then
  try
    Mpris := TMpris2.Create();
    Mpris.Activate();
    Mpris.OnMmKey := CecKey;
  except
    on e: Exception do
    begin
      OvoLogger.Log(llERROR, 'MPRIS2 ->' + e.Message);
      mmkey := nil;
    end;
  end
  else
    Mpris := nil;
  {$ENDIF}

  OSDTimer      := TFPTimer.Create(nil);
  OSDTimer.Enabled := False;
  OSDTimer.Interval := 8000;
  OSDTimer.OnTimer := OSDTimerTimer;
  CurrentIndex  := -1;
  PreviousIndex := -1;
  ShowingInfo   := False;

end;

destructor TBackend.Destroy;
begin
  OvoLogger.OnLevelChange := nil;
  MpvEngine.Free;
  OsdTimer.Free;
  EpgData.Free;
  M3ULoader.Free;
  {$IFDEF USE_MMKEYS}
  mmkey.Free;
  {$ENDIF}
  {$IFDEF USE_LIBCEC}
  HDMI_CEC.Free;
  {$ENDIF}

  {$IFDEF USE_MPRIS2}
  Mpris.Free;
  {$ENDIF}
  inherited Destroy;
end;


initialization
  fBackend := nil;

finalization
  fBackend.Free;
end.
