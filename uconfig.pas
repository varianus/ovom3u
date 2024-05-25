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
unit uconfig;

interface

uses
  Classes, SysUtils, SQLDB, SQLite3Conn, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, EditBtn, ButtonPanel, Buttons, ValEdit, Spin, ExtCtrls,
  DBGrids, DBCtrls, Config, uBackEnd, LoggerUnit;

type

  { TfConfig }

  TfConfig = class(TForm)
  published
    bpConfig: TButtonPanel;
    cbMpris2: TCheckBox;
    cbMMkeys: TCheckBox;
    cbHardwareAcceleration: TCheckBox;
    cbLibCEC: TCheckBox;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    dbeName: TDBEdit;
    dbeName1: TDBEdit;
    dbeName2: TDBEdit;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    dsList: TDataSource;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    pcSettings: TPageControl;
    qListsEPG: TStringField;
    qListsGetLogo: TLongintField;
    qListsID: TAutoIncField;
    qListsName: TStringField;
    qListsPosition: TStringField;
    qListsUseNumber: TLongintField;
    rgKeyCaptureMode: TRadioGroup;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    lbWarning: TLabel;
    SpeedButton3: TSpeedButton;
    qLists: TSQLQuery;
    tsPlugins: TTabSheet;
    tsMpv: TTabSheet;
    tsChannels: TTabSheet;
    vleCustomOptions: TValueListEditor;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    FOnWorkDone: TNotifyEvent;
    procedure SetOnWorkDone(AValue: TNotifyEvent);
  public
    property OnWorkDone: TNotifyEvent read FOnWorkDone write SetOnWorkDone;

  end;

var
  fConfig: TfConfig;


function ShowConfig: integer;

implementation

uses um3uloader;

function ShowConfig: integer;
begin
  if not Assigned(fConfig) then
    fConfig := TfConfig.Create(nil);
  Result := fConfig.ShowModal;
end;


{$R *.lfm}

{ TfConfig }

procedure TfConfig.FormShow(Sender: TObject);
var
  Kind: TProviderKind;
begin
  ConfigObj.ReadConfig;

  qLists.DataBase := ConfigObj.DB;
  qLists.Transaction := ConfigObj.TR;;
  qLists.Open;
  { mcmcmcmcmcmcmc
  Kind := Backend.M3ULoader.ActiveList.ChannelKind;
  cbChannelsKind.ItemIndex := Ord(kind);
  cbChannelsKind.OnChange(cbChannelsKind);
  edtChannelsUrl.Text := Backend.M3ULoader.ActiveList.ChannelsUrl;

  Kind := Backend.EpgData.EpgProperties.EpgKind;
  cbEpgKind.ItemIndex := Ord(kind);
  cbEpgKind.OnChange(cbChannelsKind);
  edtEpgFileName.Text := Backend.EpgData.EpgProperties.EpgFileName;
  edtEpgUrl.Text := Backend.EpgData.EpgProperties.EpgUrl;

  cbUseChno.Checked := Backend.M3ULoader.ActiveList.UseChno;
  cbDownloadLogo.Checked := Backend.M3ULoader.ActiveList.ChannelsDownloadLogo;       }

  cbHardwareAcceleration.Checked := BackEnd.MpvEngine.MPVProperties.HardwareAcceleration;
  vleCustomOptions.Strings.Assign(BackEnd.MpvEngine.MPVProperties.CustomOptions);

  cbLibCEC.Checked := BackEnd.PluginsProperties.EnableCEC;
  cbMpris2.Checked := BackEnd.PluginsProperties.EnableMPRIS2;
  cbMMkeys.Checked := BackEnd.PluginsProperties.EnableMMKeys;
  rgKeyCaptureMode.ItemIndex := BackEnd.PluginsProperties.MMKeysMode;

end;

procedure TfConfig.OKButtonClick(Sender: TObject);
var
  ListProperties: TListProperties;
begin

  if qLists.State in dsEditModes then
    qLists.Post;

  BackEnd.MpvEngine.MPVProperties.HardwareAcceleration := cbHardwareAcceleration.Checked;
  BackEnd.MpvEngine.MPVProperties.CustomOptions.Assign(vleCustomOptions.Strings);

  BackEnd.PluginsProperties.EnableCEC := cbLibCEC.Checked;
  BackEnd.PluginsProperties.EnableMPRIS2 := cbMpris2.Checked;
  BackEnd.PluginsProperties.EnableMMKeys := cbMMkeys.Checked;
  BackEnd.PluginsProperties.MMKeysMode := rgKeyCaptureMode.ItemIndex;

  ModalResult := mrOk;
  try
    if Assigned(FOnWorkDone) then
      FOnWorkDone(self);
  finally
    ConfigObj.SaveConfig;
  end;

end;

procedure TfConfig.SpeedButton1Click(Sender: TObject);
begin
  pcSettings.ActivePage := tsChannels;
end;

procedure TfConfig.SpeedButton2Click(Sender: TObject);
begin
  pcSettings.ActivePage := tsMpv;
end;

procedure TfConfig.SpeedButton3Click(Sender: TObject);
begin
  pcSettings.ActivePage := tsPlugins;
end;

procedure TfConfig.SetOnWorkDone(AValue: TNotifyEvent);
begin

  FOnWorkDone := AValue;
end;

procedure TfConfig.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  if Assigned(FOnWorkDone) then
    FOnWorkDone(self);
end;

initialization


finalization
end.
