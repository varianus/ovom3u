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
unit uconfig;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  EditBtn, ButtonPanel, Buttons, ValEdit, Spin, Config, uBackEnd, LoggerUnit;

type

  { TfConfig }

  TfConfig = class(TForm)
  published
    bpConfig: TButtonPanel;
    cbChannelsKind: TComboBox;
    cbEpgKind: TComboBox;
    cbUseChno: TCheckBox;
    cbDownloadLogo: TCheckBox;
    cbHardwareAcceleration: TCheckBox;
    cbLibCEC: TCheckBox;
    edtChannelsFileName: TFileNameEdit;
    edtEpgFileName: TFileNameEdit;
    edtChannelsUrl: TEdit;
    edtEpgUrl: TEdit;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    lb: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lb1: TLabel;
    pcSettings: TPageControl;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    lbWarning: TLabel;
    SpeedButton3: TSpeedButton;
    tsPlugins: TTabSheet;
    tsMpv: TTabSheet;
    tsChannels: TTabSheet;
    vleCustomOptions: TValueListEditor;
    procedure CancelButtonClick(Sender: TObject);
    procedure cbChannelsKindChange(Sender: TObject);
    procedure cbEpgKindChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    FOnWorkDone: TNotifyEvent;
    procedure SetOnWorkDone(AValue: TNotifyEvent);

  public
    Property OnWorkDone:TNotifyEvent read FOnWorkDone write SetOnWorkDone;

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

  Kind := Backend.List.ListProperties.ChannelsKind;
  cbChannelsKind.ItemIndex := Ord(kind);
  cbChannelsKind.OnChange(cbChannelsKind);
  edtChannelsFileName.Text := Backend.List.ListProperties.ChannelsFileName;
  edtChannelsUrl.Text := Backend.List.ListProperties.ChannelsUrl;

  Kind := Backend.EpgData.EpgProperties.EpgKind;
  cbEpgKind.ItemIndex := Ord(kind);
  cbEpgKind.OnChange(cbChannelsKind);
  edtEpgFileName.Text := Backend.EpgData.EpgProperties.EpgFileName;
  edtEpgUrl.Text := Backend.EpgData.EpgProperties.EpgUrl;

  cbUseChno.Checked := Backend.List.ListProperties.UseChno;
  cbDownloadLogo.Checked := Backend.List.ListProperties.ChannelsDownloadLogo;

  cbHardwareAcceleration.Checked := BackEnd.MpvEngine.MPVProperties.HardwareAcceleration;
  vleCustomOptions.Strings.Assign(BackEnd.MpvEngine.MPVProperties.CustomOptions);

  cbLibCEC.Checked := BackEnd.PluginsProperties.EnableCEC;

end;

procedure TfConfig.OKButtonClick(Sender: TObject);
var
  ListProperties: TListProperties;
begin

  Backend.List.ListProperties.ChannelsKind := TProviderKind(cbChannelsKind.ItemIndex);
  Backend.List.ListProperties.ChannelsFileName := edtChannelsFileName.Text;
  Backend.List.ListProperties.ChannelsUrl := edtChannelsUrl.Text;
  Backend.List.ListProperties.ChannelsDownloadLogo := cbDownloadLogo.Checked;
  Backend.List.ListProperties.UseChno := cbUseChno.Checked;

  Backend.EpgData.EpgProperties.EpgKind := TProviderKind(cbEpgKind.ItemIndex);
  Backend.EpgData.EpgProperties.EpgFileName := edtEpgFileName.Text;
  Backend.EpgData.EpgProperties.EpgUrl := edtEpgUrl.Text;

  BackEnd.MpvEngine.MPVProperties.HardwareAcceleration := cbHardwareAcceleration.Checked;
  BackEnd.MpvEngine.MPVProperties.CustomOptions.Assign(vleCustomOptions.Strings);

  BackEnd.PluginsProperties.EnableCEC := cbLibCEC.Checked;

  ConfigObj.SaveConfig;

  ModalResult := mrOk;
  If Assigned(FOnWorkDone) then
    FOnWorkDone(self);
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

  FOnWorkDone:=AValue;
end;

procedure TfConfig.cbChannelsKindChange(Sender: TObject);
begin
  case cbChannelsKind.ItemIndex of
    0:
    begin
      edtChannelsFileName.Enabled := True;
      edtChannelsUrl.Enabled := False;
    end;
    1:
    begin
      edtChannelsFileName.Enabled := False;
      edtChannelsUrl.Enabled := True;
    end;
  end;
end;

procedure TfConfig.cbEpgKindChange(Sender: TObject);
begin
  case cbEpgKind.ItemIndex of
    0:
    begin
      edtEpgFileName.Enabled := True;
      edtEpgUrl.Enabled := False;
    end;
    1:
    begin
      edtEpgFileName.Enabled := False;
      edtEpgUrl.Enabled := True;
    end;
  end;
end;


procedure TfConfig.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  If Assigned(FOnWorkDone) then
    FOnWorkDone(self);
end;

initialization


finalization
end.
