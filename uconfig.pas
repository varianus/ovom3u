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
  EditBtn, ButtonPanel, Buttons, ValEdit, Spin, Config;

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
  private

  public

  end;


Function ShowConfig: integer;

implementation

uses um3uloader;

var
  fConfig: TfConfig;

Function ShowConfig: integer;
begin
  if not Assigned(fConfig) then
    fConfig := TfConfig.Create(nil);
  Result:=fConfig.ShowModal;
end;


{$R *.lfm}

{ TfConfig }

procedure TfConfig.FormShow(Sender: TObject);
var
  Kind: TProviderKind;
begin
  ConfigObj.ReadConfig;

  Kind := ConfigObj.ListProperties.ChannelsKind;
  cbChannelsKind.ItemIndex := Ord(kind);
  cbChannelsKind.OnChange(cbChannelsKind);
  edtChannelsFileName.Text := ConfigObj.ListProperties.ChannelsFileName;
  edtChannelsUrl.Text := ConfigObj.ListProperties.ChannelsUrl;

  Kind := ConfigObj.ListProperties.EpgKind;
  cbEpgKind.ItemIndex := Ord(kind);
  cbEpgKind.OnChange(cbChannelsKind);
  edtEpgFileName.Text := ConfigObj.ListProperties.EpgFileName;
  edtEpgUrl.Text := ConfigObj.ListProperties.EpgUrl;

  cbUseChno.Checked := ConfigObj.ListProperties.UseChno;
  cbDownloadLogo.Checked := ConfigObj.ListProperties.ChannelsDownloadLogo;

  cbHardwareAcceleration.Checked := ConfigObj.MPVProperties.HardwareAcceleration;
  ConfigObj.ReadStrings('mpv/CustomOptions', vleCustomOptions.Strings);

end;

procedure TfConfig.OKButtonClick(Sender: TObject);
var
  ListProperties: TListProperties;
begin

  ListProperties.ChannelsKind := TProviderKind(cbChannelsKind.ItemIndex);
  ListProperties.ChannelsFileName := edtChannelsFileName.Text;
  ListProperties.ChannelsUrl := edtChannelsUrl.Text;
  ListProperties.ChannelsDownloadLogo := cbDownloadLogo.Checked;

  ListProperties.EpgKind := TProviderKind(cbEpgKind.ItemIndex);
  ListProperties.EpgFileName := edtEpgFileName.Text;
  ListProperties.EpgUrl := edtEpgUrl.Text;
  ListProperties.UseChno := cbUseChno.Checked;
  ConfigObj.ListProperties := ListProperties;

  ConfigObj.MPVProperties.HardwareAcceleration := cbHardwareAcceleration.Checked;
  ConfigObj.WriteStrings('mpv/CustomOptions', vleCustomOptions.Strings);
  ConfigObj.SaveConfig;
  ModalResult:=mrOK;
end;

procedure TfConfig.SpeedButton1Click(Sender: TObject);
begin
  pcSettings.ActivePage:= tsChannels;
end;

procedure TfConfig.SpeedButton2Click(Sender: TObject);
begin
  pcSettings.ActivePage:= tsMpv;
end;

procedure TfConfig.cbChannelsKindChange(Sender: TObject);
begin
  case cbChannelsKind.ItemIndex of
    0: begin
         edtChannelsFileName.Enabled := true;
         edtChannelsUrl.Enabled := false;
    end;
    1: begin
         edtChannelsFileName.Enabled := false;
         edtChannelsUrl.Enabled := true;
    end;
  end;
end;

procedure TfConfig.cbEpgKindChange(Sender: TObject);
begin
  case cbEpgKind.ItemIndex of
    0: begin
         edtEpgFileName.Enabled := true;
         edtEpgUrl.Enabled := false;
    end;
    1: begin
         edtEpgFileName.Enabled := false;
         edtEpgUrl.Enabled := true;
    end;
  end;
end;

procedure TfConfig.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

initialization
  fConfig := nil;

finalization
  if Assigned(fConfig) then
    fConfig.Free;
end.
