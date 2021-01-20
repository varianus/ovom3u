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
  EditBtn, ButtonPanel, Config;

type

  { TfConfig }

  TfConfig = class(TForm)
  published
    bpConfig: TButtonPanel;
    cbChannelsKind: TComboBox;
    cbEpgKind: TComboBox;
    cbUseChno: TCheckBox;
    edtChannelsFileName: TFileNameEdit;
    edtEpgFileName: TFileNameEdit;
    edtChannelsUrl: TEdit;
    edtEpgUrl: TEdit;
    Label5: TLabel;
    lb: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lb1: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure cbChannelsKindChange(Sender: TObject);
    procedure cbEpgKindChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
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

  Kind := ConfigObj.M3UProperties.ChannelsKind;
  cbChannelsKind.ItemIndex := Ord(kind);
  cbChannelsKind.OnChange(cbChannelsKind);
  edtChannelsFileName.Text := ConfigObj.M3UProperties.ChannelsFileName;
  edtChannelsUrl.Text := ConfigObj.M3UProperties.ChannelsUrl;

  Kind := ConfigObj.M3UProperties.EpgKind;
  cbEpgKind.ItemIndex := Ord(kind);
  cbEpgKind.OnChange(cbChannelsKind);
  edtEpgFileName.Text := ConfigObj.M3UProperties.EpgFileName;
  edtEpgUrl.Text := ConfigObj.M3UProperties.EpgUrl;


  cbUseChno.Checked := ConfigObj.M3UProperties.UseChno;

end;

procedure TfConfig.OKButtonClick(Sender: TObject);
var
  M3UProperties: TM3UProperties;
begin

  M3UProperties.ListChanged := (TProviderKind(cbChannelsKind.ItemIndex) <> ConfigObj.M3UProperties.ChannelsKind) or
     (ConfigObj.M3UProperties.ChannelsFileName <> edtChannelsFileName.Text) or
     (ConfigObj.M3UProperties.ChannelsUrl <> edtChannelsUrl.Text) or
     (ConfigObj.M3UProperties.UseChno <> cbUseChno.Checked);
  M3UProperties.ChannelsKind := TProviderKind(cbChannelsKind.ItemIndex);
  M3UProperties.ChannelsFileName := edtChannelsFileName.Text;
  M3UProperties.ChannelsUrl := edtChannelsUrl.Text;

  M3UProperties.EPGChanged := (TProviderKind(cbEpgKind.ItemIndex) <> ConfigObj.M3UProperties.EpgKind) or
     (ConfigObj.M3UProperties.EpgFileName <> edtEpgFileName.Text) or
     (ConfigObj.M3UProperties.EpgUrl <> edtEpgUrl.Text) or
     (ConfigObj.M3UProperties.UseChno <> cbUseChno.Checked);
  M3UProperties.EpgKind := TProviderKind(cbEpgKind.ItemIndex);
  M3UProperties.EpgFileName := edtEpgFileName.Text;
  M3UProperties.EpgUrl := edtEpgUrl.Text;

  M3UProperties.UseChno := cbUseChno.Checked;
  ConfigObj.M3UProperties := M3UProperties;
  ConfigObj.SaveConfig;
  ModalResult:=mrOK;
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
