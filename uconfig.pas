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
    bpConfig: TButtonPanel;
    cbKind: TComboBox;
    cbUseChno: TCheckBox;
    edtFileName: TFileNameEdit;
    edtUrl: TEdit;
    edtUrl1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pcM3u: TPageControl;
    tsLocal: TTabSheet;
    tsURL: TTabSheet;
    procedure CancelButtonClick(Sender: TObject);
    procedure cbKindChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public

  end;


procedure ShowConfig;

implementation

uses um3uloader;

var
  fConfig: TfConfig;

procedure ShowConfig;
begin
  if not Assigned(fConfig) then
    fConfig := TfConfig.Create(nil);
  fConfig.Show;
end;


{$R *.lfm}

{ TfConfig }

procedure TfConfig.FormShow(Sender: TObject);
var
  Kind: TProviderKind;
begin
  pcM3u.ShowTabs := False;
  ConfigObj.ReadConfig;

  Kind := ConfigObj.M3UProperties.Kind;
  cbKind.ItemIndex := Ord(kind);
  cbKind.OnChange(cbKind);
  edtFileName.Text := ConfigObj.M3UProperties.FileName;
  edtUrl.Text := ConfigObj.M3UProperties.Url;
  cbUseChno.Checked := ConfigObj.M3UProperties.UseChno;
  edtUrl1.Text := ConfigObj.M3UProperties.EPGUrl;

end;

procedure TfConfig.OKButtonClick(Sender: TObject);
var
  M3UProperties: TM3UProperties;
begin

  M3UProperties.Kind := TProviderKind(cbKind.ItemIndex);
  M3UProperties.FileName := edtFileName.Text;
  M3UProperties.Url := edtUrl.Text;
  M3UProperties.UseChno := cbUseChno.Checked;
  M3UProperties.EPGUrl := edtUrl1.Text;

  ConfigObj.M3UProperties := M3UProperties;
  ConfigObj.SaveConfig;
  Close;
end;

procedure TfConfig.cbKindChange(Sender: TObject);
begin
  case cbKind.ItemIndex of
    0: pcM3u.ActivePage := tsLocal;
    1: pcM3u.ActivePage := tsURL;
  end;
end;

procedure TfConfig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfConfig.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

initialization
  fConfig := nil;

finalization
  if Assigned(fConfig) then
    fConfig.Free;
end.
