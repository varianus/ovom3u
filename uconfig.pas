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
  DBGrids, DBCtrls, Grids, Config, uBackEnd, LoggerUnit;

type

  { TfConfig }

  TfConfig = class(TForm)
  published
    bpConfig: TButtonPanel;
    cbMpris2: TCheckBox;
    cbMMkeys: TCheckBox;
    cbHardwareAcceleration: TCheckBox;
    cbLibCEC: TCheckBox;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    lbLists: TListBox;
    pcSettings: TPageControl;
    rgKeyCaptureMode: TRadioGroup;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    lbWarning: TLabel;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    ToolBar1: TToolBar;
    tbAdd: TToolButton;
    tbRemove: TToolButton;
    tbSpacer: TToolButton;
    tsPlugins: TTabSheet;
    tsMpv: TTabSheet;
    tsChannels: TTabSheet;
    ValueListEditor1: TValueListEditor;
    vleCustomOptions: TValueListEditor;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbListsSelectionChange(Sender: TObject; User: boolean);
    procedure OKButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure tbAddClick(Sender: TObject);
    procedure tbRemoveClick(Sender: TObject);
  private
    PreviousIndex: integer;
    FOnWorkDone: TNotifyEvent;
    procedure ListItemToScreen(CurrItem: TM3UList);
    procedure ScreenToListItem(CurrItem: TM3UList);
    procedure SetEditMode(Editing: boolean);
    procedure SetOnWorkDone(AValue: TNotifyEvent);
  public
    procedure Init;
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
  List: TM3UList;
begin
  ConfigObj.ReadConfig;

  cbHardwareAcceleration.Checked := BackEnd.MpvEngine.MPVProperties.HardwareAcceleration;
  vleCustomOptions.Strings.Assign(BackEnd.MpvEngine.MPVProperties.CustomOptions);

  cbLibCEC.Checked := BackEnd.PluginsProperties.EnableCEC;
  cbMpris2.Checked := BackEnd.PluginsProperties.EnableMPRIS2;
  cbMMkeys.Checked := BackEnd.PluginsProperties.EnableMMKeys;
  rgKeyCaptureMode.ItemIndex := BackEnd.PluginsProperties.MMKeysMode;

  lbLists.Clear;
  if ConfigObj.ListManager.Count > 0 then
  begin
    for List in ConfigObj.ListManager do
      lbLists.AddItem(List.Name, List);
    lbLists.Selected[0] := True;
  end;

end;

procedure TfConfig.ListItemToScreen(CurrItem: TM3UList);
begin
  ValueListEditor1.Values[ValueListEditor1.Keys[0]] := CurrItem.Name;
  ValueListEditor1.Values[ValueListEditor1.Keys[1]] := CurrItem.ChannelsUrl;
  ValueListEditor1.Values[ValueListEditor1.Keys[2]] := BoolToStr(CurrItem.UseChno, True);
  ValueListEditor1.Values[ValueListEditor1.Keys[3]] := BoolToStr(CurrItem.ChannelsDownloadLogo, True);
  ValueListEditor1.Values[ValueListEditor1.Keys[4]] := BoolToStr(CurrItem.EPGFromM3U, True);
  ValueListEditor1.Values[ValueListEditor1.Keys[5]] := CurrItem.EPGUrl;
  ValueListEditor1.Modified:= false;
  ValueListEditor1.Invalidate;
  ;
end;

procedure TfConfig.ScreenToListItem(CurrItem: TM3UList);
begin
  CurrItem.Name := ValueListEditor1.Values[ValueListEditor1.Keys[0]];
  CurrItem.ChannelsUrl := ValueListEditor1.Values[ValueListEditor1.Keys[1]];
  CurrItem.UseChno := StrToBool(ValueListEditor1.Values[ValueListEditor1.Keys[2]]);
  CurrItem.ChannelsDownloadLogo := StrToBool(ValueListEditor1.Values[ValueListEditor1.Keys[3]]);
  CurrItem.EPGFromM3U := StrToBool(ValueListEditor1.Values[ValueListEditor1.Keys[4]]);
  CurrItem.EPGUrl := ValueListEditor1.Values[ValueListEditor1.Keys[5]];
  lbLists.Items[PreviousIndex] := CurrItem.Name;
  if ValueListEditor1.Modified then
    ConfigObj.ListManager.ListAdd(CurrItem);
end;

procedure TfConfig.lbListsSelectionChange(Sender: TObject; User: boolean);
begin
  if PreviousIndex = -1 then
  begin
    PreviousIndex := lbLists.ItemIndex;
    ListItemToScreen(TM3UList(lbLists.Items.Objects[PreviousIndex]));
    exit;
  end;
  if (PreviousIndex <> -1) and (PreviousIndex <> lbLists.ItemIndex) then
    ScreenToListItem(TM3UList(lbLists.Items.Objects[PreviousIndex]));
  PreviousIndex := lbLists.ItemIndex;
  ListItemToScreen(TM3UList(lbLists.Items.Objects[PreviousIndex]));

end;

procedure TfConfig.OKButtonClick(Sender: TObject);
var
  ListProperties: TListProperties;
begin

  BackEnd.MpvEngine.MPVProperties.HardwareAcceleration := cbHardwareAcceleration.Checked;
  BackEnd.MpvEngine.MPVProperties.CustomOptions.Assign(vleCustomOptions.Strings);

  BackEnd.PluginsProperties.EnableCEC := cbLibCEC.Checked;
  BackEnd.PluginsProperties.EnableMPRIS2 := cbMpris2.Checked;
  BackEnd.PluginsProperties.EnableMMKeys := cbMMkeys.Checked;
  BackEnd.PluginsProperties.MMKeysMode := rgKeyCaptureMode.ItemIndex;
  ScreenToListItem(TM3UList(lbLists.Items.Objects[lbLists.ItemIndex]));

  try
    if Assigned(FOnWorkDone) then
      FOnWorkDone(self);
  finally
    ConfigObj.SaveConfig;
  end;
  ModalResult := mrOk;
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

procedure TfConfig.tbAddClick(Sender: TObject);
var
  NewList: TM3UList;
begin
  NewList := TM3UList.Create('Untitled');
  ConfigObj.ListManager.ListAdd(NewList);
  lbLists.AddItem('Untitled', NewList);
  lbLists.Selected[lbLists.Count - 1] := True;
end;

procedure TfConfig.SetEditMode(Editing: boolean);
begin
end;

procedure TfConfig.tbRemoveClick(Sender: TObject);
var
  CurrentItem: TM3UList;
begin
  CurrentItem := TM3UList(lbLists.Items.Objects[lbLists.ItemIndex]);
  if Dialogs.MessageDlg('', 'Delete list ?', mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    if CurrentItem.ListID <> 0 then
      ConfigObj.ListManager.ListDelete(CurrentItem);
    lbLists.DeleteSelected;
  end;
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

procedure TfConfig.FormCreate(Sender: TObject);
begin
  init;
end;

procedure TfConfig.Init;
begin
  PreviousIndex := -1;
  with ValueListEditor1.ItemProps[0] do
    EditStyle := esSimple;
  with ValueListEditor1.ItemProps[1] do
    EditStyle := esSimple;
  with ValueListEditor1.ItemProps[2] do
  begin
    EditStyle := esPickList;
    PickList.Text := 'True' + sLineBreak + 'False';
  end;
  with ValueListEditor1.ItemProps[3] do
  begin
    EditStyle := esPickList;
    PickList.Text := 'True' + sLineBreak + 'False';
  end;
end;


initialization


finalization
end.
