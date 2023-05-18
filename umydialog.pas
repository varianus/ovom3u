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
unit uMyDialog;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ButtonPanel, ComCtrls,
  LCLStrConsts, Buttons;

type

  { TMyDialog }

  TMyDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    imgIcon: TImage;
    lbText: TLabel;
    lbTitle: TLabel;
    Panel1: TPanel;
    pnlActions: TPanel;
    ToolBar1: TPanel;
  private
    btnList: array [0..11] of TBitBtn;
  public

  end;

function ShowMyDialog(DlgType: TMsgDlgType; const Title: string; const Message: string; Buttons: TMsgDlgButtons; CustomButtons: Array of string): integer;

implementation

function ShowMyDialog(DlgType: TMsgDlgType; const Title: string; const Message: string; Buttons: TMsgDlgButtons; CustomButtons: Array of string ): integer;
var
  frm: TMyDialog;
  NumButtons, i: Integer;
  DefaultSpacing : TControlBorderSpacing;
begin
  frm := TMyDialog.Create(nil);
  with frm do
  begin
    case DlgType of
      mtWarning:
      begin
        Caption := rsMtWarning;
        imgicon.Picture.LoadFromResourceName(hInstance, 'dialog_warning', TPortableNetworkGraphic);
      end;
      mtError:
      begin
        Caption := rsMtError;
        imgicon.Picture.LoadFromResourceName(hInstance, 'dialog_error', TPortableNetworkGraphic);
      end;
      mtConfirmation:
      begin
        Caption := rsMtConfirmation;
        imgicon.Picture.LoadFromResourceName(hInstance, 'dialog_confirmation', TPortableNetworkGraphic);
      end;
      mtInformation:
      begin
        Caption := rsMtInformation;
        imgicon.Picture.LoadFromResourceName(hInstance, 'dialog_information', TPortableNetworkGraphic);
      end;
      mtCustom:
      begin
        Caption := ApplicationName;
        imgicon.Width := 8;
        imgicon.Hide;
      end;
    end;
    NumButtons := 0;
    DefaultSpacing := TControlBorderSpacing.Create(ToolBar1);
    DefaultSpacing.Left := 9;
    DefaultSpacing.Right := 9;
    if mbHelp in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkHelp;
       inc(NumButtons);
     end;
     if mbYes in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkYes;
       btnList[NumButtons].ModalResult:= mrYes;
       inc(NumButtons);
     end;
     if mbYesToAll in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkYesToAll;
       btnList[NumButtons].ModalResult:= mrYesToAll;
       inc(NumButtons);
     end;
     if mbNo in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkNo;
       btnList[NumButtons].ModalResult:= mrNo;
       inc(NumButtons);
     end;
     if mbNoToAll in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkNoToAll;
       btnList[NumButtons].ModalResult:= mrNoToAll;
       inc(NumButtons);
     end;
     if mbAll in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkAll;
       btnList[NumButtons].ModalResult:= mrAll;
       inc(NumButtons);
     end;
     if mbOK in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkOK;
       btnList[NumButtons].ModalResult:= mrOK;
       inc(NumButtons);
     end;
     if mbRetry in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkRetry;
       btnList[NumButtons].ModalResult:= mrRetry;
       inc(NumButtons);
     end;
     if mbIgnore in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkIgnore;
       btnList[NumButtons].ModalResult:= mrIgnore;
       inc(NumButtons);
     end;
     if mbCancel in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkCancel;
       btnList[NumButtons].ModalResult:= mrCancel;
       inc(NumButtons);
     end;
     if mbAbort in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkAbort;
       btnList[NumButtons].ModalResult:= mrAbort;
       inc(NumButtons);
     end;
     if mbClose in Buttons then begin
       btnList[NumButtons] := TBitBtn.Create(ToolBar1);
       btnList[NumButtons].Parent := ToolBar1;
       btnList[NumButtons].Kind:= bkClose;
       btnList[NumButtons].ModalResult:= mrClose;
       inc(NumButtons);
     end;

     for i:= NumButtons -1 downto 0 do
       begin
         btnList[i].Align := alRight;
         btnList[i].BorderSpacing.Assign(DefaultSpacing);
       end;
    NumButtons := 0;
    for i := 0 to Length(CustomButtons) -1 do
      begin
        btnList[NumButtons] := TBitBtn.Create(pnlActions);
        btnList[NumButtons].Parent := pnlActions;
        btnList[NumButtons].Kind:= bkCustom;
        btnList[NumButtons].Align := alTop;
        btnList[NumButtons].Caption := CustomButtons[i];
        btnList[NumButtons].ModalResult := 100+i;
        inc(NumButtons);
      end;

    lbTitle.Caption := Title;
    lbText.Caption := Message;
    Result := ShowModal;
  end;
end;

{$R *.lfm}

end.
