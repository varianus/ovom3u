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
unit uLogViewer;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LoggerUnit, LCLType;

type

  { TfLogViewer }

  TfLogViewer = class(TForm)
    Button1: TButton;
    cbAutoScroll: TCheckBox;
    cbLevel: TComboBox;
    LogMemo: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure cbAutoScrollChange(Sender: TObject);
    procedure cbLevelChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LogMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private
    procedure OnLog(Sender: TObject; const Message: string);
  public
    procedure Init;

  end;

var
  fLogViewer: TfLogViewer;

implementation

{$R *.lfm}

{ TfLogViewer }

procedure TfLogViewer.FormDestroy(Sender: TObject);
begin
  OvoLogger.OnLogMessage := nil;
end;

procedure TfLogViewer.LogMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_END then
    if ssCtrl in Shift then
      begin
        LogMemo.SelStart  := Length(LogMemo.Lines.Text) - Length(LineEnding);
        LogMemo.SelLength := 0;
        Key:=0;
      end;


end;

procedure TfLogViewer.Button1Click(Sender: TObject);
begin
  LogMemo.Clear;
end;

procedure TfLogViewer.cbAutoScrollChange(Sender: TObject);
begin
  if not cbAutoScroll.Checked then
  begin
    LogMemo.SelStart  := Length(LogMemo.Lines.Text) - Length(LineEnding);
    LogMemo.SelLength := 0;
  end;
end;

procedure TfLogViewer.cbLevelChange(Sender: TObject);
begin
  OvoLogger.LevelFromString(cbLevel.Items[cbLevel.ItemIndex]);
end;

procedure TfLogViewer.OnLog(Sender: TObject; const Message: string);
begin
  LogMemo.Lines.BeginUpdate;
  LogMemo.Lines.Append(Message);
  LogMemo.Lines.EndUpdate;
  if cbAutoScroll.Checked then
  begin
    LogMemo.SelStart  := MaxInt;
    LogMemo.SelLength := 0;
  end;
end;

procedure TfLogViewer.Init;
begin
  LogMemo.Clear;
  cbLevel.ItemIndex      := cbLevel.Items.IndexOf(OvoLogger.DecodeLevel);
  OvoLogger.OnLogMessage := onlog;

end;

end.
