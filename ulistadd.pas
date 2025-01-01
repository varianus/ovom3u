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

unit uListAdd;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EditBtn, Buttons, ButtonPanel, um3uloader, Config;

type

  { TfListAdd }

  TfListAdd = class(TForm)
    BitBtn1: TBitBtn;
    bTest: TButton;
    ButtonPanel1: TButtonPanel;
    leList: TLabeledEdit;
    Memo1: TMemo;
    OpenM3U: TOpenDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure bTestClick(Sender: TObject);
    Procedure Logger(Message:string);
  private

  public

  end;

var
  fListAdd: TfListAdd;

implementation

{$R *.lfm}

{ TfListAdd }

procedure TfListAdd.BitBtn1Click(Sender: TObject);
begin
  OpenM3U.FileName := leList.Text;
  if OpenM3U.Execute then
    leList.Text := OpenM3U.FileName;
end;

procedure TfListAdd.bTestClick(Sender: TObject);
var
  TestLoader: TM3ULoader;
  List: TM3UList;
begin
  List := TM3UList.Create;
  try
    TestLoader := TM3ULoader.Create;
    try
      List.ChannelsUrl := leList.Text;
      Memo1.Clear;
      list.EPGFromM3U := True;
      TestLoader.TestList(List, Logger);

    finally
      TestLoader.Free;
    end;
  finally
    List.Free;
  end;

end;

procedure TfListAdd.Logger(Message: string);
begin
 Memo1.Lines.Add(Message);
end;

end.
