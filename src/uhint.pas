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

unit uhint;

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, BaseTypes,
  GeneralFunc;

type

  { TChannelHint }

  TChannelHint = class(THintWindowRendered)
  private
    procedure MakeFormVisible(var ARect: TRect);
  public
    procedure ActivateHintData(ARect: TRect; const AHint: string; AData: pointer); override;
    function CalcHintRect(MaxWidth: integer; const AHint: string; AData: pointer): TRect; override;
    destructor Destroy; override;

  end;

  { TChannelHintForm }


  TChannelHintForm = class(TForm)
    mmPlot: TMemo;
    pnlDetail: TPanel;
    stChannel: TStaticText;
    stTime: TStaticText;
    stTitle: TStaticText;
  private
  public
    procedure UpdateDetail(const EpgInfo: REpgInfo);

  end;

var
  ChannelHintForm: TChannelHintForm;

implementation

{$R *.lfm}

{ TChannelHint }

procedure TChannelHint.MakeFormVisible(var ARect: TRect);
var
  AMonitor: TMonitor;
  MonitorBounds: TRect;
begin
  AMonitor      := Screen.MonitorFromPoint(ARect.TopLeft);
  MonitorBounds := AMonitor.WorkareaRect;

  // Adjust form position to ensure it's fully visible on the current monitor
  with ARect do
  begin
    if Left < MonitorBounds.Left then
      Left := MonitorBounds.Left;
    if Top < MonitorBounds.Top then
      Top := MonitorBounds.Top;
    if (Left + Width) > MonitorBounds.Right then
      Left := MonitorBounds.Right - Width;
    if (Top + Height) > MonitorBounds.Bottom then
      Top := MonitorBounds.Bottom - Height;
  end;

end;

procedure TChannelHint.ActivateHintData(ARect: TRect; const AHint: string; AData: pointer);
begin
  MakeFormVisible(ARect);
  HintRect := ARect;
  ChannelHintForm.Parent := self;
  ChannelHintForm.top := 0;
  ChannelHintForm.Left := 0;
  ChannelHintForm.Visible := True;
  Visible  := True;
  ActivateRendered;
end;

function TChannelHint.CalcHintRect(MaxWidth: integer; const AHint: string; AData: pointer): TRect;
begin
  Result := Rect(0, 0, ChannelHintForm.Width, ChannelHintForm.Height); //ChannelHintForm.BoundsRect;
end;

destructor TChannelHint.Destroy;
begin
  ChannelHintForm.Parent := nil;
  inherited Destroy;
end;

procedure TChannelHintForm.UpdateDetail(const EpgInfo: REpgInfo);
begin
  stChannel.Caption := EpgInfo.Channel;
  stTime.Caption    := FormatTimeRange(EpgInfo.StartTime, EpgInfo.EndTime, False);
  stTitle.Caption   := EpgInfo.Title;
  mmPlot.Lines.Text := EpgInfo.Plot;
end;

end.
