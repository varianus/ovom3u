unit uEPGForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, Arrow, StdCtrls, epg,
  BaseTypes, Types, Math;

type

  { TEPGForm }

  TEPGForm = class(TForm)
    arBackward: TArrow;
    arForward: TArrow;
    mmPlot: TMemo;
    Panel1: TPanel;
    stChannel: TStaticText;
    stTime: TStaticText;
    stTitle: TStaticText;
    TimeGrid: TDrawGrid;
    lbMessage: TLabel;
    pnlControl: TPanel;
    TimerCheck: TTimer;
    procedure arBackwardClick(Sender: TObject);
    procedure arForwardClick(Sender: TObject);
    procedure TimeGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure TimeGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TimerCheckTimer(Sender: TObject);
  private
    FEpgData: TEpg;
    procedure SetEpgData(AValue: TEpg);
    procedure UpdateTimeRange;

  public
    property EpgData: TEpg read FEpgData write SetEpgData;
  public
    StartTime: TTime;
    EndTime: TTime;
  end;

var
  EPGForm: TEPGForm;

implementation

uses umain, GeneralFunc;

const
  OneHour = 1 / 24;

{$R *.lfm}

{ TEPGForm }

procedure TEPGForm.TimeGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
const
  Segments = 8;
var
  Divider: single;
  i: integer;
  st: string;
  ws: integer;
  CellHeight: integer;
  fCanvas: tcanvas;
  ChannelInfo: AREpgInfo;
  StartPos, EndPos: integer;
  r1: TRect;
  Style: TTextStyle;
begin
  fCanvas := TimeGrid.Canvas;
  if (aRow = 0) and (Acol = 1) then
  begin

    Divider := (aRect.Right - aRect.Left) / Segments;
    CellHeight := aRect.Bottom - arect.top;
    fCanvas.Pen.Color := clBlack;

    fCanvas.Font.Size := 12;
    for i := 1 to Segments - 1 do
    begin
      st := FormatDateTime('hh:nn', StartTime + i / 48);
      ws := fCanvas.TextWidth(st);
      fCanvas.TextOut(arect.Left + trunc(Divider * i - (ws div 2)), aRect.Top + 2, st);
      fCanvas.MoveTo(arect.Left + trunc(divider * i), CellHeight div 2);
      fCanvas.LineTo(arect.Left + trunc(divider * i), aRect.Bottom);
    end;
  end;
  if (aRow > 0) and (Acol = 1) and fEpgData.EpgAvailable then
  begin
    Style.EndEllipsis := True;
    Style.Alignment := taCenter;
    Style.Layout := tlCenter;
    Style.Clipping := True;
    ChannelInfo := EpgData.GetEpgInfo(Arow, StartTime, EndTime);
    Divider := (TimeGrid.Columns[1].Width) / (endTime - StartTime);
    for i := 0 to Length(ChannelInfo) - 1 do
    begin
      if Odd(i) then
        fCanvas.Brush.Color := clWindow
      else
        fCanvas.Brush.Color := clBtnFace;

      StartPos := round(frac(max(ChannelInfo[i].StartTime, StartTime) - StartTime) * Divider) + aRect.Left;
      EndPos := round(frac(Min(ChannelInfo[i].EndTime, EndTime) - StartTime) * Divider)  + aRect.Left;
      R1 := Rect(StartPos - 1, aRect.Top - 1, EndPos - 1, aRect.Bottom - 1);
      fCanvas.FillRect(R1);
      fCanvas.pen.Color := clCaptionText;
      R1.Inflate(1, 1, 1, 1);
      fCanvas.Rectangle(r1);

      fCanvas.Font.Color := clCaptionText;
      R1.Inflate(-5, -5, -5, -5);
      fCanvas.TextRect(r1, 0, 0, ChannelInfo[i].Title, Style);
    end;
  end;
  if (aRow > 0) and (Acol = 0) then
    fCanvas.TextRect(aRect, 0, aRect.top + 5, Format('%3.3d: %s', [fPlayer.LIST[aRow - 1].Number, fPlayer.List[aRow - 1].title]));
end;

procedure TEPGForm.UpdateTimeRange;
begin
  lbMessage.Caption := FormatTimeRange(StartTime, EndTime);
end;

procedure TEPGForm.arBackwardClick(Sender: TObject);
begin
  StartTime := StartTime - OneHour;
  EndTime := EndTime - OneHour;
  UpdateTimeRange;
  TimeGrid.Invalidate;
end;

procedure TEPGForm.arForwardClick(Sender: TObject);
begin
  StartTime := StartTime + OneHour;
  EndTime := EndTime + OneHour;
  UpdateTimeRange;
  TimeGrid.Invalidate;
end;

procedure TEPGForm.FormCreate(Sender: TObject);
begin
  StartTime := trunc(now) + Floor(frac(now - OneHour) * 24) / 24;
  EndTime := StartTime + 4 * OneHour;
  UpdateTimeRange;
  TimeGrid.RowCount := fPlayer.List.Count + 1;

end;

procedure TEPGForm.TimeGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Coord: TGridCoord;
  ClickTime: TDateTime;
  EpgInfo: REpgInfo;
begin
  Coord := TimeGrid.MouseCoord(X, y);
  if (Coord.X = 0) or (coord.Y = 0) then
    exit;

  ClickTime := StartTime + (x - TimeGrid.Columns[0].Width) / (TimeGrid.Columns[1].Width / (endTime - StartTime));
  EpgInfo := FEpgData.GetEpgInfo(Coord.Y, ClickTime);
  stChannel.Caption := fPlayer.List[Coord.y-1].title;
  stTime.Caption := FormatTimeRange(EpgInfo.StartTime,EpgInfo.EndTime, false);
  stTitle.Caption := EpgInfo.Title;
  mmPlot.Lines.Text := EpgInfo.Plot;
end;

procedure TEPGForm.TimerCheckTimer(Sender: TObject);
begin
  if not EpgData.EpgAvailable then
    lbMessage.Caption := 'EPG Loading - Please wait'
  else
  begin
    UpdateTimeRange;
    TimeGrid.Invalidate;
    TimerCheck.Enabled := False;
  end;
end;

procedure TEPGForm.SetEpgData(AValue: TEpg);
begin
  if FEpgData = AValue then
    Exit;
  FEpgData := AValue;
  if Assigned(FEpgData) then
    TimerCheck.Enabled := not FEpgData.EpgAvailable;

end;

end.
