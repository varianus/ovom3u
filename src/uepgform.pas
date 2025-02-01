unit uEPGForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, Arrow, StdCtrls, Buttons, ActnList, ComCtrls,
  epg, BaseTypes, uBackEnd, Config, Types, Math;

type

  { TEPGForm }

  TEPGForm = class(TForm)
    actBackward: TAction;
    actForward: TAction;
    actNow: TAction;
    ActionList1: TActionList;
    arBackward: TArrow;
    arForward: TArrow;
    bBack1: TButton;
    bNow: TBitBtn;
    bViewSearch: TButton;
    bForceReload: TButton;
    ResultGrid: TDrawGrid;
    Search: TButton;
    lbeSearch: TLabeledEdit;
    mmPlot: TMemo;
    pcView: TPageControl;
    pnlDetail: TPanel;
    bBack: TButton;
    stChannel: TStaticText;
    stTime: TStaticText;
    stTitle: TStaticText;
    DetailGrid: TDrawGrid;
    tsFullGuide: TTabSheet;
    tsDetail: TTabSheet;
    tsSearch: TTabSheet;
    TimeGrid: TDrawGrid;
    lbMessage: TLabel;
    pnlControl: TPanel;
    TimerCheck: TTimer;
    procedure actBackwardExecute(Sender: TObject);
    procedure actForwardExecute(Sender: TObject);
    procedure actNowExecute(Sender: TObject);
    procedure arBackwardClick(Sender: TObject);
    procedure arForwardClick(Sender: TObject);
    procedure bForceReloadClick(Sender: TObject);
    procedure bViewSearchClick(Sender: TObject);
    procedure PaintGridCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure ResultGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
    procedure bBackClick(Sender: TObject);
    procedure SearchClick(Sender: TObject);
    procedure TimeGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure TimeGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure TimeGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TimerCheckTimer(Sender: TObject);
  private
    FEpgData: TEpg;
    Details: AREpgInfo;
    procedure SetEpgData(AValue: TEpg);
    procedure setNow;
    procedure UpdateDetail(const EpgInfo: REpgInfo);
    procedure UpdateTimeRange;

  public
    property EpgData: TEpg read FEpgData write SetEpgData;
  public
    StartTime: TTime;
    EndTime: TTime;
  end;

resourcestring
  RS_Loading = 'EPG Loading';
  RS_NotAvalilable = 'EPG Data not available';

var
  EPGForm: TEPGForm;

implementation

uses GeneralFunc, LCLType;

const
  OneHour = 1 / 24;
  HalfHour = OneHour / 2;

  {$R *.lfm}

  { TEPGForm }

procedure TEPGForm.TimeGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
const
  Segments = 6;
var
  Divider: single;
  i: integer;
  st: string;
  ws: integer;
  CellHeight: integer;
  fCanvas: tcanvas;
  ChannelInfo: AREpgInfo;
  StartPos, EndPos, LinePos: integer;
  r1: TRect;
  Style: TTextStyle;
  CurrTime: TDateTime;
begin
  fCanvas := TimeGrid.Canvas;
  if (aRow = 0) and (Acol = 1) then
  begin

    Divider    := TimeGrid.Columns[1].Width / Segments;
    CellHeight := aRect.Bottom - arect.top;
    fCanvas.Pen.Color := clBlack;
    fCanvas.Pen.Width := 1;
    fCanvas.Font.Size := 12;
    for i := 1 to Segments - 1 do
    begin
      st := FormatDateTime('hh:nn', StartTime + i * HalfHour);
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
    Style.Wordbreak := False;
    fCanvas.Pen.Width := 1;
    ChannelInfo := EpgData.GetEpgInfo(Arow - 1, StartTime, EndTime);
    Divider     := (TimeGrid.Columns[1].Width) / (endTime - StartTime);
    for i := 0 to Length(ChannelInfo) - 1 do
    begin
      if Odd(i) then
        fCanvas.Brush.Color := clWindow
      else
        fCanvas.Brush.Color := clBtnFace;

      StartPos := round(frac(max(ChannelInfo[i].StartTime, StartTime) - StartTime) * Divider) + aRect.Left;
      EndPos := round(frac(Min(ChannelInfo[i].EndTime, EndTime) - StartTime) * Divider) + aRect.Left;
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
    fCanvas.TextRect(aRect, 0, aRect.top + 5, Format('%3.3d: %s', [BackEnd.M3ULoader[aRow - 1].Number, BackEnd.M3ULoader[aRow - 1].title]));
  CurrTime := now;
  if (CurrTime >= StartTime) and (CurrTime <= endtime) then
  begin
    LinePos := Round((CurrTime - StartTime) * (TimeGrid.Columns[1].Width / (endTime - StartTime))) + aRect.Left;
    fCanvas.Pen.Color := clHighlight;
    fCanvas.Pen.Width := 3;
    fCanvas.MoveTo(LinePos, arect.top);
    fCanvas.LineTo(LinePos, aRect.Bottom);
  end;
end;

procedure TEPGForm.UpdateTimeRange;
begin
  lbMessage.Caption := FormatTimeRange(StartTime, EndTime);
end;

procedure TEPGForm.arBackwardClick(Sender: TObject);
begin
  actBackward.Execute;
end;

procedure TEPGForm.actBackwardExecute(Sender: TObject);
begin
  StartTime := StartTime - OneHour;
  EndTime   := EndTime - OneHour;
  UpdateTimeRange;
  TimeGrid.Invalidate;
end;

procedure TEPGForm.actForwardExecute(Sender: TObject);
begin
  StartTime := StartTime + OneHour;
  EndTime   := EndTime + OneHour;
  UpdateTimeRange;
  TimeGrid.Invalidate;
end;

procedure TEPGForm.actNowExecute(Sender: TObject);
begin
  SetNow;
  TimeGrid.Invalidate;
end;

procedure TEPGForm.arForwardClick(Sender: TObject);
begin
  actForward.Execute;
end;

procedure TEPGForm.bForceReloadClick(Sender: TObject);
begin
  ConfigObj.ListManager.SetLastScan(EpgData.ActiveList.ListID, 'epg', 0);
  TimerCheck.Enabled := True;
  EpgData.Scan;
end;

procedure TEPGForm.bViewSearchClick(Sender: TObject);
begin
  SetLength(Details, 0);
  ResultGrid.RowCount := 1;
  pcView.ActivePage   := tsSearch;
  lbeSearch.SetFocus;
end;

procedure TEPGForm.PaintGridCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  Grid: TDrawGrid;
  cv: Tcanvas;
  Style: TTextStyle;
begin

  if (aRow = 0) then
    exit;
  Style.EndEllipsis := True;
  Style.Wordbreak   := True;

  Grid := Sender as TDrawGrid;
  cv   := Grid.Canvas;
  case Grid.Columns[aCol].Tag of
    0:
      cv.TextRect(aRect, aRect.Left, aRect.Top, Details[aRow - 1].Channel, style);
    1:
      cv.TextRect(aRect, aRect.Left, aRect.Top, DateTimeToStr(trunc(Details[aRow - 1].StartTime)), style);
    2:
      cv.TextRect(aRect, aRect.Left, aRect.Top, FormatTimeRange(Details[aRow - 1].StartTime, Details[aRow - 1].EndTime, True), style);
    3:
      cv.TextRect(aRect, aRect.Left, aRect.Top, Details[aRow - 1].Title, style);
    4:
      cv.TextRect(aRect, aRect.Left, aRect.Top, Details[aRow - 1].Plot, style);
  end;

end;

procedure TEPGForm.ResultGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
begin
  if (aRow > 0) and (Length(Details) > 0) then
    UpdateDetail(Details[aRow - 1]);
end;

procedure TEPGForm.bBackClick(Sender: TObject);
begin
  SetLength(Details, 0);
  pcView.ActivePage := tsFullGuide;
end;

procedure TEPGForm.SearchClick(Sender: TObject);
begin
  Details := EpgData.GetEpgInfo(lbeSearch.Text);
  ResultGrid.RowCount := Length(Details) + 1;
  ResultGrid.Invalidate;
end;

procedure TEPGForm.FormCreate(Sender: TObject);
begin
  pcView.ActivePage := tsFullGuide;
  SetNow;
  TimeGrid.RowCount := BackEnd.M3ULoader.Count + 1;
  Details := nil;
  ;
end;

procedure TEPGForm.TimeGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    VK_LEFT:
      actBackward.Execute;
    VK_RIGHT:
      actForward.Execute;
    vk_down:
      TimeGrid.ScrollBy(0, 1);
    vk_up:
      TimeGrid.ScrollBy(0, -1);
    VK_N:
      actNow.Execute;
    VK_E:
      Self.Close;
  end;

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
  EpgInfo   := FEpgData.GetEpgInfo(Coord.Y - 1, ClickTime);
  EpgInfo.Channel := BackEnd.M3ULoader[Coord.y - 1].title;
  UpdateDetail(EpgInfo);

end;

procedure TEPGForm.UpdateDetail(const EpgInfo: REpgInfo);
begin
  stChannel.Caption := EpgInfo.Channel;
  stTime.Caption    := FormatTimeRange(EpgInfo.StartTime, EpgInfo.EndTime, False);
  stTitle.Caption   := EpgInfo.Title;
  mmPlot.Lines.Text := EpgInfo.Plot;
end;

procedure TEPGForm.TimerCheckTimer(Sender: TObject);
begin
  if not EpgData.EpgAvailable then
    lbMessage.Caption := RS_NotAvalilable
  else
  if EpgData.Scanning then
    lbMessage.Caption := RS_Loading
  else
  begin
    UpdateTimeRange;
    TimerCheck.Enabled := False;
  end;
  TimeGrid.Invalidate;
end;

procedure TEPGForm.setNow;
begin
  StartTime := trunc(now) + Floor(frac(now - HalfHour) * 24) / 24;
  EndTime   := StartTime + OneHour * 4;
  UpdateTimeRange;
end;

procedure TEPGForm.SetEpgData(AValue: TEpg);
begin
  if FEpgData = AValue then
    Exit;
  FEpgData := AValue;
  if Assigned(FEpgData) then
    TimerCheck.Enabled := not FEpgData.EpgAvailable or FEpgData.Scanning;

end;

initialization
  EPGForm := nil;
end.
