unit uEPGFOrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, Arrow, epg, BaseTypes, Types, Math;

type

  { TEPGForm }

  TEPGForm = class(TForm)
    Arrow1: TArrow;
    Arrow2: TArrow;
    DrawGrid1: TDrawGrid;
    Panel1: TPanel;
    procedure Arrow1Click(Sender: TObject);
    procedure Arrow2Click(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
  private

  public
    EpgData: TEpg;
    StartTime: TTime;
    EndTime: TTime;
  end;

var
  EPGForm: TEPGForm;

implementation

uses umain;

{$R *.lfm}

{ TEPGForm }

procedure TEPGForm.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
const
  Segments = 6;
var
  Divider: single;
  i: integer;
  st, aString: string;
  ws: integer;
  CellHeight: integer;
  fCanvas: tcanvas;
  ChannelInfo: AREpgInfo;
  StartPos, EndPos: integer;
  r1: TRect;
  tmp: double;
  Style: TTextStyle;
begin
  fCanvas := DrawGrid1.Canvas;
  if (aRow = 0) and (Acol = 1) then
  begin

    Divider := (aRect.Right - aRect.Left) / Segments;
    CellHeight := aRect.Bottom - arect.top;
    fCanvas.Pen.Color := clBlack;

    fCanvas.Font.Size := 12;
    for i := 1 to Segments - 1 do
    begin
      st := TimeToStr(StartTime + i / 48);
      ws := fCanvas.TextWidth(st);
      fCanvas.TextOut(arect.Left + trunc(Divider * i - (ws div 2)), arect.Top + 2, st);
      fCanvas.MoveTo(arect.Left + trunc(divider * i), CellHeight div 2);
      fCanvas.LineTo(arect.Left + trunc(divider * i), aRect.Bottom);
    end;
  end;
  if (aRow > 0) and (Acol = 1) then
  begin
    Style.EndEllipsis := True;
    Style.Alignment := taCenter;
    Style.Layout := tlCenter;
    Style.Clipping := True;
    ChannelInfo := EpgData.GetEpgInfo(Arow, StartTime, EndTime);
    Divider := (aRect.Right - aRect.Left) / (endTime - StartTime);
    for i := 0 to Length(ChannelInfo) - 1 do
    begin

      StartPos := Trunc(frac(max(ChannelInfo[i].StartTime, StartTime) - StartTime) * Divider);
      EndPos := round(frac(Min(ChannelInfo[i].EndTime, EndTime) - StartTime) * Divider);
      fCanvas.FillRect(Rect(StartPos - 1, arect.Top - 1, EndPos - 1, arect.Bottom - 1));
      fCanvas.pen.Color := clBlack;
      fCanvas.Rectangle(Rect(StartPos, arect.Top, EndPos, arect.Bottom));

      fCanvas.Font.Color := clblack;
      r1 := Rect(StartPos + 5, arect.Top + 5, EndPos - 5, arect.Bottom - 8);
      aString := string(ChannelInfo[i].Title);
      fCanvas.TextRect(r1, 0, 0, aString, Style);
    end;
  end;
  if (aRow > 0) and (Acol = 0) then
    fCanvas.TextRect(aRect, 0, aRect.top + 5, Format('%3.3d: %s', [fPlayer.LIST[arow].Number, fPlayer.List[arow].title]));
end;

procedure TEPGForm.Arrow1Click(Sender: TObject);
begin
  StartTime := StartTime - 1 / 24;
  EndTime := EndTime - 1 / 24;
  DrawGrid1.Invalidate;
end;

procedure TEPGForm.Arrow2Click(Sender: TObject);
begin
  StartTime := StartTime + 1 / 24;
  EndTime := EndTime + 1 / 24;
  DrawGrid1.Invalidate;
end;

procedure TEPGForm.FormCreate(Sender: TObject);
begin
  StartTime := trunc(now) + Floor(frac(now) * 24) / 24;
  Writeln(datetimetostr(StartTime));
  EndTime := StartTime + 4 / 24;
  DrawGrid1.RowCount := fPlayer.List.Count + 1;
end;

end.
