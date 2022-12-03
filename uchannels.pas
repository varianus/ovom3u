unit uChannels;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  StdCtrls, Types, um3uloader, uBackEnd, GeneralFunc;

type

  { TfChannels }

  TfChannels = class(TForm)
    cbViewEPG: TCheckBox;
    ChannelList: TDrawGrid;
    cbViewLogo: TCheckBox;
    Edit1: TEdit;
    Panel1: TPanel;
    procedure cbViewLogoChange(Sender: TObject);
    procedure ChannelListDblClick(Sender: TObject);
    procedure ChannelListDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ChannelListSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fFilter: TFilteredList;
    procedure ComputeGridCellSize(data: ptrint);
  public

  end;

var
  fChannels: TfChannels;

implementation

uses umain, epg, uEPGForm, Config, BaseTypes, Math;

{$R *.lfm}

{ TfChannels }

procedure TfChannels.ChannelListDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Idx :Integer;
  cv: TCanvas;
  Element: TM3UItem;
  bmp: TPicture;
  r: Trect;
  Scale: double;
  H: integer;
  Spacing: integer;
  epgInfo: REpgInfo;
begin
  idx := arow*4 + aCol;
  if idx >= fFilter.Count then
    Exit;

  Element := ffilter[idx];
  Idx := fFilter.Map(idx);
  h := 0;
  cv := ChannelList.Canvas;
  if cbViewLogo.Checked then
  begin
    h := ChannelList.RowHeights[aRow];
    if Element.IconAvailable then
    begin
      bmp := TPicture.Create;
      bmp.LoadFromFile(element.IconLocal);
      if bmp.Height > bmp.Width then
      begin
        scale := bmp.Width / bmp.Height;
        r := rect(arect.left, arect.Top, arect.Left + round(h * scale), aRect.Top + round(h));
      end
      else
      begin
        scale := bmp.Height / bmp.Width;
        r := rect(arect.left, arect.Top, arect.Left + round(h), aRect.Top + round(h * scale));
      end;
      cv.StretchDraw(r, bmp.Graphic);
      bmp.Free;
    end
    else
    begin
      bmp := TPicture.Create;
      bmp.LoadFromFile(ConfigObj.GetResourcesPath+ 'no-logo.png');
      cv.StretchDraw(rect(arect.left, arect.Top, arect.Left + h, aRect.Top + h), bmp.Graphic);
      bmp.Free;
    end;

  end;

  cv.Font.Height := Scale96Toscreen(-16);
{  if fPlayer.CurrentChannel = aRow then
  begin
    cv.Font.Style := [fsBold, fsUnderline];
    cv.Font.color := clHighlightText;
    cv.Brush.color := clHighlight;
    cv.Rectangle(aRect);
  end
  else      }
    cv.Font.Style := [fsBold];

  Spacing := Scale96ToScreen(2);
  cv.TextRect(aRect, aRect.left+ h + Spacing, aRect.top + Spacing * 2, Format('%3.3d: %s', [Element.Number, Element.title]));
  if cbViewEPG.Checked then
  begin
    epgInfo := Backend.epgdata.GetEpgInfo(idx, now);
    if epgInfo.HaveData then
    begin
      cv.Font.Height := Scale96ToScreen(-12);
      cv.Font.Style := [];
      Element.CurrProgram := FormatTimeRange(EpgInfo.StartTime, EpgInfo.EndTime, True);
      cv.TextRect(aRect, aRect.left+h + Spacing, aRect.top + scale96toscreen(25), Element.CurrProgram);
      cv.TextRect(aRect, aRect.left+h + spacing, aRect.top + scale96toscreen(37), EpgInfo.Title);

    end;
  end;
end;

procedure TfChannels.ChannelListSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := (arow*4 + aCol) <  fFilter.Count;
end;

procedure TfChannels.ComputeGridCellSize(data: ptrint);
begin
  if cbViewLogo.Checked or cbViewEPG.Checked then
    ChannelList.DefaultRowHeight := Scale96ToScreen(64)
  else
    ChannelList.DefaultRowHeight := Scale96ToScreen(32);
  ChannelList.Invalidate;

end;

procedure TfChannels.cbViewLogoChange(Sender: TObject);
begin
  ComputeGridCellSize(0);
end;

procedure TfChannels.ChannelListDblClick(Sender: TObject);
var
  idx:integer;
begin
  idx := ChannelList.Selection.Top*4 + ChannelList.Selection.Left;
  if idx >= fFilter.Count then
    exit;
 BackEnd.MpvEngine.Play(fFilter[idx].mrl);
end;

procedure TfChannels.Edit1Change(Sender: TObject);
var
  Filter: TFilterParam;
begin
  Filter := Default(TFilterParam);
  if Edit1.Text <> EmptyStr then
    Filter.Title := Edit1.Text;

  fFilter := Backend.List.Filter(Filter);
  ChannelList.RowCount:=ceil(ffilter.Count / 4 );
  ChannelList.Invalidate;
end;

procedure TfChannels.FormCreate(Sender: TObject);
begin
  fFilter := BackEnd.List.Filter(Default(TFilterParam));
end;


end.

