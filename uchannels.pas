unit uChannels;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  StdCtrls, LCLType, Types, um3uloader, uBackEnd, GeneralFunc;

type

  { TfChannels }

  TfChannels = class(TForm)
    cbGroups: TComboBox;
    cbViewEPG: TCheckBox;
    ChannelList: TDrawGrid;
    cbViewLogo: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    procedure cbViewLogoChange(Sender: TObject);
    procedure ChannelListDblClick(Sender: TObject);
    procedure ChannelListDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ChannelListSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GotFocus(Sender: TObject);
    procedure LostFocus(Sender: TObject);
  private
    fFilter: TFilteredList;
    procedure ComputeGridCellSize(data: ptrint);
  public
    Procedure Init;
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

  cv.Font.Height := Scale96Toscreen(-14);
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
  cv.TextRect(aRect, aRect.left+ h + Spacing*2, aRect.top + Spacing * 2, Format('%3.3d: %s', [Element.Number, Element.title]));
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
  //mcmcmcmcmcmc
  //begin
  //  if GuiProperties.ViewCurrentProgram then
  //    ChannelList.DefaultRowHeight := Scale96ToScreen(64)
  //  else
  //    if GuiProperties.ViewLogo then
  //      ChannelList.DefaultRowHeight := Scale96ToScreen(48)
  //    else
  //      ChannelList.DefaultRowHeight := Scale96ToScreen(32);
  //
  //end;
  ChannelList.Invalidate;

end;

procedure TfChannels.Init;
begin
  if (BackEnd.list.Groups.Count > 1) then
    begin
      cbGroups.Items.clear;
      cbGroups.items.add(um3uloader.RSAnyGroup);
      cbGroups.Items.AddStrings(BackEnd.List.Groups, false);
      cbGroups.ItemIndex:=0;
      cbGroups.Visible:=true;
    end
  else
    cbGroups.Visible:=false;
  ChannelList.RowCount:=ceil(ffilter.Count / 4 );
  ComputeGridCellSize(0);
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
 BackEnd.Play(fFilter.Map(idx));
end;

procedure TfChannels.FilterChange(Sender: TObject);
var
  Filter: TFilterParam;
begin
  Filter := Default(TFilterParam);
  if Edit1.Text <> EmptyStr then
    Filter.Title := Edit1.Text;

  if cbGroups.ItemIndex <> 0 then
    Filter.Group := cbGroups.Items[cbGroups.ItemIndex];

  fFilter := Backend.List.Filter(Filter);
  ChannelList.RowCount:=ceil(ffilter.Count / 4 );
  ChannelList.Invalidate;
end;

procedure TfChannels.FormCreate(Sender: TObject);
begin
  fFilter := BackEnd.List.Filter(Default(TFilterParam));
end;

procedure TfChannels.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
    VK_RETURN : ChannelListDblClick(Sender);
  end;
end;

procedure TfChannels.GotFocus(Sender: TObject);
begin
  TControl(Sender).Color:= clHighlight;
  TControl(Sender).Invalidate;
end;

procedure TfChannels.LostFocus(Sender: TObject);
begin
  TControl(Sender).Color:= clDefault;
  TControl(Sender).Invalidate;
end;


end.

