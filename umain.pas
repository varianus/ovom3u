{
This file is part of OvoM3U
Copyright (C) 2020 Marco Caselli

OvoPlayer is free software; you can redistribute it and/or
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
unit umain;

interface

uses
  Classes, Forms, Controls, Graphics, LCLTaskDialog,Dialogs, ExtCtrls,
  Grids, LCLIntf, lcltype, ComCtrls, Menus, um3uloader,
  OpenGLContext, Types, Math, SysUtils,
  MPV_Engine, Config, GeneralFunc, Generics.collections, UITypes, epg;

  { TfPlayer }
type

  TfPlayer = class(TForm)
    ChannelList: TDrawGrid;
    OSDTimer: TTimer;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuSub: TMenuItem;
    mnuAudio: TMenuItem;
    mnuVideo: TMenuItem;
    GLRenderer: TOpenGLControl;
    pnlChannel: TPanel;
    pnlContainer: TPanel;
    pmPlayer: TPopupMenu;
    HideMouse: TTimer;
    LoadingTimer: TTimer;
    TaskDialog1: TTaskDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure ChannelListDblClick(Sender: TObject);
    procedure ChannelListDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure ChannelListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure LoadingTimerStartTimer(Sender: TObject);
    procedure LoadingTimerTimer(Sender: TObject);
    procedure GLRendererDblClick(Sender: TObject);
    procedure GLRendererMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure HideMouseTimer(Sender: TObject);
    procedure OSDTimerTimer(Sender: TObject);
    procedure pmPlayerPopup(Sender: TObject);
    procedure pnlContainerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlContainerPaint(Sender: TObject);
    procedure TaskDialog1ButtonClicked(Sender: PTaskDialog; AButtonID: integer; var ACanClose: Boolean);
    procedure ToolButton1Click(Sender: TObject);
  private
    FLoading: boolean;
    ChannelSelecting: boolean;
    ChannelSelected: integer;
    fLastMessage: string;
    IPTVList: string;
    Kind: TProviderKind;
    function CheckConfigAndSystem: boolean;
    procedure OnLoadingState(Sender: TObject);
    procedure OsdMessage(Message: string; TimeOut: boolean=true);
    procedure Play(Row: integer);
    procedure SetLoading(AValue: boolean);
    procedure ShowEpg;
  private
    MpvEngine: TMPVEngine;
    epgData: TEpg;
    isRenderActive: boolean;
    flgFullScreen: boolean;
    CurrentChannel: integer;
    RestoredBorderStyle: TBorderStyle;
    RestoredWindowState: TWindowState;
    Progress: integer;
    property Loading: boolean read FLoading write SetLoading;
    procedure LoadTracks;
    procedure mnuTrackClick(Sender: TObject);
    procedure SetFullScreen;
    procedure LoadList;
  public
    List: TM3ULoader;
  end;

var
  fPlayer: TfPlayer;
  openglHandle: Thandle;

implementation

uses uconfig, BaseTypes;

{$R *.lfm}

{ TfPlayer }

Function TfPlayer.CheckConfigAndSystem:boolean;
var
  Dialog : LCLTaskDialog.TTaskDialog;
  Retry: boolean;
begin
  //if TMPVEngine.CheckMPV then
  //  begin
  //    Dialog.Inst := 'Can''t initialize libMPV';
  //    Dialog.Content := 'LibMPV shared library is missing or could not be initialized\n'+
  //                      'OvoM3U uses this library to decode and play videos\n'+
  //                      'Click the following to open a wiki page with information on\n' +
  //                      'how to install libMPV on your platform'
  //                      ;
  //    Dialog.Buttons := 'https://github.com/varianus/ovom3u/wiki/LibMPV'+#10;
  //
  //    repeat
  //    case  Dialog.Execute([cbRetry,cbClose],1,[tdfUseCommandLinks],LCLTaskDialog.TTaskDialogIcon.tiWarning,tfiBlank,0,0,0,true,true,TaskDialog1ButtonClicked) of
  //      mrClose: begin
  //                Result := false;
  //                Retry := false;
  //                exit;
  //              end;
  //      mrRetry: Retry := true;
  //     end;
  //
  //    until Retry = false;
  //  end;
  //
  Result:= true;
  Kind:= ConfigObj.M3UProperties.Kind;
  Case Kind of
    Local: IPTVList:= ConfigObj.M3UProperties.FileName;
    URL : IPTVList:=  ConfigObj.M3UProperties.Url;
  end;

  if IPTVList.IsEmpty then
    begin
      Dialog.Inst := 'Welcome to OvoM3U';
      Dialog.Content := 'Configure';
      Dialog.Buttons := '' ;
      Dialog.Execute([cbOK],1,[tdfUseCommandLinks],LCLTaskDialog.TTaskDialogIcon.tiWarning,tfiBlank,0,0,0,true,true) ;
    end;

end;

procedure TfPlayer.LoadList;
var
  CacheDir: string;
begin

  ConfigObj.ReadConfig;

  Kind:= ConfigObj.M3UProperties.Kind;

  if Kind = URL then
    begin
      CacheDir:=GetCacheDir;
      Try
        if epgData.LastScan('channels') +12 > now then
           DownloadFromUrl(IPTVList,CacheDir +'current-iptv.m3u');
        IPTVList:=CacheDir+'current-iptv.m3u';
      finally
      end;
    end;

  list.Load(IPTVList);
  if ConfigObj.M3UProperties.UseChno then
    List.FixChannelNumbering;

  epgData := TEpg.Create;

  if not Configobj.M3UProperties.EPGUrl.IsEmpty then
    begin
      epgData.LoadChannelList(List);
      epgData.Scan;
    end;


  ChannelList.RowCount := List.Count;

end;

procedure TfPlayer.FormCreate(Sender: TObject);
begin
  Progress := 0;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  flgFullScreen := False;

  if CheckConfigAndSystem then
    begin
      ChannelList.RowCount:=0;;
      List := TM3ULoader.Create;
      Loadlist;

      MpvEngine := TMPVEngine.Create;
      MpvEngine.Initialize(GLRenderer);
      CurrentChannel := -1;
      ChannelSelecting:=false;
      fLoading := false;
      ChannelSelected:=0;

    end;
end;

procedure TfPlayer.FormDestroy(Sender: TObject);
begin
  MpvEngine.Free;

  List.Free;
end;

procedure TfPlayer.OnLoadingState(Sender: TObject);
begin
  if Loading then
    Loading:= MpvEngine.IsIdle;
end;

procedure TfPlayer.OsdMessage(Message: string; TimeOut: boolean = true);
begin
  fLastMessage:= Message;
  if GLRenderer.Visible then
    MpvEngine.OsdMessage(message)
  else
    pnlContainer.Invalidate;
  OSDTimer.Enabled:=TimeOut;
end;

procedure TfPlayer.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if flgFullScreen then
  case key of
    VK_ESCAPE: SetFullScreen;
    VK_DOWN: begin ChannelList.Row:= CurrentChannel+1; play(ChannelList.Row); end;
    VK_UP: begin ChannelList.Row:= CurrentChannel-1; play(ChannelList.Row); end;
    VK_RETURN: if ChannelSelecting then
                 begin
                   if ConfigObj.M3UProperties.UseChno then
                     ChannelSelected:=List.ItemByChno(ChannelSelected);
                   play(ChannelSelected) ;
                   ChannelSelecting := false;
                   key:=0;
                end;
  end;
  Case key of
    VK_I :  OsdMessage(Format('%3.3d: %s',[List[CurrentChannel].Number, List[CurrentChannel].title]), true);
    VK_S : begin MpvEngine.Stop;  OsdMessage('Stop', true); end;
    VK_SPACE : begin MpvEngine.Pause; end;
    VK_F : SetFullScreen;
    VK_E : ShowEpg;

    VK_0..VK_9,VK_NUMPAD0..VK_NUMPAD9: begin
               if not ChannelSelecting then
                 begin
                   ChannelSelecting := true;
                   ChannelSelected:= Key -$30;
                   if Key >= $60 then
                     ChannelSelected:= ChannelSelected -$30
                 end
               else
                 begin
                   ChannelSelected := (ChannelSelected*10) + Key -$30;
                   if Key >= $60 then
                     ChannelSelected:= ChannelSelected -$30

                 end;
                 OsdMessage(Inttostr(ChannelSelected), true);
    end;
  end;
end;

Procedure TfPlayer.ShowEpg;
var
  Info :REpgInfo;
begin
  Info := epgData.GetEpgInfo(CurrentChannel+1, now);
  MpvEngine.OsdEpg(info, true);
  OSDTimer.Enabled:=true;
end;

procedure TfPlayer.FormKeyPress(Sender: TObject; var Key: char);
begin
//

end;

procedure TfPlayer.LoadingTimerStartTimer(Sender: TObject);
begin
  Progress := 0;
end;

procedure TfPlayer.LoadingTimerTimer(Sender: TObject);
begin
  Inc(progress, 10);
  if progress mod 50 = 0 then
    begin
      Loading := MpvEngine.isIdle;
      if not loading then
        begin
          LoadingTimer.Enabled := false;
          MpvEngine.LoadTracks;
        end;
    end;

  if progress > 720 then
    begin
    Progress := 0;
    end;
  pnlContainer.Invalidate;
end;

procedure TfPlayer.ChannelListDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  cv: TCanvas;
begin
  cv := ChannelList.Canvas;
  cv.Font.Size := 9;
  if CurrentChannel = aRow then
    begin
      cv.Font.Style := [fsBold, fsUnderline] ;
      cv.Font.color := clHighlightText;
      cv.Brush.color := clHighlight;
      cv.Rectangle(aRect);
    end
  else
    cv.Font.Style := [fsBold];

  cv.TextRect(aRect, 0, aRect.top + 5, Format('%3.3d: %s',[List[arow].Number, List[arow].title]));

  cv.Font.Size  := 9;
  cv.Font.Style := [];
  cv.TextRect(aRect, 0, aRect.top + 25, List[arow].CurrProgram);
end;

procedure TfPlayer.ChannelListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_RETURN then
    Play(ChannelList.Row);
end;

procedure TfPlayer.ChannelListDblClick(Sender: TObject);
begin
  Play(ChannelList.Row);
end;

Procedure TfPlayer.Play(Row: integer);
begin
  if (row > List.Count ) or (row < 0) then
    begin
      OsdMessage('No Channel', true);
      exit;
    end;
  if (CurrentChannel = Row) and
     not MpvEngine.IsIdle then
     exit;
  ChannelList.Invalidate;
  CurrentChannel := Row;
  Caption := list[CurrentChannel].title;
  MpvEngine.Play(list[CurrentChannel].Mrl);
  Loading := true;
  fLastMessage:='Loading: '+ list[CurrentChannel].title;
  pnlContainer.Invalidate;

end;

procedure TfPlayer.GLRendererDblClick(Sender: TObject);
begin
  SetFullScreen;
end;

procedure TfPlayer.GLRendererMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  Screen.Cursor     := crdefault;
  HideMouse.Enabled := flgFullScreen;
end;

procedure TfPlayer.HideMouseTimer(Sender: TObject);
begin
  screen.cursor := crNone;
end;

procedure TfPlayer.OSDTimerTimer(Sender: TObject);
begin
  fLastMessage:='';
  if GLRenderer.Visible then
     MpvEngine.OsdMessage()
  else
     pnlContainer.invalidate;
  OSDTimer.Enabled:=false;
  If ChannelSelecting then
    begin
      if ConfigObj.M3UProperties.UseChno then
         ChannelSelected:=List.ItemByChno(ChannelSelected)
      else
         ChannelSelected := ChannelSelected -1;
      Play(ChannelSelected);
      ChannelSelecting:=false;
    end;
end;

procedure TfPlayer.pmPlayerPopup(Sender: TObject);
begin
  if mnuVideo.Count = 0 then
    mnuVideo.Enabled := False;
  if mnuAudio.Count = 0 then
    mnuAudio.Enabled := False;
  if mnuSub.Count = 0 then
    mnuSub.Enabled := False;
end;

procedure TfPlayer.pnlContainerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  GLRendererMouseMove(Sender, Shift,x,y);
end;

procedure TfPlayer.pnlContainerPaint(Sender: TObject);
var
  cv: Tcanvas;
  a, b: integer;
  p: tpoint;
  Scaling: double;
begin
  cv := pnlContainer.Canvas;
  cv.font.Color:=  clwhite;
  // MPV use a default font of 55 pixel for a 720 pixel high window
  // try to replicate same scaling
  Scaling := (cv.Height / 720) / 1.25;
  cv.font.Height:= trunc(Scaling * 55) ;
  cv.TextOut(trunc(scaling *25) ,trunc(scaling *22), fLastMessage);

  if floading then
  begin

    cv.Brush.Color := clblack;
    cv.Clear;
    cv.Pen.Color := clwhite;
    cv.pen.Width := 10;
    if progress < 360 then
    begin
      A := progress * 16;
      b := 0;
    end
    else
    begin
      A := (progress - 721) * 16;
      b := -360 * 16;
    end;
    p.X := cv.Width div 2;
    p.y := cv.Height div 2;
    cv.Arc(p.x - 50, p.y - 50, p.x + 50, p.y + 50, b, a);
  end;
end;

procedure TfPlayer.TaskDialog1ButtonClicked(Sender: PTaskDialog; AButtonID: integer; var ACanClose: Boolean);
begin

    if AButtonID = 100 then
   OpenUrl('http://github.com/varianus/ovom3u/wiki/LibMPV/');

  ACanClose := AButtonID = mrClose;


end;

procedure TfPlayer.ToolButton1Click(Sender: TObject);
begin
  ShowConfig;
end;

procedure TfPlayer.SetLoading(AValue: boolean);
begin

  FLoading := AValue;
  LoadingTimer.Enabled := FLoading;
  GLRenderer.Visible := not FLoading;
  if not loading then
     LoadTracks;
end;

procedure TfPlayer.LoadTracks;
var
  Track: TTrack;
  mnu: TMenuItem;
  i: Integer;
begin
  mnuAudio.Clear;
  mnuVideo.Clear;
  mnuAudio.Clear;
  for i := 0 to Length(MpvEngine.TrackList) - 1 do
  begin
    Track := MpvEngine.TrackList[i];
    case Track.Kind of
      tkVideo:
      begin
        if Track.Title = EmptyStr then
          Track.Title := format('%dx%d (%d bitrate %s)', [Track.W, track.h, track.Bitrate, track.codec]);
        mnu     := tmenuitem.Create(mnuVideo);
        mnu.RadioItem := True;
        mnu.Checked := Track.Selected;
        mnu.Caption := track.Title;
        mnu.Tag := i + 1;
        mnu.GroupIndex := 2;
        mnu.OnClick := mnuTrackClick;
        mnuVideo.Add(mnu);
      end;
      tkAudio:
      begin
        if Track.Title = EmptyStr then
          Track.Title := format('%s (%d ch %d  %s)', [Track.Lang, Track.Channels, track.Bitrate, track.codec]);

        mnu     := tmenuitem.Create(mnuAudio);
        mnu.RadioItem := True;
        mnu.Checked := Track.Selected;
        mnu.Caption := track.Title;
        mnu.Tag := i + 1;
        mnu.GroupIndex := 1;
        mnu.OnClick := mnuTrackClick;
        mnuAudio.Add(mnu);
      end;
      tksub:
      begin
        if Track.Title = EmptyStr then
          Track.Title := format('%s %s', [Track.Lang, track.codec]);
        mnu     := tmenuitem.Create(mnuSub);
        mnu.RadioItem := True;
        mnu.Checked := Track.Selected;
        mnu.Caption := track.Title;
        mnu.Tag := i + 1;
        mnu.GroupIndex := 3;
        mnu.OnClick := mnuTrackClick;
        mnuSub.Add(mnu);
      end;
    end;
  end;

end;

procedure TfPlayer.mnuTrackClick(Sender: TObject);
var
  mnu: TMenuItem;
begin
  mnu := TMenuItem(Sender);
  MpvEngine.SetTrack(mnu.Tag);
  mnu.Checked := True;

end;


procedure TfPlayer.SetFullScreen;
begin
  flgFullScreen := not flgFullScreen;
  if flgFullScreen then
    try
      isRenderActive := False;
      pnlChannel.Visible := False;
      RestoredBorderStyle := BorderStyle;
      RestoredWindowState := WindowState;
        {$IFDEF WINDOWS}
      BorderStyle := bsNone;
        {$ENDIF}
      WindowState := wsFullScreen;
      HideMouse.Enabled := True;
    finally
      isRenderActive := True;
    end
  else
  begin
    isRenderActive := False;
    pnlChannel.Visible := True;
    WindowState    := wsNormal;
    WindowState    := RestoredWindowState;
    BorderStyle    := RestoredBorderStyle;
    screen.cursor  := crdefauLt;
    HideMouse.Enabled := False;
    isRenderActive := True;
  end;

end;

end.

