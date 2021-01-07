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
  Classes, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, LCLIntf, lcltype, ComCtrls, Menus, um3uloader,
  OpenGLContext, Types, Math, SysUtils,
  MPV_Engine, Config, GeneralFunc, UITypes, epg, uMyDialog, uEPGFOrm;

{ TfPlayer }
type

  TfPlayer = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ChannelList: TDrawGrid;
    OSDTimer: TTimer;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuSub: TMenuItem;
    mnuAudio: TMenuItem;
    mnuVideo: TMenuItem;
    GLRenderer: TOpenGLControl;
    ChannelTimer: TTimer;
    pnlChannel: TPanel;
    pnlContainer: TPanel;
    pmPlayer: TPopupMenu;
    HideMouse: TTimer;
    LoadingTimer: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure ChannelListDblClick(Sender: TObject);
    procedure ChannelListDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure ChannelListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ChannelTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure LoadingTimerStartTimer(Sender: TObject);
    procedure LoadingTimerTimer(Sender: TObject);
    procedure GLRendererDblClick(Sender: TObject);
    procedure GLRendererMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure HideMouseTimer(Sender: TObject);
    procedure OSDTimerTimer(Sender: TObject);
    procedure pmPlayerPopup(Sender: TObject);
    procedure pnlContainerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure pnlContainerPaint(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    FLoading: boolean;
    ChannelSelecting: boolean;
    ChannelSelected: integer;
    fLastMessage: string;
    IPTVList: string;
    Kind: TProviderKind;
    function CheckConfigAndSystem: boolean;
    procedure DebugLnHook(Sender: TObject; S: string; var Handled: Boolean);
    procedure OnLoadingState(Sender: TObject);
    procedure OsdMessage(Message: string; TimeOut: boolean = True);
    procedure Play(Row: integer);
    procedure SetLoading(AValue: boolean);
    procedure ShowEpg;
  private
    MpvEngine: TMPVEngine;
    epgData: TEpg;
    isRenderActive: boolean;
    flgFullScreen: boolean;
    ShowingInfo: boolean;
    CurrentChannel: integer;
    PreviousChannel: integer;
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

uses uconfig, BaseTypes, LazUTF8, LazLogger;
var
  f: text;

{$R *.lfm}

const
  WIKI_MPV_LINK = 'https://github.com/varianus/ovom3u/wiki/LibMPV';

{ TfPlayer }

function TfPlayer.CheckConfigAndSystem: boolean;
var
  Retry: boolean;
begin
  repeat
    Retry := False;
    if not TMPVEngine.CheckMPV then
      case ShowMyDialog(mtWarning, 'Can''t initialize libMPV',
          'LibMPV shared library is missing or could not be initialized.' + #10 +
          'OvoM3U uses this library to decode and play video.' + #10 +
          'Click the following link to open a wiki page with information on' + #10 +
          'how to install libMPV on your platform', [mbRetry, mbClose],
          [WIKI_MPV_LINK]) of

        mrClose:
        begin
          Result := False;
          Retry := False;
          exit;
        end;
        mrRetry: Retry := True;
        100: OpenURL(WIKI_MPV_LINK);
      end;
  until Retry = False;


  Result := True;
  Kind := ConfigObj.M3UProperties.Kind;
  case Kind of
    Local: IPTVList := ConfigObj.M3UProperties.FileName;
    URL: IPTVList := ConfigObj.M3UProperties.Url;
  end;

  if IPTVList.IsEmpty then
    case ShowMyDialog(mtWarning, 'Welcome to OvoM3U',
        'No list configured' + #10 +
        'Message for configuration', [mbClose],
        ['Open Config']) of
      mrClose:
      begin
        Result := False;
        exit;
      end;
      100: ShowConfig;
    end;

end;

procedure TfPlayer.LoadList;
var
  CacheDir: string;
begin

  ConfigObj.ReadConfig;

  Kind := ConfigObj.M3UProperties.Kind;

  if Kind = URL then
  begin
    CacheDir := GetCacheDir;
    try
      if epgData.LastScan('channels') + 12/24 < now then
        begin
          DownloadFromUrl(IPTVList, CacheDir + 'current-iptv.m3u');
          epgData.SetLastScan('channels',now);
        end;

      IPTVList := CacheDir + 'current-iptv.m3u';
    finally
    end;
  end;

  list.Load(IPTVList);
  if ConfigObj.M3UProperties.UseChno then
    List.FixChannelNumbering;

  if not Configobj.M3UProperties.EPGUrl.IsEmpty then
  begin
    if List.ListMd5 <> epgData.LastChannelMd5 then
      begin
        epgData.LoadChannelList(List);
        epgData.SetLastChannelMd5(List.ListMd5);
        epgData.SetLastScan('epg',0);
        epgData.Scan;
      end;
  end;


  ChannelList.RowCount := List.Count;

end;

procedure TfPlayer.FormCreate(Sender: TObject);
begin
  Progress := 0;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  flgFullScreen := False;
  ShowingInfo := false;
  if CheckConfigAndSystem then
  begin
    ChannelList.RowCount := 0;
    List := TM3ULoader.Create;
    epgData := TEpg.Create;
    Loadlist;

    MpvEngine := TMPVEngine.Create;
    MpvEngine.Initialize(GLRenderer);
    CurrentChannel := -1;
    PreviousChannel := -1;
    ChannelSelecting := False;
    fLoading := False;
    ChannelSelected := 0;

  end;
end;

procedure TfPlayer.FormDestroy(Sender: TObject);
begin
  Application.ProcessMessages;
  MpvEngine.Free;
  List.Free;
  epgData.Free;
end;

procedure TfPlayer.OnLoadingState(Sender: TObject);
begin
  if Loading then
    Loading := MpvEngine.IsIdle;
  if not Loading then
    OsdMessage('',false);
end;

procedure TfPlayer.OsdMessage(Message: string; TimeOut: boolean = True);
begin

  if GLRenderer.Visible then
    begin
      MpvEngine.OsdMessage(message);
      OSDTimer.Enabled := TimeOut;
    end
  else
    begin
       fLastMessage := Message;
       pnlContainer.Invalidate;
    end;

end;

procedure TfPlayer.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  Pass: boolean;
begin
  Pass :=false;
  if flgFullScreen then
    case key of
      VK_ESCAPE: SetFullScreen;
      VK_DOWN:
      begin
        ChannelList.Row := CurrentChannel + 1;
        play(ChannelList.Row);
      end;
      VK_UP:
      begin
        ChannelList.Row := CurrentChannel - 1;
        play(ChannelList.Row);
      end;
      VK_RETURN: if ChannelSelecting then
        begin
          if ConfigObj.M3UProperties.UseChno then
            ChannelSelected := List.ItemByChno(ChannelSelected);
          play(ChannelSelected);
          ChannelSelecting := False;
          key := 0;
        end;
      else
        Pass:= true;
    end;
  case key of
    VK_I: ShowEpg;
    VK_S:
    begin
      MpvEngine.Stop;
      OsdMessage('Stop', True);
    end;
    VK_SPACE: begin
               if MpvEngine.Pause then
                 begin
                   MpvEngine.OsdEpg('',Default(REpgInfo), False);
                   OsdMessage('Pause', False)
                 end
               else
                  MpvEngine.OsdMessage();
    end;
    VK_F: SetFullScreen;
    VK_E: begin
        EpgForm.EpgData := epgData;
        EPGForm.Show;
    end;
    VK_B: begin
        If PreviousChannel <> -1 then
        Play(PreviousChannel);
    end;

    VK_RIGHT: begin
        MpvEngine.Seek(5);
    end;
    VK_LEFT: begin
        MpvEngine.Seek(-5);
    end;

    VK_0..VK_9, VK_NUMPAD0..VK_NUMPAD9:
    begin
      if not ChannelSelecting then
      begin
        ChannelSelecting := True;
        ChannelSelected := Key - $30;
        if Key >= $60 then
          ChannelSelected := ChannelSelected - $30;
      end
      else
      begin
        ChannelSelected := (ChannelSelected * 10) + Key - $30;
        if Key >= $60 then
          ChannelSelected := ChannelSelected - $30;

      end;
      if  GLRenderer.Visible then
         OsdMessage(IntToStr(ChannelSelected), false)
      else
        begin
         fLastMessage := IntToStr(ChannelSelected);
         pnlContainer.Invalidate;
        end;
      ChannelTimer.Enabled := true;
    end;
    else
     Pass:= true;
  end;
  if not pass then
    key:=0;
end;

procedure TfPlayer.ShowEpg;
var
  Info: REpgInfo;
begin
  if not ShowingInfo then
    begin
      Info := epgData.GetEpgInfo(CurrentChannel + 1, now);
      MpvEngine.OsdEpg(Format('%3.3d: %s', [List[CurrentChannel].Number, List[CurrentChannel].title]),info, True);
      ShowingInfo := true;
      OSDTimer.Enabled := True;
    end
  else
    begin
     OSDTimerTimer(self);
    end;

end;

procedure TfPlayer.LoadingTimerStartTimer(Sender: TObject);
begin
  Progress := 10;
end;

procedure TfPlayer.LoadingTimerTimer(Sender: TObject);
begin
  Inc(progress, 10);
  if progress mod 50 = 0 then
  begin
    Loading := MpvEngine.isIdle;
    if not loading then
    begin
      OsdMessage('');
      LoadingTimer.Enabled := False;
    end;
  end;

  if progress > 720 then
    Progress := 10;
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
    cv.Font.Style := [fsBold, fsUnderline];
    cv.Font.color := clHighlightText;
    cv.Brush.color := clHighlight;
    cv.Rectangle(aRect);
  end
  else
    cv.Font.Style := [fsBold];

  cv.TextRect(aRect, 0, aRect.top + 5, Format('%3.3d: %s', [List[arow].Number, List[arow].title]));

  cv.Font.Size := 9;
  cv.Font.Style := [];
  cv.TextRect(aRect, 0, aRect.top + 25, List[arow].CurrProgram);
end;

procedure TfPlayer.ChannelListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = VK_RETURN then
    Play(ChannelList.Row);
end;

procedure TfPlayer.ChannelTimerTimer(Sender: TObject);
begin
  if ChannelSelecting then
  begin
    if ConfigObj.M3UProperties.UseChno then
      ChannelSelected := List.ItemByChno(ChannelSelected)
    else
      ChannelSelected := ChannelSelected - 1;

    ChannelSelecting := False;
    OsdMessage('',false);
    Play(ChannelSelected);

  end;
  ChannelTimer.Enabled := false;
end;

procedure TfPlayer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfPlayer.ChannelListDblClick(Sender: TObject);
begin
  Play(ChannelList.Row);
end;

procedure TfPlayer.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin

  Done := false;
end;

procedure TfPlayer.DebugLnHook(Sender: TObject; S: string; var Handled: Boolean);
begin
 if (TextRec(f).mode <> fmClosed) and (IOResult = 0) then
    WriteLn(f, S);
end;

procedure TfPlayer.Play(Row: integer);
begin
  if (row > List.Count) or (row < 0) then
  begin
    OsdMessage('No Channel', True);
    exit;
  end;

  if (CurrentChannel = Row) and not MpvEngine.IsIdle then
    exit;

  if list[Row].Mrl.IsEmpty then
  begin
    OsdMessage('Missing Channel Address', True);
    exit;
  end;

  ChannelList.Invalidate;
  PreviousChannel := CurrentChannel;
  CurrentChannel := Row;
  Caption := list[CurrentChannel].title;
  MpvEngine.Play(list[CurrentChannel].Mrl);
  Loading := True;
  fLastMessage := 'Loading: ' + list[CurrentChannel].title;
  pnlContainer.Invalidate;

end;

procedure TfPlayer.GLRendererDblClick(Sender: TObject);
begin
  SetFullScreen;
end;

procedure TfPlayer.GLRendererMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  Screen.Cursor := crdefault;
  HideMouse.Enabled := flgFullScreen;
end;

procedure TfPlayer.HideMouseTimer(Sender: TObject);
begin
  screen.cursor := crNone;
end;

procedure TfPlayer.OSDTimerTimer(Sender: TObject);
begin

  ShowingInfo := false;
  if GLRenderer.Visible then
  begin
    MpvEngine.OsdEpg('',Default(REpgInfo), False);
    MpvEngine.OsdMessage();
  end ;
  OSDTimer.Enabled := False;
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

procedure TfPlayer.pnlContainerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  GLRendererMouseMove(Sender, Shift, x, y);
end;

procedure TfPlayer.pnlContainerPaint(Sender: TObject);
var
  cv: Tcanvas;
  a, b: integer;
  p: tpoint;
  Scaling: double;
begin
  if GLRenderer.Visible then
    exit;
  cv := pnlContainer.Canvas;
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
      A := (progress - 720) * 16 +10;
      b := -360 * 16;
    end;
    p.X := pnlcontainer.Width div 2;
    p.y := pnlcontainer.Height div 2;
    cv.Arc(p.x - 50, p.y - 50, p.x + 50, p.y + 50, b, a);
  end;
  cv.font.Color := clwhite;
  // MPV use a default font of 55 pixel for a 720 pixel high window
  // try to replicate same scaling
  Scaling := (pnlcontainer.Height / 720);
  cv.font.Height := trunc(55 * scaling);
  cv.TextOut(trunc(scaling * 25), trunc(scaling * 22), fLastMessage);

end;

procedure TfPlayer.ToolButton1Click(Sender: TObject);
begin
  ShowConfig;
end;

procedure TfPlayer.ToolButton2Click(Sender: TObject);
begin
  EpgForm.EpgData := epgData;
  EPGForm.Show;
end;

procedure TfPlayer.SetLoading(AValue: boolean);
begin

  FLoading := AValue;
  LoadingTimer.Enabled := FLoading;
  GLRenderer.Visible := not FLoading;
  if not loading then
    begin
      fLastMessage := '';
      MpvEngine.LoadTracks;
      LoadTracks;
    end;
end;

procedure TfPlayer.LoadTracks;
var
  Track: TTrack;
  mnu: TMenuItem;
  i: integer;
begin
  mnuAudio.Clear;
  mnuVideo.Clear;
  mnuAudio.Clear;
  for i := 0 to Length(MpvEngine.TrackList) - 1 do
  begin
    Track := MpvEngine.TrackList[i];
    if track.Id <> 0 then
      case Track.Kind of
        trkVideo:
        begin
          if Track.Title = EmptyStr then
            Track.Title := format('%dx%d (%s)', [Track.W, track.h, track.codec]);
          mnu := tmenuitem.Create(mnuVideo);
          mnu.RadioItem := True;
          mnu.Checked := Track.Selected;
          mnu.Caption := track.Title;
          mnu.Tag := i;
          mnu.GroupIndex := 2;
          mnu.OnClick := mnuTrackClick;
          mnuVideo.Add(mnu);
        end;
        trkAudio:
        begin
          if Track.Title = EmptyStr then
            Track.Title := format('%s %d ch %d  (%s)', [Track.Lang, Track.Channels, track.Bitrate, track.codec]);

          mnu := tmenuitem.Create(mnuAudio);
          mnu.RadioItem := True;
          mnu.Checked := Track.Selected;
          mnu.Caption := track.Title;
          mnu.Tag := i;
          mnu.GroupIndex := 1;
          mnu.OnClick := mnuTrackClick;
          mnuAudio.Add(mnu);
        end;
        trkSub:
        begin
          if Track.Title = EmptyStr then
            Track.Title := format('%s %s', [Track.Lang, track.codec]);
          mnu := tmenuitem.Create(mnuSub);
          mnu.RadioItem := True;
          mnu.Checked := Track.Selected;
          mnu.Caption := track.Title;
          mnu.Tag := i;
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
    WindowState := wsNormal;
    WindowState := RestoredWindowState;
    BorderStyle := RestoredBorderStyle;
    screen.cursor := crdefauLt;
    HideMouse.Enabled := False;
    isRenderActive := True;
  end;

end;

end.
