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
unit umain;

interface

uses
  System.UITypes, Classes, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  LCLIntf, lcltype, ComCtrls, Menus, ActnList, Buttons, StdCtrls,
  IniPropStorage, PopupNotifier, um3uloader, OpenGLContext, Types, Math,
  SysUtils, MPV_Engine, Config,
  {$IFDEF LINUX} clocale,{$endif}
  GeneralFunc, epg, uMyDialog, uEPGFOrm, uBackEnd, BaseTypes, mouseandkeyinput,
  LoggerUnit, uhint;

type

  { TGuiProperties }

  TGuiProperties = class(TConfigParam)
    fViewLogo: boolean;
    fViewCurrentProgram: boolean;
  private
    FBoundsRect: TRect;
    FChannelGridWidth: integer;
    FEmbeddedSubForm: boolean;
    procedure SetBoundRect(AValue: TRect);
    procedure SetChannelGridWidth(AValue: integer);
    procedure SetEmbeddedSubForm(AValue: boolean);
    procedure SetViewCurrentProgram(AValue: boolean);
    procedure SetViewLogo(AValue: boolean);
  protected
    procedure InternalSave; override;
  public
    property ViewLogo: boolean read fViewLogo write SetViewLogo;
    property ViewCurrentProgram: boolean read fViewCurrentProgram write SetViewCurrentProgram;
    property ChannelGridWidth: integer read FChannelGridWidth write SetChannelGridWidth;
    property BoundsRect: TRect read FBoundsRect write SetBoundRect;
    property EmbeddedSubForm: boolean read FEmbeddedSubForm write SetEmbeddedSubForm;
    procedure Load; override;
    constructor Create(aOwner: TConfig; ABoundsRect: TRect); reintroduce;
  end;

Resourcestring
  RS_Cannot_Initialize = 'Cannot initialize libMPV';
  RS_Missing_MPV =
          'LibMPV shared library is missing or could not be initialized.' + #10 +
          'OvoM3U uses this library to decode and play video.' + #10 +
          'Click the following link to open a wiki page with information on' + #10 +
          'how to install libMPV on your platform';
  RS_Welcome = 'Welcome to OvoM3U';
  RS_No_List = 'No list configured' + #10 +
        'Message for configuration';

  { TfPlayer }
type

  TfPlayer = class(TForm)
    actShowLog: TAction;
    actShowList: TAction;
    actShowConfig: TAction;
    actShowEpg: TAction;
    actViewLogo: TAction;
    actViewCurrentProgram: TAction;
    actList: TActionList;
    AppProperties: TApplicationProperties;
    ChannelList: TDrawGrid;
    EPGList: TDrawGrid;
    lvLists: TListView;
    MainMenu1: TMainMenu;
    MemoEpg: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    N1: TMenuItem;
    mnuSub: TMenuItem;
    mnuAudio: TMenuItem;
    mnuVideo: TMenuItem;
    GLRenderer: TOpenGLControl;
    ChannelTimer: TTimer;
    Panel2: TPanel;
    pcLists: TPageControl;
    Panel1: TPanel;
    pnlEpg: TPanel;
    pnlSubForm: TPanel;
    pnlChannel: TPanel;
    pnlContainer: TPanel;
    pmPlayer: TPopupMenu;
    HideMouse: TTimer;
    LoadingTimer: TTimer;
    pmuView: TPopupMenu;
    ChannelSplitter: TSplitter;
    tsList: TTabSheet;
    cbGroups: TListBox;
    tsGroups: TTabSheet;
    tsChannels: TTabSheet;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    ToolButton5: TSpeedButton;
    procedure actListUpdate(AAction: TBasicAction; var Handled: boolean);
    procedure actShowEpgExecute(Sender: TObject);
    procedure actShowConfigExecute(Sender: TObject);
    procedure actShowListExecute(Sender: TObject);
    procedure actShowLogExecute(Sender: TObject);
    procedure actViewCurrentProgramExecute(Sender: TObject);
    procedure actViewLogoExecute(Sender: TObject);
    procedure AppPropertiesException(Sender: TObject; E: Exception);
    procedure AppPropertiesShowHint(var HintStr: string; var CanShow: boolean;
      var HintInfo: THintInfo);
    procedure cbGroupsDblClick(Sender: TObject);
    procedure cbGroupsKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ChannelListDblClick(Sender: TObject);
    procedure ChannelListDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure ChannelListGetCellHint(Sender: TObject; ACol, ARow: integer; var HintText: string);
    procedure ChannelListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ChannelSplitterMoved(Sender: TObject);
    procedure ChannelTimerTimer(Sender: TObject);
    procedure EPGListDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure EPGListSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure GLRendererChangeBounds(Sender: TObject);
    procedure lvListsDblClick(Sender: TObject);
    procedure lvListsKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure LoadingTimerStartTimer(Sender: TObject);
    procedure LoadingTimerTimer(Sender: TObject);
    procedure GLRendererDblClick(Sender: TObject);
    procedure GLRendererMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure HideMouseTimer(Sender: TObject);
    procedure pmPlayerClose(Sender: TObject);
    procedure pmPlayerPopup(Sender: TObject);
    procedure pnlContainerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure pnlContainerPaint(Sender: TObject);
    procedure ToolButton5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private
    ChannelInfo: AREpgInfo;
    FLoading: boolean;
    ChannelSelecting: boolean;
    fLastMessage: string;
    IPTVList: string;
    Kind: TProviderKind;
    fFilteredList: TFilteredList;
    SubForm: TForm;
    SubFormVisible: boolean;
    OldLogLevel: TOvoLogLevel;
    function CheckConfigAndSystem: boolean;
    procedure CloseSubForm;
    procedure ComputeGridCellSize;
    function ComputeTrackTitle(Track: TTrack): string;
    procedure ConfigDone(Sender: TObject);
    procedure DebugLnHook(Sender: TObject; S: string; var Handled: boolean);
    procedure DoExternalInput(Data: PtrInt);
    procedure EmbedSubForm(AForm: TForm);
    procedure ExternalInput(Sender: TObject; var Key: word);
    procedure InitializeGui(Data: ptrint);
    procedure LoadDailyEpg;
    procedure OnListChanged(Sender: TObject);
    procedure OnLoadingState(Sender: TObject);
    procedure OnPlayError(const msg: string);
    procedure OnTrackChange(Sender: TObject);
    procedure Play(Row: integer);
    procedure SelectGroup;
    procedure SelectList;
    procedure SetLoading(AValue: boolean);
    procedure OnPlay(Sender: TObject);

  private
    ChannelSelected: integer;
    flgFullScreen: boolean;
    RestoredBorderStyle: TFormBorderStyle;
    RestoredWindowState: TWindowState;
    Progress: integer;
    property Loading: boolean read FLoading write SetLoading;
    procedure LoadTracks;
    procedure mnuTrackClick(Sender: TObject);
    procedure SetFullScreen;
    procedure LoadGroups;
  public
    GuiProperties: TGuiProperties;
    procedure InitializeLists;

  end;

var
  fPlayer: TfPlayer;
  openglHandle: Thandle;

implementation

uses uconfig, AppConsts, uChannels, uLogViewer, LazUTF8, LazLogger;

var
  f: Text;

  {$R *.lfm}

  { TGuiProperties }

procedure TGuiProperties.SetChannelGridWidth(AValue: integer);
begin
  if FChannelGridWidth = AValue then Exit;
  FChannelGridWidth := AValue;
  Dirty := True;
end;

procedure TGuiProperties.SetEmbeddedSubForm(AValue: boolean);
begin
  if FEmbeddedSubForm = AValue then Exit;
  FEmbeddedSubForm := AValue;
  Dirty := True;
end;

procedure TGuiProperties.SetBoundRect(AValue: TRect);
begin
  if FBoundsRect = AValue then Exit;
  FBoundsRect := AValue;
  Dirty := True;
end;

procedure TGuiProperties.SetViewCurrentProgram(AValue: boolean);
begin
  if fViewCurrentProgram = AValue then Exit;
  fViewCurrentProgram := AValue;
  Dirty := True;
end;

procedure TGuiProperties.SetViewLogo(AValue: boolean);
begin
  if fViewLogo = AValue then Exit;
  fViewLogo := AValue;
  Dirty := True;
end;

procedure TGuiProperties.InternalSave;
begin
  Owner.WriteBoolean('gui/ViewLogo', ViewLogo);
  Owner.WriteBoolean('gui/ViewCurrentProgram', ViewCurrentProgram);
  Owner.WriteInteger('gui/ChannelGridWidth', ChannelGridWidth);
  Owner.WriteRect('gui/MainForm/Position', BoundsRect);
  Owner.WriteBoolean('gui/EmbeddedSubForm', EmbeddedSubForm);
end;

procedure TGuiProperties.Load;
begin
  ViewLogo := Owner.ReadBoolean('gui/ViewLogo', False);
  ViewCurrentProgram := Owner.ReadBoolean('gui/ViewCurrentProgram', False);
  ChannelGridWidth := Owner.ReadInteger('gui/ChannelGridWidth', 215);
  BoundsRect := Owner.ReadRect('gui/MainForm/Position', BoundsRect);
  EmbeddedSubForm := Owner.ReadBoolean('gui/EmbeddedSubForm', True);

  Dirty := False;
end;

constructor TGuiProperties.Create(aOwner: TConfig; ABoundsRect: TRect);
begin
  FBoundsRect := ABoundsRect;
  inherited Create(aOwner);
  Dirty := False;
end;

{ TfPlayer }

function TfPlayer.CheckConfigAndSystem: boolean;
var
  Retry: boolean;
begin
  repeat
    Retry := False;
    if not Tmpvengine.CheckMPV then
    begin
      OvoLogger.Log(llERROR, RS_Cannot_initialize);
      case ShowMyDialog(mtWarning, RS_Cannot_initialize, RS_Missing_MPV, [mbRetry, mbClose], [WIKI_MPV_LINK]) of

        mrClose:
        begin
          Result := False;
          Retry := False;
          exit;
        end;
        mrRetry:
          Retry := True;
        100:
          OpenURL(WIKI_MPV_LINK);
        else
          Retry := False;
      end;

    end;
  until Retry = False;


  Result := True;

  if ConfigObj.ListManager.Count = 0 then
    case ShowMyDialog(mtWarning, RS_Welcome, RS_No_List, [mbClose], ['Open Config']) of
      mrClose:
      begin
        Result := False;
        exit;
      end;
      100:
        actShowConfig.Execute;
    end
  else
    LoadGroups;

end;

procedure TfPlayer.LoadGroups;
var
  CacheDir: string;
  i, j, k: integer;
begin

  fFilteredList := BackEnd.M3ULoader.Filter(Default(TFilterParam));
  i := BackEnd.m3uloader.Groups.Count;
  cbGroups.Items.Clear;
  k := 0;
  if cbGroups.Items.Capacity < i + 1 then
    cbGroups.Items.Capacity := i + 1;

  for j := 0 to i - 1 do
  begin
    cbGroups.Items.Add(BackEnd.m3uloader.Groups[j] + ' (' + IntToStr(PtrInt(BackEnd.m3uloader.Groups.Objects[j])) + ')');
    Inc(k, PtrInt(BackEnd.m3uloader.Groups.Objects[j]));
  end;

  cbGroups.items.Insert(0, um3uloader.RSAnyGroup + ' (' + IntToStr(k) + ')');
  cbGroups.ItemIndex := 0;

  ChannelList.RowCount := BackEnd.M3ULoader.Count;

end;

procedure TfPlayer.OnListChanged(Sender: TObject);
begin
  ChannelList.invalidate;
end;

procedure TfPlayer.DoExternalInput(Data: PtrInt);
var
  key: word;
begin
  Key := word(PtrInt(Data));
  if key > $1ff then
    FormKeyDown(self, Key, [])
  else
    KeyInput.Press(Key);
end;

procedure TfPlayer.ExternalInput(Sender: TObject; var Key: word);
begin
  Application.QueueAsyncCall(DoExternalInput, PtrInt(key));
end;

procedure TfPlayer.FormCreate(Sender: TObject);
begin
  SetLength(ChannelInfo, 0);
  SubFormVisible := False;
  OvoLogger.Log(llINFO, 'Load configuration from %s', [ConfigObj.ConfigFile]);

  Progress := 0;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  flgFullScreen := False;
  OvoLogger.Log(llINFO, 'Create main GUI');

  pcLists.ActivePage := tsList;

  BackEnd.OnExternalInput := ExternalInput;
  BackEnd.OnPlay := OnPlay;
  BackEnd.OnListChanged := OnListChanged;
  ChannelList.RowCount := 0;

  ChannelSelecting := False;
  fLoading := False;
  ChannelSelected := 0;
  Application.QueueAsyncCall(InitializeGui, 0);

  if CheckConfigAndSystem then
  begin
    backend.InitializeEngine(GLRenderer);
    backend.mpvengine.OnLoadingState := OnLoadingState;
    backend.mpvengine.OnPlayError := OnPlayError;
    backend.mpvengine.OnTrackChange := OnTrackChange;
  end
  else
    OvoLogger.Log(llWARN, 'Invalid config');
  OldLogLevel := OvoLogger.Level;
end;

procedure TfPlayer.FormDestroy(Sender: TObject);
begin
  Application.ProcessMessages;
  OvoLogger.Log(llINFO, 'Closed main GUI');
end;

procedure TfPlayer.OnLoadingState(Sender: TObject);
begin
  if Loading then
    Loading := backend.mpvengine.IsIdle;
  if not Loading then
    Backend.OsdMessage('', False);
end;

procedure TfPlayer.OnPlayError(const msg:string);
begin
  Backend.MpvEngine.PlayIMG('ovoimg://empty.png'); //PlayIMG(ConfigObj.GetResourcesPath + 'prova.png');
  Application.ProcessMessages;
  Backend.OsdMessage(msg, False);
end;

procedure TfPlayer.OnTrackChange(Sender: TObject);
begin
  LoadTracks;
end;

procedure TfPlayer.OnPlay(Sender: TObject);
var
  Idx: integer;
begin
  Idx := fFilteredList.IndexOf(BackEnd.CurrentIndex);
  if Idx <> -1 then
    ChannelList.Row := Idx
  else
    ChannelList.Selection := Rect(-1, -1, -1, -1);
end;


procedure TfPlayer.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  Pass: boolean;
  channel: integer;
  ExtendedKey: boolean;
begin
  Pass := False;
  if (key and $300) = $300 then
  begin
    hide;
    application.ProcessMessages;
    Application.Terminate;
  end;

  if (Shift = [ssShift, ssCtrl]) and (key = VK_D) then
    if OvoLogger.Level <> llDEBUG then
      OvoLogger.Level := llDEBUG
    else
      OvoLogger.Level := OldLogLevel;

  if (Key and $200) <> 0 then
  begin
    ExtendedKey := True;
    Key := key and $1FF;
  end
  else
    ExtendedKey := False;

  case key of
    VK_ESCAPE:
    begin
      if SubFormVisible then
        CloseSubForm
      else
      if flgFullScreen then
        SetFullScreen;
      Key := 0;
    end;

    VK_MEDIA_NEXT_TRACK:
    begin
      channel := fFilteredList.IndexOf(BackEnd.CurrentIndex);
      play(fFilteredList.Map(channel + 1));
    end;
    VK_MEDIA_PREV_TRACK:
    begin
      channel := fFilteredList.IndexOf(BackEnd.CurrentIndex);
      play(fFilteredList.Map(channel - 1));
    end;
    VK_MEDIA_STOP:
    begin
      backend.mpvengine.Stop;
      Backend.OsdMessage('Stop', True);
    end;

  end;

  if (not SubFormVisible) or ExtendedKey then
  begin
    case key of
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
        Backend.OsdMessage(IntToStr(ChannelSelected), False);
        ChannelTimer.Enabled := True;
      end;
      VK_B:
        BackEnd.SwapChannel;
      VK_C:
      begin
        pnlChannel.Visible := not pnlChannel.Visible;
        ChannelSplitter.Visible := pnlChannel.Visible;
        HideMouse.Enabled := (not pnlChannel.Visible) and flgFullScreen;
        if pnlChannel.Visible then
          ChannelList.SetFocus;
      end;
      VK_D:
        actShowLog.Execute;
      VK_E:
        actShowEpg.Execute;
      VK_F:
        SetFullScreen;
      VK_I:
        Backend.ShowEpg;
      VK_L:
        actShowList.Execute;
      VK_M:
        backend.mpvengine.Mute;
      VK_O:
        backend.mpvengine.ShowStats();
      VK_P:
        if not pnlEpg.Visible then
        begin
          LoadDailyEpg;
          pnlEpg.Visible := True;
        end
        else
          pnlEpg.Visible := False;
      VK_R:
      begin
        backend.mpvengine.Stop;
        Sleep(2000);
        Play(BackEnd.CurrentIndex);
      end;
      VK_S:
      begin
        backend.mpvengine.Stop;
        Backend.OsdMessage('Stop', True);
      end;
      VK_T:
        Backend.OsdMessage(FormatDateTime('t', now), True);
      VK_SPACE:
        if backend.mpvengine.Pause then
        begin
          backend.mpvengine.OsdEpg('', Default(REpgInfo), False);
          Backend.OsdMessage('Pause', False);
        end
        else
          backend.mpvengine.OsdMessage();

      VK_RIGHT:
      begin
        if ExtendedKey then
          backend.mpvengine.Seek(5);
        pass := True;
      end;
      VK_LEFT:
      begin
        if ExtendedKey then
          backend.mpvengine.Seek(-5);
        pass := True;
      end;
      VK_RETURN:
        if ChannelSelecting then
        begin
          if BackEnd.M3ULoader.ActiveList.UseChno then
            ChannelSelected := BackEnd.M3ULoader.ItemByChno(ChannelSelected)
          else
            ChannelSelected := ChannelSelected - 1;
          play(ChannelSelected);
          ChannelSelecting := False;
          key := 0;
        end
        else
          pass := True;
      else
        Pass := True;
    end;
  end
  else
    Pass := True;
  if not pass then
    key := 0;
end;

procedure TfPlayer.FormShow(Sender: TObject);
begin
  if pnlChannel.Visible then
    lvLists.SetFocus;
end;

procedure TfPlayer.GLRendererChangeBounds(Sender: TObject);
begin
  BackEnd.MpvEngine.Refresh;
end;

procedure TfPlayer.SelectList;
begin
  BackEnd.LoadList(lvLists.Selected.Data);
  LoadGroups;
  if cbGroups.Count < 3 then
    pcLists.ActivePage := tsChannels
  else
    pcLists.ActivePage := tsGroups;
end;

procedure TfPlayer.lvListsDblClick(Sender: TObject);
begin
  SelectList;
  ComputeGridCellSize;
end;

procedure TfPlayer.lvListsKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin

  if (key) = VK_RETURN then
    SelectList;

  if key = VK_RIGHT then
    pcLists.ActivePage := tsGroups;

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
    Loading := backend.mpvengine.isIdle;
    if not loading then
    begin
      Backend.OsdMessage('');
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
  Element: TM3UItem;
  bmp: TPicture;
  r: Trect;
  Scale: double;
  H: integer;
  Spacing: integer;
  epgInfo: REpgInfo;
begin
  if not Assigned(GuiProperties) then
    exit;
  Element := fFilteredList[Arow];
  h := 0;
  cv := ChannelList.Canvas;
  if GuiProperties.ViewLogo then
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
      bmp.LoadFromResourceName(HINSTANCE, 'NO-LOGO.PNG');//lo LoadFromFile(ConfigObj.GetResourcesPath + 'no-logo.png');
      cv.StretchDraw(rect(arect.left, arect.Top, arect.Left + h, aRect.Top + h), bmp.Graphic);
      bmp.Free;
    end;

  end;

  cv.Font.Height := Scale96Toscreen(-14);
  if Backend.CurrentIndex = fFilteredList.Map(aRow) then
  begin
    cv.Font.Style := [fsBold, fsUnderline];
    cv.Font.color := clHighlightText;
    cv.Brush.color := clHighlight;
    cv.Rectangle(aRect);
  end
  else
    cv.Font.Style := [fsBold];

  Spacing := Scale96ToScreen(2);
  cv.TextRect(aRect, h + Spacing * 2, aRect.top + Spacing * 2, Format('%3.3d: %s', [Element.Number, Element.title]));
  if GuiProperties.ViewCurrentProgram and
    (BackEnd.EpgData.ActiveList.EpgKind <> None) then
  begin
    epgInfo := BackEnd.epgdata.GetEpgInfo(fFilteredList.Map(arow), now);
    if epgInfo.HaveData then
    begin
      cv.Font.Height := Scale96ToScreen(-12);
      cv.Font.Style := [];
      Element.CurrProgram := FormatTimeRange(EpgInfo.StartTime, EpgInfo.EndTime, True);
      cv.TextRect(aRect, h + Spacing, aRect.top + scale96toscreen(25), Element.CurrProgram);
      cv.TextRect(aRect, h + spacing, aRect.top + scale96toscreen(37), EpgInfo.Title);

    end;
  end;
end;

procedure TfPlayer.ChannelListGetCellHint(Sender: TObject; ACol, ARow: integer; var HintText: string);
var
  Element: TM3UItem;
  epgInfo: REpgInfo;
begin
  Element := fFilteredList[arow];
  epgInfo := BackEnd.epgdata.GetEpgInfo(fFilteredList.Map(arow), now);

  HintText := Format('%3.3d: %s', [Element.Number, Element.title]) + sLineBreak +
    FormatTimeRange(EpgInfo.StartTime, EpgInfo.EndTime, True) + sLineBreak + EpgInfo.Title;

  HintText := HintText + sLineBreak + epgInfo.Plot;

end;

procedure TfPlayer.ChannelListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (key) = VK_RETURN then
    Play(fFilteredList.Map(ChannelList.Row));
  if key = VK_LEFT then
    pcLists.ActivePage := tsGroups;
end;

procedure TfPlayer.ChannelSplitterMoved(Sender: TObject);
begin
  GuiProperties.ChannelGridWidth := ScaleScreenTo96(ChannelSplitter.Left);
end;

procedure TfPlayer.ChannelTimerTimer(Sender: TObject);
begin
  if ChannelSelecting then
  begin
    if BackEnd.M3ULoader.ActiveList.UseChno then
      ChannelSelected := BackEnd.M3ULoader.ItemByChno(ChannelSelected)
    else
      ChannelSelected := ChannelSelected - 1;

    ChannelSelecting := False;
    Backend.OsdMessage('', False);
    Play(ChannelSelected);

  end;
  ChannelTimer.Enabled := False;
end;

procedure TfPlayer.LoadDailyEpg;
var
  StartTime, EndTime: TDateTime;
  i: integer;
begin
  StartTime := Trunc(now);
  EndTime := Trunc(now) + 1;
  ChannelInfo := BackEnd.EpgData.GetEpgInfo(BackEnd.CurrentIndex, StartTime, EndTime);
  if Length(ChannelInfo) > 0 then
  begin
    EPGList.RowCount := Length(ChannelInfo);
    EPGList.Visible := True;
  end
  else
    EPGList.Visible := False;

end;

procedure TfPlayer.EPGListDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  i, Spacing: integer;
  epgInfo: REpgInfo;
  cv: TCanvas;
  CurrProgram: string;
begin
  epgInfo := ChannelInfo[Arow];
  cv := EPGList.Canvas;
  if epgInfo.HaveData then
  begin
    cv.Font.Height := Scale96ToScreen(-12);
    cv.Font.Style := [];
    Spacing := Scale96ToScreen(5);
    CurrProgram := FormatTimeRange(EpgInfo.StartTime, EpgInfo.EndTime, True);
    //    IF (EpgInfo.StartTime <= Now) and (EpgInfo.EndTime > Now) then
    //      CurrProgram := 'LIVE '+CurrProgram;
    cv.TextRect(aRect, aRect.Left + Spacing, aRect.top + Spacing, CurrProgram);
    cv.TextRect(aRect, aRect.Left + spacing, aRect.top + Spacing + scale96toscreen(12), EpgInfo.Title);
  end;

end;

procedure TfPlayer.EPGListSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
var
  i, Spacing: integer;
  epgInfo: REpgInfo;
  cv: TCanvas;
  CurrProgram: string;
begin
  epgInfo := ChannelInfo[Arow];
  MemoEpg.Lines.Text := epgInfo.Plot;
end;

procedure TfPlayer.FormChangeBounds(Sender: TObject);
begin
  if SubFormVisible then
    pnlsubform.Height := min(600, pnlcontainer.Height - 100);
  GuiProperties.BoundsRect := BoundsRect;
end;

procedure TfPlayer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  hide;
  if Assigned(backend.mpvengine) then
  begin
    backend.mpvengine.isRenderActive := False;
    BackEnd.MpvEngine.Stop;
  end;

  Application.ProcessMessages;
  CloseAction := caFree;
end;

procedure TfPlayer.ChannelListDblClick(Sender: TObject);
begin
  Play(fFilteredList.Map(ChannelList.Row));
end;

procedure TfPlayer.AppPropertiesException(Sender: TObject; E: Exception);
begin
  try
    OvoLogger.Log(llERROR, 'EXCEPTION : %s' + LineEnding + '%s', [e.message, BackTraceStrFunc(ExceptAddr)]);

  except
    Halt(999);
    // avoid exception on exception
  end;
end;

procedure TfPlayer.AppPropertiesShowHint(var HintStr: string;
  var CanShow: boolean; var HintInfo: THintInfo);
var
  ACol, ARow: integer;
  Element: TM3UItem;
  epgInfo: REpgInfo;
  CellRect: TRect;
begin
  if HintInfo.HintControl = ChannelList then
  begin
    // Determine the cell under the mouse pointer
    ChannelList.MouseToCell(HintInfo.CursorPos.X, HintInfo.CursorPos.Y, ACol, ARow);
    Element := fFilteredList[arow];
    if (BackEnd.EpgData.ActiveList.EpgKind <> None) then
      epgInfo := BackEnd.epgdata.GetEpgInfo(fFilteredList.Map(arow), now)
    else
      epgInfo := Default(REpgInfo);

    if not epgInfo.HaveData then
      epgInfo.Channel := Format('%3.3d: %s', [Element.Number, Element.title]);

    ChannelHintForm.UpdateDetail(epgInfo);
    HintInfo.HintWindowClass := TChannelHint;
    CanShow := True;
    CellRect := ChannelList.ClientToScreen(ChannelList.CellRect(Acol, ARow));
    HintInfo.HintPos.X:= CellRect.Right+ GetSystemMetrics(SM_CXVSCROLL);
    HintInfo.HintPos.y:= CellRect.Top;
    HintStr := 'custom';
    HintInfo.HideTimeout:=7000;
  end
  else
  begin
    // ChannelHintForm.Hide;
  end;
end;


procedure TfPlayer.SelectGroup;
var
  Filter: TFilterParam;
begin
  Filter := Default(TFilterParam);
  if cbGroups.ItemIndex > 0 then
    Filter.Group := BackEnd.M3ULoader.Groups[cbGroups.ItemIndex - 1];
  fFilteredList := BackEnd.M3ULoader.Filter(Filter);
  ChannelList.RowCount := fFilteredList.Count;
  ChannelList.Invalidate;
  pcLists.ActivePage := tsChannels;
end;

procedure TfPlayer.cbGroupsDblClick(Sender: TObject);
begin
  SelectGroup;
end;

procedure TfPlayer.cbGroupsKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  Filter: TFilterParam;
begin
  if (key) = VK_RETURN then
    SelectGroup;

  if key = VK_LEFT then
    pcLists.ActivePage := tsList;
  if key = VK_RIGHT then
    pcLists.ActivePage := tsChannels;

end;

procedure TfPlayer.EmbedSubForm(AForm: TForm);
begin
  if GuiProperties.EmbeddedSubForm then
  begin
    if Assigned(SubForm) then
      CloseSubForm;
    AForm.BorderIcons := [];
    AForm.BorderStyle := bsNone;
    AForm.Parent := pnlSubForm;
    pnlSubForm.Height := min(600, pnlcontainer.Height - 100);
    AForm.Align := alclient;
  end;
  SubFormVisible := True;
  SubForm := AForm;
  AForm.Show;

end;

procedure TfPlayer.CloseSubForm;
begin
  SubForm.Hide;
  if GuiProperties.EmbeddedSubForm then
  begin
    pnlSubForm.Height := 0;
    BackEnd.MpvEngine.Refresh;
    SubForm.Parent := nil;
    HideMouse.Enabled := flgFullScreen and not pnlChannel.Visible and not SubFormVisible;
  end;

  SubForm.Close;
  SubFormVisible := False;
  SubForm := nil;

end;

procedure TfPlayer.actShowEpgExecute(Sender: TObject);
begin
  if not Assigned(EPGForm) then
    Application.CreateForm(TEPGForm, EPGForm);
  EpgForm.EpgData := BackEnd.epgData;
  EmbedSubForm(EPGForm);

end;

procedure TfPlayer.actListUpdate(AAction: TBasicAction; var Handled: boolean);
begin
  if not Assigned(GuiProperties) then
    exit;
  actViewLogo.Checked := GuiProperties.ViewLogo;
  actViewCurrentProgram.Checked := GuiProperties.ViewCurrentProgram;
end;

procedure TfPlayer.ConfigDone(Sender: TObject);
begin
  InitializeLists;
//  fConfig.Close;
  CloseSubForm();
end;

procedure TfPlayer.actShowConfigExecute(Sender: TObject);
begin
  if not Assigned(fConfig) then
    Application.CreateForm(TfConfig, fConfig);

  fConfig.Init;
  fConfig.OnWorkDone := ConfigDone;

  EmbedSubForm(fConfig);

end;

procedure TfPlayer.actShowListExecute(Sender: TObject);
begin
  if not Assigned(fChannels) then
    Application.CreateForm(TfChannels, fChannels);
  fChannels.Init;
  EmbedSubForm(fChannels);
end;

procedure TfPlayer.actShowLogExecute(Sender: TObject);
begin
  if not Assigned(fLogViewer) then
    Application.CreateForm(TfLogViewer, fLogViewer);
  fLogViewer.Init;
  EmbedSubForm(fLogViewer);
end;

procedure TfPlayer.actViewCurrentProgramExecute(Sender: TObject);
begin
  actViewCurrentProgram.Checked := not actViewCurrentProgram.Checked;
  GuiProperties.ViewCurrentProgram := actViewCurrentProgram.Checked;
  ComputeGridCellSize;
end;

procedure TfPlayer.actViewLogoExecute(Sender: TObject);
begin
  actViewLogo.Checked := not actViewLogo.Checked;
  GuiProperties.ViewLogo := actViewLogo.Checked;
  ComputeGridCellSize;
  //mcmcmcmcmc
  //  if actViewLogo.Checked then
  //    BackEnd.M3ULoader.UpdateLogo;
end;

procedure TfPlayer.InitializeGui(Data: ptrint);
begin
  GuiProperties := TGuiProperties.Create(ConfigObj, BoundsRect);
  ChannelSplitter.Left := Scale96ToScreen(GuiProperties.ChannelGridWidth);
  BoundsRect := GuiProperties.BoundsRect;
  ComputeGridCellSize;
  InitializeLists;
  pcLists.ActivePage := tsList;
  if pnlChannel.Visible then
    lvLists.SetFocus;
  if lvLists.Items.Count > 0 then
    lvLists.ItemIndex := 0;

end;

procedure TfPlayer.InitializeLists;
var
  item: TM3UList;
begin
  lvLists.Clear;
  for item in ConfigObj.ListManager do
    lvLists.AddItem(Item.Name, item);

end;

procedure TfPlayer.ComputeGridCellSize;
begin
  if Assigned(BackEnd.EpgData.ActiveList) and
    GuiProperties.ViewCurrentProgram and
    (BackEnd.EpgData.ActiveList.EpgKind <> None) then
    ChannelList.DefaultRowHeight := Scale96ToScreen(64)
  else
  if GuiProperties.ViewLogo then
    ChannelList.DefaultRowHeight := Scale96ToScreen(48)
  else
    ChannelList.DefaultRowHeight := Scale96ToScreen(32);
  ChannelList.Invalidate;

end;

procedure TfPlayer.DebugLnHook(Sender: TObject; S: string; var Handled: boolean);
begin
  if (TextRec(f).mode <> fmClosed) and (IOResult = 0) then
    WriteLn(f, S);

end;

procedure TfPlayer.Play(Row: integer);
begin
  BackEnd.Play(Row);
  ChannelList.Invalidate;
  Caption := format('%3.3d %s', [Row + 1, BackEnd.M3ULoader[Backend.CurrentIndex].title]);
  if pnlEpg.Visible then
  begin
    LoadDailyEpg;
    EPGList.Invalidate;
  end;
  Loading := True;

end;

procedure TfPlayer.GLRendererDblClick(Sender: TObject);
begin
  SetFullScreen;
end;

procedure TfPlayer.GLRendererMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if flgFullScreen then
  begin
    Screen.Cursor := crdefault;
    HideMouse.Enabled := flgFullScreen and not pnlChannel.Visible and not SubFormVisible;
  end;
end;

procedure TfPlayer.HideMouseTimer(Sender: TObject);
begin
  screen.cursor := crNone;
end;

procedure TfPlayer.pmPlayerClose(Sender: TObject);
begin
  if flgFullScreen then
    HideMouse.Enabled := True;
end;

procedure TfPlayer.pmPlayerPopup(Sender: TObject);
begin
  if mnuVideo.Count = 0 then
    mnuVideo.Enabled := False;
  if mnuAudio.Count = 0 then
    mnuAudio.Enabled := False;
  if mnuSub.Count = 0 then
    mnuSub.Enabled := False;
  if flgFullScreen then
    HideMouse.Enabled := False;
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
      A := (progress - 720) * 16 + 10;
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

procedure TfPlayer.ToolButton5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  p: Tpoint;
begin
  p := ToolButton5.ClientToScreen(Point(x, y));
  pmuView.PopUp(p.x, p.y);
end;

procedure TfPlayer.SetLoading(AValue: boolean);
begin

  FLoading := AValue;
  LoadingTimer.Enabled := FLoading;
  if not loading then
  begin
    GLRenderer.Visible := True;
    fLastMessage := '';
    backend.mpvengine.LoadTracks;
    LoadTracks;
  end;
end;

function TfPlayer.ComputeTrackTitle(Track: TTrack): string;
begin
  Result := '';
  if not trim(Track.Title).IsEmpty then
    Result := QuotedStr(Track.Title) + ' ';
  case track.Kind of
    trkVideo:
    begin
      Result := Result + '(' + Track.Codec + ' ';
      if (Track.w <> 0) or (Track.h <> 0) then
        Result := Result + format('%dx%d ', [Track.W, track.h]);
      if track.Fps <> 0 then
        Result := Result + format('%2.3ffps ', [Track.Fps]);
      Result := trim(Result) + ') ';
      if track.BitRate <> 0 then
        Result := Result + format('(%d kbps) ', [trunc(Track.BitRate / 1024)]);
    end;
    trkAudio:
    begin
      if not trim(Track.Title).IsEmpty then
        Result := QuotedStr(Track.Title) + ' ';
      if not trim(Track.Lang).IsEmpty then
        Result := Track.Lang + ' ';

      Result := Result + '(' + Track.Codec + ' ';

      if (Track.Channels <> 0) then
        Result := Result + format('%dch ', [Track.Channels]);
      if track.SampleRate <> 0 then
        Result := Result + format('%dHz ', [Track.SampleRate]);
      Result := trim(Result) + ') ';
      if track.BitRate <> 0 then
        Result := Result + format('(%d kbps) ', [trunc(Track.BitRate / 1024)]);
    end;
    trkSub:
    begin
      if not trim(Track.Title).IsEmpty then
        Result := QuotedStr(Track.Title) + ' ';
      if not trim(Track.Lang).IsEmpty then
        Result := Track.Lang + ' ';

      Result := Result + '(' + Track.Codec + ')';

    end;

  end;
  Result := trim(Result);
end;

procedure TfPlayer.LoadTracks;
var
  Track: TTrack;
  mnu: TMenuItem;
  i: integer;
begin
  OvoLogger.Log(llDEBUG, 'Loading tracks');
  mnuAudio.Clear;
  mnuVideo.Clear;
  mnuAudio.Clear;
  for i := 0 to Length(backend.mpvengine.TrackList) - 1 do
  begin
    Track := backend.mpvengine.TrackList[i];
    if track.Id <> 0 then
      case Track.Kind of
        trkVideo:
        begin
          mnu := tmenuitem.Create(mnuVideo);
          mnu.RadioItem := True;
          mnu.Checked := Track.Selected;
          mnu.Caption := ComputeTrackTitle(track);
          mnu.Tag := i;
          mnu.GroupIndex := 2;
          mnu.OnClick := mnuTrackClick;
          mnuVideo.Add(mnu);
        end;
        trkAudio:
        begin
          mnu := tmenuitem.Create(mnuAudio);
          mnu.RadioItem := True;
          mnu.Checked := Track.Selected;
          mnu.Caption := ComputeTrackTitle(track);
          mnu.Tag := i;
          mnu.GroupIndex := 1;
          mnu.OnClick := mnuTrackClick;
          mnuAudio.Add(mnu);
        end;
        trkSub:
        begin
          mnu := tmenuitem.Create(mnuSub);
          mnu.RadioItem := True;
          mnu.Checked := Track.Selected;
          mnu.Caption := ComputeTrackTitle(track);
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
  backend.mpvengine.SetTrack(mnu.Tag);
  mnu.Checked := True;

end;


procedure TfPlayer.SetFullScreen;
const
  ShowCommands: array[TWindowState] of integer =
    (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED, SW_SHOWFULLSCREEN);
begin
  flgFullScreen := not flgFullScreen;
  if flgFullScreen then
  try
    OvoLogger.Log(llDEBUG, 'Going fullscreen');
    backend.mpvengine.isRenderActive := False;
    Application.ProcessMessages;
    pnlChannel.Visible := False;
    ChannelSplitter.Visible := False;
    RestoredBorderStyle := BorderStyle;
    RestoredWindowState := WindowState;
    {$IFDEF WINDOWS}
    // On windows this is required to go fullscreen
    // but there is a bug in LCL and I get only a black screen!!
    // BorderStyle := bsNone;
    {$ENDIF}
    //      WindowState := wsFullScreen;
    ShowWindow(Handle, SW_SHOWFULLSCREEN);
    HideMouse.Enabled := True;
    GLRenderer.SetFocus;
  finally
    backend.mpvengine.isRenderActive := True;
  end
  else
  begin
    OvoLogger.Log(llDEBUG, 'Going windowed');
    backend.mpvengine.isRenderActive := False;
    Application.ProcessMessages;
    pnlChannel.Visible := True;
    ChannelSplitter.Visible := True;
    ShowWindow(Handle, ShowCommands[RestoredWindowState]);
    BorderStyle := RestoredBorderStyle;
    screen.cursor := crdefauLt;
    HideMouse.Enabled := False;
    backend.mpvengine.isRenderActive := True;
  end;

end;

end.
