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
unit MPV_Engine;

interface

uses
  Classes, SysUtils, libmpv, decoupler, BaseTypes, render, render_gl,
  OpenGLContext, forms;

type
  TTrackType = (tkAudio, tkVideo, tkSub, tkUnkown);

  TTrack = record
    Kind: TTrackType;
    Selected: boolean;
    Id: int64;
    Title: string;
    Lang: string;
    Channels: integer;
    Codec: string;
    w, h: integer;
    Bitrate: integer;
  end;

  TTrackList = array of TTrack;
  { TMPVEngine }

  TMPVEngine = class
  private
    FGLRenderControl: TOpenGlControl;
    fHandle: Pmpv_handle;
    fdecoupler: TDecoupler;
    fisGlEnabled: boolean;
    fIsRenderActive: boolean;
    Context: pmpv_render_context;
    FOnLoadingState: TNotifyEvent;
    fTrackList: TTrackList;
    EngineState: TEngineState;
    Loading: boolean;

    function GetBoolProperty(const PropertyName: string): boolean;
    function GetMainVolume: integer;
    procedure GLRenderControlPaint(Sender: TObject);
    procedure InitRenderer(Data: PtrInt);
    procedure LoadTracks;
    procedure PostCommand(Command: TEngineCommand; Param: integer);
    procedure ReceivedCommand(Sender: TObject; Command: TEngineCommand;
      Param: integer);
    procedure SetBoolProperty(const PropertyName: string; AValue: boolean);
    procedure SetGLRenderControl(AValue: TOpenGlControl);
    procedure SetMainVolume(const AValue: integer);
    procedure SetOnLoadingState(AValue: TNotifyEvent);
  public
    Property GLRenderControl: TOpenGlControl read FGLRenderControl write SetGLRenderControl;
    property isRenderActive: boolean read fIsRenderActive write  fIsRenderActive;
    property isGlEnabled: boolean read fisGlEnabled write  fisGlEnabled;
    property OnLoadingState: TNotifyEvent read FOnLoadingState write SetOnLoadingState;
    Property TrackList: TTrackList read fTrackList;
    Property Volume: integer read GetMainVolume write SetMainVolume;
    function Initialize(Renderer: TOpenGLControl): boolean;
    function IsIdle: boolean;
    Procedure SetTrack(TrackType: TTrackType; Id: integer); overload;
    procedure SetTrack(Index: integer); overload;
    procedure OsdMessage(msg: string='');
    procedure Play(mrl: string);
    procedure Stop;
    procedure Pause;
    Constructor Create;
    Destructor Destroy; override;
    procedure Test;


  end;

implementation
uses
  gl, GLext
{$ifdef LINUX}
 ,ctypes
{$endif}
;

{$ifdef LINUX}
function setlocale(category: cint; locale: PChar): PChar; cdecl; external 'c' Name 'setlocale';
{$endif}

{$IFDEF Windows}
{ Declared in Windows unit as well in FPC; but declared here as well, to be
fully compatible to upstream version  - sg }
function wglGetProcAddress(proc: PChar): Pointer; cdecl; external 'OpenGL32.dll';
{$ELSE}
function wglGetProcAddress(proc: PChar): Pointer;
begin
  Result := GetProcAddress(LibGL, proc);
end;

{$ENDIF}

// Used by libmpv to load OpenGL functions
function get_proc_address(ctx: pointer; Name: PChar): pointer; cdecl;
begin
  Result := GetProcAddress(LibGL, Name);

  if Result = nil then
    Result := wglGetProcAddress(Name);

end;

// Callbacks
procedure LibMPVEvent(Data: Pointer); cdecl;
begin
  if (Data = nil) then
    exit;
  TMPVEngine(Data).PostCommand(ecEvent, 1);
end;


procedure Update_gl(cb_ctx: pointer); cdecl;
begin
  if (cb_ctx = nil) then
    exit;

  TMPVEngine(cb_ctx).PostCommand(ecPaint, 1);
end;

{ TMPVEngine }

procedure TMPVEngine.SetGLRenderControl(AValue: TOpenGlControl);
begin
  if FGLRenderControl=AValue then Exit;
  FGLRenderControl:=AValue;
end;

procedure TMPVEngine.SetOnLoadingState(AValue: TNotifyEvent);
begin
  FOnLoadingState := AValue;
end;

function TMPVEngine.Initialize(Renderer: TOpenGLControl): boolean;
begin
  Result:= true;
  try
    fhandle := mpv_create();
    mpv_set_option_string(fHandle^, 'hwdec', 'auto');
    mpv_initialize(fHandle^);
    //  mpv_request_log_messages(fhandle^, 'v');
    fdecoupler := TDecoupler.Create;
    fdecoupler.OnCommand := ReceivedCommand;
    mpv_set_wakeup_callback(fhandle^, @LibMPVEvent, self);

    GLRenderControl := Renderer;


    {$ifdef LINUX}
    GLRenderControl.OnPaint := GLRenderControlPaint;
    Application.QueueAsyncCall(InitRenderer, 0);
    {$endif}
    {$ifdef WINDOWS}
    wid := GLRenderControl.Handle;
    mpv_set_option(fHandle^, 'wid', MPV_FORMAT_INT64, @wid);
    {$endif}

  Except
    Result := false;
  end;
end;

constructor TMPVEngine.Create;
begin
  {$ifdef LINUX}
  setlocale(1, 'C');
  {$endif}
  Load_libmpv(libmpv.External_library);
  Loadrender(libmpv.External_library);
  EngineState:= ENGINE_IDLE;
  fHandle:= nil;
end;

destructor TMPVEngine.Destroy;
begin
  if Assigned(fHandle) then
    begin
      mpv_render_context_free(Context^);
      mpv_set_wakeup_callback(fhandle^, nil, self);
      mpv_terminate_destroy(fhandle^);
    end;

  Free_libmpv;
  inherited Destroy;
end;

// initialize OpenGL rendering
procedure TMPVEngine.InitRenderer(Data: PtrInt);
var
  Params: array of mpv_render_param;
  glParams: mpv_opengl_init_params;
  i: integer;
begin

  glext_LoadExtension('GL_version_1_3');
  glext_LoadExtension('GL_version_2_0');
  Load_GL_version_1_3();
  Load_GL_VERSION_2_1();
  GLRenderControl.MakeCurrent();
  Params := nil;
  SetLength(Params, 3);
  Params[0]._type := MPV_RENDER_PARAM_API_TYPE;
  Params[0].Data := PChar(MPV_RENDER_API_TYPE_OPENGL);
  Params[1]._type := MPV_RENDER_PARAM_OPENGL_INIT_PARAMS;
  glParams.get_proc_address := @get_proc_address;
  glParams.extra_exts := nil;
  Params[1].Data := @glParams;
  Params[2]._type := MPV_RENDER_PARAM_INVALID;
  Params[2].Data := nil;
  Params[3]._type := MPV_RENDER_PARAM_INVALID;
  Params[3].Data := nil;
  i := mpv_render_context_create(Context, fHandle^, Pmpv_render_param(@Params[0]));
  if (i < 0) then
    raise Exception.Create('failed to initialize mpv GL context');
  mpv_render_context_set_update_callback(Context^, @update_gl, self);
  isRenderActive := True;
end;

// Handle player messages to avoid threading issues
procedure TMPVEngine.PostCommand(Command: TEngineCommand; Param: integer);
begin
  fdecoupler.SendCommand(Command, Param);
end;

procedure TMPVEngine.ReceivedCommand(Sender: TObject; Command: TEngineCommand; Param: integer);
var
  Event: Pmpv_event;
  p: cint;
begin
  if (Command = ecEvent) and (param = 1) then
  begin
    Event := mpv_wait_event(fhandle^, 0);
    while Event^.event_id <> MPV_EVENT_NONE do
    begin
      case (Event^.event_id) of
        MPV_EVENT_LOG_MESSAGE:
          WriteLn(Pmpv_event_log_message(Event^.Data)^.Text);
        MPV_EVENT_PLAYBACK_RESTART:
          begin
            Loading:= false;
            LoadTracks;
            if Assigned(FOnLoadingState) then
              FOnLoadingState(self);
          end;
        MPV_EVENT_PROPERTY_CHANGE:
          begin
            if Pmpv_event_property(Event^.Data)^.Name = 'core-idle' then
            begin
              if Loading then
              begin
                mpv_get_property(fhandle^,'core-idle',MPV_FORMAT_FLAG, @p);
                Loading := P = 1;
                if not loading then
                  begin
                   if Assigned(FOnLoadingState) then
                     FOnLoadingState(self);
                    LoadTracks;
                  end;
              end;
            end;
          end;
      end;
      Event := mpv_wait_event(fhandle^, 0);
    end;
  end;
  if (Command = ecPaint) and (param = 1) then
  begin
    isGlEnabled := True;
    GLRenderControl.Repaint;
  end;
end;
procedure TMPVEngine.Play(mrl: string);
var
  Args: array of PChar;
  res: longint;
begin

  args := nil;
  setlength(args, 4);
  args[0] := 'loadfile';
  args[1] := PChar(mrl);
  args[2] := 'replace';
  args[3] := nil;
  res     := mpv_command(fhandle^, ppchar(@args[0]));
  Loading := true;

end;

procedure TMPVEngine.LoadTracks;
var
  Node: mpv_node;
  Map: mpv_node;
  Detail: mpv_Node;
  i, j: integer;
  pc: ppchar;
  Value, Value2: string;
  Track: TTrack;
begin
  SetLength(fTrackList, 0);
  try
    mpv_get_property(fhandle^, 'track-list', MPV_FORMAT_NODE, @Node);
    SetLength(fTrackList, Node.u.list_^.num);
    for i := 0 to Node.u.list_^.num - 1 do
    begin
      map := pmpv_node(PtrUInt(Node.u.list_^.values) + i * 16)^;
      pc  := map.u.list_^.keys;
      for j := 0 to map.u.list_^.num - 1 do
      begin
        Detail := Pmpv_node(PtrUInt(map.u.list_^.values) + j * 16)^;
        Value  := strpas(pc^);
        if Value = 'id' then
          fTrackList[i].Id := Detail.u.int64_;
        if Value = 'title' then
          fTrackList[i].Title := Detail.u.string_;
        if Value = 'type' then
        begin
          Value2 := Detail.u.string_;
          if Value2 = 'audio' then
            fTrackList[i].kind := tkAudio
          else if Value2 = 'video' then
            fTrackList[i].kind := tkVideo
          else if Value2 = 'sub' then
            fTrackList[i].kind := tkSub
          else
            fTrackList[i].kind := tkUnkown;
        end;
        if Value = 'lang' then
          fTrackList[i].Lang := Detail.u.string_;
        if Value = 'demux-w' then
          fTrackList[i].w := Detail.u.int64_;
        if Value = 'demux-h' then
          fTrackList[i].h := Detail.u.int64_;
        if Value = 'demux-channel-count' then
          fTrackList[i].Channels := Detail.u.int64_;
        if Value = 'demux-bitrate' then
          fTrackList[i].Bitrate := Detail.u.int64_;
        if Value = 'selected' then
          fTrackList[i].Selected := Detail.u.flag_ = 1;
        Inc(pc);
      end;
    end;
    mpv_free_node_contents(node);
  except
  end;

end;

procedure TMPVEngine.GLRenderControlPaint(Sender: TObject);
var
  mpfbo: mpv_opengl_fbo;
  Flip, Skip: longint;
var
  Params: array of mpv_render_param;
  a, b: integer;
  Angle: double;

begin
  if not isGlEnabled then
    exit;

  Params := nil;
  if not isRenderActive then
  begin
    SetLength(Params, 2);
    Params[0]._type := MPV_RENDER_PARAM_SKIP_RENDERING;
    Skip := 1;
    Params[0].Data := @Skip;
    Params[1]._type := MPV_RENDER_PARAM_INVALID;
    Params[1].Data := nil;
    GLRenderControl.MakeCurrent();
    mpv_render_context_render(Context^, Pmpv_render_param(@Params[0]));
    GLRenderControl.SwapBuffers();
  end
  else
  begin
    SetLength(Params, 4);
    Params[0]._type := MPV_RENDER_PARAM_OPENGL_FBO;
    Params[0].Data := @mpfbo;
    Params[1]._type := MPV_RENDER_PARAM_SKIP_RENDERING;
    Skip := 0;
    Params[1].Data := @Skip;
    Params[2]._type := MPV_RENDER_PARAM_FLIP_Y;
    Flip := 1;
    Params[2].Data := @Flip;
    Params[3]._type := MPV_RENDER_PARAM_INVALID;
    Params[3].Data := nil;
    GLRenderControl.MakeCurrent();
    //glClearColor(0, 0, 0, 0);
    //glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    //glMatrixMode(GL_PROJECTION);
    //glLoadIdentity();
    //glMatrixMode(GL_MODELVIEW);
    //glLoadIdentity();
    mpfbo.fbo := 0;
    mpfbo.h   := GLRenderControl.Height;
    mpfbo.w   := GLRenderControl.Width;
    mpfbo.internal_format := 0;
    mpv_render_context_render(Context^, Pmpv_render_param(@Params[0]));
    GLRenderControl.SwapBuffers();
  end;

end;
function TMPVEngine.GetBoolProperty(const PropertyName:string):boolean;
var
  res: integer;
  p:integer;
begin
 res:=mpv_get_property(fhandle^,pchar(PropertyName),MPV_FORMAT_FLAG,@p);
 result := Boolean(p);
end;

procedure TMPVEngine.SetBoolProperty(const PropertyName:string; AValue: boolean);
var
  res: integer;
  p: Integer;
begin
   if AValue then
     p:= 1
   else
     p:=0;
 res:=mpv_set_property(fhandle^,pchar(PropertyName),MPV_FORMAT_FLAG,@p);
end;


function TMPVEngine.IsIdle: boolean;
begin
 Result := GetBoolProperty('core-idle');
 if Result then
   EngineState:= ENGINE_IDLE
 else
   EngineState:= ENGINE_PLAY;
end;

procedure TMPVEngine.SetTrack(TrackType: TTrackType; Id: integer);
var
  TrackTypeString: string;
begin
 Case TrackType of
   tkAudio : TrackTypeString := 'aid';
   tkVideo : TrackTypeString := 'vid';
   tkSub : TrackTypeString := 'sid';
 else
   exit;
 end;
 mpv_set_property(fHandle^, pchar(TrackTypeString), MPV_FORMAT_INT64, @id);
end;

procedure TMPVEngine.SetTrack(Index: integer);
var
  TrackTypeString: string;
begin
 SetTrack(TrackList[index].Kind, TrackList[index].Id);
end;

function TMPVEngine.GetMainVolume: integer;
var
  vol : Double;
  res:integer;
begin
  res :=mpv_get_property(fhandle^,'volume',MPV_FORMAT_DOUBLE,@vol);
  Result := trunc(vol);
end;

procedure TMPVEngine.SetMainVolume(const AValue: integer);
var                                         vol : Double;
  res:integer;
begin
  vol := AValue ;
  res := mpv_set_property(fhandle^,'volume',MPV_FORMAT_DOUBLE,@vol);

end;


procedure TMPVEngine.Stop;
var
  Args: array of pchar;
  res: longint;
begin
 setlength(args,2);
 args[0] := 'stop';
 args[1] := nil ;
 res:= mpv_command(fhandle^, ppchar(@args[0])) ;
 EngineState:= ENGINE_STOP;

end;

procedure TMPVEngine.OsdMessage(msg:string='');
begin
  mpv_set_property_string(fHandle^,'osd-msg1',pchar(msg));
end;

procedure TMPVEngine.Pause;
begin
 if (EngineState = ENGINE_PAUSE) then
   begin
       SetBoolProperty('pause', false);
       OsdMessage();
       EngineState:=ENGINE_PLAY;
   end
 else if  EngineState = ENGINE_PLAY then
   begin
     SetBoolProperty('pause', true);
     OsdMessage('Paused');
     EngineState:=ENGINE_Pause;
   end;

end;

procedure TMPVEngine.Test;
var s: int64;
begin
 mpv_get_property(fHandle^,'osd-font-size',MPV_FORMAT_INT64,@s);
 OsdMessage(inttostr(s));
end;

end.
