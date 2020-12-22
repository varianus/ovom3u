
{$mode objfpc}
unit render;
interface

uses
  ctypes, libmpv;

Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}
  mpv_render_context = record end;
  Pmpv_render_context = ^mpv_render_context;

  Pmpv_render_param_type = ^mpv_render_param_type;
  mpv_render_param_type =  Longint;
  Const
    MPV_RENDER_PARAM_INVALID = 0;
    MPV_RENDER_PARAM_API_TYPE = 1;
    MPV_RENDER_PARAM_OPENGL_INIT_PARAMS = 2;
    MPV_RENDER_PARAM_OPENGL_FBO = 3;
    MPV_RENDER_PARAM_FLIP_Y = 4;
    MPV_RENDER_PARAM_DEPTH = 5;
    MPV_RENDER_PARAM_ICC_PROFILE = 6;
    MPV_RENDER_PARAM_AMBIENT_LIGHT = 7;
    MPV_RENDER_PARAM_X11_DISPLAY = 8;
    MPV_RENDER_PARAM_WL_DISPLAY = 9;
    MPV_RENDER_PARAM_ADVANCED_CONTROL = 10;
    MPV_RENDER_PARAM_NEXT_FRAME_INFO = 11;
    MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME = 12;
    MPV_RENDER_PARAM_SKIP_RENDERING = 13;
    MPV_RENDER_PARAM_DRM_DISPLAY = 14;
    MPV_RENDER_PARAM_DRM_DRAW_SURFACE_SIZE = 15;
    MPV_RENDER_PARAM_DRM_DISPLAY_V2 = 16;
    MPV_RENDER_PARAM_SW_SIZE = 17;
    MPV_RENDER_PARAM_SW_FORMAT = 18;
    MPV_RENDER_PARAM_SW_STRIDE = 19;
    MPV_RENDER_PARAM_SW_POINTER = 20;

  MPV_RENDER_PARAM_DRM_OSD_SIZE = MPV_RENDER_PARAM_DRM_DRAW_SURFACE_SIZE;  

type
  Pmpv_render_param = ^mpv_render_param;
  mpv_render_param = record
      _type : mpv_render_param_type;
      data : pointer;
    end;


const
  MPV_RENDER_API_TYPE_OPENGL = 'opengl';  
  MPV_RENDER_API_TYPE_SW = 'sw';  

type
  Pmpv_render_frame_info_flag = ^mpv_render_frame_info_flag;
  mpv_render_frame_info_flag =  Longint;
  Const
    MPV_RENDER_FRAME_INFO_PRESENT = 1 shl 0;
    MPV_RENDER_FRAME_INFO_REDRAW = 1 shl 1;
    MPV_RENDER_FRAME_INFO_REPEAT = 1 shl 2;
    MPV_RENDER_FRAME_INFO_BLOCK_VSYNC = 1 shl 3;
  Const
    MPV_RENDER_UPDATE_FRAME = 1 shl 0;

type
  Pmpv_render_frame_info = ^mpv_render_frame_info;
  mpv_render_frame_info = record
      flags : uint64;
      target_time : int64;
    end;

  mpv_render_update_fn = procedure (cb_ctx:pointer);cdecl;
  Pmpv_render_update_flag = ^mpv_render_update_flag;
  mpv_render_update_flag =  Longint;

  mpv_render_context_flag = mpv_render_update_flag;
  Pmpv_render_context_flag = ^mpv_render_context_flag;


var
mpv_render_context_create : function(var res:Pmpv_render_context; var mpv:mpv_handle; params:Pmpv_render_param):cint;cdecl;

mpv_render_context_set_parameter : function(var ctx:mpv_render_context; param:mpv_render_param):cint;cdecl;

mpv_render_context_get_info : function(var ctx:mpv_render_context; param:mpv_render_param):cint;cdecl;

mpv_render_context_set_update_callback : procedure(var ctx:mpv_render_context; callback:mpv_render_update_fn; callback_ctx:pointer);cdecl;

mpv_render_context_update : function(var ctx:mpv_render_context):uint64;cdecl;


var
mpv_render_context_render : function(var ctx:mpv_render_context; params:Pmpv_render_param):cint;cdecl;

mpv_render_context_report_swap : procedure(var ctx:mpv_render_context);cdecl;

mpv_render_context_free : procedure(var ctx:mpv_render_context);cdecl;

procedure Freerender;
procedure Loadrender(lib : pchar);

implementation

  uses
    SysUtils, dynlibs;

  var
    hlib : tlibhandle;


  procedure Freerender;
    begin
      FreeLibrary(hlib);
      mpv_render_context_create:=nil;
      mpv_render_context_set_parameter:=nil;
      mpv_render_context_get_info:=nil;
      mpv_render_context_set_update_callback:=nil;
      mpv_render_context_update:=nil;
      mpv_render_context_render:=nil;
      mpv_render_context_report_swap:=nil;
      mpv_render_context_free:=nil;
    end;


  procedure Loadrender(lib : pchar);
    begin

      hlib:=LoadLibrary(lib);
      if hlib=0 then
        raise Exception.Create(format('Could not load library: %s',[lib]));

      pointer(mpv_render_context_create):=GetProcAddress(hlib,'mpv_render_context_create');
      pointer(mpv_render_context_set_parameter):=GetProcAddress(hlib,'mpv_render_context_set_parameter');
      pointer(mpv_render_context_get_info):=GetProcAddress(hlib,'mpv_render_context_get_info');
      pointer(mpv_render_context_set_update_callback):=GetProcAddress(hlib,'mpv_render_context_set_update_callback');
      pointer(mpv_render_context_update):=GetProcAddress(hlib,'mpv_render_context_update');
      pointer(mpv_render_context_render):=GetProcAddress(hlib,'mpv_render_context_render');
      pointer(mpv_render_context_report_swap):=GetProcAddress(hlib,'mpv_render_context_report_swap');
      pointer(mpv_render_context_free):=GetProcAddress(hlib,'mpv_render_context_free');
    end;


end.
