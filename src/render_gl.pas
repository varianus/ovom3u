
{$mode objfpc}
unit render_gl;
interface

uses
  ctypes, libmpv;

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

  _drmModeAtomicReq  = record end;
P_drmModeAtomicReq  = ^_drmModeAtomicReq;



{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  Pmpv_opengl_init_params = ^mpv_opengl_init_params;
  mpv_opengl_init_params = record
      get_proc_address : function (ctx:pointer; name:pchar):pointer;cdecl;
      get_proc_address_ctx : pointer;
      extra_exts : pcchar;
    end;





  Pmpv_opengl_fbo = ^mpv_opengl_fbo;
  mpv_opengl_fbo = record
      fbo : cint;
      w : cint;
      h : cint;
      internal_format : cint;
    end;


  Pmpv_opengl_drm_params = ^mpv_opengl_drm_params;
  mpv_opengl_drm_params = record
      fd : cint;
      crtc_id : cint;
      connector_id : cint;
      atomic_request_ptr : ^P_drmModeAtomicReq;
      render_fd : cint;
    end;



  Pmpv_opengl_drm_draw_surface_size = ^mpv_opengl_drm_draw_surface_size;
  mpv_opengl_drm_draw_surface_size = record
      width : cint;
      height : cint;
    end;

  Pmpv_opengl_drm_params_v2 = ^mpv_opengl_drm_params_v2;
  mpv_opengl_drm_params_v2 = record
      fd : cint;
      crtc_id : cint;
      connector_id : cint;
      atomic_request_ptr : ^P_drmModeAtomicReq;
      render_fd : cint;
    end;

  mpv_opengl_drm_osd_size = mpv_opengl_drm_draw_surface_size;

implementation

end.
