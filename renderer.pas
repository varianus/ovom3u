{
This file is part of OvoPlayer
Copyright (C) 2011 Marco Caselli

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

unit Renderer;

interface

uses
  Classes, SysUtils, BaseTypes, libmpv, OpenGLContext, render, render_gl, gl, glext;

type

  { TRenderThread }
  TRender = class;

  TRenderThread = class(TThread)
  private
    fControl:TOpenGlControl;
    Context: pmpv_render_context;
    fHandle: Pmpv_handle;
    Params: array of mpv_render_param;
    glParams: mpv_opengl_init_params;
    RenderParams: array of mpv_render_param;
    procedure InitRendering;
  public
    fOwner:     TRender;
    WaitEvent: PRtlEvent;
    constructor Create;
    procedure Init(Owner: TRender; AControl: TOpenGlControl; AHandle: Pmpv_handle);
    procedure Execute; override;
    destructor Destroy; override;
  end;

  TRender = class
  private
    RenderThread: TRenderThread;
  public
    constructor Create(AControl:TOpenGlControl; AHandle: pmpv_handle);
    destructor Destroy; override;
    procedure Render;
  end;

implementation
const
  Flip: longint = 1;
  Skip: longint = 0;

  function get_proc_address(ctx: pointer; Name: PChar): pointer; cdecl;
  begin
    Result := GetProcAddress(LibGL, Name);

    if Result = nil then
      Result := wglGetProcAddress(Name);

  end;

{ TRenderThread }

procedure Update_gl(cb_ctx: pointer); cdecl;
begin
  if (cb_ctx = nil) then
    exit;
  TRender(cb_ctx).render;
end;

constructor TRenderThread.Create;

begin
  inherited Create(True);
end;

procedure TRenderThread.Init(Owner: TRender; AControl: TOpenGlControl; AHandle: Pmpv_handle);
begin
  fOwner := owner;
  fControl:= AControl;
  fHandle := AHandle;
  fControl.Visible := true;
end;

procedure TRenderThread.InitRendering;
var
  i: integer;
begin
  mpv_set_option_string(fHandle^,'vd-lavc-dr','no');
  Params := nil;
  SetLength(Params, 4);
  Params[0]._type := MPV_RENDER_PARAM_API_TYPE;
  Params[0].Data := PChar(MPV_RENDER_API_TYPE_OPENGL);
  Params[1]._type := MPV_RENDER_PARAM_OPENGL_INIT_PARAMS;
  glParams.get_proc_address := @get_proc_address;
  glParams.extra_exts := nil;
  Params[1].Data := @glParams;
  Params[2]._type := MPV_RENDER_PARAM_ADVANCED_CONTROL;
  Params[2].Data := @flip;
  Params[3]._type := MPV_RENDER_PARAM_INVALID;
  Params[3].Data := nil;
  fControl.MakeCurrent();
  i := mpv_render_context_create(Context, fHandle^, Pmpv_render_param(@Params[0]));
  if (i < 0) then
    raise Exception.Create('failed to initialize mpv GL context');
  SetLength(RenderParams, 3);
  RenderParams[0]._type := MPV_RENDER_PARAM_OPENGL_FBO;
  RenderParams[0].Data := nil;
  RenderParams[1]._type := MPV_RENDER_PARAM_FLIP_Y;
  RenderParams[1].Data := @Flip;
  RenderParams[2]._type := MPV_RENDER_PARAM_INVALID;
  RenderParams[3].Data := nil;
  WaitEvent := RTLEventCreate;
  fControl.MakeCurrent();
  mpv_render_context_set_update_callback(Context^, @update_gl, fOwner);
  mpv_render_context_update(Context^);
end;

procedure TRenderThread.Execute;
var
  mpfbo: mpv_opengl_fbo;
  res : UInt64;
begin
  InitRendering();
  while not Terminated do
    begin
    RtlEventWaitFor(WaitEvent);
    begin
      begin
        while (mpv_render_context_update(Context^) and MPV_RENDER_UPDATE_FRAME) <> 0 do
          begin
            fControl.MakeCurrent();
            mpfbo.fbo := 0;
            mpfbo.h := FControl.Height;
            mpfbo.w := FControl.Width;
            mpfbo.internal_format := 0;
            RenderParams[0].Data := @mpfbo;
            mpv_render_context_render(Context^, Pmpv_render_param(@RenderParams[0]));
            fControl.SwapBuffers();
            mpv_render_context_report_swap(Context^);

          end;
      end;

    end;
    RTLeventResetEvent(WaitEvent);
    end;
end;

destructor TRenderThread.Destroy;
begin
  mpv_render_context_set_update_callback(Context^, nil, nil);
  mpv_render_context_update(Context^);
  mpv_render_context_free(Context^);
  RTLeventdestroy(WaitEvent);
  inherited Destroy;
end;

{ TRender }

constructor TRender.Create(AControl: TOpenGlControl; AHandle: pmpv_handle);
begin
  RenderThread := TRenderThread.Create;
  RenderThread.FreeOnTerminate := true;
  RenderThread.Init(Self,AControl, AHandle);
  RenderThread.Start;

end;

destructor TRender.Destroy;
begin
  RenderThread.Terminate;
  RTLeventSetEvent(RenderThread.WaitEvent);
  inherited Destroy;
end;


procedure TRender.Render;
begin
  RTLeventSetEvent(RenderThread.WaitEvent);
end;

end.
