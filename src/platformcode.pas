unit PlatformCode;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF LCLGTK2}
  , gdk2, gdk2x
  {$ENDIF}
  {$IFDEF LCLGTK3}
  , LazGdk3, LazGLib2
  {$ENDIF}
  {$IFDEF LCLQt5}
  , Qt5
  {$ENDIF}
  {$IFDEF LCLQt6}
  , Qt6
  {$ENDIF}
  ;

type
  TGraphicsPlatform = (gpUnknown, gpX11, gpXWayland, gpWaylandNative, gpWindows);

function GetCurrentGraphicsPlatform: TGraphicsPlatform;
function GetDisplay(Platform: TGraphicsPlatform): pointer;

implementation

{$IFDEF UNIX}
{$IFDEF LCLGTK2}
function GetCurrentGraphicsPlatform: TGraphicsPlatform;
begin
  Result := gpX11;
end;

function GetDisplay(Platform: TGraphicsPlatform): Pointer;
begin
  Result := gdk_x11_display_get_xdisplay(gdk_display_get_default());
end;

{$ENDIF}

{$IFDEF LCLGTK3}
function gdk_display_get_default: PGdkDisplay; cdecl; external 'libgdk-3.so.0';
function g_type_name(g_type: TGType): pchar; cdecl; external 'libgobject-2.0.so.0';
function gdk_x11_display_get_xdisplay(display:PGdkDisplay): pointer; external 'libgdk-3.so.0';

function GetCurrentGraphicsPlatform: TGraphicsPlatform;
var
  Display: PGdkDisplay;
  TypeName: string;
  XdgSession: string;
begin
  Result := gpUnknown;

  // 1. Retrieve the default display initialized by the GTK application
  try
    Display := gdk_display_get_default();
  except
    Display := nil;
  end;

  if Display <> nil then
  begin
    // 2. Extract the runtime GObject class name of the display instance
    // Display^.g_type_instance.g_class^.g_type identifies the active backend class at runtime
    TypeName := string(g_type_name(Display^.g_type_instance.g_class^.g_type));

    // 3. Determine if GDK is utilizing the Wayland or X11 backend natively
    if TypeName = 'GdkWaylandDisplay' then
    begin
      Exit(gpWaylandNative);
    end
    else if TypeName = 'GdkX11Display' then
    begin
      // If the backend is X11, the app might still be running inside Wayland via XWayland translation.
      // We cross-reference this by checking the environment session variables.
      XdgSession := LowerCase(GetEnvironmentVariable('XDG_SESSION_TYPE'));

      if (XdgSession = 'wayland') or (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '') then
        Result := gpXWayland
      else
        Result := gpX11;
    end;
  end;

  // Heuristic fallback if GDK does not respond (e.g., headless execution or early initialization phase)
  if Result = gpUnknown then
  begin
    XdgSession := LowerCase(GetEnvironmentVariable('XDG_SESSION_TYPE'));
    if XdgSession = 'wayland' then
      Result := gpXWayland // Conservative assumption if GDK falls back to X11 emulation
    else if XdgSession = 'x11' then
      Result := gpX11;
  end;
end;

function GetDisplay(Platform: TGraphicsPlatform): Pointer;
begin
  Result := gdk_x11_display_get_xdisplay(gdk_display_get_default());
end;

{$ENDIF}
{$IFDEF LCLQt5}
function GetCurrentGraphicsPlatform: TGraphicsPlatform;
var
  PlatformName: string;
  XdgSession: string;
  pcPlatformName: widestring;
begin
  Result := gpUnknown;
  try
    // 1. Query the active QPA (Qt Platform Abstraction) layer
    // This calls the internal C++ wrapper to return standard Qt strings like 'wayland' or 'xcb'
    QGuiApplication_platformName(@pcPlatformName);
    PlatformName := LowerCase(string(pcPlatformName));
  except
    PlatformName := '';
  end;
  // 2. Cross-reference the active QPA plugin with system environment variables
  if PlatformName = 'wayland' then
  begin
    Result :=
      gpWaylandNative;
  end
  else if PlatformName = 'xcb' then
  begin
    // If Qt loaded the 'xcb' (X11) plugin, it might still run inside Wayland using XWayland translation
    XdgSession :=
      LowerCase(GetEnvironmentVariable('XDG_SESSION_TYPE'));
    if (XdgSession = 'wayland') or (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '') then
      Result := gpXWayland
    else
      Result := gpX11;
  end;
  // 3. Heuristic fallback if called before Application.Initialize or on non-Qt builds  if Result = gpUnknown thenbeginXdgSession :=
  LowerCase(GetEnvironmentVariable('XDG_SESSION_TYPE'));
  if XdgSession = 'wayland' then Exit(gpXWayland)
  else if XdgSession = 'x11' then Exit(gpX11);
  Exit;
end;

function GetDisplay(Platform: TGraphicsPlatform): pointer;
begin
  Result := nil;
  try
    case Platform of
      gpX11: Result := QX11Info_display();
    end;
  except
    Result := nil;
  end;
end;
{$ENDIF}
{$ENDIF}
{$IFDEF WINDOWS}
function GetDisplay(Platform: TGraphicsPlatform): pointer;
begin
  Result := nil;
end;

function GetCurrentGraphicsPlatform: TGraphicsPlatform;
begin
  Result := gpWindows;
end;
{$ENDIF}

end.
