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
  // General options for code generation
  /// compiler and interface mode
  {$mode DELPHI}{$H+}
  {$interfaces corba}

  {$MACRO ON}
  {$if FPC_fullVersion > 30202}
  {$define GENERIC_CONST:=const}
  {$ELSEIF}
  {$define GENERIC_CONST:=constref}
  {$endif}


  /// Platform dependant code
  {$IFDEF UNIX}
  {$DEFINE NEED_C_LIBS} //threading and locales use C library
  {$ENDIF}
  {$IF defined(LCLGTK2) or defined(LCLGTK3)}
    {$IF defined(Linux) or defined(FreeBSD)}
      {$DEFINE NEED_XLIB} //Needed to avoid threading error on GTK under XLIB
    {$ENDIF}
  {$ENDIF}

  // Additional features (mainly platform dependant)
  {$IFDEF LINUX}
    {$DEFINE USE_MPRIS2} // allow integration with desktop sound applet, available on most Linux desktop manager
  {$ENDIF}

  {$DEFINE USE_LIBCEC} //add support for HDMI-CEC, allow to control player using TV remote, need hardware support (Raspberry PI has it)

  {$DEFINE USE_MMKEYS} //Add support for keyboard with multimedia keys

  ///if release is defined, try to be quiet and efficient...
  {$IFDEF RELEASE}
  {$HINTS OFF}
  {$INLINE ON}
  {$warn 6058 off}
  {$warn 5024 off}
  {$ENDIF RELEASE}


