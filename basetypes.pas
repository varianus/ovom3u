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
unit BaseTypes;

interface
type
  TEngineState = (
    ENGINE_STOP,
    ENGINE_PLAY,
    ENGINE_PAUSE,
    ENGINE_IDLE);

  TEngineCommand = (ecEvent,ecPaint);

  { REpgInfo }

  REpgInfo = record
    Channel: string;
    Title: string;
    Plot: string;
    StartTime: TDateTime;
    EndTime: TDateTime;
    HaveData:boolean;
  end;

  AREpgInfo = array of REpgInfo;

implementation


end.

