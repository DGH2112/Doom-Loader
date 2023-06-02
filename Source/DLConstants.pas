(**
  
  This module contains constants for use in the application.

  @Author  David Hoyle
  @Version 1.139
  @Date    02 Jun 2023
  
  @license

    DOOM Loader is a simple application to allow you to select different DOOM
    game engines, IWADs and PWADs in one single place.
    
    Copyright (C) 2023  David Hoyle (https://github.com/DGH2112/Doom-Loader/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

**)
Unit DLConstants;

Interface

Uses
  DLTypes;

ResourceString
  (** A resource string for option to pause the media player on launch. **)
  strPauseMediaPlayer = 'Pause system media player music before launching';
  (** A resource string for option to start the media player after finishing. **)
  strStartMediaPlayer = 'Start system media player music after finishing';

Const
  (** A constant record of boolean option descriptions. **)
  astrOptionDescription : Array[TDLOption] Of String = (
    strPauseMediaPlayer,
    strStartMediaPlayer
  );

Implementation

End.
