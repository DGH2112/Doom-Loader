(**
  
  This module contains constants for use in the application.

  @Author  David Hoyle
  @Version 1.220
  @Date    03 Jun 2023
  
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
  (** A resource string to describe the extra options being associated with the Game Engines. **)
  strExtraOpsGameEngines = 'Extra Options are associated with the Game Engines';
  (** A resource string to describe the extra options being associated with the IWAD. **)
  strExtraOpsIWADs = 'Extra Options are associated with the IWAD';
  (** A resource string to describe the extra options being associated with the PWAD. **)
  strExtraOpsPWADs = 'Extra Options are associated with the PWAD';

Const
  (** A constant record of boolean option descriptions. **)
  astrOptionDescription : Array[TDLOption] Of String = (
    strPauseMediaPlayer,
    strStartMediaPlayer
  );

  (** A constant record to describe the extra options associations. **)
  astrExtraOpsDescriptions : Array[TDLExtraOpsAssociation] Of String = (
    strExtraOpsGameEngines,
    strExtraOpsIWADs,
    strExtraOpsPWADs
  );

Implementation

End.
