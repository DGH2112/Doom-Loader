(**
  
  This module contains simple types for use in the application.

  @Author  David Hoyle
  @Version 1.166
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
Unit DLTypes;

Interface

Type
  (** An enumerate to define the boolean options for the application. **)
  TDLOption = (dloPauseMedia, dloStartMedia);

  (** A set of the above options. **)
  TDLOptions = Set Of TDLOption;

  (** This enumerate defines how the Extra Options are associated to the Games files or Engines. **)
  TDLExtraOpsAssociation = (doaGameEngine, doaIWAD, doaPWAD);

  (** A record to encapsulate the above options (and more when they are added). **)
  TDLOptionsRecord = Record
    FOptions  : TDLOptions;
    FExtraOps : TDLExtraOpsAssociation;
  End;

Implementation

End.
