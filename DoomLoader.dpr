(**
  
  This module contains the project code for creating the application.

  @Author  David Hoyle
  @Version 1.107
  @Date    20 May 2023
  
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
Program DoomLoader;

{$R 'DoomLoaderITHVerInfo.res' 'DoomLoaderITHVerInfo.RC'}

uses
  Vcl.Forms,
  DLMainForm in 'Source\DLMainForm.pas' {frmDLMainForm};

{$R *.res}


Begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Doom Loader';
  Application.CreateForm(TfrmDLMainForm, frmDLMainForm);
  Application.Run;

End.
