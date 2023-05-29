(**
  
  This module contains a form for configuring and launching Doom and its various WADs.

  @Author  David Hoyle
  @Version 12.822
  @Date    29 May 2023

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
Unit DLMainForm;

Interface

Uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons;

Type
  (** A form to represent the applications main interface. **)
  TfrmDLMainForm = Class(TForm)
    gpnlBottomLayout: TGridPanel;
    lvGameEngines: TListView;
    lblGameEngines: TLabel;
    pnlGameEngines: TPanel;
    pnlGameEngineBtns: TPanel;
    lblIWADs: TLabel;
    pnlIWADs: TPanel;
    pnlPWADs: TPanel;
    lblPWADs: TLabel;
    pnlWADFolder: TPanel;
    lblWADFolder: TLabel;
    edtWADFolder: TEdit;
    pnlOptions: TPanel;
    btnAdd: TBitBtn;
    btnEdit: TBitBtn;
    btnDelete: TBitBtn;
    btnBrowse: TBitBtn;
    btnLaunch: TBitBtn;
    dlgOpen: TOpenDialog;
    Splitter: TSplitter;
    pnlWADs: TPanel;
    lblParams: TLabel;
    tvIWADs: TTreeView;
    tvPWADs: TTreeView;
    cbxExtraParams: TComboBox;
    pnlText: TPanel;
    lblWADText: TLabel;
    mmoWADText: TMemo;
    btnUp: TBitBtn;
    btnDown: TBitBtn;
    procedure btnAddClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure edtWADFolderChange(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    procedure lvGameEnginesEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure tvIWADsClick(Sender: TObject);
    procedure tvPWADsClick(Sender: TObject);
    procedure lvGameEnginesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  Strict Private
    FINIFileName        : String;
    FINIFile            : TMemINIFile;
    FGameEngines        : TStringList;
    FGameEngineNames    : TStringList;
    FSelectedGameEngine : String;
    FSelectedIWAD       : String;
    FSelectedPWAD       : String;
    FIWADExceptions     : TStringList;
    FAssociatedIWAD     : TStringList;
    FAssociatedOptions  : TStringList;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure PopulateGameEngines;
    Procedure PopulateWADs;
    Procedure UpdateLaunchBtn;
    Function  IsIWAD(Const strFileName : String) : Boolean;
    Procedure RecurseWADFolders(Const strFolder : String);
    Procedure AddWADFileToList(Const TreeView : TTreeView; Const strFileName, SelectedWAD : String);
    Procedure LoadVersionInfo();
    Function  TreePath(Const Node : TTreeNode) : String;
    Function  Find(Const TreeView : TTreeView; Const Node : TTreeNode;
      Const strFileNamePart : String) : TTreeNode;
    Procedure LoadWADText(Const strWADFileName  :String);
    Procedure UpdateUpAndDownBtns();
  Public
  End;

Var
  (** Form variable used by Delphi to create the applications main form. **)
  frmDLMainForm: TfrmDLMainForm;

Implementation

uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  System.IOUtils,
  System.StrUtils,
  System.UITypes,
  Winapi.ShellAPI,
  Vcl.FileCtrl;

Const
  (** A constant to define the INI Section for the positional date of the application. **)
  strPositionINISection = 'Position';
  (** A constant to define the top position of the main form. **)
  strTopINIKey = 'Top';
  (** A constant to define the left position of the main form. **)
  strLeftINIKey = 'Left';
  (** A constant to define the width of the main form. **)
  strWidthINIKey = 'Width';
  (** A constant to define the height of the main form. **)
  strHeightINIKey = 'Height';
  (** A constant to define the INI Section for the list of Game Engines. **)
  strGameEnginesINISection = 'Game Engines';
  (** A constant to define the INI Section for the list of Game Engine Names. **)
  strGameEngineNamesINISection = 'Game Engine Names';
  (** A constant to define the splitter height of the Game Engine section. **)
  strSplitterHeightINIKey = 'SplitterHeight';
  (** A constant to define the selected Game Engine. **)
  strSelectGameEngineINIKey = 'Select Game Engine';
  (** A constant to define the main INI section for the application settings. **)
  strSetupINISection = 'Setup';
  (** A constant to define the INI Key for the folder in which the WAD files are stored. **)
  strWADFolderINIKey = 'WAD Folder';
  (** A constant to define the INI Key for the selected IWAD file. **)
  strSelectedIWADINIKey = 'Selected IWAD';
  (** A constant to define the INI Key for the selected PWAD file. **)
  strSelectedPWADINIKey = 'Selected PWAD';
  (** A constant to define the INI Key for the extra command line parameters. **)
  strExtraParamsINIKey = 'Extra Params';
  (** A constant to define the INI section for the list of IWAD Exceptions, i.e. really PWAD. **)
  strIWADExceptions = 'IWAD Exceptions';
  (** A constant to define the INI section for the list of IWAD associated with a PWAD. **)
  strAsscoiatedIWADs = 'Associated IWADs';
  (** A constant to define the INI section for the list of Extra Options. **)
  strExtraOptionsINISection = 'Extra Options';
  (** A constant to define the INI section for the list of Associated Options. **)
  strAssociatedOptionsINISection = 'Associated Options';
  (** A resource string for no WAD file. **)
  strNone = '(none)';
  (** A constant to define the game engine name with INI Key. **)
  strGameEngineNameWidthINIKey = 'Game Engine Name Width';

{$R *.dfm}


(**

  This method adds the given WAD filename to the given treeview and optionally selects the item if it 
  matches the given selected item.

  @precon  Treeview must be a valid instance.
  @postcon The WWAD file is added to the treeview and optionally selected.

  @param   TreeView    as a TTreeView as a constant
  @param   strFileName as a String as a constant
  @param   SelectedWAD as a String as a constant

**)
Procedure TfrmDLMainForm.AddWADFileToList(Const TreeView : TTreeView; Const strFileName,
  SelectedWAD : String);

Var
  astrFileName : TArray<String>;
  iPath: Integer;
  Node : TTreeNode;
  ParentNode : TTreeNode;
  
Begin
  astrFileName := strFileName.Split(['\']);
  ParentNode := Nil;
  For iPath := 0 To Pred(High(astrFileName)) Do // Process path parts not filename
    Begin
      Node := Find(Treeview, ParentNode, astrFileName[iPath]);
      If Not Assigned(Node) Then
        ParentNode := TreeView.Items.AddChild(ParentNode, astrFileName[iPath])
      Else
        ParentNode := Node;
    End;
  Node := TreeView.Items.AddChild(ParentNode, astrFileName[High(astrFileName)]);
  Node.Selected := CompareText(SelectedWAD, TreePath(Node)) = 0;
End;

(**

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Displays a dialogue to the user to browse for executable files and if confirmed the EXE is
           added the list of Game Engines.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.btnAddClick(Sender: TObject);

Const
  strItem = 'Item%4.4d';

Begin
  If dlgOpen.Execute Then
    Begin
      FGameEngines.AddPair(Format(strItem, [FGameEngines.Count]), dlgOpen.FileName);
      FGameEngineNames.AddPair(Format(strItem, [FGameEngines.Count]), ExtractFileName(dlgOpen.FileName));
      FSelectedGameEngine := dlgOpen.FileName;
      PopulateGameEngines;
      UpdateLaunchBtn;
    End;
End;

(**

  This is an on click event handler for the Browse button.

  @precon  None.
  @postcon Allows the user to select a directory from which WAD files can be loaded.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.btnBrowseClick(Sender: TObject);

Var
  strFolder: String;

Begin
  strFolder := edtWADFolder.Text;
  If SelectDirectory(strFolder, [], 0) Then
    edtWADFolder.Text := strFolder;
End;

(**

  This is an on click event handler for the Delete button.

  @precon  None.
  @postcon Deletes the selected engine from the list.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.btnDeleteClick(Sender: TObject);

Begin
  If lvGameEngines.ItemIndex = - 1 Then
    Exit;
  FGameEngines.Delete(lvGameEngines.ItemIndex);
  FGameEngineNames.Delete(lvGameEngines.ItemIndex);
  PopulateGameEngines;
  UpdateLaunchBtn;
End;

(**

  This is an on click event handler for the Down button.

  @precon  None.
  @postcon Move the selected game engine down the list.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.btnDownClick(Sender: TObject);

Begin
  FGameEngines.Exchange(lvGameEngines.ItemIndex, lvGameEngines.ItemIndex + 1);
  FGameEngineNames.Exchange(lvGameEngines.ItemIndex, lvGameEngines.ItemIndex + 1);
  PopulateGameEngines;
End;

(**

  This is an on click event handler for the Edit button.

  @precon  None.
  @postcon This event handler allow the user to edit the select game engine.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.btnEditClick(Sender: TObject);

Begin
  If lvGameEngines.ItemIndex = -1 Then
    Exit;
  dlgOpen.InitialDir := ExtractFilePath(FGameEngines.ValueFromIndex[lvGameEngines.ItemIndex]);
  dlgOpen.FileName := ExtractFileName(FGameEngines.ValueFromIndex[lvGameEngines.ItemIndex]);
  If dlgOpen.Execute Then
    Begin
      FGameEngines.ValueFromIndex[lvGameEngines.ItemIndex] := dlgOpen.FileName;
      FSelectedGameEngine := dlgOpen.FileName;
      PopulateGameEngines;
      UpdateLaunchBtn;
    End;
End;

(**

  This is an on click event handler for the Launch button.

  @precon  None.
  @postcon Asks the shell to load the game engine with the selected WAD files.

  @nocheck EmptyWhile

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.btnLaunchClick(Sender: TObject);

ResourceString
  strExitCodeMsg = 'The process ''%s'' exited with error code %d';

Const
  strIWADCmd = ' -iwad "%s"';
  strPWADCmd = ' -file "%s"';
  iWaitInMilliSec = 100;

Var
  strParams :String;
  strFolder : String;
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
  iExitCode : Cardinal;
 
Begin
  // Save Associated IWAD
  If Assigned(tvPWADs.Selected) Then
    FAssociatedIWAD.Values[TreePath(tvPWADs.Selected)] := FSelectedIWAD;
  // Save Extra Options to the dropdown list
  If Length(cbxExtraParams.Text) > 0 Then
    Begin
      If cbxExtraParams.Items.IndexOf(cbxExtraParams.Text) = -1 Then
        cbxExtraParams.Items.Add(cbxExtraParams.Text);
      //: @note Might want to have this configurable, i.e. IWAD, PWAD or Game Engine.
      FAssociatedOptions.Values[FSelectedIWAD] := cbxExtraParams.Text;
    End;
  // Launch the game engine with IWAD and optional PWAD
  strParams := Format('"%s"', [FGameEngines.ValueFromIndex[lvGameEngines.ItemIndex]]);
  If CompareText(FSelectedIWAD, strNone) <> 0 Then
    Begin
      strParams := strParams + Format(strIWADCmd, [edtWADFolder.Text + '\' + FSelectedIWAD]);
      strParams := strParams + IfThen(
        (FSelectedPWAD.Length > 0) And (CompareText(FSelectedPWAD, strNone) <> 0),
        Format(strPWADCmd, [edtWADFolder.Text + '\' + FSelectedPWAD]),
        ''
      );
    End;
  strParams := strParams + IfThen(Length(cbxExtraParams.Text) > 0, #32 + cbxExtraParams.Text, '');
  strFolder := ExtractFilePath(FGameEngines.ValueFromIndex[lvGameEngines.ItemIndex]);
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_NORMAL;
  StartupInfo.hStdOutput  := 0;
  StartupInfo.hStdError   := 0;
  Win32Check(CreateProcess(Nil, PChar(strParams), Nil, Nil, False, 0, Nil,
    PChar(strFolder), StartupInfo, ProcessInfo));
  Try
    While WaitforSingleObject(ProcessInfo.hProcess, iWaitInMilliSec) = WAIT_TIMEOUT Do;
    If GetExitCodeProcess(ProcessInfo.hProcess, iExitCode) Then
      If iExitCode > 0 Then
        TaskMessageDlg(Application.Title, Format(strExitCodeMsg, [strParams, iExitCode]), mtError,
          [mbOK], 0);
  Finally
    Win32Check(CloseHandle(ProcessInfo.hThread));
    Win32Check(CloseHandle(ProcessInfo.hProcess));
  End;
End;

(**

  This is an on click event handler for the Up button.

  @precon  None.
  @postcon Moves the selected game engine up the list.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.btnUpClick(Sender: TObject);

Begin
  FGameEngines.Exchange(lvGameEngines.ItemIndex, lvGameEngines.ItemIndex - 1);
  FGameEngineNames.Exchange(lvGameEngines.ItemIndex, lvGameEngines.ItemIndex - 1);
  PopulateGameEngines;
End;

(**

  This is an on change event handler for the WAD edit control.

  @precon  None.
  @postcon Forces a [re-]population of the WAD file lists.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.edtWADFolderChange(Sender: TObject);

Begin
  PopulateWADs;
End;

(**

  This function attempts to find the tree node in the given treeview with the given parent node and the
  given file name part.

  @precon  Treeview and Node must be valid instances.
  @postcon Returns the node if found else returns nil.

  @note    Really wish I used VirtualTreeview here as this is a bad linear search. Not using VTVs to keep
           the dependencies to stock Delphi components. This can change if needed.

  @param   TreeView        as a TTreeView as a constant
  @param   Node            as a TTreeNode as a constant
  @param   strFileNamePart as a String as a constant
  @return  a TTreeNode

**)
Function TfrmDLMainForm.Find(Const TreeView : TTreeView; Const Node: TTreeNode; Const strFileNamePart: String): TTreeNode;

Var
  i : Integer;
  N : TTreeNode;
  
Begin
  Result := Nil;
  For i := 0 To TreeView.Items.Count - 1 Do
    Begin
      N := Treeview.Items[i];
      If (N.Parent = Node) And (CompareText(N.Text, strFileNamePart) = 0) Then
        Begin
          Result := N;
          Break;
        End;
    End;
End;

(**

  This is an On Form Create Event Handler for the TfrmDLMainForm class.

  @precon  None.
  @postcon Initialises the form and loads the settings.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.FormCreate(Sender: TObject);

Const
  strSeasonFallDoomLoaderIni = '\Season''s Fall\Doom Loader.ini';
  strHEXDDWAD = 'HEXDD.wad';

Begin
  FINIFileName := TPath.GetHomePath + strSeasonFallDoomLoaderIni;
  FINIFile := TMemINIFile.Create(FINIFileName);
  FGameEngines := TStringList.Create;
  FGameEngineNames := TStringList.Create;
  FIWADExceptions := TStringList.Create;
  FIWADExceptions.Sorted := True;
  FIWADExceptions.CaseSensitive := False;
  FIWADExceptions.Duplicates := dupIgnore;
  FIWADExceptions.Add(strHEXDDWAD);
  FAssociatedIWAD := TStringList.Create;
  FAssociatedOptions := TStringList.Create;
  LoadVersionInfo;
  LoadSettings;
  PopulateGameEngines;
  PopulateWADs;
  UpdateLaunchBtn;
  LoadWADText(edtWADFolder.Text + '\' + FSelectedPWAD);
End;

(**

  This is an On Form Destroy Event Handler for the TfrmDLMainForm class.

  @precon  None.
  @postcon Saves the applications settings and releases memory used by the application.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.FormDestroy(Sender: TObject);

Begin
  SaveSettings;
  FAssociatedOptions.Free;
  FAssociatedIWAD.Free;
  FIWADExceptions.Free;
  FGameEngineNames.Free;
  FGameEngines.Free;
  FINIFile.Free;
End;

(**

  This method check to see if the given file is an IWAD and if so returned true else returns false.

  @precon  None.
  @postcon Returns true if the file is an IWAD else returns false.

  @param   strFileName as a String as a constant
  @return  a Boolean

**)
Function TfrmDLMainForm.IsIWAD(Const strFileName: String): Boolean;

Const
  abIWAD : Array[0..3] Of Byte = (73, 87, 65, 68); 

Var
  boolIWAD: Boolean;
  FileStream : TFileStream;
  Buffer : Array[0..3] Of Byte;
  iBytes: Integer;
  i: Integer;
  iIndex: Integer;

Begin
  Result := False;
  FileStream := TFileStream.Create(strFileName, fmOpenRead);
  Try
    iBytes := FileStream.Read(Buffer, Length(Buffer));
    If iBytes = Length(Buffer) Then
      Begin
        If FIWADExceptions.Find(ExtractFileName(strFileName), iIndex) Then
          Exit;
        boolIWAD := True;
        For i := Low(Buffer) To High(Buffer) Do
          boolIWAD := boolIWAD And (Buffer[i] = abIWAD[i]);
        If boolIWAD Then
          Result := True;
      End;
  Finally
    FileStream.Free;
  End;
End;

(**

  This method loads the applications settings.

  @precon  None.
  @postcon The settings are loaded.

**)
Procedure TfrmDLMainForm.LoadSettings;

Var
  sl : TStringList;
  strItem: String;

Begin
  Top := FINIFile.ReadInteger(strPositionINISection, strTopINIKey, Top);
  Left := FINIFile.ReadInteger(strPositionINISection, strLeftINIKey, Left);
  Width := FINIFile.ReadInteger(strPositionINISection, strWidthINIKey, Width);
  Height := FINIFile.ReadInteger(strPositionINISection, strHeightINIKey, Height);
  pnlGameEngines.Height := FINIFile.ReadInteger(strPositionINISection, strSplitterHeightINIKey,
    pnlGameEngines.Height);
  sl := TStringList.Create;
  Try
    FINIFile.ReadSection(strGameEnginesINISection, sl);
    For strItem In sl Do
      Begin
        FGameEngines.AddPair(strItem, FINIFile.ReadString(strGameEnginesINISection, strItem, ''));
        FGameEngineNames.AddPair(strItem, FINIFile.ReadString(strGameEngineNamesINISection, strItem,
           // Default is the Game Engine Name
          ExtractFileName(FINIFile.ReadString(strGameEnginesINISection, strItem, ''))));
      End;
    FINIFile.ReadSection(strIWADExceptions, sl);
    For strItem In sl Do
      FIWADExceptions.Add(strItem);
    FINIFile.ReadSection(strAsscoiatedIWADs, sl);
    For strItem In sl Do
      FAssociatedIWAD.AddPair(strItem, FINIFile.ReadString(strAsscoiatedIWADs, strItem, ''));
    FINIFile.ReadSection(strExtraOptionsINISection, sl);
    For strItem In sl Do
      cbxExtraParams.Items.Add(FINIFile.ReadString(strExtraOptionsINISection, strItem, ''));
    FINIFile.ReadSection(strAssociatedOptionsINISection, sl);
    For strItem In sl Do
      FAssociatedOptions.AddPair(strItem, FINIFile.ReadString(strAssociatedOptionsINISection, strItem, ''));
  Finally
    sl.Free;
  End;
  FSelectedGameEngine := FINIFile.ReadString(strSetupINISection, strSelectGameEngineINIKey, '');
  edtWADFolder.Text := FINIFile.ReadString(strSetupINISection, strWADFolderINIKey, '');
  FSelectedIWAD := FINIFile.ReadString(strSetupINISection, strSelectedIWADINIKey, '');
  FSelectedPWAD := FINIFile.ReadString(strSetupINISection, strSelectedPWADINIKey, '');
  cbxExtraParams.Text := FINIFile.ReadString(strSetupINISection, strExtraParamsINIKey, '');
  lvGameEngines.Columns[0].Width := FINIFile.ReadInteger(strSetupINISection, strGameEngineNameWidthINIKey,
    lvGameEngines.Columns[0].Width);
End;

(**

  This method loads the applications build information from the resources.

  @precon  None.
  @postcon The build information is loaded and reflected in the application title and main form caption.

**)
Procedure TfrmDLMainForm.LoadVersionInfo;

Type
  TBuildInfo = Record
    FMajor  : Integer;
    FMinor  : Integer;
    FBugFix :Integer;
    FBuild  : Integer;
  End;

Const
  strBugFix = ' abcdefghijklmnopqrstuvwxyz';
  iShiftRigh = 16;
  iWordMask = $FFFF;
  {$IFDEF DEBUG}
  {$IFDEF WIN32}
  strBitness = '32-Bit ';
  {$ELSE WIN32}
  strBitness = '64-Bit ';
  {$ENDIF WIN32}
  {$ENDIF DEBUG}

ResourceString
  {$IFDEF DEBUG}
  strBuild = ' %d.%d%s BETA ' + strBitness + '(DEBUG Build %d.%d.%d.%d)';
  {$ELSE}
  strBuild = ' %d.%d%s';
  {$ENDIF DEBUG}

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  BuildInfo : TBuildInfo;
  
Begin
  BuildInfo.FMajor := 0;
  BuildInfo.FMinor := 0;
  BuildInfo.FBugFix := 0;
  BuildInfo.FBuild := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      BuildInfo.FMajor := VerValue^.dwFileVersionMS Shr iShiftRigh;
      BuildInfo.FMinor := VerValue^.dwFileVersionMS And iWordMask;
      BuildInfo.FBugfix := VerValue^.dwFileVersionLS Shr iShiftRigh;
      BuildInfo.FBuild := VerValue^.dwFileVersionLS And iWordMask;
      FreeMem(VerInfo, VerInfoSize);
    End;
  Application.Title := Application.Title + Format(strBuild, [
    BuildInfo.FMajor,
    BuildInfo.FMinor,
    strBugFix[Succ(BuildInfo.FBugFix)],
    BuildInfo.FMajor,
    BuildInfo.FMinor,
    BuildInfo.FBugFix,
    BuildInfo.FBuild
  ]);
  Caption := Application.Title;
End;

(**

  This method attempts to load the text file that is associated with the WAD file (same name but ending
  in TXT). If found it is loaded for displayed else a message is displayed saying a text file cannot be
  found.

  @precon  None.
  @postcon The text file is displayed if found.

  @param   strWADFileName as a String as a constant

**)
Procedure TfrmDLMainForm.LoadWADText(Const strWADFileName: String);

ResourceString
  strNoAssociatedTextFound = 'No associated text found!';

Const
  strTxtExt = '.txt';

Var
  strTxtFileName : String;
  
Begin
  strTxtFileName := ChangeFileExt(strWADFileName, strTxtExt);
  If FileExists(strTxtFileName) Then
    mmoWADText.Lines.LoadFromFile(strTxtFileName)
  Else
    mmoWADText.Lines.Text := strNoAssociatedTextFound;
End;

(**

  This is an on edited event handler for the Game Engines list view.

  @precon  None.
  @postcon Updates the game engine name based on the edited text.

  @param   Sender as a TObject
  @param   Item   as a TListItem
  @param   S      as a String as a reference

**)
Procedure TfrmDLMainForm.lvGameEnginesEdited(Sender: TObject; Item: TListItem; Var S: String);

Begin
  FGameEngineNames.ValueFromIndex[lvGameEngines.ItemIndex] := S;
End;

(**

  This is an on select item event handler for the Game Engine list view.

  @precon  None.
  @postcon The FSelectedGameEngine field is updated to reflect the selection and the launch button
           updated.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
Procedure TfrmDLMainForm.lvGameEnginesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

Begin
  FSelectedGameEngine := Item.SubItems[0];
  UpdateUpAndDownBtns;
  UpdateLaunchBtn;
End;

(**

  This method populates the game engines from the FGameEngines string list.

  @precon  None.
  @postcon The Game engines are populates in the list view.

**)
Procedure TfrmDLMainForm.PopulateGameEngines;

Var
  iGameEngine: Integer;
  Item: TListItem;

Begin
  lvGameEngines.Items.BeginUpdate;
  Try
    lvGameEngines.Clear;
    For iGameEngine := 0 To FGameEngines.Count - 1 Do
      Begin
        Item := lvGameEngines.Items.Add;
        Item.Caption := FGameEngineNames.ValueFromIndex[iGameEngine];
        Item.SubItems.Add(FGameEngines.ValueFromIndex[iGameEngine]);
        If CompareText(FSelectedGameEngine, Item.SubItems[0]) = 0 Then
          Item.Selected := True;
      End;
  Finally
    lvGameEngines.Items.EndUpdate;
  End;
End;

(**

  This method populates the list of IWAD which the engines can use.

  @precon  None.
  @postcon The list of WAD files is populated.

**)
Procedure TfrmDLMainForm.PopulateWADs;

Var
  Item: TTreeNode;
  
Begin
  tvIWADs.Items.Clear;
  tvPWADs.Items.Clear;
  Item := tvIWADs.Items.AddChild(Nil, strNone);
  Item.Selected := CompareText(FSelectedIWAD, strNone) = 0;
  Item := tvPWADs.Items.AddChild(Nil, strNone);
  Item.Selected := CompareText(FSelectedPWAD, strNone) = 0;
  RecurseWADFolders(edtWADFolder.Text);
End;

(**

  This method recurses folders searching for WAD files and adds them to the IWAD or PWAD list boxes.

  @precon  None.
  @postcon The WAD files in the given folder are added to the list boxes and any sub-folders are also
           searched.

  @param   strFolder as a String as a constant

**)
Procedure TfrmDLMainForm.RecurseWADFolders(Const strFolder: String);

Const
  strWADFilter = '\*.wad';

Var
  iResult : Integer;
  recSearch : TSearchRec;
  strFileName: String;

Begin
  // Search for WAD files
  iResult := FindFirst(strFolder + strWADFilter, faAnyFile, recSearch);
  Try
    While iResult = 0 Do
      Begin
        If recSearch.Attr And faDirectory = 0 Then
          Begin
            strFileName :=  strFolder + '\' + recSearch.Name;
            Delete(strFileName, 1, Length(edtWADFolder.Text) + 1);
            Case IsIWAD(strFolder + '\' + recSearch.Name) Of
              True:  AddWADFileToList(tvIWADs, strFileName, FSelectedIWAD);
              False: AddWADFileToList(tvPWADs, strFileName, FSelectedPWAD);
            End;
          End;
        iResult := FindNext(recSearch);
      End;
  Finally
    System.SysUtils.FindClose(recSearch);
  End;
  // Search for Sub-Folder
  iResult := FindFirst(strFolder + '\*.*', faDirectory, recSearch);
  Try
    While iResult = 0 Do
      Begin
        If (recSearch.Attr And faDirectory <> 0) And (recSearch.Name <> '.') And (recSearch.Name <> '..') Then
          RecurseWADFolders(strFolder + '\' + recSearch.Name);
        iResult := FindNext(recSearch);
      End;
  Finally
    System.SysUtils.FindClose(recSearch);
  End;
End;

(**

  This method saves the applications settings.

  @precon  None.
  @postcon The setting are saved.

**)
Procedure TfrmDLMainForm.SaveSettings;

Const
  strItem = 'Item%4.4d';

Var
  i: Integer;

Begin
  FINIFile.WriteInteger(strPositionINISection, strTopINIKey, Top);
  FINIFile.WriteInteger(strPositionINISection, strLeftINIKey, Left);
  FINIFile.WriteInteger(strPositionINISection, strWidthINIKey, Width);
  FINIFile.WriteInteger(strPositionINISection, strHeightINIKey, Height);
  FINIFile.WriteInteger(strPositionINISection, strSplitterHeightINIKey, pnlGameEngines.Height);
  FINIFile.EraseSection(strGameEnginesINISection);
  For i := 0 To FGameEngines.Count - 1 Do
    FINIFile.WriteString(strGameEnginesINISection, Format(strItem, [i]), FGameEngines.ValueFromIndex[i]);
  FINIFile.EraseSection(strGameEngineNamesINISection);
  For i := 0 To FGameEngineNames.Count - 1 Do
    FINIFile.WriteString(strGameEngineNamesINISection, Format(strItem, [i]), FGameEngineNames.ValueFromIndex[i]);
  FINIFile.EraseSection(strIWADExceptions);
  For i := 0 To FIWADExceptions.Count - 1 Do
    FINIFile.WriteString(strIWADExceptions, FIWADExceptions[i], FIWADExceptions[i]);
  FINIFile.WriteString(strSetupINISection, strSelectGameEngineINIKey, FSelectedGameEngine);
  FINIFile.WriteString(strSetupINISection, strWADFolderINIKey, edtWADFolder.Text);
  FINIFile.WriteString(strSetupINISection, strSelectedIWADINIKey, FSelectedIWAD);
  FINIFile.WriteString(strSetupINISection, strSelectedPWADINIKey, FSelectedPWAD);
  FINIFile.WriteString(strSetupINISection, strExtraParamsINIKey, cbxExtraParams.Text);
  FINIFile.EraseSection(strAsscoiatedIWADs);
  For i := 0 To FAssociatedIWAD.Count - 1 Do
    FINIFile.WriteString(strAsscoiatedIWADs, FAssociatedIWAD.Names[i], FAssociatedIWAD.ValueFromIndex[i]);
  FINIFile.EraseSection(strExtraOptionsINISection);
  For i := 0 To cbxExtraParams.Items.Count - 1 Do
    FINIFile.WriteString(strExtraOptionsINISection, Format(strItem, [i]), cbxExtraParams.Items[i]);
  FINIFile.EraseSection(strAssociatedOptionsINISection);
  For i := 0 To FAssociatedOptions.Count - 1 Do
    FINIFile.WriteString(strAssociatedOptionsINISection, FAssociatedOptions.Names[i],
      FAssociatedOptions.ValueFromIndex[i]);
  FINIFile.WriteInteger(strSetupINISection, strGameEngineNameWidthINIKey, lvGameEngines.Columns[0].Width);
  If FINIFile.Modified Then
    FINIFile.UpdateFile;
End;

(**

  This method returns the filename and parent folders for the selected node.

  @precon  Node must be a valid instance.
  @postcon Returns the filename and parent folders for the selected node.

  @param   Node as a TTreeNode as a constant
  @return  a String

**)
Function TfrmDLMainForm.TreePath(Const Node: TTreeNode): String;

Var
  N : TTreeNode;
  
Begin
  Result := '';
  N := Node;
  While Assigned(N) Do
    Begin
      If Result.Length > 0 Then
        Result := '\' + Result;
      Result := N.Text + Result;
      N := N.Parent;
    End;
End;

(**

  This is an on click event handler for the IWAD list box.

  @precon  None.
  @postcon Stored the selected item and updates the launch button.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.tvIWADsClick(Sender: TObject);

Begin
  FSelectedIWAD := TreePath(tvIWADs.Selected);
  // Update Extra Options
  cbxExtraParams.Text := FAssociatedOptions.Values[FSelectedIWAD];
  // Update Launch button
  UpdateLaunchBtn;
  LoadWADText(edtWADFolder.Text + '\' + FSelectedIWAD);
End;

(**

  This is an on click event handler for the PWAD list box.

  @precon  None.
  @postcon Stored the selected item and updates the launch button.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.tvPWADsClick(Sender: TObject);

Var
  astrFileName: TArray<String>;
  iPath: Integer;
  Node: TTreeNode;
  ParentNode: TTreeNode;
  strIWAD: String;
  
Begin
  FSelectedPWAD := TreePath(tvPWADs.Selected);
  // Update associated IWAD
  strIWAD := FAssociatedIWAD.Values[FSelectedPWAD];
  astrFileName := strIWAD.Split(['\']);
  ParentNode := Nil;
  For iPath := 0 To High(astrFileName) Do
    Begin
      Node := Find(tvIWADs, ParentNode, astrFileName[iPath]);
      If Assigned(Node) Then
        ParentNode := Node;
    End;
  If Assigned(ParentNode) Then
    Begin
      ParentNode.Selected := True;
      FSelectedIWAD := strIWAD;
    End;
  // Update Launch button
  UpdateLaunchBtn;
  LoadWADText(edtWADFolder.Text + '\' + FSelectedPWAD);
End;

(**

  This method updates the enabled property of the launch button, i.e., the selections in the main form
  need to be valid to launch the game.

  @precon  None.
  @postcon The launch button is either enabled or disabled.

**)
Procedure TfrmDLMainForm.UpdateLaunchBtn;

Begin
  btnLaunch.Enabled :=
    (lvGameEngines.SelCount = 1) And
    (tvIWADs.SelectionCount > 0) And
    (tvPWADs.SelectionCount > 0);
End;

(**

  This method enables/disables the up and down buttons depending upon which game engine is selected.

  @precon  None.
  @postcon The up and down buttons are enabled or disabled according to which game engine is selected.

**)
Procedure TfrmDLMainForm.UpdateUpAndDownBtns;

Begin
  btnUp.Enabled := lvGameEngines.ItemIndex > 0;
  btnDown.Enabled := lvGameEngines.ItemIndex < FGameEngines.Count - 1;
End;

End.
