(**
  
  This module contains a form for configuring and launching Doom and its various WADs.

  @Author  David Hoyle
  @Version 5.508
  @Date    17 May 2023
  
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
    lbxIWADs: TListBox;
    pnlIWADs: TPanel;
    pnlPWADs: TPanel;
    lblPWADs: TLabel;
    lbxPWADs: TListBox;
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
    edtExtraParams: TEdit;
    lblParams: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure edtWADFolderChange(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    procedure lbxIWADsClick(Sender: TObject);
    procedure lbxPWADsClick(Sender: TObject);
    procedure lvGameEnginesSelectItem(Sender: TObject; Item: TListItem; Selected:
        Boolean);
  Strict Private
    FINIFileName        : String;
    FINIFile            : TMemINIFile;
    FGameEngines        : TStringList;
    FSelectedGameEngine : String;
    FSelectedIWAD       : String;
    FSelectedPWAD       : String;
    FIWADExceptions     : TStringList;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure PopulateGameEngines;
    Procedure PopulateIWADs;
    Procedure UpdateLaunchBtn;
    Function  IsIWAD(Const strFileName : String) : Boolean;
    Procedure RecurseWADFolders(Const strFolder : String);
    Procedure AddWADFileToList(Const ListBox : TListBox; Const strFileName, SelectedWAD : String);
  Public
  End;

Var
  (** Form variable used by Delphi to create the applications main form. **)
  frmDLMainForm: TfrmDLMainForm;

Implementation

uses
  System.IOUtils,
  System.StrUtils,
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
  (** A constant to define the splitter height of the Game Engine section. **)
  strSplitterHeightINIKey = 'SplitterHeight';
  (** A constant to define the selected Game Engine. **)
  strSelectGameEngineINIKey = 'Select Game Engine';
  strSetupINISection = 'Setup';
  strWADFolderINIKey = 'WAD Folder';
  strSelectedIWADINIKey = 'Selected IWAD';
  strSelectedPWADINIKey = 'Selected PWAD';
  strExtraParamsINIKey = 'Extra Params';

{$R *.dfm}


(**

  This method adds the given WAD filename to the given listbox and optionally selects the item if it
  matches the given selected item.

  @precon  Listbox must be a valid instance.
  @postcon The WWAD file is added to the listbox and optionally selected.

  @param   ListBox     as a TListBox as a constant
  @param   strFileName as a String as a constant
  @param   SelectedWAD as a String as a constant

**)
Procedure TfrmDLMainForm.AddWADFileToList(Const ListBox : TListBox; Const strFileName,
  SelectedWAD : String);

Begin
  ListBox.Items.Add(strFileName);
  If CompareText(SelectedWAD, strFileName) = 0 Then
    ListBox.ItemIndex := ListBox.Count - 1;
End;

(**

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Displays a dialogue to the user to browse for executable files and if confirmed the EXE is
           added the list of Game Engines.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.btnAddClick(Sender: TObject);

Begin
  If dlgOpen.Execute Then
    Begin
      FGameEngines.AddPair(
        ExtractFileName(dlgOpen.FileName),
        ExtractFilePath(dlgOpen.FileName)
      );
      FSelectedGameEngine := ExtractFileName(dlgOpen.FileName);
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
  PopulateGameEngines;
  UpdateLaunchBtn;
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
  dlgOpen.InitialDir := FGameEngines.ValueFromIndex[lvGameEngines.ItemIndex];
  dlgOpen.FileName := FGameEngines.Names[lvGameEngines.ItemIndex];
  If dlgOpen.Execute Then
    Begin
      FGameEngines[lvGameEngines.ItemIndex] :=
        ExtractFileName(dlgOpen.FileName) + '=' + ExtractFilePath(dlgOpen.FileName);
      FSelectedGameEngine := ExtractFileName(dlgOpen.FileName);
      PopulateGameEngines;
      UpdateLaunchBtn;
    End;
End;

(**

  This is an on click event handler for the Launch button.

  @precon  None.
  @postcon Asks the shell to load the game engine with the selected WAD files.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.btnLaunchClick(Sender: TObject);

Const
  strOpenVerb = 'open';
  strIWADCmd = '-iwad "%s"';
  strPWADCmd = '-file "%s"';

Begin
  ShellExecute(
    Handle,
    PChar(strOpenVerb),
    PChar
      ('"' + FGameEngines.ValueFromIndex[lvGameEngines.ItemIndex] +
      FGameEngines.Names[lvGameEngines.ItemIndex] + '"'
    ), // Game Engine
    PChar(
      Format(strIWADCmd, [edtWADFolder.Text + '\' + FSelectedIWAD]) +
      IfThen(lbxPWADs.ItemIndex > 0, Format(#32 + strPWADCmd, [edtWADFolder.Text + '\' + FSelectedPWAD]), '') +
      IfThen(Length(edtExtraParams.Text) > 0, #32 + edtExtraParams.Text, '')
    ), // Parameter
    PChar(
      FGameEngines.ValueFromIndex[lvGameEngines.ItemIndex]
    ), // Directory
    SW_NORMAL
  );
End;

(**

  This is an on change event handler for the WAD edit control.

  @precon  None.
  @postcon Forces a [re-]population of the WAD file lists.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.edtWADFolderChange(Sender: TObject);

Begin
  PopulateIWADs;
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
  FIWADExceptions := TStringList.Create;
  FIWADExceptions.Sorted := True;
  FIWADExceptions.CaseSensitive := False;
  FIWADExceptions.Add(strHEXDDWAD);
  LoadSettings;
  PopulateGameEngines;
  PopulateIWADs;
  UpdateLaunchBtn;
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
  FIWADExceptions.Free;
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

  This is an on click event handler for the IWAD list box.

  @precon  None.
  @postcon Stored the selected item and updates the launch button.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.lbxIWADsClick(Sender: TObject);

Begin
  FSelectedIWAD := lbxIWADs.Items[lbxIWADs.ItemIndex];
  UpdateLaunchBtn;
End;

(**

  This is an on click event handler for the PWAD list box.

  @precon  None.
  @postcon Stored the selected item and updates the launch button.

  @param   Sender as a TObject

**)
Procedure TfrmDLMainForm.lbxPWADsClick(Sender: TObject);

Begin
  FSelectedPWAD := lbxPWADs.Items[lbxPWADs.ItemIndex];
  UpdateLaunchBtn;
End;

(**

  This method loads the applications settings.

  @precon  None.
  @postcon The settings are loaded.

**)
Procedure TfrmDLMainForm.LoadSettings;

Var
  sl : TStringList;
  strGameEngine: String;

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
    For strGameEngine In sl Do
      FGameEngines.AddPair(
        strGameEngine,
        FINIFile.ReadString(strGameEnginesINISection, strGameEngine, '')
      );
    FSelectedGameEngine := FINIFile.ReadString(strSetupINISection, strSelectGameEngineINIKey, '');
    edtWADFolder.Text := FINIFile.ReadString(strSetupINISection, strWADFolderINIKey, '');
    FSelectedIWAD := FINIFile.ReadString(strSetupINISection, strSelectedIWADINIKey, '');
    FSelectedPWAD := FINIFile.ReadString(strSetupINISection, strSelectedPWADINIKey, '');
    edtExtraParams.Text := FINIFile.ReadString(strSetupINISection, strExtraParamsINIKey, '');
  Finally
    sl.Free;
  End;
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
  FSelectedGameEngine := Item.Caption;
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
        Item.Caption := FGameEngines.Names[iGameEngine];
        Item.SubItems.Add(FGameEngines.ValueFromIndex[iGameEngine]);
        If CompareText(FSelectedGameEngine, Item.Caption) = 0 Then
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
Procedure TfrmDLMainForm.PopulateIWADs;

Const
  strNone = '(none)';

Begin
  lbxIWADs.Clear;
  lbxPWADs.Clear;
  lbxPWADs.Items.Add(strNone);
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
              True:  AddWADFileToList(lbxIWADs, strFileName, FSelectedIWAD);
              False: AddWADFileToList(lbxPWADs, strFileName, FSelectedPWAD);
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

Var
  iGameEngine: Integer;

Begin
  FINIFile.WriteInteger(strPositionINISection, strTopINIKey, Top);
  FINIFile.WriteInteger(strPositionINISection, strLeftINIKey, Left);
  FINIFile.WriteInteger(strPositionINISection, strWidthINIKey, Width);
  FINIFile.WriteInteger(strPositionINISection, strHeightINIKey, Height);
  FINIFile.WriteInteger(strPositionINISection, strSplitterHeightINIKey, pnlGameEngines.Height);
  FINIFile.EraseSection(strGameEnginesINISection);
  For iGameEngine := 0 To FGameEngines.Count - 1 Do
    FINIFile.WriteString(strGameEnginesINISection, FGameEngines.Names[iGameEngine],
      FGameEngines.ValueFromIndex[iGameEngine]);
  FINIFile.WriteString(strSetupINISection, strSelectGameEngineINIKey, FSelectedGameEngine);
  FINIFile.WriteString(strSetupINISection, strWADFolderINIKey, edtWADFolder.Text);
  FINIFile.WriteString(strSetupINISection, strSelectedIWADINIKey, FSelectedIWAD);
  FINIFile.WriteString(strSetupINISection, strSelectedPWADINIKey, FSelectedPWAD);
  FINIFile.WriteString(strSetupINISection, strExtraParamsINIKey, edtExtraParams.Text);
  If FINIFile.Modified Then
    FINIFile.UpdateFile;
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
    (lbxIWADs.ItemIndex > -1) And
    (lbxPWADs.ItemIndex > -1);
End;

End.
