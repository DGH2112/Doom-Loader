(**
  
  This module contains a class to represent a form for displaying the applications options.

  @Author  David Hoyle
  @Version 1.272
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
Unit DLOptionsForm;

Interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.CheckLst,
  DLTypes;

Type
  (** A class to represent a form for displaying the application options. **)
  TfrmDLOptions = Class(TForm)
    lblOptions: TLabel;
    lbxOptions: TCheckListBox;
    pnlButtons: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    rgpExtraOptions: TRadioGroup;
  Strict Private
  Strict Protected
    Procedure InitialiseForm(Const Options : TDLOptionsRecord);
    Procedure FinaliseForm(Var Options : TDLOptionsRecord);
  Public
    Class Function Execute(Var Options : TDLOptionsRecord) : Boolean;
  End;

Implementation

Uses
  DLConstants;

{$R *.dfm}

(**

  This method is the main way to invoke the dialogue.

  @precon  None.
  @postcon The dialogue is created and displayed and once closed the form memory is freed.

  @param   Options as a TDLOptionsRecord as a reference
  @return  a Boolean

**)
Class Function TfrmDLOptions.Execute(Var Options: TDLOptionsRecord): Boolean;

Var
  F : TfrmDLOptions;
  
Begin
  Result := False;
  F := TfrmDLOptions.Create(Application.MainForm);
  Try
    F.InitialiseForm(Options);
    If F.ShowModal = mrOK Then
      Begin
        F.FinaliseForm(Options);
        Result := True;
      End;
  Finally
    F.Free;
  End;
End;

(**

  This method extracts the selected options from the list box and updates the passed options record.

  @precon  None.
  @postcon The passed options record is updated.

  @param   Options as a TDLOptionsRecord as a reference

**)
Procedure TfrmDLOptions.FinaliseForm(Var Options: TDLOptionsRecord);

Var
  iOption : Integer;
  
Begin
  Options.FOptions := [];
  For iOption := 0 To lbxOptions.Count - 1 Do
    If lbxOptions.Checked[iOption] Then
      Include(Options.FOptions, TDLOption(iOption));
  Options.FExtraOps := TDLExtraOpsAssociation(rgpExtraOptions.ItemIndex);
End;

(**

  This method initialises the dialogue by populating the list box with options.

  @precon  None.
  @postcon The options dialogue is initialised.

  @param   Options as a TDLOptionsRecord as a constant

**)
Procedure TfrmDLOptions.InitialiseForm(Const Options: TDLOptionsRecord);

Var
  eOption : TDLOption;
  iIndex: Integer;
  eExtraOp : TDLExtraOpsAssociation;

Begin
  For eOption := Low(TDLOption) To High(TDLOption) Do
    Begin
      iIndex := lbxOptions.Items.Add(astrOptionDescription[eOption]);
      lbxOptions.Checked[iIndex] := eOption In Options.FOptions;
    End;
  For eExtraOp := Low(TDLExtraOpsAssociation) To High(TDLExtraOpsAssociation) Do
    rgpExtraOptions.Items.Add(astrExtraOpsDescriptions[eExtraOp]);
  rgpExtraOptions.ItemIndex := Integer(Options.FExtraOps);
End;

End.
