object frmDLOptions: TfrmDLOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Doom Loader Options'
  ClientHeight = 296
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 16
  object lblOptions: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 405
    Height = 16
    Align = alTop
    Caption = '&Options'
    ExplicitWidth = 43
  end
  object lblVCLTheme: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 213
    Width = 405
    Height = 16
    Align = alBottom
    Caption = 'VCL &Theme'
    ExplicitLeft = 8
    ExplicitTop = 313
  end
  object lbxOptions: TCheckListBox
    AlignWithMargins = True
    Left = 3
    Top = 25
    Width = 405
    Height = 60
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 411
    ExplicitHeight = 63
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 262
    Width = 411
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -56
    ExplicitTop = 301
    DesignSize = (
      411
      34)
    object btnOK: TBitBtn
      Left = 248
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 254
    end
    object btnCancel: TBitBtn
      Left = 329
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 335
    end
  end
  object rgpExtraOptions: TRadioGroup
    AlignWithMargins = True
    Left = 3
    Top = 91
    Width = 405
    Height = 116
    Align = alBottom
    Caption = '&Extra Options'
    TabOrder = 2
    ExplicitLeft = 8
    ExplicitTop = 82
  end
  object cbxVCLThemes: TComboBox
    AlignWithMargins = True
    Left = 3
    Top = 235
    Width = 405
    Height = 24
    Align = alBottom
    Style = csDropDownList
    Sorted = True
    TabOrder = 3
    OnChange = cbxVCLThemesChange
  end
end
