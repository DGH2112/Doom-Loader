object frmDLOptions: TfrmDLOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Doom Loader Options'
  ClientHeight = 249
  ClientWidth = 425
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
    Width = 419
    Height = 16
    Align = alTop
    Caption = '&Options'
    ExplicitWidth = 43
  end
  object lbxOptions: TCheckListBox
    AlignWithMargins = True
    Left = 3
    Top = 25
    Width = 419
    Height = 65
    Align = alClient
    ItemHeight = 17
    TabOrder = 0
    ExplicitHeight = 129
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 215
    Width = 425
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 159
    ExplicitWidth = 421
    DesignSize = (
      425
      34)
    object btnOK: TBitBtn
      Left = 266
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 262
    end
    object btnCancel: TBitBtn
      Left = 347
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 343
    end
  end
  object rgpExtraOptions: TRadioGroup
    AlignWithMargins = True
    Left = 3
    Top = 96
    Width = 419
    Height = 116
    Align = alBottom
    Caption = '&Extra Options'
    TabOrder = 2
  end
end
