object frmDLOptions: TfrmDLOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Doom Loader Options'
  ClientHeight = 194
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
    Height = 132
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 160
    ExplicitTop = 72
    ExplicitWidth = 121
    ExplicitHeight = 97
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 160
    Width = 425
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
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
      ExplicitTop = 11
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
      ExplicitTop = 11
    end
  end
end
