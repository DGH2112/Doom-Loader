object frmDLOptions: TfrmDLOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Doom Loader Options'
  ClientHeight = 248
  ClientWidth = 421
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
    Width = 415
    Height = 16
    Align = alTop
    Caption = '&Options'
    ExplicitWidth = 43
  end
  object lbxOptions: TCheckListBox
    AlignWithMargins = True
    Left = 3
    Top = 25
    Width = 415
    Height = 64
    Align = alClient
    ItemHeight = 17
    TabOrder = 0
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 214
    Width = 421
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 159
    DesignSize = (
      421
      34)
    object btnOK: TBitBtn
      Left = 258
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
      Left = 339
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
    Top = 95
    Width = 415
    Height = 116
    Align = alBottom
    Caption = '&Extra Options'
    TabOrder = 2
    ExplicitTop = 96
    ExplicitWidth = 419
  end
end
