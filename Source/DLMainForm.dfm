object frmDLMainForm: TfrmDLMainForm
  Left = 0
  Top = 0
  Caption = 'Doom Loader'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 16
  object lblGameEngines: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 622
    Height = 16
    Align = alTop
    Caption = 'Game Engines'
    FocusControl = lvGameEngines
    ExplicitWidth = 81
  end
  object Splitter: TSplitter
    AlignWithMargins = True
    Left = 3
    Top = 185
    Width = 622
    Height = 5
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    MinSize = 160
    ResizeStyle = rsUpdate
    ExplicitLeft = 0
    ExplicitTop = 177
    ExplicitWidth = 628
  end
  object pnlGameEngines: TPanel
    Left = 0
    Top = 22
    Width = 628
    Height = 160
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lvGameEngines: TListView
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 531
      Height = 154
      Align = alClient
      Columns = <
        item
          Caption = 'Game Engine'
          Width = 200
        end
        item
          AutoSize = True
          Caption = 'Location'
        end>
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnEdited = lvGameEnginesEdited
      OnSelectItem = lvGameEnginesSelectItem
    end
    object pnlGameEngineBtns: TPanel
      Left = 537
      Top = 0
      Width = 91
      Height = 160
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 155
      object btnAdd: TBitBtn
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 85
        Height = 25
        Align = alTop
        Caption = '&Add'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          33333333FF33333333FF333993333333300033377F3333333777333993333333
          300033F77FFF3333377739999993333333333777777F3333333F399999933333
          33003777777333333377333993333333330033377F3333333377333993333333
          3333333773333333333F333333333333330033333333F33333773333333C3333
          330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
          993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
          333333333337733333FF3333333C333330003333333733333777333333333333
          3000333333333333377733333333333333333333333333333333}
        NumGlyphs = 2
        ParentDoubleBuffered = True
        TabOrder = 0
        OnClick = btnAddClick
      end
      object btnEdit: TBitBtn
        AlignWithMargins = True
        Left = 3
        Top = 34
        Width = 85
        Height = 25
        Align = alTop
        Caption = '&Edit'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
          000033333377777777773333330FFFFFFFF03FF3FF7FF33F3FF700300000FF0F
          00F077F777773F737737E00BFBFB0FFFFFF07773333F7F3333F7E0BFBF000FFF
          F0F077F3337773F3F737E0FBFBFBF0F00FF077F3333FF7F77F37E0BFBF00000B
          0FF077F3337777737337E0FBFBFBFBF0FFF077F33FFFFFF73337E0BF0000000F
          FFF077FF777777733FF7000BFB00B0FF00F07773FF77373377373330000B0FFF
          FFF03337777373333FF7333330B0FFFF00003333373733FF777733330B0FF00F
          0FF03333737F37737F373330B00FFFFF0F033337F77F33337F733309030FFFFF
          00333377737FFFFF773333303300000003333337337777777333}
        NumGlyphs = 2
        ParentDoubleBuffered = True
        TabOrder = 1
        OnClick = btnEditClick
      end
      object btnDelete: TBitBtn
        AlignWithMargins = True
        Left = 3
        Top = 65
        Width = 85
        Height = 25
        Align = alTop
        Caption = '&Delete'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          333333333333333333FF33333333333330003333333333333777333333333333
          300033FFFFFF3333377739999993333333333777777F3333333F399999933333
          3300377777733333337733333333333333003333333333333377333333333333
          3333333333333333333F333333333333330033333F33333333773333C3333333
          330033337F3333333377333CC3333333333333F77FFFFFFF3FF33CCCCCCCCCC3
          993337777777777F77F33CCCCCCCCCC399333777777777737733333CC3333333
          333333377F33333333FF3333C333333330003333733333333777333333333333
          3000333333333333377733333333333333333333333333333333}
        NumGlyphs = 2
        ParentDoubleBuffered = True
        TabOrder = 2
        OnClick = btnDeleteClick
      end
      object btnUp: TBitBtn
        AlignWithMargins = True
        Left = 3
        Top = 96
        Width = 85
        Height = 25
        Align = alTop
        Caption = '&Up'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000120B0000120B00001000000010000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          33333333330C033333333333330C033333333333330C033333333333330C0333
          33333333330C033333333330000C000033333330CCCCCCC0333333330CCCCC03
          333333330CCCCC033333333330CCC0333333333330CCC03333333333330C0333
          33333333330C0333333333333330333333333333333033333333}
        ParentDoubleBuffered = True
        TabOrder = 3
        OnClick = btnUpClick
        ExplicitTop = 127
      end
      object btnDown: TBitBtn
        AlignWithMargins = True
        Left = 3
        Top = 127
        Width = 85
        Height = 25
        Align = alTop
        Caption = 'D&own'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000120B0000120B00001000000010000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
          333333333330333333333333330C033333333333330C03333333333330CCC033
          3333333330CCC033333333330CCCCC03333333330CCCCC0333333330CCCCCCC0
          33333330000C000033333333330C033333333333330C033333333333330C0333
          33333333330C033333333333330C033333333333330003333333}
        ParentDoubleBuffered = True
        TabOrder = 4
        OnClick = btnDownClick
        ExplicitTop = 135
      end
    end
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 401
    Width = 628
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      628
      41)
    object lblParams: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 9
      Width = 43
      Height = 16
      Caption = '&Params'
      FocusControl = cbxExtraParams
    end
    object btnLaunch: TBitBtn
      Left = 536
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Launch'
      Default = True
      ParentDoubleBuffered = True
      TabOrder = 1
      OnClick = btnLaunchClick
    end
    object cbxExtraParams: TComboBox
      Left = 64
      Top = 6
      Width = 466
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object pnlWADs: TPanel
    Left = 0
    Top = 193
    Width = 628
    Height = 208
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 188
    ExplicitHeight = 213
    object pnlWADFolder: TPanel
      Left = 0
      Top = 0
      Width = 628
      Height = 40
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        628
        40)
      object lblWADFolder: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 9
        Width = 68
        Height = 16
        Caption = 'WAD &Folder'
        FocusControl = edtWADFolder
      end
      object edtWADFolder: TEdit
        Left = 96
        Top = 6
        Width = 434
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 0
        OnChange = edtWADFolderChange
      end
      object btnBrowse: TBitBtn
        Left = 536
        Top = 6
        Width = 85
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Browse'
        ParentDoubleBuffered = True
        TabOrder = 1
        OnClick = btnBrowseClick
      end
    end
    object gpnlBottomLayout: TGridPanel
      Left = 0
      Top = 40
      Width = 628
      Height = 168
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 175.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 175.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = pnlIWADs
          Row = 0
        end
        item
          Column = 1
          Control = pnlPWADs
          Row = 0
        end
        item
          Column = 2
          Control = pnlText
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      ExplicitHeight = 173
      object pnlIWADs: TPanel
        Left = 0
        Top = 0
        Width = 175
        Height = 168
        Align = alClient
        Anchors = []
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitHeight = 173
        object lblIWADs: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 169
          Height = 16
          Align = alTop
          Anchors = []
          Caption = 'IWAD Files'
          FocusControl = tvIWADs
          ExplicitWidth = 62
        end
        object tvIWADs: TTreeView
          AlignWithMargins = True
          Left = 3
          Top = 25
          Width = 169
          Height = 140
          Align = alClient
          Anchors = []
          HideSelection = False
          Indent = 19
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          OnClick = tvIWADsClick
          ExplicitHeight = 145
        end
      end
      object pnlPWADs: TPanel
        Left = 175
        Top = 0
        Width = 175
        Height = 168
        Align = alClient
        Anchors = []
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitHeight = 173
        object lblPWADs: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 169
          Height = 16
          Align = alTop
          Anchors = []
          Caption = 'PWAD Files'
          FocusControl = tvPWADs
          ExplicitWidth = 65
        end
        object tvPWADs: TTreeView
          AlignWithMargins = True
          Left = 3
          Top = 25
          Width = 169
          Height = 140
          Align = alClient
          Anchors = []
          HideSelection = False
          Indent = 19
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          OnClick = tvPWADsClick
          ExplicitHeight = 145
        end
      end
      object pnlText: TPanel
        Left = 350
        Top = 0
        Width = 278
        Height = 168
        Align = alClient
        Anchors = []
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitHeight = 173
        object lblWADText: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 156
          Height = 16
          Align = alTop
          Caption = 'WAD File &Text (if available)'
        end
        object mmoWADText: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 25
          Width = 272
          Height = 140
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
          ExplicitHeight = 145
        end
      end
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '.exe'
    Filter = 'Game Engines (*.exe)|*.exe'
    Title = 'Game Engine'
    Left = 392
    Top = 64
  end
end
