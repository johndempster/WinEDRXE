object PageViewFrm: TPageViewFrm
  Left = 882
  Top = 396
  Width = 696
  Height = 480
  Caption = 'Page View'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object scDisplay: TScopeDisplay
    Left = 168
    Top = 32
    Width = 513
    Height = 337
    OnMouseUp = scDisplayMouseUp
    OnCursorChange = scDisplayCursorChange
    CursorChangeInProgress = False
    NumChannels = 1
    NumPoints = 0
    MaxPoints = 1024
    XMin = 0
    XMax = 1023
    XOffset = 0
    CursorsEnabled = True
    TScale = 1.000000000000000000
    TUnits = 's'
    TCalBar = -1.000000000000000000
    ZoomDisableHorizontal = True
    ZoomDisableVertical = False
    DisableChannelVisibilityButton = False
    PrinterFontSize = 0
    PrinterPenWidth = 0
    PrinterLeftMargin = 0
    PrinterRightMargin = 0
    PrinterTopMargin = 0
    PrinterBottomMargin = 0
    PrinterDisableColor = False
    PrinterShowLabels = True
    PrinterShowZeroLevels = True
    MetafileWidth = 0
    MetafileHeight = 0
    StorageMode = False
    RecordNumber = -1
    DisplayGrid = False
    MaxADCValue = 2047
    MinADCValue = -2048
    NumBytesPerSample = 2
    FixZeroLevels = False
    DisplaySelected = False
    FontSize = 8
  end
  object Label4: TLabel
    Left = 168
    Top = 8
    Width = 30
    Height = 15
    Caption = 'Ident:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object ControlsGrp: TGroupBox
    Left = 8
    Top = 0
    Width = 153
    Height = 449
    TabOrder = 0
    object GroupBox2: TGroupBox
      Left = 8
      Top = 8
      Width = 137
      Height = 73
      TabOrder = 0
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 16
        Height = 14
        Caption = 'Ch.'
      end
      object Label2: TLabel
        Left = 24
        Top = 40
        Width = 37
        Height = 14
        Caption = 'Start At'
      end
      object cbChannel: TComboBox
        Left = 40
        Top = 16
        Width = 89
        Height = 22
        ItemHeight = 14
        TabOrder = 0
        Text = 'cbChannel'
        OnChange = cbChannelChange
      end
      object edStartTime: TValidatedEdit
        Left = 72
        Top = 40
        Width = 57
        Height = 20
        OnKeyPress = edStartTimeKeyPress
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        Text = ' 0.00 s'
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.2f'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 80
      Width = 137
      Height = 121
      Caption = ' Page Settings '
      TabOrder = 1
      object Label3: TLabel
        Left = 32
        Top = 16
        Width = 59
        Height = 14
        Caption = 'Lines / page'
      end
      object lbDisplayPoints: TLabel
        Left = 7
        Top = 40
        Width = 62
        Height = 14
        Caption = 'Line duration'
      end
      object edLinesPerPage: TValidatedEdit
        Left = 96
        Top = 16
        Width = 33
        Height = 20
        OnKeyPress = edLinesPerPageKeyPress
        AutoSize = False
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%g'
        LoLimit = 1.000000000000000000
        HiLimit = 16.000000000000000000
      end
      object edLineDuration: TValidatedEdit
        Left = 72
        Top = 40
        Width = 57
        Height = 20
        OnKeyPress = edLineDurationKeyPress
        AutoSize = False
        Text = ' 0.05 s'
        Value = 0.050000000745058060
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.4g'
        LoLimit = 0.050000000745058060
        HiLimit = 10000.000000000000000000
      end
      object ckShowLineTimes: TCheckBox
        Left = 8
        Top = 64
        Width = 121
        Height = 17
        Caption = 'Show Line Times'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = ckShowLineTimesClick
      end
      object ckShowZeroLevels: TCheckBox
        Left = 8
        Top = 80
        Width = 121
        Height = 17
        Caption = 'Show Zero Levels'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnClick = ckShowZeroLevelsClick
      end
      object ckFixedZeroLevels: TCheckBox
        Left = 8
        Top = 96
        Width = 129
        Height = 17
        Caption = 'Fix Zero Levels'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        OnClick = ckFixedZeroLevelsClick
      end
    end
  end
  object sbDisplay: TScrollBar
    Left = 168
    Top = 384
    Width = 513
    Height = 17
    PageSize = 0
    TabOrder = 1
    OnChange = sbDisplayChange
  end
  object edIdent: TEdit
    Left = 208
    Top = 8
    Width = 401
    Height = 20
    Hint = 'Experiment indentification line'
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 2
    OnChange = edIdentChange
  end
end
