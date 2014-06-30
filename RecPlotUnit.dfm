object RecPlotFrm: TRecPlotFrm
  Left = 1023
  Top = 237
  Width = 319
  Height = 322
  Caption = 'Real Time Plot'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object plPlot: TXYPlotDisplay
    Left = 8
    Top = 8
    Width = 281
    Height = 241
    MaxPointsPerLine = 4096
    XAxisMax = 1.000000000000000000
    XAxisTick = 0.200000002980232200
    XAxisLaw = axLinear
    XAxisLabel = 'X Axis'
    XAxisAutoRange = False
    YAxisMax = 1.000000000000000000
    YAxisTick = 0.200000002980232200
    YAxisLaw = axLinear
    YAxisLabel = 'Y Axis'
    YAxisAutoRange = False
    YAxisLabelAtTop = False
    ScreenFontName = 'Arial'
    ScreenFontSize = 10
    LineWidth = 1
    MarkerSize = 6
    ShowLines = True
    ShowMarkers = True
    HistogramFullBorders = False
    HistogramFillColor = clWhite
    HistogramFillStyle = bsClear
    HistogramCumulative = False
    HistogramPercentage = False
    PrinterFontSize = 10
    PrinterFontName = 'Arial'
    PrinterLineWidth = 1
    PrinterMarkerSize = 5
    PrinterLeftMargin = 14
    PrinterRightMargin = 14
    PrinterTopMargin = 14
    PrinterBottomMargin = 14
    PrinterDisableColor = False
    MetafileWidth = 500
    MetafileHeight = 400
  end
  object ControlsGrp: TGroupBox
    Left = 8
    Top = 248
    Width = 281
    Height = 41
    TabOrder = 0
    object bClearPoints: TButton
      Left = 192
      Top = 12
      Width = 81
      Height = 17
      Caption = 'Clear Plot'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = bClearPointsClick
    end
    object ConductancePanel: TPanel
      Left = 8
      Top = 8
      Width = 121
      Height = 25
      BevelOuter = bvNone
      TabOrder = 1
      object Label3: TLabel
        Left = 16
        Top = 0
        Width = 28
        Height = 14
        Alignment = taRightJustify
        Caption = 'Units'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Panel4: TPanel
        Left = 152
        Top = 24
        Width = 185
        Height = 41
        Caption = 'Panel1'
        TabOrder = 0
      end
      object cbConductanceUnits: TComboBox
        Left = 48
        Top = 0
        Width = 65
        Height = 22
        Style = csDropDownList
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ItemHeight = 14
        ItemIndex = 0
        ParentFont = False
        TabOrder = 1
        Text = 'pS'
        OnChange = cbConductanceUnitsChange
        Items.Strings = (
          'pS'
          'nS'
          'uS'
          'mS'
          'S')
      end
    end
    object ResistancePanel: TPanel
      Left = 8
      Top = 8
      Width = 121
      Height = 25
      BevelOuter = bvNone
      TabOrder = 2
      object Label2: TLabel
        Left = 16
        Top = 0
        Width = 28
        Height = 14
        Alignment = taRightJustify
        Caption = 'Units'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Panel3: TPanel
        Left = 152
        Top = 24
        Width = 185
        Height = 41
        Caption = 'Panel1'
        TabOrder = 0
      end
      object cbResistanceUnits: TComboBox
        Left = 48
        Top = 0
        Width = 65
        Height = 22
        Style = csDropDownList
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ItemHeight = 14
        ItemIndex = 0
        ParentFont = False
        TabOrder = 1
        Text = 'GOhms'
        OnChange = cbResistanceUnitsChange
        Items.Strings = (
          'GOhms'
          'MOhms'
          'KOhms'
          'Ohms')
      end
    end
    object CurrentPanel: TPanel
      Left = 8
      Top = 8
      Width = 121
      Height = 25
      BevelOuter = bvNone
      TabOrder = 3
      object Label4: TLabel
        Left = 16
        Top = 0
        Width = 28
        Height = 14
        Alignment = taRightJustify
        Caption = 'Units'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Panel5: TPanel
        Left = 152
        Top = 24
        Width = 185
        Height = 41
        Caption = 'Panel1'
        TabOrder = 0
      end
      object cbCurrentUnits: TComboBox
        Left = 48
        Top = 0
        Width = 65
        Height = 22
        Style = csDropDownList
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ItemHeight = 14
        ItemIndex = 0
        ParentFont = False
        TabOrder = 1
        Text = 'pA'
        OnChange = cbConductanceUnitsChange
        Items.Strings = (
          'pA'
          'nA'
          'uA'
          'mA'
          'A')
      end
    end
  end
end
