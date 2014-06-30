object ECGFrm: TECGFrm
  Left = 914
  Top = 270
  Width = 727
  Height = 606
  Caption = 'ECG Analysis'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 705
    Height = 569
    ActivePage = ECGPage
    TabOrder = 0
    OnChange = PageControlChange
    object ECGPage: TTabSheet
      Caption = 'ECG'
      object ECGControlGrp: TGroupBox
        Left = 8
        Top = 0
        Width = 161
        Height = 529
        TabOrder = 0
        object GroupBox8: TGroupBox
          Left = 8
          Top = 8
          Width = 145
          Height = 169
          Caption = ' Source '
          TabOrder = 0
          object Label6: TLabel
            Left = 24
            Top = 16
            Width = 16
            Height = 14
            Caption = 'Ch.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object rbAllRecords: TRadioButton
            Left = 8
            Top = 40
            Width = 81
            Height = 18
            Hint = 'Analysis all record in the data file'
            Caption = 'Whole file'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            TabStop = True
          end
          object rbRange: TRadioButton
            Left = 8
            Top = 56
            Width = 57
            Height = 17
            Hint = 'Analysis a limited range of records within the data file'
            Caption = 'Range'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
          object edRange: TRangeEdit
            Left = 24
            Top = 72
            Width = 105
            Height = 20
            Hint = 'Limits of sub-range within recording  to be scanned for events'
            AutoSize = False
            ShowHint = True
            Text = ' 0 - 1E030 s '
            HiValue = 1.000000015047466E30
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.5g - %.5g s'
          end
          object cbChannel: TComboBox
            Left = 40
            Top = 16
            Width = 89
            Height = 23
            Hint = 'Channel containing events to be detected'
            ItemHeight = 15
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            Text = 'cbChannel'
            OnChange = cbChannelChange
          end
          object GroupBox4: TGroupBox
            Left = 8
            Top = 96
            Width = 129
            Height = 65
            TabOrder = 4
            object rbShowRaw: TRadioButton
              Left = 8
              Top = 8
              Width = 113
              Height = 17
              Caption = 'Raw ECG'
              Checked = True
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 0
              TabStop = True
              OnClick = rbShowRawClick
            end
            object rbShowHPFiltered: TRadioButton
              Left = 8
              Top = 24
              Width = 113
              Height = 17
              Caption = 'HP Filtered'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 1
              OnClick = rbShowRawClick
            end
            object rbShowSubtracted: TRadioButton
              Left = 8
              Top = 40
              Width = 113
              Height = 17
              Caption = 'Avg. Subtracted'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 2
              OnClick = rbShowRawClick
            end
          end
        end
        object GroupBox2: TGroupBox
          Left = 8
          Top = 240
          Width = 145
          Height = 161
          Caption = ' R Wave Detection '
          TabOrder = 1
          object Label1: TLabel
            Left = 24
            Top = 72
            Width = 50
            Height = 14
            Caption = 'Dead Time'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object Label2: TLabel
            Left = 24
            Top = 96
            Width = 48
            Height = 14
            Caption = 'Threshold'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object edDeadTime: TValidatedEdit
            Left = 80
            Top = 72
            Width = 57
            Height = 20
            Hint = 'Minimum time interval between '
            AutoSize = False
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            Text = ' 0 ms'
            Scale = 1000.000000000000000000
            Units = 'ms'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object GroupBox3: TGroupBox
            Left = 8
            Top = 16
            Width = 129
            Height = 53
            Caption = ' R wave '
            TabOrder = 1
            object rbPositiveRWave: TRadioButton
              Left = 8
              Top = 16
              Width = 65
              Height = 17
              Caption = 'Positive'
              Checked = True
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 0
              TabStop = True
            end
            object rbNegativeRWave: TRadioButton
              Left = 8
              Top = 32
              Width = 65
              Height = 17
              Caption = 'Negative'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 1
            end
          end
          object edThreshold: TValidatedEdit
            Left = 80
            Top = 96
            Width = 57
            Height = 20
            AutoSize = False
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            Text = ' 0 %'
            Scale = 100.000000000000000000
            Units = '%'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object bDetect: TButton
            Left = 8
            Top = 120
            Width = 129
            Height = 17
            Caption = 'Detect R waves'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 3
            OnClick = bDetectClick
          end
        end
        object GroupBox6: TGroupBox
          Left = 8
          Top = 400
          Width = 145
          Height = 121
          Caption = ' Beat Average '
          TabOrder = 2
          object bComputeAverageECG: TButton
            Left = 8
            Top = 24
            Width = 129
            Height = 17
            Caption = 'Compute Average'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = bComputeAverageECGClick
          end
          object bSubtractECGAverage: TButton
            Left = 8
            Top = 48
            Width = 129
            Height = 17
            Caption = 'Subtract Average'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = bSubtractECGAverageClick
          end
          object ckUseLPFilter: TCheckBox
            Left = 8
            Top = 64
            Width = 129
            Height = 17
            Caption = 'Low pass filter'
            Checked = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            State = cbChecked
            TabOrder = 2
          end
        end
        object GroupBox5: TGroupBox
          Left = 8
          Top = 176
          Width = 145
          Height = 65
          Caption = ' High Pass Filter '
          TabOrder = 3
          object bDigitalFilter: TButton
            Left = 8
            Top = 16
            Width = 97
            Height = 17
            Caption = 'Apply Filter'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = bDigitalFilterClick
          end
          object cbHPFilter: TComboBox
            Left = 8
            Top = 34
            Width = 99
            Height = 23
            ItemHeight = 15
            TabOrder = 1
            Text = 'cbHPFilter'
          end
        end
      end
      object ECGGrp: TGroupBox
        Left = 176
        Top = 0
        Width = 497
        Height = 249
        Caption = ' ECG '
        TabOrder = 1
        object scECGDisplay: TScopeDisplay
          Left = 8
          Top = 16
          Width = 481
          Height = 161
          OnMouseDown = scECGDisplayMouseDown
          OnCursorChange = scECGDisplayCursorChange
          CursorChangeInProgress = False
          NumChannels = 1
          NumPoints = 1024
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
          DisplayGrid = True
          MaxADCValue = 2047
          MinADCValue = -2048
          NumBytesPerSample = 2
          FixZeroLevels = False
          DisplaySelected = False
          FontSize = 8
        end
        object lbRWave: TLabel
          Left = 16
          Top = 200
          Width = 31
          Height = 14
          Caption = 'Beat #'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lbECGDisplay: TLabel
          Left = 208
          Top = 232
          Width = 64
          Height = 14
          Caption = 'lbSpectrum'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object sbECGDisplay: TScrollBar
          Left = 8
          Top = 178
          Width = 481
          Height = 17
          Hint = 'ECG display scroll bar'
          PageSize = 0
          TabOrder = 0
          OnChange = sbECGDisplayChange
        end
        object sbRWave: TScrollBar
          Left = 160
          Top = 200
          Width = 329
          Height = 19
          Hint = 'ECG Beat # selection bar'
          Min = 1
          PageSize = 0
          Position = 1
          TabOrder = 1
          OnChange = sbRWaveChange
        end
        object edRWave: TRangeEdit
          Left = 52
          Top = 200
          Width = 105
          Height = 20
          Hint = 'ECG beat # on display'
          OnKeyPress = edRWaveKeyPress
          AutoSize = False
          Text = ' 1 / 1.00000001504746624E30 '
          LoValue = 1.000000000000000000
          HiValue = 1.000000015047466E30
          LoLimit = 1.000000000000000000
          HiLimit = 1.000000015047466E30
          Scale = 1.000000000000000000
          NumberFormat = '%.0f / %.0f'
        end
      end
      object AvgGrp: TGroupBox
        Left = 176
        Top = 256
        Width = 497
        Height = 257
        Caption = ' Average ECG Beat '
        TabOrder = 2
        object scAvgDisplay: TScopeDisplay
          Left = 8
          Top = 16
          Width = 481
          Height = 161
          OnMouseDown = scAvgDisplayMouseDown
          OnCursorChange = scAvgDisplayCursorChange
          CursorChangeInProgress = False
          NumChannels = 1
          NumPoints = 1024
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
          DisplayGrid = True
          MaxADCValue = 2047
          MinADCValue = -2048
          NumBytesPerSample = 2
          FixZeroLevels = False
          DisplaySelected = False
          FontSize = 8
        end
        object lbAvgI0Cursor: TLabel
          Left = 112
          Top = 224
          Width = 3
          Height = 15
          Caption = '|'
        end
        object lbAvgI1Cursor: TLabel
          Left = 144
          Top = 224
          Width = 3
          Height = 15
          Caption = '|'
        end
        object shAvgI0I1Line: TShape
          Left = 224
          Top = 241
          Width = 145
          Height = 1
        end
        object lbAvgDisplay: TLabel
          Left = 8
          Top = 184
          Width = 51
          Height = 14
          Caption = 'Average #'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lbDisplayPoints: TLabel
          Left = 385
          Top = 208
          Width = 48
          Height = 14
          Caption = 'No. Points'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object sbAvgDisplay: TScrollBar
          Left = 160
          Top = 186
          Width = 329
          Height = 17
          PageSize = 0
          TabOrder = 0
          OnChange = sbAvgDisplayChange
        end
        object edAvgDisplay: TRangeEdit
          Left = 64
          Top = 184
          Width = 89
          Height = 20
          OnKeyPress = edAvgDisplayKeyPress
          AutoSize = False
          Text = ' 1 / 1.00000001504746624E30 '
          LoValue = 1.000000000000000000
          HiValue = 1.000000015047466E30
          LoLimit = 1.000000000000000000
          HiLimit = 1.000000015047466E30
          Scale = 1.000000000000000000
          NumberFormat = '%.0f / %.0f'
        end
        object edECGDisplayPoints: TValidatedEdit
          Left = 440
          Top = 208
          Width = 49
          Height = 20
          Hint = 'No. of sample points in display'
          OnKeyPress = edECGDisplayPointsKeyPress
          AutoSize = False
          Text = ' 256 '
          Value = 256.000000000000000000
          Scale = 1.000000000000000000
          NumberFormat = '%.0f'
          LoLimit = 256.000000000000000000
          HiLimit = 1.000000015047466E29
        end
      end
    end
    object SpectrumPage: TTabSheet
      Caption = 'Power Spectra'
      ImageIndex = 1
      object WindowGrp: TGroupBox
        Left = 8
        Top = 0
        Width = 681
        Height = 257
        Caption = ' Time Window Records '
        TabOrder = 0
        object scSpecDisplay: TScopeDisplay
          Left = 176
          Top = 24
          Width = 465
          Height = 193
          OnMouseDown = scSpecDisplayMouseDown
          OnCursorChange = scSpecDisplayCursorChange
          CursorChangeInProgress = False
          NumChannels = 1
          NumPoints = 1024
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
          DisplayGrid = True
          MaxADCValue = 2047
          MinADCValue = -2048
          NumBytesPerSample = 2
          FixZeroLevels = False
          DisplaySelected = False
          FontSize = 8
        end
        object lbSpecDisplay: TLabel
          Left = 352
          Top = 232
          Width = 64
          Height = 14
          Caption = 'lbSpectrum'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object GroupBox9: TGroupBox
          Left = 8
          Top = 16
          Width = 161
          Height = 233
          TabOrder = 0
          object Label4: TLabel
            Left = 24
            Top = 16
            Width = 44
            Height = 14
            Caption = 'Record #'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object edSpecDisplay: TRangeEdit
            Left = 72
            Top = 16
            Width = 81
            Height = 20
            OnKeyPress = edSpecDisplayKeyPress
            AutoSize = False
            Text = ' 1 / 1.00000001504746624E30 '
            LoValue = 1.000000000000000000
            HiValue = 1.000000015047466E30
            LoLimit = 1.000000000000000000
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.0f / %.0f'
          end
          object sbSpecDisplay: TScrollBar
            Left = 8
            Top = 40
            Width = 145
            Height = 17
            Min = 1
            PageSize = 0
            Position = 1
            TabOrder = 1
            OnChange = sbSpecDisplayChange
          end
          object ckRejected: TCheckBox
            Left = 8
            Top = 64
            Width = 81
            Height = 17
            Caption = 'Rejected'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            OnClick = ckRejectedClick
          end
          object meWindowResults: TMemo
            Left = 8
            Top = 88
            Width = 145
            Height = 49
            Lines.Strings = (
              'meSpecResults')
            ReadOnly = True
            TabOrder = 3
          end
          object GroupBox16: TGroupBox
            Left = 8
            Top = 144
            Width = 145
            Height = 81
            Caption = ' Auto Rejection '
            TabOrder = 4
            object Label8: TLabel
              Left = 8
              Top = 16
              Width = 75
              Height = 15
              Caption = 'Max. Variance'
            end
            object bRejectRecords: TButton
              Left = 8
              Top = 56
              Width = 105
              Height = 17
              Caption = 'Reject Records'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 0
              OnClick = bRejectRecordsClick
            end
            object edVarianceLimit: TValidatedEdit
              Left = 8
              Top = 32
              Width = 105
              Height = 20
              AutoSize = False
              Text = ' 0 uV^2'
              Scale = 1.000000000000000000
              Units = 'uV^2'
              NumberFormat = '%.4g'
              LoLimit = -1.000000015047466E29
              HiLimit = 1.000000015047466E29
            end
          end
        end
      end
      object SpectrumGrp: TGroupBox
        Left = 8
        Top = 272
        Width = 681
        Height = 257
        Caption = ' Power Spectrum '
        TabOrder = 1
        object plSpectrum: TXYPlotDisplay
          Left = 176
          Top = 24
          Width = 465
          Height = 209
          OnMouseDown = plSpectrumMouseDown
          OnCursorChange = plSpectrumCursorChange
          MaxPointsPerLine = 4096
          XAxisMax = 1.000000000000000000
          XAxisTick = 0.200000002980232200
          XAxisLaw = axLinear
          XAxisLabel = 'Hz'
          XAxisAutoRange = True
          YAxisMax = 1.000000000000000000
          YAxisTick = 0.200000002980232200
          YAxisLaw = axLinear
          YAxisLabel = 'mV^2/Hz'
          YAxisAutoRange = True
          YAxisLabelAtTop = False
          ScreenFontName = 'Arial'
          ScreenFontSize = 10
          LineWidth = 1
          MarkerSize = 10
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
          PrinterLeftMargin = 17
          PrinterRightMargin = 17
          PrinterTopMargin = 11
          PrinterBottomMargin = 11
          PrinterDisableColor = False
          MetafileWidth = 500
          MetafileHeight = 400
        end
        object lbSpectrum: TLabel
          Left = 352
          Top = 232
          Width = 64
          Height = 14
          Caption = 'lbSpectrum'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object SpectrumControlsGrp: TGroupBox
          Left = 8
          Top = 16
          Width = 161
          Height = 233
          TabOrder = 0
          object Label5: TLabel
            Left = 32
            Top = 16
            Width = 55
            Height = 14
            Caption = 'Spectrum #'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object Label7: TLabel
            Left = 24
            Top = 64
            Width = 88
            Height = 15
            Caption = 'Recs./Spectrum'
          end
          object Label10: TLabel
            Left = 8
            Top = 88
            Width = 72
            Height = 15
            Caption = 'Y Axis Range'
          end
          object edSpecNum: TRangeEdit
            Left = 88
            Top = 16
            Width = 65
            Height = 20
            OnKeyPress = edRWaveKeyPress
            AutoSize = False
            Text = ' 1 / 1.00000001504746624E30 '
            LoValue = 1.000000000000000000
            HiValue = 1.000000015047466E30
            LoLimit = 1.000000000000000000
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.0f / %.0f'
          end
          object sbSpecNum: TScrollBar
            Left = 8
            Top = 38
            Width = 145
            Height = 17
            Min = 1
            PageSize = 0
            Position = 1
            TabOrder = 1
            OnChange = sbSpecNumChange
          end
          object meSpecResults: TMemo
            Left = 8
            Top = 112
            Width = 145
            Height = 113
            Lines.Strings = (
              'meSpecResults')
            ReadOnly = True
            TabOrder = 2
          end
          object edNumRecordsPerSpectrum: TValidatedEdit
            Left = 120
            Top = 64
            Width = 33
            Height = 20
            OnKeyPress = edNumRecordsPerSpectrumKeyPress
            AutoSize = False
            Text = ' 0 '
            Scale = 1.000000000000000000
            NumberFormat = '%.0f'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object edSpectrumYMax: TValidatedEdit
            Left = 80
            Top = 88
            Width = 73
            Height = 20
            OnKeyPress = edSpectrumYMaxKeyPress
            AutoSize = False
            Text = ' 0 uv^2'
            Scale = 1.000000000000000000
            Units = 'uv^2'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
      end
    end
    object PlotPage: TTabSheet
      Caption = 'Graph Plots'
      ImageIndex = 2
      object PlotControlsGrp: TGroupBox
        Left = 8
        Top = 0
        Width = 169
        Height = 505
        TabOrder = 0
        object GroupBox14: TGroupBox
          Left = 8
          Top = 8
          Width = 153
          Height = 169
          TabOrder = 0
          object Label11: TLabel
            Left = 8
            Top = 16
            Width = 69
            Height = 15
            Caption = 'Plot Variable'
          end
          object cbPlotVar: TComboBox
            Left = 8
            Top = 32
            Width = 137
            Height = 23
            ItemHeight = 0
            TabOrder = 0
            Text = 'cbPlotVar'
          end
          object bPlotGraph: TButton
            Left = 8
            Top = 120
            Width = 105
            Height = 17
            Caption = 'Plot Graph'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = bPlotGraphClick
          end
          object GroupBox15: TGroupBox
            Left = 8
            Top = 64
            Width = 137
            Height = 49
            Caption = ' AF Frequency Band '
            TabOrder = 2
            object edAFFrequencyBand: TRangeEdit
              Left = 8
              Top = 16
              Width = 121
              Height = 23
              Text = ' 3.0 - 12.0 Hz'
              LoValue = 3.000000000000000000
              HiValue = 12.000000000000000000
              HiLimit = 1.000000015047466E30
              Scale = 1.000000000000000000
              Units = 'Hz'
              NumberFormat = '%.1f - %.1f'
            end
          end
          object bSePlotAxes: TButton
            Left = 8
            Top = 140
            Width = 81
            Height = 17
            Caption = 'Set Axes'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 3
            OnClick = bSePlotAxesClick
          end
        end
        object GroupBox17: TGroupBox
          Left = 8
          Top = 184
          Width = 153
          Height = 113
          Caption = ' Inclusion Criteria '
          TabOrder = 1
          object Label12: TLabel
            Left = 56
            Top = 16
            Width = 90
            Height = 14
            Caption = 'Records/Spectrum'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object Label13: TLabel
            Left = 32
            Top = 56
            Width = 113
            Height = 14
            Caption = 'Peak Frequency Power'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object Label14: TLabel
            Left = 22
            Top = 72
            Width = 16
            Height = 16
            Caption = '>='
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label15: TLabel
            Left = 94
            Top = 32
            Width = 16
            Height = 16
            Caption = '>='
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edRecordsPerSpectrumRequired: TValidatedEdit
            Left = 112
            Top = 32
            Width = 33
            Height = 20
            AutoSize = False
            Text = ' 1 '
            Value = 1.000000000000000000
            Scale = 1.000000000000000000
            NumberFormat = '%.0f'
            LoLimit = 1.000000000000000000
            HiLimit = 1.000000015047466E29
          end
          object edPeakAmplitudeRequired: TValidatedEdit
            Left = 40
            Top = 72
            Width = 105
            Height = 20
            AutoSize = False
            Text = ' 0 uV^2/Hz'
            Scale = 1.000000000000000000
            Units = 'uV^2/Hz'
            NumberFormat = '%.3g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
      end
      object PlotGrp: TGroupBox
        Left = 184
        Top = 0
        Width = 505
        Height = 385
        TabOrder = 1
        object plPlot: TXYPlotDisplay
          Left = 8
          Top = 16
          Width = 481
          Height = 337
          OnCursorChange = plPlotCursorChange
          MaxPointsPerLine = 4096
          XAxisMax = 1.000000000000000000
          XAxisTick = 0.200000002980232200
          XAxisLaw = axLinear
          XAxisLabel = 'X Axis'
          XAxisAutoRange = True
          YAxisMax = 1.000000000000000000
          YAxisTick = 0.200000002980232200
          YAxisLaw = axLinear
          YAxisLabel = 'Y Axis'
          YAxisAutoRange = True
          YAxisLabelAtTop = False
          ScreenFontName = 'Arial'
          ScreenFontSize = 10
          LineWidth = 1
          MarkerSize = 10
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
          PrinterLeftMargin = 17
          PrinterRightMargin = 17
          PrinterTopMargin = 11
          PrinterBottomMargin = 11
          PrinterDisableColor = False
          MetafileWidth = 500
          MetafileHeight = 400
        end
        object lbPlot: TLabel
          Left = 296
          Top = 360
          Width = 64
          Height = 14
          Caption = 'lbSpectrum'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
    end
  end
  object ECGFile: TADCDataFile
    NumChannelsPerScan = 1
    NumBytesPerSample = 2
    NumScansPerRecord = 512
    FloatingPointSamples = False
    MaxADCValue = 2047
    MinADCValue = -2048
    RecordNum = 0
    NumFileHeaderBytes = 0
    WCPNumZeroAvg = 0
    WCPRecordAccepted = False
    ABFAcquisitionMode = ftGapFree
    EDREventDetectorChannel = 0
    EDREventDetectorRecordSize = 0
    EDRVarianceRecordSize = 0
    EDRVarianceRecordOverlap = 0
    EDRBackedUp = False
    ASCIISeparator = #9
    ASCIITimeDataInCol0 = True
    ASCIITimeUnits = 's'
    ASCIITitleLines = 2
    ASCIIFixedRecordSize = False
    Top = 560
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Top = 528
  end
end
