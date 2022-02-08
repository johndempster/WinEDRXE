object EventDetFrm: TEventDetFrm
  Left = 855
  Top = 131
  Caption = 'Event Detection'
  ClientHeight = 854
  ClientWidth = 885
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    885
    854)
  PixelsPerInch = 96
  TextHeight = 15
  object Page: TPageControl
    Left = 8
    Top = 8
    Width = 869
    Height = 838
    ActivePage = DetectEventsPage
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PageChange
    ExplicitHeight = 817
    object DetectEventsPage: TTabSheet
      Caption = 'Detect Events'
      ExplicitLeft = 0
      ExplicitHeight = 787
      DesignSize = (
        861
        808)
      object scDisplay: TScopeDisplay
        Left = 221
        Top = 3
        Width = 399
        Height = 107
        OnCursorChange = scDisplayCursorChange
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
        FloatingPointSamples = False
        FixZeroLevels = False
        DisplaySelected = False
        FontSize = 8
      end
      object scDetDisplay: TScopeDisplay
        Left = 221
        Top = 185
        Width = 401
        Height = 107
        OnCursorChange = scDetDisplayCursorChange
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
        FloatingPointSamples = False
        FixZeroLevels = False
        DisplaySelected = False
        FontSize = 8
      end
      object sbDisplay: TScrollBar
        Left = 221
        Top = 764
        Width = 637
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        PageSize = 0
        TabOrder = 0
        OnChange = sbDisplayChange
        ExplicitTop = 743
        ExplicitWidth = 607
      end
      object DetDisplayPanel: TPanel
        Left = 713
        Top = 782
        Width = 145
        Height = 23
        Anchors = [akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 683
        ExplicitTop = 761
        object Label18: TLabel
          Left = 9
          Top = 2
          Width = 31
          Height = 15
          Alignment = taRightJustify
          Caption = 'Width'
        end
        object edDetDisplayWidth: TValidatedEdit
          Left = 60
          Top = 1
          Width = 65
          Height = 20
          OnKeyPress = edDetDisplayWidthKeyPress
          AutoSize = False
          Text = ' 0.1667 s'
          Value = 10.001980781555180000
          Scale = 0.016666699200868610
          Units = 's'
          NumberFormat = '%.4g'
          LoLimit = 10.000000000000000000
          HiLimit = 1.000000015047466E29
        end
        object bTDisplayDouble: TButton
          Left = 126
          Top = 2
          Width = 16
          Height = 18
          Caption = '4'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Webdings'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = bTDisplayDoubleClick
        end
        object bTDisplayHalf: TButton
          Left = 43
          Top = 2
          Width = 16
          Height = 18
          Caption = '3'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Webdings'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          OnClick = bTDisplayHalfClick
        end
      end
      object DetectGrp: TGroupBox
        Left = 6
        Top = 3
        Width = 209
        Height = 802
        Anchors = [akLeft, akTop, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        object bDetect: TButton
          Left = 9
          Top = 13
          Width = 145
          Height = 21
          Hint = 'Start scanning for events'
          Caption = 'Detect Events'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = bDetectClick
        end
        object bAbort: TButton
          Left = 160
          Top = 16
          Width = 41
          Height = 17
          Hint = 'Abort event detection scan'
          Caption = 'Abort'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = bAbortClick
        end
        object GroupBox8: TGroupBox
          Left = 8
          Top = 40
          Width = 193
          Height = 105
          Caption = ' Source '
          TabOrder = 2
          object Label6: TLabel
            Left = 26
            Top = 16
            Width = 47
            Height = 15
            Alignment = taRightJustify
            Caption = 'Channel'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
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
            Width = 161
            Height = 21
            Hint = 'Limits of sub-range within recording  to be scanned for events'
            AutoSize = False
            ShowHint = True
            Text = ' 0 - 1E030 s '
            HiValue = 1.000000015047466E30
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.6g - %.6g s'
          end
          object cbChannel: TComboBox
            Left = 80
            Top = 16
            Width = 105
            Height = 23
            Hint = 'Channel containing events to be detected'
            Style = csDropDownList
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            OnChange = cbChannelChange
          end
        end
        object CriteriaGrp: TGroupBox
          Left = 8
          Top = 151
          Width = 193
          Height = 410
          Caption = ' Detector '
          TabOrder = 3
          object GroupBox1: TGroupBox
            Left = 8
            Top = 16
            Width = 177
            Height = 70
            Caption = ' Mode '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            object rbRateOfRise: TRadioButton
              Left = 8
              Top = 30
              Width = 97
              Height = 17
              Hint = 'Detect events using rate of rise'
              Caption = 'Rate of Rise'
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
              OnClick = rbThresholdClick
            end
            object rbPatternMatch: TRadioButton
              Left = 8
              Top = 46
              Width = 97
              Height = 17
              Hint = 
                'Detect events using by matching with a decaying exponential temp' +
                'late'
              Caption = 'Template'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              OnClick = rbThresholdClick
            end
            object rbThreshold: TRadioButton
              Left = 8
              Top = 14
              Width = 97
              Height = 17
              Hint = 'Detect events using amplitude/time based threshold'
              Caption = 'Threshold'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 2
              OnClick = rbThresholdClick
            end
          end
          object GroupBox2: TGroupBox
            Left = 8
            Top = 86
            Width = 177
            Height = 95
            Caption = ' Thresholds '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            object Label8: TLabel
              Left = 8
              Top = 18
              Width = 76
              Height = 19
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Amplitude'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object lbTimeThreshold: TLabel
              Left = 8
              Top = 44
              Width = 76
              Height = 19
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Time'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object edThreshold: TValidatedEdit
              Left = 88
              Top = 18
              Width = 81
              Height = 21
              Hint = 'Detection threshold level'
              OnKeyPress = edThresholdKeyPress
              AutoSize = False
              ShowHint = True
              Text = ' 0 %'
              Scale = 1.000000000000000000
              Units = '%'
              NumberFormat = '%.5g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1.000000015047466E30
            end
            object edTimeThreshold: TValidatedEdit
              Left = 88
              Top = 44
              Width = 81
              Height = 21
              Hint = 
                'Duration of time that threshold has to be exceeded before detect' +
                'ion is accepted'
              OnKeyPress = edTimeThresholdKeyPress
              AutoSize = False
              ShowHint = True
              Text = ' 0 ms'
              Scale = 1000.000000000000000000
              Units = 'ms'
              NumberFormat = '%.5g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1.000000015047466E30
            end
            object bSetThresholdTo4SD: TButton
              Left = 10
              Top = 70
              Width = 159
              Height = 17
              Hint = 'Set threshold to 4x standard deviation of detection criterion'
              Caption = 'Set Ampl. = 4 x S.D.'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 2
              OnClick = bSetThresholdTo4SDClick
            end
          end
          object ModePage: TNotebook
            Left = 7
            Top = 187
            Width = 183
            Height = 73
            PageIndex = 2
            TabOrder = 2
            object TPage
              Left = 0
              Top = 0
              Caption = 'Threshold'
              object GroupBox7: TGroupBox
                Left = 4
                Top = 4
                Width = 177
                Height = 64
                Caption = ' Baseline Tracking '
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                object Label25: TLabel
                  Left = 20
                  Top = 18
                  Width = 64
                  Height = 15
                  Alignment = taRightJustify
                  Caption = 'Avg. Interval'
                end
                object edBaselineAveragingInterval: TValidatedEdit
                  Left = 90
                  Top = 18
                  Width = 80
                  Height = 21
                  Hint = 'Averaging interval for running mean  baseline tracking'
                  OnKeyPress = edBaselineAveragingIntervalKeyPress
                  AutoSize = False
                  ShowHint = True
                  Text = ' 1000 ms'
                  Value = 1.000000000000000000
                  Scale = 1000.000000000000000000
                  Units = 'ms'
                  NumberFormat = '%.6g'
                  LoLimit = -1.000000015047466E30
                  HiLimit = 1000000.000000000000000000
                end
                object ckEnableBaselineTracking: TCheckBox
                  Left = 59
                  Top = 39
                  Width = 110
                  Height = 22
                  Hint = 'Enable baseline tracking'
                  Alignment = taLeftJustify
                  Caption = 'Enable Tracking'
                  Checked = True
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clBlack
                  Font.Height = -12
                  Font.Name = 'Arial'
                  Font.Style = [fsBold]
                  ParentFont = False
                  ParentShowHint = False
                  ShowHint = True
                  State = cbChecked
                  TabOrder = 1
                  OnClick = ckEnableBaselineTrackingClick
                end
              end
            end
            object TPage
              Left = 0
              Top = 0
              Caption = 'RateOfRise'
              object GroupBox11: TGroupBox
                Left = 4
                Top = 0
                Width = 177
                Height = 70
                TabOrder = 0
              end
            end
            object TPage
              Left = 0
              Top = 0
              Caption = 'Template'
              object GroupBox9: TGroupBox
                Left = 1
                Top = 3
                Width = 177
                Height = 70
                Caption = ' Template '
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                object Label10: TLabel
                  Left = 8
                  Top = 16
                  Width = 76
                  Height = 20
                  Alignment = taRightJustify
                  AutoSize = False
                  Caption = 'Tau(rise)'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clBlack
                  Font.Height = -12
                  Font.Name = 'Arial'
                  Font.Style = []
                  ParentFont = False
                  WordWrap = True
                end
                object Label12: TLabel
                  Left = 8
                  Top = 42
                  Width = 76
                  Height = 20
                  Alignment = taRightJustify
                  AutoSize = False
                  Caption = 'Tau(decay)'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clBlack
                  Font.Height = -12
                  Font.Name = 'Arial'
                  Font.Style = []
                  ParentFont = False
                  WordWrap = True
                end
                object edTauRise: TValidatedEdit
                  Left = 88
                  Top = 16
                  Width = 80
                  Height = 21
                  OnKeyPress = edTauRiseKeyPress
                  AutoSize = False
                  Text = ' 0.1 ms'
                  Value = 0.000100000004749745
                  Scale = 1000.000000000000000000
                  Units = 'ms'
                  NumberFormat = '%.4g'
                  LoLimit = -1.000000015047466E30
                  HiLimit = 1000000.000000000000000000
                end
                object edTauDecay: TValidatedEdit
                  Left = 88
                  Top = 42
                  Width = 80
                  Height = 21
                  OnKeyPress = edTauRiseKeyPress
                  AutoSize = False
                  Text = ' 10 ms'
                  Value = 0.009999999776482582
                  Scale = 1000.000000000000000000
                  Units = 'ms'
                  NumberFormat = '%.6g'
                  LoLimit = -1.000000015047466E30
                  HiLimit = 1000000.000000000000000000
                end
              end
            end
          end
          object GroupBox10: TGroupBox
            Left = 8
            Top = 326
            Width = 177
            Height = 43
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            object Label23: TLabel
              Left = 22
              Top = 8
              Width = 61
              Height = 15
              Alignment = taRightJustify
              Caption = 'Dead Time'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object edDeadTime: TValidatedEdit
              Left = 88
              Top = 8
              Width = 81
              Height = 21
              Hint = 
                'Time interval after detection during which no further events can' +
                ' be detected'
              OnKeyPress = edDeadTimeKeyPress
              AutoSize = False
              ShowHint = True
              Text = ' 1000 ms'
              Value = 1.000000000000000000
              Scale = 1000.000000000000000000
              Units = 'ms'
              NumberFormat = '%.6g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1000000.000000000000000000
            end
          end
          object gpEventAlignment: TGroupBox
            Left = 8
            Top = 266
            Width = 177
            Height = 52
            Caption = ' Event Alignment '
            TabOrder = 4
            object cbEventAlignment: TComboBox
              Left = 8
              Top = 20
              Width = 161
              Height = 23
              Hint = 'Event alignment point on rising edge of event waveform'
              Style = csDropDownList
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              OnChange = cbEventAlignmentChange
            end
          end
        end
      end
    end
    object EditEventsPage: TTabSheet
      Caption = 'Review/Edit Events'
      ImageIndex = 1
      ExplicitLeft = 12
      ExplicitHeight = 787
      DesignSize = (
        861
        808)
      object scMarkDisplay: TScopeDisplay
        Left = 224
        Top = 118
        Width = 409
        Height = 107
        OnCursorChange = scDetDisplayCursorChange
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
        ZoomDisableVertical = True
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
        FloatingPointSamples = False
        FixZeroLevels = False
        DisplaySelected = False
        FontSize = 8
      end
      object scEditDisplay: TScopeDisplay
        Left = 224
        Top = 8
        Width = 409
        Height = 107
        OnMouseUp = scEditDisplayMouseUp
        OnCursorChange = scEditDisplayCursorChange
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
        FloatingPointSamples = False
        FixZeroLevels = False
        DisplaySelected = False
        FontSize = 8
      end
      object EditDisplayWidthPanel: TPanel
        Left = 568
        Top = 679
        Width = 281
        Height = 25
        Anchors = [akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 3
        ExplicitTop = 658
        object lbEditDisplayPoints: TLabel
          Left = 6
          Top = 2
          Width = 113
          Height = 15
          Alignment = taRightJustify
          Caption = 'Width / Pre-detection'
        end
        object edEditDisplayWidth: TValidatedEdit
          Left = 140
          Top = 2
          Width = 77
          Height = 20
          Hint = 'No. of A/D sample points in display window'
          OnKeyPress = edEditDisplayWidthKeyPress
          AutoSize = False
          ShowHint = True
          Text = ' 10000 ms'
          Value = 10000.000000000000000000
          Scale = 1.000000000000000000
          Units = 'ms'
          NumberFormat = '%.5g'
          LoLimit = 10.000000000000000000
          HiLimit = 1.000000015047466E29
        end
        object edPreTrigger: TValidatedEdit
          Left = 240
          Top = 2
          Width = 41
          Height = 20
          Hint = '% of samples before detection point'
          OnKeyPress = edPreTriggerKeyPress
          AutoSize = False
          Text = ' 20 %'
          Value = 0.200000002980232200
          Scale = 100.000000000000000000
          Units = '%'
          NumberFormat = '%.0f'
          LoLimit = -1.000000015047466E30
          HiLimit = 100.000000000000000000
        end
        object bDoubleEditDisplayWidth: TButton
          Left = 220
          Top = 2
          Width = 16
          Height = 18
          Caption = '4'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Webdings'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          OnClick = bDoubleEditDisplayWidthClick
        end
        object bEditDisplayWidthHalve: TButton
          Left = 123
          Top = 2
          Width = 16
          Height = 18
          Caption = '3'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Webdings'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          OnClick = bEditDisplayWidthHalveClick
        end
      end
      object EditEventsGrp: TGroupBox
        Left = 4
        Top = 4
        Width = 209
        Height = 796
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 0
        ExplicitHeight = 775
        object Label14: TLabel
          Left = 8
          Top = 16
          Width = 30
          Height = 15
          Caption = 'Event'
        end
        object edEvent: TRangeEdit
          Left = 48
          Top = 16
          Width = 153
          Height = 20
          OnKeyPress = edEventKeyPress
          AutoSize = False
          Text = ' 0 / 1.00000001504746622E30 '
          HiValue = 1.000000015047466E30
          HiLimit = 1.000000015047466E30
          Scale = 1.000000000000000000
          NumberFormat = '%.0f / %.0f'
        end
        object sbEvent: TScrollBar
          Left = 48
          Top = 38
          Width = 153
          Height = 17
          Min = 1
          PageSize = 0
          Position = 1
          TabOrder = 1
          OnChange = sbEventChange
        end
        object editGrp: TGroupBox
          Left = 8
          Top = 520
          Width = 193
          Height = 63
          Caption = ' Edit Events '
          TabOrder = 2
          object bInsertEvent: TButton
            Left = 8
            Top = 16
            Width = 85
            Height = 17
            Hint = 
              'Insert a new event into list at time indicated by current cursor' +
              ' position.'
            Caption = 'Insert (F1)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = bInsertEventClick
          end
          object bDeleteEvent: TButton
            Left = 100
            Top = 16
            Width = 85
            Height = 17
            Hint = 'Delete currently selected event from list.'
            Caption = 'Delete  (F2)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = bDeleteEventClick
          end
          object cktCursorAtDetectionPoint: TCheckBox
            Left = 8
            Top = 38
            Width = 177
            Height = 17
            Hint = 'Cursor placed at detection point of displayed event'
            Caption = 'Init. cursor to detection point'
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
        end
        object ExportGrp: TGroupBox
          Left = 8
          Top = 588
          Width = 193
          Height = 120
          Caption = ' Export Events '
          TabOrder = 3
          object Label20: TLabel
            Left = 8
            Top = 85
            Width = 37
            Height = 15
            Alignment = taRightJustify
            Caption = 'Events'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object bExportToWCPFile: TButton
            Left = 9
            Top = 14
            Width = 129
            Height = 17
            Hint = 'Export detected events as a WinWCP data file'
            Caption = 'Export Events'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = bExportToWCPFileClick
          end
          object bExportNonEvents: TButton
            Left = 8
            Top = 36
            Width = 129
            Height = 17
            Hint = 'Export gaps between events as a  WinEDR data file.'
            Caption = 'Export Gaps'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = bExportNonEventsClick
          end
          object edExportRange: TRangeEdit
            Left = 48
            Top = 85
            Width = 137
            Height = 20
            AutoSize = False
            Text = ' 1.00 - 1.00 '
            LoValue = 1.000000000000000000
            HiValue = 1.000000000000000000
            LoLimit = 1.000000000000000000
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.f - %.f'
          end
          object bAbortExport: TButton
            Left = 144
            Top = 16
            Width = 41
            Height = 17
            Caption = 'Abort'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 3
            OnClick = bAbortExportClick
          end
          object bExportAnalysis: TButton
            Left = 8
            Top = 59
            Width = 129
            Height = 17
            Hint = 'Export event waveform measurements as a .csv file'
            Caption = 'Export Analysis'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            OnClick = bExportAnalysisClick
          end
        end
        object AnalysisGrp: TGroupBox
          Left = 8
          Top = 61
          Width = 193
          Height = 455
          Caption = ' Event Analysis '
          TabOrder = 4
          object Label11: TLabel
            Left = 27
            Top = 20
            Width = 47
            Height = 15
            Alignment = taRightJustify
            Caption = 'Channel'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object meResults: TMemo
            Left = 8
            Top = 52
            Width = 177
            Height = 187
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            Lines.Strings = (
              ''
              ''
              ''
              ''
              '')
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
          end
          object gpEventPolarity: TGroupBox
            Left = 8
            Top = 245
            Width = 177
            Height = 35
            Caption = ' Polarity '
            TabOrder = 1
            object rbPositive: TRadioButton
              Left = 8
              Top = 14
              Width = 65
              Height = 17
              Hint = 'Analyse positive-going events'
              Caption = 'Positive'
              Checked = True
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              TabStop = True
              OnClick = rbPositiveClick
            end
            object rbNegative: TRadioButton
              Left = 84
              Top = 14
              Width = 73
              Height = 17
              Hint = 'Analyse negative-going events'
              Caption = 'Negative'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              OnClick = rbPositiveClick
            end
          end
          object GroupBox23: TGroupBox
            Left = 8
            Top = 382
            Width = 177
            Height = 65
            Caption = ' T(X%) decay time'
            TabOrder = 2
            object Label4: TLabel
              Left = 17
              Top = 16
              Width = 48
              Height = 15
              Alignment = taRightJustify
              Caption = 'Decay %'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object Label7: TLabel
              Left = 81
              Top = 16
              Width = 29
              Height = 15
              Caption = 'From'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object edTDecayPercentage: TValidatedEdit
              Left = 8
              Top = 32
              Width = 57
              Height = 21
              OnKeyPress = edTDecayPercentageKeyPress
              AutoSize = False
              Text = ' 90 %'
              Value = 90.000000000000000000
              Scale = 1.000000000000000000
              Units = '%'
              NumberFormat = '%.0f'
              LoLimit = -1.000000015047466E30
              HiLimit = 100.000000000000000000
            end
            object cbDecayFrom: TComboBox
              Left = 80
              Top = 32
              Width = 81
              Height = 23
              Style = csDropDownList
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ItemIndex = 0
              ParentFont = False
              TabOrder = 1
              Text = 'Peak'
              OnChange = cbDecayFromChange
              Items.Strings = (
                'Peak'
                '50% rise'
                'a0 cursor')
            end
          end
          object gpZeroLevel: TGroupBox
            Left = 8
            Top = 286
            Width = 177
            Height = 94
            Caption = ' Zero level  '
            TabOrder = 3
            object Label3: TLabel
              Left = 11
              Top = 65
              Width = 52
              Height = 15
              Alignment = taRightJustify
              Caption = 'Pts. Avgd.'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object Label5: TLabel
              Left = 110
              Top = 65
              Width = 23
              Height = 15
              Alignment = taRightJustify
              Caption = 'Gap'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object ckSubtractBaseline: TCheckBox
              Left = 8
              Top = 42
              Width = 137
              Height = 17
              Hint = 'Remove trends in signal baseline'
              Caption = 'Subt. Baseline Trend'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              OnClick = ckSubtractBaselineClick
            end
            object edZeroNumAvg: TValidatedEdit
              Left = 64
              Top = 65
              Width = 33
              Height = 20
              OnKeyPress = edZeroNumAvgKeyPress
              AutoSize = False
              Text = ' 10 '
              Value = 10.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%.0f'
              LoLimit = 1.000000000000000000
              HiLimit = 99.000000000000000000
            end
            object edZeroGap: TValidatedEdit
              Left = 136
              Top = 65
              Width = 33
              Height = 20
              OnKeyPress = edZeroGapKeyPress
              AutoSize = False
              Text = ' 0 '
              Value = 0.100000001490116100
              Scale = 1.000000000000000000
              NumberFormat = '%.0f'
              LoLimit = 0.100000001490116100
              HiLimit = 99.000000000000000000
            end
            object cbBaseline: TComboBox
              Left = 8
              Top = 16
              Width = 161
              Height = 23
              Hint = 'Zero level position'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 3
              Text = 'cbBaseline'
              OnChange = cbBaselineChange
            end
          end
          object cbReviewChannel: TComboBox
            Left = 80
            Top = 23
            Width = 105
            Height = 23
            Hint = 'Channel containing events to be detected'
            Style = csDropDownList
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            OnClick = cbReviewChannelChange
          end
        end
        object EventListGrp: TGroupBox
          Left = 8
          Top = 710
          Width = 193
          Height = 49
          Caption = ' Event List '
          TabOrder = 5
          object bLoadEventList: TButton
            Left = 100
            Top = 18
            Width = 85
            Height = 17
            Hint = 'Load event position list from file.'
            Caption = 'Load List'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = bLoadEventListClick
          end
          object bSaveEventList: TButton
            Left = 8
            Top = 18
            Width = 85
            Height = 17
            Hint = 'Save current event position list to file.'
            Caption = 'Save List'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = bSaveEventListClick
          end
        end
      end
      object sbEditDisplay: TScrollBar
        Left = 219
        Top = 658
        Width = 630
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        PageSize = 0
        TabOrder = 1
        OnChange = sbEditDisplayChange
        ExplicitTop = 637
      end
      object EventFilterGrp: TGroupBox
        Left = 219
        Top = 698
        Width = 630
        Height = 105
        Anchors = [akLeft, akRight, akBottom]
        Caption = ' Event Filter '
        TabOrder = 2
        ExplicitTop = 677
        object GroupBox4: TGroupBox
          Left = 312
          Top = 12
          Width = 257
          Height = 85
          Caption = ' Event Rejection Criterion '
          TabOrder = 0
          object cbVariable: TComboBox
            Left = 8
            Top = 18
            Width = 89
            Height = 23
            TabOrder = 0
            Text = 'cbVariable'
            OnChange = cbVariableChange
          end
          object GroupBox15: TGroupBox
            Left = 102
            Top = 12
            Width = 147
            Height = 68
            Hint = 
              'Upper and lower limits of event rejection window for selected va' +
              'riable.'
            Caption = ' Limits '
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            object Label15: TLabel
              Left = 8
              Top = 16
              Width = 34
              Height = 15
              Caption = 'Lower'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object Label16: TLabel
              Left = 8
              Top = 38
              Width = 34
              Height = 15
              Caption = 'Upper'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object edLoLimit: TValidatedEdit
              Left = 46
              Top = 14
              Width = 90
              Height = 20
              Hint = 'Analysis variable must be greater than this value '
              OnKeyPress = edLoLimitKeyPress
              AutoSize = False
              Text = ' 0 '
              Scale = 1.000000000000000000
              NumberFormat = '%.6g'
              LoLimit = -1.000000015047466E29
              HiLimit = 1.000000015047466E29
            end
            object edHiLimit: TValidatedEdit
              Left = 46
              Top = 38
              Width = 90
              Height = 20
              OnKeyPress = edHiLimitKeyPress
              AutoSize = False
              Text = ' 0 '
              Scale = 1.000000000000000000
              NumberFormat = '%.6g'
              LoLimit = -1.000000015047466E29
              HiLimit = 1.000000015047466E29
            end
          end
          object bAddFilter: TButton
            Left = 8
            Top = 44
            Width = 89
            Height = 17
            Hint = 'Add event filter criterion to list'
            Caption = 'Add to Filter'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnClick = bAddFilterClick
          end
        end
        object GroupBox18: TGroupBox
          Left = 8
          Top = 12
          Width = 297
          Height = 85
          TabOrder = 1
          object meFilterSet: TMemo
            Left = 6
            Top = 12
            Width = 161
            Height = 65
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            Lines.Strings = (
              'meFilterSet')
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
          end
          object GroupBox14: TGroupBox
            Left = 172
            Top = 8
            Width = 65
            Height = 68
            Caption = ' Combine '
            TabOrder = 1
            object rbAND: TRadioButton
              Left = 8
              Top = 16
              Width = 49
              Height = 17
              Caption = 'AND'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 0
              OnClick = rbANDClick
            end
            object rbOR: TRadioButton
              Left = 8
              Top = 32
              Width = 41
              Height = 17
              Caption = 'OR'
              Checked = True
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 1
              TabStop = True
              OnClick = rbANDClick
            end
          end
          object bDelete: TButton
            Left = 240
            Top = 34
            Width = 49
            Height = 17
            Hint = 'Clear filter event filter criteria list'
            Caption = 'Clear'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnClick = bDeleteClick
          end
          object bApply: TButton
            Left = 240
            Top = 12
            Width = 49
            Height = 17
            Hint = 'Remove events matching filter criteria from list'
            Caption = 'Apply'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            OnClick = bApplyClick
          end
        end
      end
    end
    object XYPlotPage: TTabSheet
      Caption = 'X-Y Plot'
      ImageIndex = 2
      ExplicitWidth = 831
      ExplicitHeight = 787
      DesignSize = (
        861
        808)
      object plPlot: TXYPlotDisplay
        Left = 224
        Top = 8
        Width = 377
        Height = 273
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
        PrinterLeftMargin = 0
        PrinterRightMargin = 0
        PrinterTopMargin = 0
        PrinterBottomMargin = 0
        PrinterDisableColor = False
        MetafileWidth = 500
        MetafileHeight = 400
      end
      object XYPlotGrp: TGroupBox
        Left = 4
        Top = 4
        Width = 209
        Height = 801
        Anchors = [akLeft, akTop, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object GroupBox5: TGroupBox
          Left = 8
          Top = 80
          Width = 193
          Height = 113
          Caption = ' Plot '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object Label9: TLabel
            Left = 8
            Top = 60
            Width = 31
            Height = 15
            Caption = 'Y Axis'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object Label1: TLabel
            Left = 8
            Top = 16
            Width = 31
            Height = 15
            Caption = 'X Axis'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object cbPlotYVar: TComboBox
            Left = 8
            Top = 76
            Width = 177
            Height = 23
            Hint = 'Variable to be plotted on Y axis'
            Style = csDropDownList
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = cbPlotYVarChange
            Items.Strings = (
              'Record No.'
              'Time'
              'Mean (DC)'
              'St. Dev. [(AC)'
              'Variance (AC)'
              'Skew (AC)'
              'Median Freq (AC)')
          end
          object cbPlotXVar: TComboBox
            Left = 8
            Top = 32
            Width = 177
            Height = 23
            Hint = 'Variable to be plotted on X axis'
            Style = csDropDownList
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnChange = cbPlotXVarChange
            Items.Strings = (
              'Record No.'
              'Time'
              'Mean (DC)'
              'St. Dev. [(AC)'
              'Variance (AC)'
              'Skew (AC)'
              'Median Freq (AC)')
          end
        end
        object bNewPlot: TButton
          Left = 8
          Top = 16
          Width = 193
          Height = 21
          Hint = 'Create a new X-Y plot'
          Caption = 'New Plot'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = bNewPlotClick
        end
        object GroupBox6: TGroupBox
          Left = 8
          Top = 200
          Width = 193
          Height = 217
          Caption = ' Events  '
          TabOrder = 2
          object rbPlotAllEvents: TRadioButton
            Left = 8
            Top = 16
            Width = 89
            Height = 17
            Caption = ' All events'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            TabStop = True
          end
          object rbPlotRange: TRadioButton
            Left = 8
            Top = 32
            Width = 89
            Height = 17
            Caption = ' Range'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
          end
          object FrequencyAvgPan: TPanel
            Left = 26
            Top = 48
            Width = 159
            Height = 57
            BevelOuter = bvNone
            TabOrder = 2
            object Label2: TLabel
              Left = 26
              Top = 30
              Width = 39
              Height = 15
              Alignment = taRightJustify
              Caption = 'Interval'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object edAverageInterval: TValidatedEdit
              Left = 70
              Top = 30
              Width = 83
              Height = 20
              Hint = 'Event rate counting interval'
              OnKeyPress = edAverageIntervalKeyPress
              AutoSize = False
              ShowHint = True
              Text = ' 1 s'
              Value = 1.000000000000000000
              Scale = 1.000000000000000000
              Units = 's'
              NumberFormat = '%.3g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1.000000015047466E29
            end
            object edPlotTimeRange: TRangeEdit
              Left = 0
              Top = 5
              Width = 153
              Height = 20
              Hint = 'Sub-range of events to  be included in plot'
              AutoSize = False
              ShowHint = True
              Text = ' 1.00 - 1.00 s'
              LoValue = 1.000000000000000000
              HiValue = 1.000000000000000000
              HiLimit = 1.000000015047466E30
              Scale = 1.000000000000000000
              Units = 's'
              NumberFormat = '%.f - %.f'
            end
          end
          object EventRangePan: TPanel
            Left = 26
            Top = 136
            Width = 153
            Height = 57
            BevelOuter = bvNone
            TabOrder = 3
            object edPlotEventRange: TRangeEdit
              Left = 0
              Top = 5
              Width = 153
              Height = 20
              Hint = 'Sub-range of events to  be included in plot'
              AutoSize = False
              ShowHint = True
              Text = ' 1.00 - 1.00 '
              LoValue = 1.000000000000000000
              HiValue = 1.000000000000000000
              LoLimit = 1.000000000000000000
              HiLimit = 1000.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%.f - %.f'
            end
          end
        end
        object bSetPlotAxes: TButton
          Left = 8
          Top = 40
          Width = 193
          Height = 17
          Hint = 'Customise  X and Y plot axes'
          Caption = 'Set Axes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = bSetPlotAxesClick
        end
        object bPlotAbort: TButton
          Left = 8
          Top = 60
          Width = 49
          Height = 17
          Caption = 'Abort'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 4
          OnClick = bPlotAbortClick
        end
      end
    end
    object HistPage: TTabSheet
      Caption = 'Histogram'
      ImageIndex = 3
      ExplicitWidth = 831
      ExplicitHeight = 787
      DesignSize = (
        861
        808)
      object plHist: TXYPlotDisplay
        Left = 224
        Top = 16
        Width = 425
        Height = 201
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
        PrinterLeftMargin = 0
        PrinterRightMargin = 0
        PrinterTopMargin = 0
        PrinterBottomMargin = 0
        PrinterDisableColor = False
        MetafileWidth = 500
        MetafileHeight = 400
      end
      object HistResultsGrp: TGroupBox
        Left = 219
        Top = 598
        Width = 625
        Height = 207
        Anchors = [akLeft, akRight, akBottom]
        Caption = ' Curve fitting '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        ExplicitTop = 577
        object lbHistResults: THTMLLabel
          Left = 160
          Top = 21
          Width = 449
          Height = 172
          Caption = 'Label'
          Alignment = taLeftJustify
          LineSpacing = 1.000000000000000000
          Color = clWhite
        end
        object bHistFitCurve: TButton
          Left = 8
          Top = 16
          Width = 129
          Height = 17
          Hint = 'Fit a gaussian probability density function to the histogram'
          Caption = 'Fit Curve'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = bHistFitCurveClick
        end
        object cbHistEqn: TComboBox
          Left = 8
          Top = 36
          Width = 129
          Height = 23
          Style = csDropDownList
          TabOrder = 1
          Items.Strings = (
            'None'
            'Gaussian'
            '2 Gaussians'
            '3 Gaussians')
        end
      end
      object HistGrp: TGroupBox
        Left = 4
        Top = 4
        Width = 209
        Height = 801
        Anchors = [akLeft, akTop, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        ExplicitHeight = 780
        object GroupBox12: TGroupBox
          Left = 8
          Top = 8
          Width = 193
          Height = 214
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          object Label17: TLabel
            Left = 40
            Top = 72
            Width = 47
            Height = 15
            Alignment = taRightJustify
            Caption = 'No. Bins'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object cbHistVar: TComboBox
            Left = 8
            Top = 43
            Width = 174
            Height = 23
            Hint = 'Variable to be compiled into histogram'
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbHistVarChange
          end
          object ckPercentage: TCheckBox
            Left = 93
            Top = 190
            Width = 89
            Height = 17
            Hint = 
              'Plot vertical axis of histogram as percentage of events in histo' +
              'gram'
            Caption = 'Percentage'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
          object ckCumulative: TCheckBox
            Left = 93
            Top = 174
            Width = 81
            Height = 17
            Hint = 'Plot vertical axis as cumulative counts in bins'
            Caption = 'Cumulative'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
          end
          object edNumBins: TValidatedEdit
            Left = 93
            Top = 72
            Width = 89
            Height = 20
            Hint = 'No. of bins in histogram'
            OnKeyPress = edNumBinsKeyPress
            AutoSize = False
            Text = ' 50 '
            Value = 50.000000000000000000
            Scale = 1.000000000000000000
            NumberFormat = '%.4g'
            LoLimit = 2.000000000000000000
            HiLimit = 1024.000000000000000000
          end
          object bNewHistogram: TButton
            Left = 8
            Top = 16
            Width = 174
            Height = 21
            Hint = 'Plot a new histogram'
            Caption = 'New Histogram'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            OnClick = bNewHistogramClick
          end
          object BinRangePanel: TPanel
            Left = 15
            Top = 94
            Width = 173
            Height = 73
            BevelOuter = bvNone
            TabOrder = 5
            object Label13: TLabel
              Left = 24
              Top = 2
              Width = 50
              Height = 15
              Alignment = taRightJustify
              Caption = 'Bin width'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object Label19: TLabel
              Left = 10
              Top = 25
              Width = 64
              Height = 15
              Alignment = taRightJustify
              Caption = 'Lower Limit'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object Label22: TLabel
              Left = 10
              Top = 51
              Width = 64
              Height = 15
              Alignment = taRightJustify
              Caption = 'Upper Limit'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object edBinWidth: TValidatedEdit
              Left = 79
              Top = 2
              Width = 89
              Height = 20
              Hint = 'No. of bins in histogram'
              OnKeyPress = edBinWidthKeyPress
              AutoSize = False
              Text = ' 2 '
              Value = 2.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%.4g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1.000000015047466E30
            end
            object edBinsLower: TValidatedEdit
              Left = 79
              Top = 25
              Width = 89
              Height = 20
              Hint = 'No. of bins in histogram'
              OnKeyPress = edBinsLowerKeyPress
              AutoSize = False
              Text = ' 2 '
              Value = 2.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%.4g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1.000000015047466E30
            end
            object edBinsUpper: TValidatedEdit
              Left = 80
              Top = 51
              Width = 89
              Height = 20
              Hint = 'No. of bins in histogram'
              OnKeyPress = edBinsUpperKeyPress
              AutoSize = False
              Text = ' 2 '
              Value = 2.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%.4g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1.000000015047466E30
            end
          end
        end
        object bSetHistAxes: TButton
          Left = 15
          Top = 308
          Width = 174
          Height = 17
          Hint = 'Customise X and Y axes type, range and labels'
          Caption = 'Set Axes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = bSetHistAxesClick
        end
        object GroupBox13: TGroupBox
          Left = 8
          Top = 222
          Width = 193
          Height = 81
          Caption = ' Events  '
          TabOrder = 2
          object rbHistAllEvents: TRadioButton
            Left = 8
            Top = 16
            Width = 89
            Height = 17
            Caption = ' All events'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            TabStop = True
          end
          object rbHistRange: TRadioButton
            Left = 8
            Top = 32
            Width = 89
            Height = 17
            Caption = ' Range'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
          end
          object edHistRange: TRangeEdit
            Left = 32
            Top = 48
            Width = 153
            Height = 20
            Hint = 'Sub-range of events to be included in histogram'
            AutoSize = False
            ShowHint = True
            Text = ' 1.00 - 1.00 '
            LoValue = 1.000000000000000000
            HiValue = 1.000000000000000000
            LoLimit = 1.000000000000000000
            HiLimit = 1000.000000000000000000
            Scale = 1.000000000000000000
            NumberFormat = '%.f - %.f'
          end
        end
        object meHistResults: TMemo
          Left = 8
          Top = 330
          Width = 193
          Height = 113
          ReadOnly = True
          TabOrder = 3
        end
      end
    end
    object AveragePage: TTabSheet
      Caption = 'Average'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitHeight = 787
      DesignSize = (
        861
        808)
      object scAverageDisplay: TScopeDisplay
        Left = 224
        Top = 8
        Width = 409
        Height = 397
        OnMouseUp = scAverageDisplayMouseUp
        OnCursorChange = scAverageDisplayCursorChange
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
        FloatingPointSamples = False
        FixZeroLevels = False
        DisplaySelected = False
        FontSize = 8
      end
      object AverageGrp: TGroupBox
        Left = 4
        Top = 4
        Width = 209
        Height = 801
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 0
        DesignSize = (
          209
          801)
        object GroupBox19: TGroupBox
          Left = 8
          Top = 64
          Width = 193
          Height = 145
          Caption = ' Events  '
          TabOrder = 0
          object rbAverageAllEvents: TRadioButton
            Left = 8
            Top = 16
            Width = 89
            Height = 17
            Caption = ' All events'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            TabStop = True
          end
          object rbAverageRange: TRadioButton
            Left = 8
            Top = 32
            Width = 89
            Height = 17
            Caption = ' Range'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
          end
          object edAverageRange: TRangeEdit
            Left = 32
            Top = 48
            Width = 153
            Height = 20
            Hint = 'Sub-range of events to be included in histogram'
            AutoSize = False
            ShowHint = True
            Text = ' 1.00 - 1.00 '
            LoValue = 1.000000000000000000
            HiValue = 1.000000000000000000
            LoLimit = 1.000000000000000000
            HiLimit = 1000.000000000000000000
            Scale = 1.000000000000000000
            NumberFormat = '%.f - %.f'
          end
          object GroupBox16: TGroupBox
            Left = 8
            Top = 72
            Width = 177
            Height = 65
            TabOrder = 3
            object Label21: TLabel
              Left = 54
              Top = 30
              Width = 59
              Height = 15
              Alignment = taRightJustify
              Caption = 'No. Events'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object ckCountMatchedAvg: TCheckBox
              Left = 8
              Top = 8
              Width = 121
              Height = 25
              Hint = 'Enabled amplitude sorted count-matched averaging'
              Caption = 'Count Matched'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 0
            end
            object edNumCountMatchedAvg: TValidatedEdit
              Left = 120
              Top = 32
              Width = 49
              Height = 23
              Hint = 'No. of events sorted by amplitude to averaged'
              ShowHint = True
              Text = ' 1 '
              Value = 1.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%.4g'
              LoLimit = 1.000000000000000000
              HiLimit = 1.000000015047466E30
            end
          end
        end
        object bDoAverage: TButton
          Left = 8
          Top = 14
          Width = 193
          Height = 21
          Hint = 'Plot a new histogram'
          Caption = 'Compute Average'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = bDoAverageClick
        end
        object GroupBox20: TGroupBox
          Left = 8
          Top = 208
          Width = 193
          Height = 222
          Anchors = [akLeft, akTop, akBottom]
          Caption = ' Event Analysis '
          TabOrder = 2
          ExplicitHeight = 201
          object meAverageResults: TMemo
            Left = 8
            Top = 16
            Width = 177
            Height = 177
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            Lines.Strings = (
              ''
              ''
              ''
              ''
              '')
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
          end
        end
        object bAbortAverage: TButton
          Left = 8
          Top = 40
          Width = 57
          Height = 17
          Hint = 'Abort event detection scan'
          Caption = 'Abort'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = bAbortAverageClick
        end
      end
      object AverageResultsGrp: TGroupBox
        Left = 224
        Top = 594
        Width = 625
        Height = 211
        Anchors = [akLeft, akRight, akBottom]
        Caption = ' Curve fitting '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        ExplicitTop = 573
        object lbAvgFitResults: THTMLLabel
          Left = 160
          Top = 23
          Width = 449
          Height = 185
          Caption = 'Label'
          Alignment = taLeftJustify
          LineSpacing = 1.000000000000000000
          Color = clWhite
        end
        object bAverageFitCurve: TButton
          Left = 8
          Top = 16
          Width = 129
          Height = 17
          Hint = 'Fit a gaussian probability density function to the histogram'
          Caption = 'Fit Curve'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = bAverageFitCurveClick
        end
        object cbAverageEqn: TComboBox
          Left = 8
          Top = 36
          Width = 129
          Height = 23
          Style = csDropDownList
          TabOrder = 1
          Items.Strings = (
            'None'
            'Gaussian'
            '2 Gaussians'
            '3 Gaussians')
        end
      end
    end
  end
  object SaveDialog: TSaveDialog
    Left = 246
    Top = 528
  end
  object OpenDialog: TOpenDialog
    Left = 318
    Top = 560
  end
  object WCPFile: TADCDataFile
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
    ASCIISaveRecordsinColumns = False
    Left = 294
    Top = 522
  end
  object CVFit: TCurveFitter
    Equation = None
    ParametersSet = False
    UseBinWidths = False
    Left = 310
    Top = 474
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Left = 248
    Top = 560
  end
end
