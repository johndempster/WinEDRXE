object PrintPageViewFrm: TPrintPageViewFrm
  Left = 480
  Top = 289
  BorderStyle = bsDialog
  Caption = 'Print Page'
  ClientHeight = 244
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Page: TNotebook
    Left = 136
    Top = 144
    Width = 161
    Height = 105
    PageIndex = 1
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      Caption = 'Print'
      object GroupBox1: TGroupBox
        Left = 4
        Top = 2
        Width = 151
        Height = 95
        Caption = ' Page Margins '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 16
          Width = 19
          Height = 14
          Caption = 'Left'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label2: TLabel
          Left = 8
          Top = 52
          Width = 21
          Height = 14
          Caption = 'Top '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 82
          Top = 16
          Width = 27
          Height = 14
          Caption = 'Right '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 82
          Top = 52
          Width = 33
          Height = 14
          Caption = 'Bottom'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object edLeftMargin: TValidatedEdit
          Left = 8
          Top = 30
          Width = 57
          Height = 20
          AutoSize = False
          Text = ' 0.1 cm'
          Value = 1.000000000000000000
          LoLimit = 1.000000000000000000
          HiLimit = 100.000000000000000000
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
        end
        object edTopMargin: TValidatedEdit
          Left = 8
          Top = 66
          Width = 57
          Height = 20
          AutoSize = False
          Text = ' 0.1 cm'
          Value = 1.000000000000000000
          LoLimit = 1.000000000000000000
          HiLimit = 100.000000000000000000
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
        end
        object edRightMargin: TValidatedEdit
          Left = 82
          Top = 30
          Width = 57
          Height = 20
          AutoSize = False
          Text = ' 0.1 cm'
          Value = 1.000000000000000000
          LoLimit = 1.000000000000000000
          HiLimit = 100.000000000000000000
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
        end
        object edBottomMargin: TValidatedEdit
          Left = 82
          Top = 66
          Width = 57
          Height = 20
          AutoSize = False
          Text = ' 0.1 cm'
          Value = 1.000000000000000000
          LoLimit = 1.000000000000000000
          HiLimit = 100.000000000000000000
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Clipboard'
      object GroupBox2: TGroupBox
        Left = 4
        Top = 2
        Width = 151
        Height = 95
        Caption = ' Image size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label7: TLabel
          Left = 16
          Top = 16
          Width = 27
          Height = 14
          Caption = 'Width'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 18
          Top = 48
          Width = 30
          Height = 14
          Caption = 'Height'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object edWidth: TValidatedEdit
          Left = 58
          Top = 16
          Width = 79
          Height = 20
          AutoSize = False
          Text = ' 500  pixels'
          Value = 500.000000000000000000
          LoLimit = 100.000000000000000000
          HiLimit = 1000000.000000000000000000
          Scale = 1.000000000000000000
          Units = 'pixels'
          NumberFormat = '%.f '
        end
        object edHeight: TValidatedEdit
          Left = 56
          Top = 48
          Width = 84
          Height = 20
          AutoSize = False
          Text = ' 400 pixels'
          Value = 400.000000000000000000
          LoLimit = 100.000000000000000000
          HiLimit = 1000000.000000000000000000
          Scale = 1.000000000000000000
          Units = 'pixels'
          NumberFormat = '%.f'
        end
      end
    end
  end
  object FontGrp: TGroupBox
    Left = 138
    Top = 3
    Width = 153
    Height = 134
    Caption = ' Typeface/Line '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label9: TLabel
      Left = 72
      Top = 45
      Width = 21
      Height = 14
      Caption = 'Size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 40
      Top = 69
      Width = 50
      Height = 14
      Caption = 'Line Width'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object cbFontName: TComboBox
      Left = 8
      Top = 16
      Width = 137
      Height = 23
      ItemHeight = 15
      TabOrder = 0
      Text = 'cbFontName'
    end
    object edFontSize: TValidatedEdit
      Left = 96
      Top = 45
      Width = 49
      Height = 20
      AutoSize = False
      Text = ' 1  pts'
      Value = 1.000000000000000000
      LoLimit = 1.000000000000000000
      HiLimit = 100.000000000000000000
      Scale = 1.000000000000000000
      Units = 'pts'
      NumberFormat = '%.f '
    end
    object edLineThickness: TValidatedEdit
      Left = 96
      Top = 69
      Width = 49
      Height = 20
      AutoSize = False
      Text = ' 1  pts'
      Value = 1.000000000000000000
      LoLimit = 1.000000000000000000
      HiLimit = 100.000000000000000000
      Scale = 1.000000000000000000
      Units = 'pts'
      NumberFormat = '%.f '
    end
    object ckUseColor: TCheckBox
      Left = 72
      Top = 96
      Width = 73
      Height = 17
      Hint = 'Plot signal waveforms in colour'
      Alignment = taLeftJustify
      Caption = 'Use colour'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 3
    end
    object ckShowLabels: TCheckBox
      Left = 64
      Top = 112
      Width = 81
      Height = 17
      Hint = 'Show channel names and calibration bar values on plot'
      Alignment = taLeftJustify
      Caption = 'Show labels'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object bPrint: TButton
    Left = 8
    Top = 176
    Width = 65
    Height = 20
    Caption = 'Print'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
    OnClick = bPrintClick
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 4
    Width = 121
    Height = 93
    TabOrder = 3
    object rbCurrentPage: TRadioButton
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Current Page'
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
    object rbWholeRecord: TRadioButton
      Left = 8
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Whole record'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object rbRange: TRadioButton
      Left = 8
      Top = 40
      Width = 57
      Height = 17
      Caption = 'Range'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
    end
    object edRange: TRangeEdit
      Left = 24
      Top = 56
      Width = 89
      Height = 20
      AutoSize = False
      Text = ' 0.0 - 1.00000001504746622E30 s'
      HiValue = 1.000000015047466E30
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      Units = 's'
      NumberFormat = '%.1f - %.1f'
    end
  end
  object edVerticalCalBar: TGroupBox
    Left = 8
    Top = 100
    Width = 121
    Height = 73
    Caption = ' Calibration bars '
    TabOrder = 4
    object Label5: TLabel
      Left = 8
      Top = 16
      Width = 28
      Height = 14
      Caption = 'Horiz.'
    end
    object Label6: TLabel
      Left = 8
      Top = 40
      Width = 24
      Height = 14
      Caption = 'Vert.'
    end
    object edHorizontalCalBar: TValidatedEdit
      Left = 40
      Top = 16
      Width = 73
      Height = 20
      AutoSize = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 s'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
      Scale = 1.000000000000000000
      Units = 's'
      NumberFormat = '%.3g'
    end
    object edVertCalBar: TValidatedEdit
      Left = 40
      Top = 40
      Width = 73
      Height = 20
      AutoSize = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
    end
  end
  object bCancel: TButton
    Left = 8
    Top = 200
    Width = 57
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 5
  end
end
