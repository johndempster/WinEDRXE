object ImportRawFrm: TImportRawFrm
  Left = 251
  Top = 182
  Width = 470
  Height = 345
  Caption = 'Raw Binary Import'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 8
    Top = 7
    Width = 225
    Height = 274
    Caption = ' File Description '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label7: TLabel
      Left = 8
      Top = 16
      Width = 127
      Height = 15
      Caption = 'File header size (bytes)'
    end
    object lbSamplingInterval: TLabel
      Left = 40
      Top = 170
      Width = 95
      Height = 15
      Caption = 'Sampling interval'
    end
    object Label9: TLabel
      Left = 64
      Top = 40
      Width = 113
      Height = 15
      Caption = 'No. of data channels'
    end
    object Label1: TLabel
      Left = 64
      Top = 64
      Width = 109
      Height = 15
      Caption = 'No. of bytes/sample'
    end
    object edNumFileHeaderBytes: TValidatedEdit
      Left = 144
      Top = 16
      Width = 73
      Height = 20
      AutoSize = False
      Text = ' 0 '
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
    end
    object edNumChannelsPerScan: TValidatedEdit
      Left = 184
      Top = 40
      Width = 33
      Height = 20
      OnKeyPress = edNumChannelsPerScanKeyPress
      AutoSize = False
      Text = ' 1 '
      Value = 1.000000000000000000
      LoLimit = 1.000000000000000000
      HiLimit = 16.000000000000000000
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
    end
    object edScanInterval: TValidatedEdit
      Left = 144
      Top = 170
      Width = 73
      Height = 20
      AutoSize = False
      Text = ' 0 '
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
    end
    object edNumBytesPerSample: TValidatedEdit
      Left = 184
      Top = 64
      Width = 33
      Height = 20
      OnKeyPress = edNumBytesPerSampleKeyPress
      AutoSize = False
      Text = ' 1 '
      Value = 1.000000000000000000
      LoLimit = 1.000000000000000000
      HiLimit = 16.000000000000000000
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
    end
    object GroupBox4: TGroupBox
      Left = 144
      Top = 194
      Width = 73
      Height = 73
      Caption = ' Time units '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      object rbmsecs: TRadioButton
        Left = 8
        Top = 16
        Width = 57
        Height = 17
        Caption = 'msecs'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = rbmsecsClick
      end
      object rbSecs: TRadioButton
        Left = 8
        Top = 32
        Width = 57
        Height = 17
        Caption = 'secs'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        TabStop = True
        OnClick = rbmsecsClick
      end
      object rbMins: TRadioButton
        Left = 8
        Top = 48
        Width = 57
        Height = 17
        Caption = 'mins'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = rbmsecsClick
      end
    end
  end
  object bOK: TButton
    Left = 8
    Top = 288
    Width = 57
    Height = 25
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 72
    Top = 288
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
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 240
    Top = 8
    Width = 217
    Height = 273
    Caption = ' Channels '
    TabOrder = 3
    object ChannelTable: TStringGrid
      Left = 8
      Top = 16
      Width = 201
      Height = 177
      Hint = 'Input channel scaling factors and calibration units'
      ColCount = 4
      DefaultColWidth = 50
      DefaultRowHeight = 18
      RowCount = 9
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssNone
      TabOrder = 0
      RowHeights = (
        18
        18
        18
        18
        18
        18
        18
        18
        18)
    end
  end
  object GroupBox3: TGroupBox
    Left = 88
    Top = 94
    Width = 137
    Height = 79
    Caption = ' Sample Format'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object lbMaxADCValue: TLabel
      Left = 64
      Top = 34
      Width = 54
      Height = 14
      Caption = 'Max. Value'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object rbFloat: TRadioButton
      Left = 8
      Top = 16
      Width = 49
      Height = 17
      Caption = 'Float'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
      OnClick = rbFloatClick
    end
    object rbInteger: TRadioButton
      Left = 64
      Top = 16
      Width = 65
      Height = 17
      Caption = 'Integer'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = rbFloatClick
    end
    object edMaxADCValue: TValidatedEdit
      Left = 64
      Top = 51
      Width = 65
      Height = 20
      OnKeyPress = edMaxADCValueKeyPress
      AutoSize = False
      Text = ' 1 '
      Value = 1.000000000000000000
      LoLimit = 1.000000000000000000
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
    end
  end
end
