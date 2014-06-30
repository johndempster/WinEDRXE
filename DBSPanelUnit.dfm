object DBSPanelFrm: TDBSPanelFrm
  Left = 624
  Top = 255
  Width = 267
  Height = 276
  Caption = 'DBS Control'
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StimGrp: TGroupBox
    Left = 4
    Top = 4
    Width = 250
    Height = 69
    Caption = ' Stimulus Pulse '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 57
      Height = 13
      Caption = 'Pulse Width'
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 79
      Height = 13
      Caption = 'Pulse Frequency'
    end
    object edPulseWidth: TValidatedEdit
      Left = 96
      Top = 16
      Width = 89
      Height = 21
      OnKeyPress = edPulseWidthKeyPress
      Text = ' 50 ms'
      Value = 0.050000000745058060
      Scale = 1000.000000000000000000
      Units = 'ms'
      NumberFormat = '%.4g'
      LoLimit = 0.000009999999747379
      HiLimit = 0.500000000000000000
    end
    object edPulseFrequency: TValidatedEdit
      Left = 96
      Top = 40
      Width = 89
      Height = 21
      OnKeyPress = edPulseFrequencyKeyPress
      Text = ' 10 Hz'
      Value = 10.000000000000000000
      Scale = 1.000000000000000000
      Units = 'Hz'
      NumberFormat = '%.4g'
      LoLimit = 0.100000001490116100
      HiLimit = 500.000000000000000000
    end
    object GroupBox2: TGroupBox
      Left = 192
      Top = 10
      Width = 49
      Height = 52
      TabOrder = 2
      object rbStimulatorOn: TRadioButton
        Left = 4
        Top = 12
        Width = 40
        Height = 17
        Caption = 'On'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        TabStop = True
        OnClick = rbStimulatorOnClick
      end
      object rbStimulatorOff: TRadioButton
        Left = 4
        Top = 28
        Width = 40
        Height = 17
        Caption = 'Off'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = rbStimulatorOffClick
      end
    end
  end
  object SystermPowerGrp: TGroupBox
    Left = 4
    Top = 76
    Width = 250
    Height = 41
    Caption = ' System Power '
    TabOrder = 1
    object rbActiveMode: TRadioButton
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Active'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
      OnClick = rbActiveModeClick
    end
    object rbSleepMode: TRadioButton
      Left = 72
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Sleep Mode'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = rbSleepModeClick
    end
  end
  object bUpdateDBS: TButton
    Left = 8
    Top = 225
    Width = 89
    Height = 20
    Caption = 'Update DBS'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = bUpdateDBSClick
  end
  object TGroupBox
    Left = 4
    Top = 120
    Width = 250
    Height = 97
    Caption = ' DBS Status '
    TabOrder = 3
    object meStatus: TMemo
      Left = 8
      Top = 16
      Width = 233
      Height = 73
      TabOrder = 0
    end
    object GroupBox1: TGroupBox
      Left = 160
      Top = 48
      Width = 185
      Height = 105
      Caption = 'GroupBox1'
      TabOrder = 1
    end
  end
  object Timer: TTimer
    Interval = 200
    OnTimer = TimerTimer
    Left = 200
    Top = 216
  end
end
