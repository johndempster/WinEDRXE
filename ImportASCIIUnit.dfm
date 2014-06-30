object ImportASCIIFrm: TImportASCIIFrm
  Left = 361
  Top = 194
  BorderStyle = bsDialog
  Caption = 'ASCII Import'
  ClientHeight = 360
  ClientWidth = 406
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
  object bCancel: TButton
    Left = 56
    Top = 334
    Width = 57
    Height = 17
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 0
  end
  object bOK: TButton
    Left = 8
    Top = 334
    Width = 45
    Height = 20
    Caption = 'OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
    OnClick = bOKClick
  end
  object GroupBox3: TGroupBox
    Left = 224
    Top = 112
    Width = 177
    Height = 241
    Caption = ' Channels '
    TabOrder = 2
    object ChannelTable: TStringGrid
      Left = 8
      Top = 16
      Width = 161
      Height = 217
      Hint = 'Input channel scaling factors and calibration units'
      ColCount = 3
      DefaultColWidth = 50
      DefaultRowHeight = 18
      RowCount = 9
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
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
  object meText: TMemo
    Left = 8
    Top = 0
    Width = 393
    Height = 105
    Lines.Strings = (
      'meText')
    TabOrder = 3
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 192
    Width = 209
    Height = 137
    Caption = ' Time '
    TabOrder = 4
    object GroupBox2: TGroupBox
      Left = 8
      Top = 91
      Width = 193
      Height = 36
      Caption = ' Units '
      TabOrder = 0
      object rbmsecs: TRadioButton
        Left = 8
        Top = 14
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
        Left = 72
        Top = 14
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
        Left = 120
        Top = 14
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
    object GroupBox1: TGroupBox
      Left = 8
      Top = 14
      Width = 193
      Height = 75
      TabOrder = 1
      object lbScanInterval: TLabel
        Left = 32
        Top = 46
        Width = 84
        Height = 14
        Caption = 'Sampling Interval '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object rbTimeDataInCol0: TRadioButton
        Left = 8
        Top = 8
        Width = 121
        Height = 25
        Caption = 'Time Data in Col. 0'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        TabStop = True
        OnClick = rbTimeDataInCol0Click
      end
      object rbUserDefined: TRadioButton
        Left = 8
        Top = 24
        Width = 97
        Height = 25
        Caption = 'User Defined'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = rbTimeDataInCol0Click
      end
      object edScanInterval: TValidatedEdit
        Left = 120
        Top = 48
        Width = 65
        Height = 20
        AutoSize = False
        Text = ' 0 '
        Scale = 1.000000000000000000
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 112
    Width = 209
    Height = 41
    Caption = ' Column Separator '
    TabOrder = 5
    object rbTab: TRadioButton
      Left = 8
      Top = 16
      Width = 49
      Height = 16
      Caption = 'Tab'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
      OnClick = rbTabClick
    end
    object rbComma: TRadioButton
      Left = 56
      Top = 16
      Width = 65
      Height = 16
      Caption = 'Comma'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = rbTabClick
    end
    object rbSpace: TRadioButton
      Left = 128
      Top = 16
      Width = 65
      Height = 16
      Caption = 'Space'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = rbTabClick
    end
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 152
    Width = 209
    Height = 40
    TabOrder = 6
    object Label1: TLabel
      Left = 24
      Top = 11
      Width = 140
      Height = 14
      Alignment = taRightJustify
      Caption = 'No. of title lines to be skipped'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object edNumTitleLines: TValidatedEdit
      Left = 168
      Top = 13
      Width = 33
      Height = 20
      AutoSize = False
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
  end
end
