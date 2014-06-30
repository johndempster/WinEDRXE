object EventFilterFrm: TEventFilterFrm
  Left = -129
  Top = 311
  BorderStyle = bsDialog
  Caption = 'Event Filter Criteria'
  ClientHeight = 324
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 193
    Height = 265
    Caption = 'GroupBox1'
    TabOrder = 0
    object meFilterList: TMemo
      Left = 8
      Top = 16
      Width = 177
      Height = 241
      Lines.Strings = (
        'meFilterList')
      TabOrder = 0
    end
  end
  object GroupBox5: TGroupBox
    Left = 208
    Top = 0
    Width = 121
    Height = 321
    Caption = ' Filters '
    TabOrder = 1
    object GroupBox2: TGroupBox
      Left = 8
      Top = 12
      Width = 105
      Height = 231
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 38
        Height = 13
        Caption = 'Variable'
      end
      object cbVariable: TComboBox
        Left = 8
        Top = 24
        Width = 89
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'cbVariable'
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 48
        Width = 89
        Height = 57
        Caption = ' Action '
        TabOrder = 1
        object rbInclude: TRadioButton
          Left = 8
          Top = 16
          Width = 65
          Height = 17
          Caption = 'Include'
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
        object rbExclude: TRadioButton
          Left = 8
          Top = 32
          Width = 65
          Height = 17
          Caption = 'Exclude'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
      end
      object bAdd: TButton
        Left = 8
        Top = 208
        Width = 73
        Height = 17
        Caption = 'Add'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = bAddClick
      end
      object GroupBox6: TGroupBox
        Left = 8
        Top = 104
        Width = 89
        Height = 97
        Caption = ' Limits '
        TabOrder = 3
        object Label2: TLabel
          Left = 8
          Top = 16
          Width = 29
          Height = 13
          Caption = 'Lower'
        end
        object Label3: TLabel
          Left = 8
          Top = 52
          Width = 29
          Height = 13
          Caption = 'Upper'
        end
        object edLoLimit: TValidatedEdit
          Left = 8
          Top = 30
          Width = 65
          Height = 21
          Text = ' 0 '
          LoLimit = -1.000000015047466E29
          HiLimit = 1.000000015047466E29
          Scale = 1.000000000000000000
          NumberFormat = '%.3g'
        end
        object edHiLimit: TValidatedEdit
          Left = 8
          Top = 66
          Width = 65
          Height = 21
          Text = ' 0 '
          LoLimit = -1.000000015047466E29
          HiLimit = 1.000000015047466E29
          Scale = 1.000000000000000000
          NumberFormat = '%.3g'
        end
      end
    end
    object GroupBox4: TGroupBox
      Left = 8
      Top = 244
      Width = 105
      Height = 65
      TabOrder = 1
      object bDelete: TButton
        Left = 8
        Top = 16
        Width = 73
        Height = 17
        Caption = 'Delete'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object bDeleteAll: TButton
        Left = 8
        Top = 40
        Width = 73
        Height = 17
        Caption = 'Delete All'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bDeleteAllClick
      end
    end
  end
  object bOK: TButton
    Left = 16
    Top = 272
    Width = 41
    Height = 20
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
  end
  object bCancel: TButton
    Left = 64
    Top = 272
    Width = 57
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 3
  end
end
