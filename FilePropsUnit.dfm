object FilePropsDlg: TFilePropsDlg
  Tag = 28
  Left = 652
  Top = 169
  BorderStyle = bsDialog
  Caption = 'File Properties'
  ClientHeight = 442
  ClientWidth = 353
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
  object bCancel: TButton
    Left = 64
    Top = 406
    Width = 45
    Height = 16
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 0
    OnClick = bCancelClick
  end
  object bOK: TButton
    Left = 8
    Top = 406
    Width = 49
    Height = 25
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
  object Page: TPageControl
    Left = 8
    Top = 8
    Width = 337
    Height = 393
    ActivePage = TabProperties
    TabOrder = 2
    object TabProperties: TTabSheet
      Caption = 'Properties'
      object meProperties: TMemo
        Left = 8
        Top = 104
        Width = 313
        Height = 257
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 0
        Width = 313
        Height = 97
        TabOrder = 1
        object Label1: TLabel
          Left = 135
          Top = 16
          Width = 81
          Height = 13
          Alignment = taRightJustify
          Caption = 'Sampling Interval'
        end
        object Label2: TLabel
          Left = 99
          Top = 40
          Width = 117
          Height = 13
          Alignment = taRightJustify
          Caption = 'A/D Voltage Range (+/-)'
        end
        object Label3: TLabel
          Left = 82
          Top = 64
          Width = 134
          Height = 13
          Alignment = taRightJustify
          Caption = 'A/D sample data max. value'
        end
        object edSamplingInterval: TValidatedEdit
          Left = 224
          Top = 16
          Width = 81
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          Text = ' 0 ms'
          Scale = 1000.000000000000000000
          Units = 'ms'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edADCVoltageRange: TValidatedEdit
          Left = 224
          Top = 40
          Width = 81
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edADCMaxValue: TValidatedEdit
          Left = 224
          Top = 64
          Width = 81
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          Text = ' 1  '
          Value = 1.000000000000000000
          Scale = 1.000000000000000000
          Units = ' '
          NumberFormat = '%.6g'
          LoLimit = 1.000000000000000000
          HiLimit = 1.000000015047466E30
        end
      end
    end
    object TabCalTable: TTabSheet
      Caption = 'Calibration Table'
      ImageIndex = 1
      object ChannelTable: TStringGrid
        Left = 8
        Top = 8
        Width = 313
        Height = 353
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
        ColWidths = (
          50
          50
          50
          50)
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
    object TabFileHeader: TTabSheet
      Caption = 'File Header'
      ImageIndex = 2
      object meFileHeader: TMemo
        Left = 8
        Top = 8
        Width = 313
        Height = 353
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
