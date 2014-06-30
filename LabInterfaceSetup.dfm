object LabInterfaceSetupFrm: TLabInterfaceSetupFrm
  Left = 348
  Top = 312
  Width = 416
  Height = 223
  Caption = 'Laboratory Interface Setup'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object cbLabInterface: TComboBox
    Left = 4
    Top = 8
    Width = 393
    Height = 23
    Hint = 'Laboratory interface card used for A/D and D/A conversion'
    Style = csDropDownList
    ItemHeight = 15
    ParentShowHint = False
    ShowHint = True
    Sorted = True
    TabOrder = 0
    OnChange = cbLabInterfaceChange
  end
  object NIPanel: TPanel
    Left = 4
    Top = 34
    Width = 397
    Height = 71
    BevelOuter = bvNone
    Caption = 'NIPanel'
    TabOrder = 1
    object Label3: TLabel
      Left = 108
      Top = 2
      Width = 52
      Height = 30
      Alignment = taRightJustify
      Caption = 'A/D Input mode'
      WordWrap = True
    end
    object Label13: TLabel
      Left = 0
      Top = 2
      Width = 37
      Height = 15
      Alignment = taRightJustify
      Caption = 'Device'
    end
    object cbADCInputMode: TComboBox
      Left = 108
      Top = 18
      Width = 285
      Height = 23
      Hint = 'A/D Converter input mode'
      ItemHeight = 15
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'cbADCInputMode'
      OnChange = cbADCInputModeChange
    end
    object cbDeviceNumber: TComboBox
      Left = 0
      Top = 18
      Width = 101
      Height = 23
      ItemHeight = 15
      TabOrder = 1
      Text = 'cbDeviceNumber'
      OnChange = cbDeviceNumberChange
    end
  end
  object Panel1: TPanel
    Left = 4
    Top = 80
    Width = 393
    Height = 89
    BevelOuter = bvNone
    TabOrder = 2
    object Label5: TLabel
      Left = 112
      Top = 28
      Width = 169
      Height = 17
      AutoSize = False
      Caption = 'A/D  Converter Voltage Range'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object edModel: TEdit
      Left = 0
      Top = 0
      Width = 393
      Height = 23
      BevelInner = bvNone
      TabOrder = 0
    end
    object cbADCVoltageRange: TComboBox
      Left = 287
      Top = 28
      Width = 106
      Height = 25
      Hint = 'Working voltage range of A/D converter'
      Style = csOwnerDrawFixed
      ItemHeight = 19
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object bOK: TButton
      Left = 0
      Top = 56
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
      TabOrder = 2
      OnClick = bOKClick
    end
    object bCancel: TButton
      Left = 56
      Top = 56
      Width = 45
      Height = 16
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ModalResult = 2
      ParentFont = False
      TabOrder = 3
      OnClick = bCancelClick
    end
  end
end
