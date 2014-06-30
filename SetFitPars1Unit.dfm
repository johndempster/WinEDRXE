object SetFitPars1frm: TSetFitPars1frm
  Left = 171
  Top = 305
  BorderStyle = bsDialog
  Caption = 'Set Fitting Parameters '
  ClientHeight = 226
  ClientWidth = 391
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
  object ParametersGrp: TGroupBox
    Left = 2
    Top = 0
    Width = 383
    Height = 161
    Hint = 
      'Initial parameter values to start curve fitting.  Fixed paramete' +
      'rs are held constant during curve fitting.'
    Caption = ' Parameters '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object Label2: TLabel
      Left = 152
      Top = 16
      Width = 29
      Height = 15
      Caption = 'Fixed'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 344
      Top = 16
      Width = 29
      Height = 15
      Caption = 'Fixed'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object lbPar0: THTMLLabel
      Left = 8
      Top = 32
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object lbPar1: THTMLLabel
      Left = 8
      Top = 56
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object lbPar2: THTMLLabel
      Left = 8
      Top = 80
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object lbPar3: THTMLLabel
      Left = 8
      Top = 104
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object lbPar4: THTMLLabel
      Left = 8
      Top = 128
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object lbPar5: THTMLLabel
      Left = 200
      Top = 32
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object lbPar6: THTMLLabel
      Left = 200
      Top = 56
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object lbPar7: THTMLLabel
      Left = 200
      Top = 80
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object lbPar8: THTMLLabel
      Left = 200
      Top = 104
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object lbPar9: THTMLLabel
      Left = 200
      Top = 128
      Width = 49
      Height = 17
      Caption = 'Label'
      Alignment = taRightJustify
      LineSpacing = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
    end
    object ckFixed0: TCheckBox
      Left = 158
      Top = 32
      Width = 25
      Height = 17
      Hint = 'Keep parameter fixed at initial value during fitting'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object ckFixed1: TCheckBox
      Left = 158
      Top = 56
      Width = 25
      Height = 17
      Hint = 'Keep parameter fixed at initial value during fitting'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
    end
    object ckFixed2: TCheckBox
      Left = 158
      Top = 80
      Width = 25
      Height = 17
      Hint = 'Keep parameter fixed at initial value during fitting'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 2
    end
    object ckFixed3: TCheckBox
      Left = 158
      Top = 104
      Width = 25
      Height = 17
      Hint = 'Keep parameter fixed at initial value during fitting'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 3
    end
    object ckFixed4: TCheckBox
      Left = 158
      Top = 128
      Width = 17
      Height = 17
      Hint = 'Keep parameter fixed at initial value during fitting'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 4
    end
    object ckFixed5: TCheckBox
      Left = 352
      Top = 32
      Width = 17
      Height = 17
      Hint = 'Keep parameter fixed at initial value during fitting'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 5
    end
    object ckFixed6: TCheckBox
      Left = 352
      Top = 56
      Width = 17
      Height = 17
      Hint = 'Keep parameter fixed at initial value during fitting'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 6
    end
    object ckFixed7: TCheckBox
      Left = 352
      Top = 80
      Width = 17
      Height = 17
      ParentShowHint = False
      ShowHint = False
      TabOrder = 7
    end
    object edPar0: TValidatedEdit
      Left = 64
      Top = 32
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edPar1: TValidatedEdit
      Left = 64
      Top = 56
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edPar2: TValidatedEdit
      Left = 64
      Top = 80
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edPar3: TValidatedEdit
      Left = 64
      Top = 104
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edPar4: TValidatedEdit
      Left = 64
      Top = 128
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edPar5: TValidatedEdit
      Left = 256
      Top = 32
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edPar6: TValidatedEdit
      Left = 256
      Top = 56
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edPar7: TValidatedEdit
      Left = 256
      Top = 80
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edpar8: TValidatedEdit
      Left = 256
      Top = 104
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object ckfixed8: TCheckBox
      Left = 352
      Top = 104
      Width = 17
      Height = 17
      Caption = 'ckfixed8'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 17
    end
    object edpar9: TValidatedEdit
      Left = 256
      Top = 128
      Width = 90
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.3g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object ckfixed9: TCheckBox
      Left = 352
      Top = 128
      Width = 17
      Height = 17
      Caption = 'ckfixed8'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 19
    end
  end
  object bOK: TButton
    Left = 8
    Top = 168
    Width = 57
    Height = 20
    Caption = 'OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 8
    Top = 192
    Width = 49
    Height = 17
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
    OnClick = bCancelClick
  end
  object GroupBox1: TGroupBox
    Left = 240
    Top = 160
    Width = 145
    Height = 57
    Caption = ' Parameter Initialisation  '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object rbAutomaticGuess: TRadioButton
      Left = 16
      Top = 16
      Width = 81
      Height = 17
      Caption = 'Automatic'
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
    object rbManual: TRadioButton
      Left = 16
      Top = 32
      Width = 65
      Height = 17
      Caption = 'Manual'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object bInitialise: TButton
      Left = 80
      Top = 34
      Width = 57
      Height = 17
      Caption = 'Initialise'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = bInitialiseClick
    end
  end
end
