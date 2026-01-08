object EditSignalDataFrm: TEditSignalDataFrm
  Left = 0
  Top = 0
  Caption = 'Edit Signal Data '
  ClientHeight = 200
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 16
  object bEditData: TButton
    Left = 8
    Top = 127
    Width = 105
    Height = 25
    Hint = 'Delete selected data points from file'
    Caption = 'Edit Data'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = bEditDataClick
  end
  object bCancel: TButton
    Left = 8
    Top = 158
    Width = 105
    Height = 25
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = bCancelClick
  end
  object gpEditOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 270
    Height = 113
    Caption = ' Edit Options '
    TabOrder = 2
    object Label1: TLabel
      Left = 81
      Top = 24
      Width = 47
      Height = 17
      Alignment = taRightJustify
      Caption = 'Start At'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object edStartAtScan: TValidatedEdit
      Left = 140
      Top = 21
      Width = 121
      Height = 24
      Hint = 'Starting time point for data edit'
      ShowHint = True
      Text = ' 0 s'
      Scale = 1.000000000000000000
      Units = 's'
      NumberFormat = '%.7g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edNumScansToDelete: TValidatedEdit
      Left = 140
      Top = 50
      Width = 121
      Height = 24
      Hint = 'Time period of data to be deleted (set=0)'
      ShowHint = True
      Text = ' 0 s'
      Scale = 1.000000000000000000
      Units = 's'
      NumberFormat = '%.5g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object rbDeleteBlock: TRadioButton
      Left = 8
      Top = 50
      Width = 130
      Height = 17
      Caption = 'Delete Block'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      TabStop = True
    end
    object rbShiftChannels: TRadioButton
      Left = 8
      Top = 81
      Width = 130
      Height = 17
      Hint = 'Shift channels'
      Caption = 'Shift Channels'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
    object edShiftChannelsBy: TValidatedEdit
      Left = 140
      Top = 81
      Width = 121
      Height = 24
      Hint = 'Shift data down by <n> channels '
      ShowHint = True
      Text = ' 0 '
      Value = 1.000000000000000000
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
      LoLimit = 1.000000000000000000
      HiLimit = 1.000000015047466E30
    end
  end
  object bRestoreBackup: TButton
    Left = 119
    Top = 127
    Width = 130
    Height = 25
    Hint = 'Restore original file from backup '
    Caption = 'Restore Backup'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = bRestoreBackupClick
  end
end
