�
 TSETUPDLG 0�  TPF0	TSetupDlgSetupDlgLeftTop� BorderStylebsDialogCaptionRecording SetupClientHeightxClientWidth~Color	clBtnFaceFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 	FormStyle
fsMDIChildOldCreateOrder	PositionpoScreenCenterVisible	OnClose	FormCloseOnShowFormShowPixelsPerInch`
TextHeight 	TGroupBox	GroupBox1Left	TopWidthoHeight� Caption Record TabOrder  TLabellNumChannelsLeft	TopWidthJHeightCaptionNo. ChannelsFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont  TLabelLabel1Left	Top?Width5HeightCaptionSampling IntervalFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontWordWrap	  TLabelLabel2Left	Top� WidthHeight  TLabelLabel5Left	TopWidthMHeight#AutoSizeCaptionADC Voltage RangeFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontWordWrap	  TValidatedEditedNumChannelsLeft	Top"Width"Height
OnKeyPressedNumChannelsKeyPressAutoSizeText 1 Value       ��?Scale       ��?NumberFormat%.0fLoLimit       ��?HiLimit       �@  TValidatedEditedSamplingIntervalLeft	Top^Width]Height
OnKeyPressEdSamplingIntervalKeyPressAutoSizeText 0 msScale       �@UnitsmsNumberFormat%.4gLoLimit     ���b�HiLimit     ���b@  	TComboBoxcbADCVoltageRangeLeft	Top� Width]HeightStylecsDropDownList
ItemHeightTabOrder   	TGroupBoxChannelsLeftTopWidth� HeightmCaption Channel Settings TabOrder TStringGridChannelTableLeft	TopWidth� HeightPHint3Input channel scaling factors and calibration unitsColCountDefaultColWidth2DefaultRowHeightRowCount	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont
ScrollBarsssNoneTabOrder 
RowHeights    TButtonbOKLeft	Top� Width<HeightCaptionOKFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold ModalResult
ParentFontTabOrderOnClickbOKClick  TButtonbCancelLeft	Top� Width5HeightCaptionCancelFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold ModalResult
ParentFontTabOrderOnClickbCancelClick  	TGroupBox	GroupBox2LeftTop	Width� HeighthCaption Laboratory Interface Card Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TPanelNIPanelLeftTop(Width� Height9
BevelOuterbvNoneTabOrder  TLabelLabel3LeftTopWidth&Height	AlignmenttaRightJustifyCaptionDevice  TLabelLabel11LeftTTopWidthUHeightCaptionA/D Input modeWordWrap	  	TComboBoxcbDeviceNumberLeftTopWidthIHeight
ItemHeightTabOrder TextcbDeviceNumberOnChangecbDeviceNumberChange  	TComboBoxcbADCInputModeLeftTTopWidth� HeightHintA/D Converter input mode
ItemHeightParentShowHintShowHint	TabOrderTextcbADCInputModeOnChangecbADCInputModeChange   	TComboBoxcbLabInterfaceLeftTopWidth� HeightHint9Laboratory interface card used for A/D and D/A conversionStylecsDropDownList
ItemHeightParentShowHintShowHint	Sorted	TabOrderOnChangecbLabInterfaceChange   	TGroupBox	GroupBox3LeftTop� Width� Height� Caption Display Grid TabOrder TLabelLabel6Left2TopWidth� Height	AlignmenttaRightJustifyCaptionNo. of horizontal grid linesFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel7LeftATop)WidthHeight	AlignmenttaRightJustifyCaptionNo. of vertical grid linesFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel4Left,TopBWidth<Height	AlignmenttaRightJustifyCaption
Time UnitsFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEditedNumHorizontalGridLinesLeft� TopWidth#HeightAutoSizeText 1 Value       ��?Scale       ��?NumberFormat%.0fLoLimit       ��?HiLimit       �@  TValidatedEditedNumVerticalGridLinesLeft� Top)Width#HeightAutoSizeText 1 Value       ��?Scale       ��?NumberFormat%.0fLoLimit       ��?HiLimit       �@  TRadioButtonrbTmsecsLeft� TopBWidth<HeightCaptionmsecs.Checked	TabOrderTabStop	OnClickrbTmsecsClick  TRadioButtonrbTSecsLeftyTopBWidth3HeightCaptionsecs.TabOrderOnClickrbTSecsClick   	TGroupBoxgrp1902Left}TopWidth� Height� Caption Amplifiers TabOrder TLabelLabel8LeftTopWidthHeightCaption#1  TLabelLabel9LeftTopHWidthHeightCaption#2  	TComboBoxcbAmplifier1LeftTopWidth� HeightHint:Patch/Voltage clamp amplifier attached to channels 0 and 1
ItemHeightParentShowHintShowHint	TabOrder TextcbAmplifier1OnChangecbAmplifier1Change  	TComboBoxcbAmplifier2LeftTopHWidth� HeightHint:Patch/Voltage clamp amplifier attached to channels 2 and 3
ItemHeightParentShowHintShowHint	TabOrderTextcbAmplifiersOnChangecbAmplifier1Change  TPanelGainTelegraphPanel2LeftTop`WidthaHeight
BevelOuterbvNoneTabOrder TLabelLabel10Left TopWidth*HeightCaption	Gain Tel.Font.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEditedGainTelegraphChannel2Left8TopWidth!HeightHint=A/D channel used to monitor patch clamp gain telegraph outputShowHint	Text 0 Scale       ��?NumberFormat%.0fLoLimit       ���HiLimit       �@   TPanelModeTelegraphPanel1Left� Top(WidthaHeight
BevelOuterbvNoneTabOrder TLabelLabel12Left TopWidth.HeightCaption	Mode Tel.Font.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEditedModeTelegraphChannel1Left8TopWidth!HeightHint=A/D channel used to monitor patch clamp mode telegraph outputShowHint	Text 0 Scale       ��?NumberFormat%.0fLoLimit       ���HiLimit       �@   TPanelModeTelegraphPanel2Left� Top`WidthaHeight
BevelOuterbvNoneTabOrder TLabelLabel13Left TopWidth.HeightCaption	Mode Tel.Font.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEditedModeTelegraphChannel2Left8TopWidth!HeightHint=A/D channel used to monitor patch clamp mode telegraph outputShowHint	Text 0 Scale       ��?NumberFormat%.0fLoLimit       ���HiLimit       �@   TPanelGainTelegraphPanel1LeftTop(WidthaHeight
BevelOuterbvNoneTabOrder TLabelLabel14Left TopWidth*HeightCaption	Gain Tel.Font.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEditedGainTelegraphChannel1Left8TopWidth!HeightHint=A/D channel used to monitor patch clamp gain telegraph outputShowHint	Text 0 Scale       ��?NumberFormat%.0fLoLimit       ���HiLimit       �@     