�
 TAVGFRM 0�  TPF0TAvgFrmAvgFrmLeftTop� BorderIconsbiSystemMenu BorderStylebsSingleCaptionCreate Averaged RecordsClientHeightFClientWidth�
Font.ColorclWindowTextFont.Height�	Font.NameSystem
Font.Style 	FormStyle
fsMDIChild
KeyPreview	PixelsPerInch`PositionpoScreenCenterVisible	OnClose	FormCloseOnCreate
FormCreate	OnKeyDownFormKeyDownOnShowFormShow
TextHeight 	TPaintBox	pbDisplayLeft� TopWidth!Height
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontOnMouseDownpbDisplayMouseDownOnMouseMovepbDisplayMouseMove	OnMouseUppbDisplayMouseUpOnPaintpbDisplayPaint  TLabellbTMaxLeftxTopWidth6HeightCaption	9999.9 ms
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont  TLabellbTMinLeft� TopWidth6HeightCaption	9999.9 ms
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont  TLabel	lbCursor0Left� Top WidthHeightCaptionl
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabel	lbCursor1Left� Top(WidthHeightCaptionl
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  	TGroupBox	RecordGrpLeftTop Width� HeightyCaption Record
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder  TLabelLabel2LeftTopHWidthHeightCaptionType
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TEditedRecordNumLeftTopWidthqHeightHintITo select a record for display, enter its number and press the Return keyParentShowHintShowHint	TabOrder TextedRecordNum  	TComboBoxcbRecordTypeLeft8TopHWidthAHeightHint#Type of record currently on displayStylecsDropDownList
ItemHeightParentShowHintShowHint	TabOrderOnChangecbRecordTypeChange  	TCheckBoxckBadRecordLeftTop`WidthQHeightHint.Rejected records are excluded from the averageCaptionRejectedParentShowHintShowHint	TabOrderOnClickckBadRecordClick  
TScrollBarsbRecordNumLeftTop0WidthqHeightTabOrderOnChangesbRecordNumChange   	TGroupBoxAnalysisGrpLeftTopxWidth� Height� Caption	 Average 
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TLabelLabel5LeftTopHWidth%HeightCaptionRange  TLabelLabel7LeftTop� WidthHeightCaptionType  TLabelLabel6LeftTopbWidth>HeightCaptionIn blocks of  TLabelLabel8LeftTop� WidthZHeightCaptionAlignment mode  TEditedRangeLeft8TopHWidthAHeightHint)Range of records to be used for averagingParentShowHintShowHint	TabOrder TextedRange  TButtonbDoAveragesLeftTopWidthqHeightHintCompute averaged record(s)CaptionDo Averages
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrderOnClickbDoAveragesClick  	TComboBoxcbRecTypeToBeAveragedLeft8Top|WidthAHeightHint+Type of record(s) to be included in averageStylecsDropDownList
ItemHeightParentShowHintShowHint	TabOrder  TButtonbCancelLeftTop(Width1HeightCaptionCancelDefault	Enabled
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbCancelClick  TEditEdBlockSizeLeftPTopbWidth)HeightHint8Number of records to be included in each averaged recordParentShowHintShowHint	TabOrderTextEdBlockSize  TButtonbAbortLeftHTop(Width1HeightHintAbort averagingCaptionAbortEnabled
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrder  	TComboBoxcbAlignModeLeftTop� WidthqHeightHint`Determines whether records are aligned by the mid-points of their leading edges before averagingStylecsDropDownList
ItemHeightItems.StringsNo alignmentOn Positive RiseOn Negative Rise ParentShowHintShowHint	TabOrder   TEdit
edProgressLeft� TopWidthiHeight
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontReadOnly	TabOrderText   TTimerTimerIntervalOnTimer
TimerTimerLeft� Top`   