unit ViewSig;
{ ========================================================
  WinEDR (c) John Dempster, University of Strathclyde 1998-2000
  EDR DATA FILE DISPLAY MODULE
  V0.99 0/3/98
  28/6/98 ... PrintRecord & CopyRecordsImageToClipboard replace PlotRecords
  30/6/98 ... Ident line now updates correctly
  28/2/99 ... Signals now displayed using TChartDisplay component
  30/8/99 ... Grid option and settable Min & MaxADCValue added
  1/9/99 ... ChangeDisplayGrid added
  4/3/00 ... Bug in display upper limit with large recordinsg fixed
  9/1/02 ... Zero levels now updated when changed by other modules (.NewData)
  6.6.02 ... Display cursor readout now displayed using CursorLabel component
  25.6.03 .. Signals now displayed using Tscopedisplay (rather than TChartdisplay)
  26.6.03 .. Markers can now be added to chart
  03.03.04 .. Larger DisplayBuf now allocated
  08.03.05 .. Scroll bar can now step by 25% of display window
              Get t=0 cursor button added
  17.09.09 .. .ClearVerticalCursors added to avoid multiple readout cursors
  27.08.10 ... Cursor readout now on display
  08.12.10 ... t=0 - t=? inter-cursor average display added
  28.08.12 ... Zero Level form updated.
               Fixed Zero level tick box added, fixes zero levels in place
  07.08.15 ... Min/Max compression of large array signal arrays now handled by ScopeDisplay.pas
  27.05.21 ... SaveToDataFile public procedure added. Saves displayed data to CSV file.
  14.03.24 ... Form position saved to INI file
/ 21.03.24 ... Key presses for controlling display cursor positions now sourced from edDisplayKeySource
              rather than form key preview
  26.08.25 ... Display recursive low pass filter feature added
  05.09.25 ... Max. displayed samples now limited to <= 400000000 to avoid GetMemory() failure to allocate display buffer 05.09.25
  12.09.25 ... Display recursive high pass filter feature added
  ========================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, printers, ClipBrd,
  Grids, CDRZero, RangeEdit, ScopeDisplay, EDRFileUnit,
  ValidatedEdit, maths, seslabio, math, system.strutils ;

type

  // Display cursors record
  TCursors = record
         Base : Array[0..EDRChannelLimit] of Integer ;

         C0 : Integer ;
         C1 : Integer ;
         end ;

  TViewSigFrm = class(TForm)
    edIdent: TEdit;
    Label3: TLabel;
    DisplayGrp: TGroupBox;
    scDisplay: TScopeDisplay;
    meCursor: TMemo;
    MarkChartGrp: TGroupBox;
    bMark: TButton;
    edMarker: TEdit;
    EdNumMarkers: TEdit;
    TDisplayPanel: TPanel;
    edTDisplay: TValidatedEdit;
    bTDisplayDouble: TButton;
    bTDisplayHalf: TButton;
    sbDisplayPanel: TPanel;
    lbStartTime: TLabel;
    edStartTime: TValidatedEdit;
    sbStartTime: TScrollBar;
    TcursorGrp: TGroupBox;
    bGetTZero: TButton;
    rbTDisplayUnitsMSecs: TRadioButton;
    rbTDisplayUnitsSecs: TRadioButton;
    rbTDisplayUnitMins: TRadioButton;
    bCalcAverage: TButton;
    ckFixedZeroLevels: TCheckBox;
    edDisplayKeyPressSource: TEdit;
    gpDisplayFilters: TGroupBox;
    edLowPassFilterFrequency: TValidatedEdit;
    ckLowPassFilterActive: TCheckBox;
    edHighPassFilterFrequency: TValidatedEdit;
    ckHighPassFilterActive: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure edIdentKeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure edIdentChange(Sender: TObject);
    procedure edTDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure edStartTimeKeyPress(Sender: TObject; var Key: Char);
    procedure sbStartTimeChange(Sender: TObject);
    procedure scDisplayCursorChange(Sender: TObject);
    procedure rbTUnitsSecondsClick(Sender: TObject);
    procedure rbTUnitsMsecsClick(Sender: TObject);
    procedure bMarkClick(Sender: TObject);
    procedure rbTUnitsMinutesClick(Sender: TObject);
    procedure scDisplayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bGetTZeroClick(Sender: TObject);
    procedure rbTDisplayUnitMinsClick(Sender: TObject);
    procedure rbTDisplayUnitsSecsClick(Sender: TObject);
    procedure bTDisplayDoubleClick(Sender: TObject);
    procedure bTDisplayHalfClick(Sender: TObject);
    procedure rbTDisplayUnitsMSecsClick(Sender: TObject);
    procedure bCalcAverageClick(Sender: TObject);
    procedure ckFixedZeroLevelsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edDisplayKeyPressSourceKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ckLowPassFilterActiveClick(Sender: TObject);
    procedure edLowPassFilterFrequencyKeyPress(Sender: TObject; var Key: Char);
    procedure ckHighPassFilterActiveClick(Sender: TObject);
    procedure edHighPassFilterFrequencyKeyPress(Sender: TObject; var Key: Char);

  private
    { Private declarations }
    Cursors : TCursors ; // Display cursor record
    TZeroScan : Integer ;                // Scan # defined as zero time
    DisplayBuf : PSmallIntArray ;         // Data display buffer pointer

  public
    { Public declarations }
    procedure PrintDisplay  ;
    procedure CopyImageToClipboard ;
    procedure SaveDataToFile ;
    procedure ZoomOutAll ;
    procedure NewData ;
    procedure ChangeDisplayGrid ;
    procedure DisplayFromFile ;
    procedure ZoomIn( ChanNum : Integer ) ;
    procedure ZoomOut( ChanNum : Integer ) ;

  end;



var
  ViewSigFrm: TViewSigFrm;

implementation

uses mdiform,shared,Printrec , Zero, EventDetector;

const
     MaxDisplayScans = 2000 ;


{$R *.DFM}


procedure TViewSigFrm.FormShow(Sender: TObject);
{ ---------------------------------------
  Initialisations done when form is shown
  ---------------------------------------}
begin

     Top := 10 ;
     Left := 10 ;

     Main.mnViewSig.Enabled := False ;

     ClientHeight := TCursorGrp.Top + TCursorGrp.Height + 10 ;
     Resize ;

     { Display ident. info. }
     EdIdent.text := EDRFIle.Cdrfh.IdentLine ;

     Main.SetCopyMenu(True,True) ;

     // Initialise time readout units
     if EDRFile.Settings.TScale = 1000.0 then rbTDisplayUnitsMsecs.Checked := True
                                         else rbTDisplayUnitsSecs.Checked := True ;
     edStartTime.Scale := EDRFile.Settings.TSCale ;
     edStartTime.Units := EDRFile.Settings.TUnits ;
     edTDisplay.Value := EDRFile.Settings.DisplayDuration ;
     edTDisplay.Scale := EDRFile.Settings.TSCale ;
     edTDisplay.Units := EDRFile.Settings.TUnits ;

//   Set low and high pass display-only filter controls

     edLowPassFilterFrequency.Value := scDisplay.LowPassFilterCutOffFrequency(EDRFile.Settings.LowPassFilterCoeff) ;
     ckLowPassFilterActive.Checked := EDRFile.Settings.LowPassFilterActive ;
     scDisplay.LowPassFilterOn := ckLowPassFilterActive.Checked ;
     scDisplay.LowPassFilterCoeff :=  EDRFile.Settings.LowPassFilterCoeff ;

     edHighPassFilterFrequency.Value := scDisplay.HighPassFilterCutOffFrequency(EDRFile.Settings.HighPassFilterCoeff) ;
     ckHighPassFilterActive.Checked := EDRFile.Settings.HighPassFilterActive ;
     scDisplay.HighPassFilterOn := ckHighPassFilterActive.Checked ;
     scDisplay.HighPassFilterCoeff :=  EDRFile.Settings.HighPassFilterCoeff ;


     ckFixedZeroLevels.Checked := EDRFile.Settings.FixedZeroLevels ;

     if EDRFIle.Cdrfh.NumSamplesInFile > 0 then Main.mnPrint.enabled := True ;

     // Update display channels and plot signal
     NewData ;

     end;


procedure TViewSigFrm.FormClose(Sender: TObject; var Action: TCloseAction);
{ -------------------------
  Close and dispose of form
  -------------------------}
begin

     // Free display buffer
     FreeMem( DisplayBuf ) ;

     { Save data to file header }
     EDRFile.SaveHeader( EDRFile.CdrFH ) ;

     Main.mnViewSig.Enabled := True ;

    { Disable copy and print menus }
     Main.CopyAndPrintMenus( False, False ) ;

     // Save form position to INI file
     EDRFile.SaveFormPosition( Self ) ;

     Action := caFree ;
     end;


procedure TViewSigFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Initialisations when form is created
// ------------------------------------
begin
    DisplayBuf := Nil ;
    end;

procedure TViewSigFrm.FormResize(Sender: TObject);
{ --------------------------------------------
  Adjust size of display when form is resized
  --------------------------------------------}
begin

     // Set size and location of display readout cursor
     TDisplayPanel.Top := ClientHeight - TDisplayPanel.Height - 5 ;
     sbDisplayPanel.Top := TDisplayPanel.Top ;

     scDisplay.Width := Max( ClientWidth - scDisplay.Left - 5, 2) ;

     TDisplayPanel.Left := scDisplay.Left + scDisplay.Width - TDisplayPanel.Width ;
     sbDisplayPanel.Width := Max(TDisplayPanel.Left - sbDisplayPanel.Left - 2,2) ;
     sbStartTime.Width := sbDisplayPanel.ClientWidth - sbStartTime.Left - 1 ;

     // Set height of display
     scDisplay.Height := Max( sbDisplayPanel.Top - scDisplay.Top, 2 ) ;

     { Set width of identification text box }
     edIdent.Width := scDisplay.Left + scDisplay.Width - edIdent.Left ;
     gpDisplayFilters.Width := scDisplay.Width ;

     end;


procedure TViewSigFrm.edIdentKeyPress(Sender: TObject; var Key: Char);
{ -------------------------------------------
  Write to log file
  -------------------------------------------}
begin
     if key = #13 then EDRFile.WriteToLogFile( edIdent.text ) ;
     end;


procedure TViewSigFrm.edLowPassFilterFrequencyKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------------------------
// Change display low-pass filter cut-off frequency
// ------------------------------------------------
begin
     if Key = #13 then
        begin
        EDRFile.Settings.LowPassFilterCoeff := scDisplay.LowPassFilterSmoothingFactor(edLowPassFilterFrequency.Value) ;
        scDisplay.LowPassFilterCoeff :=  EDRFile.Settings.LowPassFilterCoeff ;
        end;
end;


procedure TViewSigFrm.PrintDisplay ;
{ -------------------
  Print display image
  -------------------}
begin
     PrintRecFrm.Destination := dePrinter ;
     PrintRecFrm.Display := scDisplay ;
     PrintRecFrm.ShowModal ;
     if PrintRecFrm.ModalResult = mrOK then begin
        scDisplay.ClearPrinterTitle ;
        scDisplay.AddPrinterTitleLine( 'File : ' + EDRFIle.Cdrfh.FileName ) ;
        scDisplay.AddPrinterTitleLine( EDRFIle.Cdrfh.IdentLine ) ;
        scDisplay.Print ;
        end ;
     end ;


procedure TViewSigFrm.CopyImageToClipboard ;
{ -------------------------------------------
  Copy display image to clipboard as metafile
  -------------------------------------------}
begin
     PrintRecFrm.Destination := deClipboard ;
     PrintRecFrm.Display := scDisplay ;
     PrintRecFrm.ShowModal ;
     if PrintRecFrm.ModalResult = mrOK then scDisplay.CopyImageToClipboard ;
     end ;


procedure TViewSigFrm.FormActivate(Sender: TObject);
begin
     { Enable copy and print menus }
     Main.CopyAndPrintMenus( True, True ) ;
     ckFixedZeroLevels.Checked := EDRFile.Settings.FixedZeroLevels ;
     end;


procedure TViewSigFrm.FormDeactivate(Sender: TObject);
begin
     { Disable copy and print menus }
     Main.CopyAndPrintMenus( False, False ) ;
     end;


procedure TViewSigFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

//     if not Self.Active then Self.KeyPreview := False ;
     exit ;
     case key of
          VK_LEFT : scDisplay.MoveActiveVerticalCursor(-1) ;
          VK_RIGHT : scDisplay.MoveActiveVerticalCursor(1) ;
          end ;
end;



procedure  TViewSigFrm.ZoomOutAll ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDisplay.MaxADCValue := EDRFile.Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -EDRFile.Channel[0].ADCMaxValue -1 ;
     scDisplay.ZoomOut ;
     end ;


procedure  TViewSigFrm.NewData ;
// -----------------------------------------
// Update display when EDR data file changes
// -----------------------------------------
var
     ch : Integer ;
begin

     { Get header data from file }
     EDRFile.GetHeader( EDRFile.CdrFH ) ;

     scdisplay.MinADCValue := -EDRFile.Channel[0].ADCMaxValue -1;
     scdisplay.MaxADCValue := EDRFile.Channel[0].ADCMaxValue ;

     scdisplay.NumChannels := EDRFIle.Cdrfh.NumChannels ;
     for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do
        begin
        scdisplay.yMin[ch] := EDRFile.Channel[ch].yMin ;
        scdisplay.yMax[ch] := EDRFile.Channel[ch].yMax ;
        scdisplay.ChanName[ch] := EDRFile.Channel[ch].ADCName ;
        scdisplay.ChanScale[ch] := EDRFile.Channel[ch].ADCScale ;
        scdisplay.ChanUnits[ch] := EDRFile.Channel[ch].ADCUnits ;
        scdisplay.ChanZero[ch] := EDRFile.Channel[ch].ADCZero ;
        scdisplay.ChanVisible[ch] := EDRFile.Channel[ch].InUse ;
        scdisplay.ChanOffsets[ch] := EDRFile.Channel[ch].ChannelOffset ;
        scdisplay.ChanColor[ch] := clBlue ;
        end ;

     EDRFile.Channel[0].xMin := 0.0 ;
     EDRFile.Channel[0].xMax := (EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels)*EDRFIle.Cdrfh.dt ;

     { Create vertical cursors }
     scDisplay.ClearVerticalCursors ;
     Cursors.C0 := scDisplay.AddVerticalCursor(AllChannels, clGreen, 't=0') ;
     Cursors.C1 := scDisplay.AddVerticalCursor(AllChannels, clGreen, '?y?t0') ;

     scDisplay.VerticalCursors[Cursors.C0] := 0 ;
     scDisplay.VerticalCursors[Cursors.C1] := scDisplay.NumPoints div 2  ;

     { Create a set of zero level cursors }
     scDisplay.ClearHorizontalCursors ;
     for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do begin
         Cursors.Base[ch] := scDisplay.AddHorizontalCursor(ch,clRed,True,'z' ) ;
         scDisplay.HorizontalCursors[ch] := EDRFile.Channel[ch].ADCZero ;
         end ;

     for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do begin
         scdisplay.HorizontalCursors[Cursors.Base[ch]] := EDRFile.Channel[ch].ADCZero ;
         end ;

     sbStartTime.Min := 0 ;
     sbStartTime.Max := (EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels) - 1 ;


     // Number of chart markers still available
     edNumMarkers.text := format('%d',[MaxMarkers-EDRFile.MarkerList.Count]);

     edTDisplay.LoLimit := EDRFIle.Cdrfh.dt*32 ;

     // Set display calibration bars
     ChangeDisplayGrid ;

     DisplayFromFile ;

     end ;


procedure TViewSigFrm.SaveDataToFile ;
// -------------------------------------
// Save data on display to CSV text file
// -------------------------------------
var
    TStart,TEnd : single ;
begin

     // Present user with standard Save File dialog box
     Main.SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     Main.SaveDialog.DefaultExt := 'csv' ;
     Main.SaveDialog.Filter := ' CSV Files (*.csv)|*.csv' ;
     Main.SaveDialog.Title := 'Export to CSV File' ;

     // Create default file name
     TStart := sbStartTime.Position*scDisplay.TScale ;
     TEnd := TStart + scDisplay.NumPoints*scDisplay.TScale ;
     Main.SaveDialog.FileName := AnsiReplaceText(
                                 LowerCase(ExtractFileName(EDRFIle.Cdrfh.FileName)),
                                 '.edr',
                                 format('.%.3f-%.3fs.csv',
                                 [TStart,TEnd] ));

     if Main.SaveDialog.execute then scDisplay.SaveDataToFile(Main.SaveDialog.FileName) ;

     end ;


procedure TViewSigFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin

     scDisplay.DisplayGrid := EDRFile.Settings.DisplayGrid ;

     end ;


procedure TViewSigFrm.edDisplayKeyPressSourceKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
// ------------------
// Handle key presses
// ------------------
begin
     case key of
          VK_LEFT : scDisplay.MoveActiveVerticalCursor(-1) ;
          VK_RIGHT : scDisplay.MoveActiveVerticalCursor(1) ;
          end ;

end;

procedure TViewSigFrm.edHighPassFilterFrequencyKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------------------------
// Change display high-pass filter cut-off frequency
// ------------------------------------------------
begin
     if Key = #13 then
        begin
        EDRFile.Settings.HighPassFilterCoeff := scDisplay.HighPassFilterSmoothingFactor(edHighPassFilterFrequency.Value) ;
        scDisplay.HighPassFilterCoeff :=  EDRFile.Settings.HighPassFilterCoeff ;
        end;
end;


procedure TViewSigFrm.edIdentChange(Sender: TObject);
{ -------------------------------------------
  Update identification string in file header
  -------------------------------------------}
begin
     { Update ident line if it is changed }
     EDRFIle.Cdrfh.IdentLine := edIdent.text ;
     EDRFile.SaveHeader(EDRFile.CdrFH) ;
     end;


procedure TViewSigFrm.DisplayFromFile ;
// ------------------------------------------
// Display digitised signals stored on file
// ------------------------------------------
var

    NumScans,MaxScans,NumBytesInBuf,MaxSamples : LongWord ;
    StartScan : Int64 ;
    FilePointer : Int64 ;
    i : Integer ;
    MarkerTime : Single ;
    MarkerAt : Integer ;
    TimeScale : Single ;
begin

     edIdent.Text := EDRFIle.Cdrfh.IdentLine ;

     // Find starting scan number
     StartScan := Round(edStartTime.Value/EDRFIle.Cdrfh.dt) ;
     scDisplay.XOffset := StartScan ;

     // No. of multi-channel scans to be displayed
     // (Note. Limited to <= 400000000 samples to avoid GetMemory() failure to allocate display buffer 05.09.25)

     MaxScans := Min( EDRFIle.Cdrfh.NumSamplesInFile,  400000000 ) div EDRFIle.Cdrfh.NumChannels ;
     edTDisplay.Value := Min( edTDisplay.Value, Max(1.0,MaxScans*EDRFIle.Cdrfh.dt));
     scDisplay.MaxPoints := Round(edTDisplay.Value/EDRFIle.Cdrfh.dt) ;
     NumScans := Max( Min(Round(edTDisplay.Value/EDRFIle.Cdrfh.dt),MaxScans-StartScan),1 ) ;
     scDisplay.NumPoints := NumScans ;

     // Allocate memory buffer
     if DisplayBuf <> Nil then FreeMem(DisplayBuf) ;
     NumBytesInBuf := scDisplay.MaxPoints*EDRFIle.Cdrfh.NumChannels*SizeOf(SmallInt) ;
     DisplayBuf := GetMemory( NumBytesInBuf ) ;

     scDisplay.TScale := EDRFIle.Cdrfh.dt*EDRFile.Settings.TScale ;
     scDisplay.TUnits := EDRFile.Settings.TUnits ;

     // Read data from file
     FilePointer := EDRFIle.Cdrfh.NumBytesInHeader + StartScan*EDRFIle.Cdrfh.NumChannels*SizeOf(SmallInt) ;
     FileSeek( EDRFIle.Cdrfh.FileHandle, FilePointer, 0 ) ;
     FileRead(EDRFIle.Cdrfh.FileHandle,DisplayBuf^,NumBytesInBuf) ;

     scDisplay.xMin := 0 ;
     scDisplay.xMax := Max(scDisplay.MaxPoints-1,1) ;

     // Enable/disable display calibration grid
     scDisplay.SetDataBuf( DisplayBuf ) ;

     scDisplay.VerticalCursors[Cursors.C0] := TZeroScan - scDisplay.xOffset ;
     scDisplay.VerticalCursors[Cursors.C1] := scDisplay.MaxPoints div 2 ;

     // Add markers (if any appear on display
     scDisplay.ClearMarkers ;
     TimeScale := scDisplay.TScale/EDRFile.Settings.TScale ;
     for i := 0 to EDRFile.MarkerList.Count-1 do
         begin
         MarkerTime := Single(EDRFile.MarkerList.Objects[i]) ;
         MarkerAt := Round(MarkerTime/TimeScale) - scDisplay.XOffset ;
         if (MarkerAt >= 0) and (MarkerAt < scDisplay.MaxPoints) then
            scDisplay.AddMarker( MarkerAt, EDRFile.MarkerList.Strings[i] );
         end ;

     scDisplay.Invalidate ;

     sbStartTime.LargeChange := Max(NumScans div 20,1) ;

     end ;


procedure TViewSigFrm.edTDisplayKeyPress(Sender: TObject; var Key: Char);
// -------------------------------
// Display window duration changed
// -------------------------------
begin
     if Key = #13 then begin
        DisplayFromFile ;
        EDRFile.Settings.DisplayDuration := edTDisplay.Value ;
        end ;
     end;


procedure TViewSigFrm.edStartTimeKeyPress(Sender: TObject; var Key: Char);
// ---------------------------------
// Display window start time changed
// ---------------------------------
begin
     if Key = #13 then begin
        sbStartTime.Position := Round(edStartTime.Value/EDRFIle.Cdrfh.dt) ;
        DisplayFromFile ;
        end ;
     end;


procedure TViewSigFrm.sbStartTimeChange(Sender: TObject);
// ------------------------------------------
// Display window position scroll bar changed
// ------------------------------------------
begin
    edStartTime.Value := EDRFIle.Cdrfh.dt*(sbStartTime.Position) ;
    DisplayFromFile ;
    end;


procedure TViewSigFrm.scDisplayCursorChange(Sender: TObject);
// -----------------------------------------
// Respond to display cursor position change
// -----------------------------------------
var
   ch,Cursor1Pos,Cursor0Pos : Cardinal ;
begin

      { Time zero cursor }
      Cursor0Pos := Round(scDisplay.VerticalCursors[Cursors.C0]) ;
      if Cursor0Pos >= 0 then begin
         TZeroScan := Round((scDisplay.XOffset + Cursor0Pos)*scDisplay.TScale/(EDRFIle.Cdrfh.dt*EDRFile.Settings.TScale)) ;
         end ;

      { Read out cursor }
      Cursor1Pos := Round(scDisplay.VerticalCursors[Cursors.C1]) ;
      Cursor1Pos := Min(Cursor1Pos,scDisplay.MaxPoints-1) ;

      EDRFile.Channel[0].CursorTime := (Cursor1Pos + scDisplay.XOffset)*scDisplay.TScale
                                - (TZeroScan*EDRFIle.Cdrfh.dt*EDRFile.Settings.TScale);

      for ch := 0 to scDisplay.NumChannels-1 do
          begin

          { Get signal baseline cursor }
          if EDRFile.Settings.FixedZeroLevels then
             begin
             if scDisplay.HorizontalCursors[ch] <> EDRFile.Channel[ch].ADCZero then
                scDisplay.HorizontalCursors[ch] := EDRFile.Channel[ch].ADCZero ;
             end
          else
             begin
             EDRFile.Channel[ch].ADCZero := Round(scDisplay.HorizontalCursors[ch]) ;
             end ;

          { Signal level at cursor }
          EDRFile.Channel[ch].CursorValue := EDRFile.Channel[ch].ADCScale *
                                    ( DisplayBuf^[(Cursor1Pos*scDisplay.NumChannels)
                                      + EDRFile.Channel[ch].ChannelOffset] - EDRFile.Channel[ch].ADCZero ) ;
          end ;

      { Update channel descriptors with any changes to display }
      for ch := 0 to scDisplay.NumChannels-1 do begin
          EDRFile.Channel[Ch].InUse := scDisplay.ChanVisible[ch] ;
          EDRFile.Channel[Ch].yMin := scDisplay.YMin[Ch] ;
          EDRFile.Channel[Ch].yMax := scDisplay.YMax[Ch] ;
          end ;
      EDRFile.Channel[0].xMin := scDisplay.xMin ;
      EDRFile.Channel[0].xMax := scDisplay.xMax ;

      TScopeDisplay(Sender).CursorChangeInProgress := False ;


      end;


procedure TViewSigFrm.rbTUnitsSecondsClick(Sender: TObject);
// ---------------------------------
// Set time readout units to seconds
// ---------------------------------
begin
    EDRFile.Settings.TUnits := 's' ;
    EDRFile.Settings.TSCale := 1.0 ;
    edStartTime.Scale := EDRFile.Settings.TSCale ;
    edStartTime.Units := EDRFile.Settings.TUnits ;
    edTDisplay.Scale := EDRFile.Settings.TSCale ;
    edTDisplay.Units := EDRFile.Settings.TUnits ;
    DisplayFromFile ;
    end;

procedure TViewSigFrm.rbTUnitsMsecsClick(Sender: TObject);
// ---------------------------------
// Set time readout units to seconds
// ---------------------------------
begin
    EDRFile.Settings.TUnits := 'ms' ;
    EDRFile.Settings.TSCale := 1000.0 ;
    edStartTime.Scale := EDRFile.Settings.TSCale ;
    edStartTime.Units := EDRFile.Settings.TUnits ;
    edTDisplay.Scale := EDRFile.Settings.TSCale ;
    edTDisplay.Units := EDRFile.Settings.TUnits ;

    DisplayFromFile ;
    end;

procedure TViewSigFrm.bMarkClick(Sender: TObject);
// ------------------------------
//  Add a text marker to the chart
// ------------------------------
var
     MarkerTime : Single ;
     TimeScale : Single ;
begin

     if EDRFile.MarkerList.Count < MaxMarkers then begin

          TimeScale := scDisplay.TScale/EDRFile.Settings.TScale ;

          // Mark replay
          MarkerTime := (scDisplay.VerticalCursors[Cursors.C1] + scDisplay.XOffset)*TimeScale ;
          // Plot marker on chart
          EDRFile.MarkerList.AddObject( EdMarker.text, TObject(MarkerTime) ) ;
          scDisplay.AddMarker( Round(MarkerTime/TimeScale) - scDisplay.XOffset,
                               EdMarker.text );

          end ;
     edNumMarkers.text := format('%d',[MaxMarkers-EDRFile.MarkerList.Count]);
     end;


procedure TViewSigFrm.rbTUnitsMinutesClick(Sender: TObject);
// ---------------------------------
// Set time readout units to minutes
// ---------------------------------
begin
    EDRFile.Settings.TUnits := 'm' ;
    EDRFile.Settings.TSCale := 1.0/60.0 ;
    edStartTime.Scale := EDRFile.Settings.TSCale ;
    edStartTime.Units := EDRFile.Settings.TUnits ;
    edTDisplay.Scale := EDRFile.Settings.TSCale ;
    edTDisplay.Units := EDRFile.Settings.TUnits ;
    DisplayFromFile ;
    end;

procedure TViewSigFrm.scDisplayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ----------------------
// Set channel zero level
// ----------------------
begin
     if (Button = mbRight) and (scDisplay.ActiveHorizontalCursor >=0) then
        begin
        // If right-mouse button down, display zero baseline level selection dialog box
        ZeroFrm.ChSel := scDisplay.ActiveHorizontalCursor ;
        ZeroFrm.ZeroLevel := EDRFile.Channel[ZeroFrm.ChSel].ADCZero ;
        ZeroFrm.ChanName := EDRFile.Channel[ZeroFrm.ChSel].ADCName ;
        ZeroFrm.NewZeroAt := Round(scDisplay.ScreenCoordToX( ZeroFrm.ChSel, X )) ;
        ZeroFrm.Left := ViewSigFrm.Left + Main.Left + 10 + scDisplay.Left + X;
        ZeroFrm.Top := ViewSigFrm.Top + Main.Top + 10 + scDisplay.Top + Y ;
        ZeroFrm.ShowModal ;
        EDRFile.Channel[ZeroFrm.ChSel].ADCZero := ZeroFrm.ZeroLevel ;
        EDRFile.Channel[ZeroFrm.ChSel].ADCZero := Max(-EDRFile.Channel[ZeroFrm.ChSel].ADCMaxValue-1,ZeroFrm.ZeroLevel) ;
        EDRFile.Channel[ZeroFrm.ChSel].ADCZero := Min(EDRFile.Channel[ZeroFrm.ChSel].ADCMaxValue,ZeroFrm.ZeroLevel) ;
        EDRFile.Channel[ZeroFrm.ChSel].ADCZeroAt := -1 ;
        EDRFile.SaveHeader( EDRFile.CDRfH ) ;
        scDisplay.HorizontalCursors[ZeroFrm.ChSel] := EDRFile.Channel[ZeroFrm.ChSel].ADCZero ;
        end ;

    // Move focus of form to hidden control used to source  <- -> arrow key presses
    // used to control display readout cursors. Note only set focus if form is active
    // to avoid cursorchange events in inactive forms pulling focus back to recently
    // inactivated forms
    if Self.Active then edDisplayKeyPressSource.SetFocus ;

     end;

procedure TViewSigFrm.bGetTZeroClick(Sender: TObject);
// ------------------------------
// Bring t=0 cursor on to display
// ------------------------------
begin
     TZeroScan := sbStartTime.Position + 1 ;
     DisplayFromFile ;
     end;

procedure TViewSigFrm.rbTDisplayUnitMinsClick(Sender: TObject);
// ---------------------------------
// Set time readout units to minutes
// ---------------------------------
begin
    EDRFile.Settings.TUnits := 'm' ;
    EDRFile.Settings.TSCale := 1.0/60.0 ;
    edStartTime.Scale := EDRFile.Settings.TSCale ;
    edStartTime.Units := EDRFile.Settings.TUnits ;
    edTDisplay.Scale := EDRFile.Settings.TSCale ;
    edTDisplay.Units := EDRFile.Settings.TUnits ;
    DisplayFromFile ;
    end;

procedure TViewSigFrm.rbTDisplayUnitsSecsClick(Sender: TObject);
// ---------------------------------
// Set time readout units to seconds
// ---------------------------------
begin
    EDRFile.Settings.TUnits := 's' ;
    EDRFile.Settings.TSCale := 1.0 ;
    edStartTime.Scale := EDRFile.Settings.TSCale ;
    edStartTime.Units := EDRFile.Settings.TUnits ;
    edTDisplay.Scale := EDRFile.Settings.TSCale ;
    edTDisplay.Units := EDRFile.Settings.TUnits ;
    DisplayFromFile ;
    end;


procedure TViewSigFrm.bTDisplayDoubleClick(Sender: TObject);
// -------------------------------
// Double display window duration
// -------------------------------
begin
     edTDisplay.Value := edTDisplay.Value*2.0 ;
     DisplayFromFile ;
     EDRFile.Settings.DisplayDuration := edTDisplay.Value ;
     end;


procedure TViewSigFrm.bTDisplayHalfClick(Sender: TObject);
// -------------------------------
// Halve display window duration
// -------------------------------
begin
     edTDisplay.Value := edTDisplay.Value*0.5 ;
     DisplayFromFile ;
     EDRFile.Settings.DisplayDuration := edTDisplay.Value ;
     end;


procedure TViewSigFrm.rbTDisplayUnitsMSecsClick(Sender: TObject);
// --------------------------------------
// Set time readout units to milliseconds
// --------------------------------------
begin
    EDRFile.Settings.TUnits := 'ms' ;
    EDRFile.Settings.TSCale := 1000.0 ;
    edStartTime.Scale := EDRFile.Settings.TSCale ;
    edStartTime.Units := EDRFile.Settings.TUnits ;
    edTDisplay.Scale := EDRFile.Settings.TSCale ;
    edTDisplay.Units := EDRFile.Settings.TUnits ;
    DisplayFromFile ;
    end;


procedure TViewSigFrm.ZoomOut(
          ChanNum : Integer ) ;
// ------------------------------------
// Magnify selected A/D channel display
// ------------------------------------
begin

     scDisplay.YZoom( ChanNum, 50.0 ) ;

     end ;


procedure TViewSigFrm.ZoomIn( ChanNum : Integer ) ;
// ------------------------------------
// Reduce selected A/D channel display
// ------------------------------------
begin

     scDisplay.YZoom( ChanNum, -50.0 ) ;

     end ;




procedure TViewSigFrm.bCalcAverageClick(Sender: TObject);
// ----------------------------------------------------------------
// Calculate and display average signal between t=0 and t=? cursors
// ----------------------------------------------------------------
const
    NumScansPerBuf = 256 ;
var
    ch,i,j,iScan : Integer ;
    TScan : Integer ;
    Sum : Array[0..MaxChannels-1] of Single ;
    Avg : Single ;
    NumAvg : Integer ;
    NumBytesPerScan,NumScansInBuf,NumScansToDo,NumScansToRead : LongWord ;
    FilePointer : Int64 ;
    Buf : PSmallIntArrayDyn ;
begin

      // Initialise
      for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do Sum[ch] := 0.0 ;
      NumAvg := 0 ;
      NumBytesPerScan := EDRFIle.Cdrfh.NumChannels*2 ;
      GetMem( Buf, NumBytesPerScan*NumScansPerBuf ) ;

      // No of scans to average
      TScan := Round((scDisplay.XOffset + scDisplay.VerticalCursors[Cursors.C1])
                      *scDisplay.TScale/(EDRFIle.Cdrfh.dt*EDRFile.Settings.TScale)) ;
      NumScansToDo := Abs(TScan - TZeroScan) + 1 ;
      // Start at scan
      iScan := Min(TScan,TZeroScan) ;

      // Add scans to summation array

      while NumScansToDo > 0 do begin

          // Read data into buffer from file
          FilePointer := EDRFIle.Cdrfh.NumBytesInHeader + iScan*EDRFIle.Cdrfh.NumChannels*2 ;
          FileSeek( EDRFIle.Cdrfh.FileHandle, FilePointer, 0 ) ;
          NumScansToRead := Min(NumScansPerBuf,NumScansToDo) ;
          NumScansInBuf := FileRead(EDRFIle.Cdrfh.FileHandle,Buf^,NumBytesPerScan*NumScansToRead)
                           div NumBytesPerScan ;

          // Add buffer data to sum
          j := 0 ;
          for i := 0 to NumScansInBuf-1 do begin
              for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do begin
                  Sum[ch] := Sum[ch] + Buf^[j] ;
                  Inc(j) ;
                  end ;
              Inc(NumAvg) ;
              end ;

          // Next buffer
          NumScansToDo := NumScansToDo - NumScansInBuf ;
          iScan := iScan + NumScansInBuf ;
          if NumScansInBuf = 0 then NumScansToDo := 0 ;

          end ;

      // Display average
      meCursor.Clear ;

      meCursor.Lines.Add( format( 't= %.4g - %.4g s ',
                          [TZeroScan*EDRFIle.Cdrfh.dt*EDRFile.Settings.TScale,
                           TScan*EDRFIle.Cdrfh.dt*EDRFile.Settings.TScale])) ;
      meCursor.Lines.Add(format( 'No. Samples=  %d ',[NumAvg])) ;
      for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do begin
          { Add to readout list }
          Avg := EDRFile.Channel[ch].ADCScale*
                 ( (Sum[ch]/Max(NumAvg,1)) - EDRFile.Channel[ch].ADCZero) ;
          meCursor.Lines.Add(format( '%s= %.4g %s',
                                     [EDRFile.Channel[ch].ADCName,
                                      Avg,
                                      EDRFile.Channel[ch].ADCUnits] ) );
          end ;

      FreeMem( Buf ) ;

     end;

procedure TViewSigFrm.ckFixedZeroLevelsClick(Sender: TObject);
// --------------------------------
// Enable/Disable fixed zero levels
// --------------------------------
begin
     EDRFile.Settings.FixedZeroLevels := ckFixedZeroLevels.Checked ;
     end;

procedure TViewSigFrm.ckHighPassFilterActiveClick(Sender: TObject);
// -----------------------------------
// Turn display high-pass filter on/off
// -----------------------------------
begin
     EDRFile.Settings.HighPassFilterCoeff := scDisplay.HighPassFilterSmoothingFactor(edHighPassFilterFrequency.Value) ;
     scDisplay.HighPassFilterCoeff :=  EDRFile.Settings.HighPassFilterCoeff ;
     EDRFile.Settings.HighPassFilterActive := ckHighPassFilterActive.Checked ;
     scDisplay.HighPassFilterOn := ckHighPassFilterActive.Checked ;
end;


procedure TViewSigFrm.ckLowPassFilterActiveClick(Sender: TObject);
// -----------------------------------
// Turn display low-pass filter on/off
// -----------------------------------
begin
     EDRFile.Settings.LowPassFilterCoeff := scDisplay.LowPassFilterSmoothingFactor(edLowPassFilterFrequency.Value) ;
     scDisplay.LowPassFilterCoeff :=  EDRFile.Settings.LowPassFilterCoeff ;
     EDRFile.Settings.LowPassFilterActive := ckLowPassFilterActive.Checked ;
     scDisplay.LowPassFilterOn := ckLowPassFilterActive.Checked ;
end;

end.
