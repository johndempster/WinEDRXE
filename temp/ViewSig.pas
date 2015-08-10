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
  ========================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, global, printers, ClipBrd, fileio,
  Grids, CDRZero, RangeEdit, ScopeDisplay,
  ValidatedEdit, maths, seslabio, math ;

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

  private
    { Private declarations }
    Cursors : TCursors ; // Display cursor record
    TZeroScan : Integer ;                // Scan # defined as zero time

  public
    { Public declarations }
    procedure PrintDisplay  ;
    procedure CopyImageToClipboard ;
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

uses mdiform,shared,Printrec , Zero;

const
     MaxDisplayScans = 2000 ;


var
   DisplayBuf : PSmallIntArray ; // Data display buffer pointer
{$R *.DFM}


procedure TViewSigFrm.FormShow(Sender: TObject);
{ ---------------------------------------
  Initialisations done when form is shown
  ---------------------------------------}
begin

     Top := 10 ;
     Left := 10 ;

     Main.mnViewSig.Enabled := False ;

     // Allocate display buffer
     GetMem( DisplayBuf, MaxDisplayScans*(EDRChannelLimit+1)*8 ) ;

     ClientHeight := TCursorGrp.Top + TCursorGrp.Height + 10 ;
     Resize ;

     { Display ident. info. }
     EdIdent.text := CdrFH.IdentLine ;

     Main.SetCopyMenu(True,True) ;

     // Initialise time readout units
     if Settings.TScale = 1000.0 then rbTDisplayUnitsMsecs.Checked := True
                                 else rbTDisplayUnitsSecs.Checked := True ;
     edStartTime.Scale := Settings.TSCale ;
     edStartTime.Units := Settings.TUnits ;
     edTDisplay.Value := Settings.DisplayDuration ;
     edTDisplay.Scale := Settings.TSCale ;
     edTDisplay.Units := Settings.TUnits ;

     ckFixedZeroLevels.Checked := Settings.FixedZeroLevels ;

     if CdrFH.NumSamplesInFile > 0 then Main.mnPrint.enabled := True ;

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
     SaveCDRHeader( CdrFH ) ;

     Main.mnViewSig.Enabled := True ;

    { Disable copy and print menus }
     Main.CopyAndPrintMenus( False, False ) ;
     Action := caFree ;
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
     TDisplayPanel.Left := scDisplay.Left + scDisplay.Width
                           - TDisplayPanel.Width ;
     sbDisplayPanel.Width := Max(TDisplayPanel.Left - sbDisplayPanel.Left - 2,2) ;
     sbStartTime.Width := sbDisplayPanel.ClientWidth - sbStartTime.Left - 1 ;

     // Set height of display
     scDisplay.Height := Max( sbDisplayPanel.Top - scDisplay.Top, 2 ) ;

     { Set width of identification text box }
     edIdent.Width := scDisplay.Left
                    + scDisplay.Width
                    - edIdent.Left ;

     end;


procedure TViewSigFrm.edIdentKeyPress(Sender: TObject; var Key: Char);
{ -------------------------------------------
  Write to log file
  -------------------------------------------}
begin
     if key = chr(13) then WriteToLogFile( edIdent.text ) ;
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
        scDisplay.AddPrinterTitleLine( 'File : ' + cdrFH.FileName ) ;
        scDisplay.AddPrinterTitleLine( CdrFH.IdentLine ) ;
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
     ckFixedZeroLevels.Checked := Settings.FixedZeroLevels ;
     end;


procedure TViewSigFrm.FormDeactivate(Sender: TObject);
begin
     { Disable copy and print menus }
     Main.CopyAndPrintMenus( False, False ) ;
     end;


procedure  TViewSigFrm.ZoomOutAll ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
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
     GetCDRHeader( CdrFH ) ;

     scdisplay.MinADCValue := -Channel[0].ADCMaxValue -1;
     scdisplay.MaxADCValue := Channel[0].ADCMaxValue ;

     scdisplay.NumChannels := CdrFH.NumChannels ;
     for ch := 0 to CdrFH.NumChannels-1 do begin
        scdisplay.yMin[ch] := Channel[ch].yMin ;
        scdisplay.yMax[ch] := Channel[ch].yMax ;
        scdisplay.ChanName[ch] := Channel[ch].ADCName ;
        scdisplay.ChanScale[ch] := Channel[ch].ADCScale ;
        scdisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
        scdisplay.ChanZero[ch] := Channel[ch].ADCZero ;
        scdisplay.ChanVisible[ch] := Channel[ch].InUse ;
        scdisplay.ChanOffsets[ch] := Channel[ch].ChannelOffset ;
        scdisplay.ChanColor[ch] := clBlue ;
        end ;

     Channel[0].xMin := 0.0 ;
     Channel[0].xMax := (CdrFH.NumSamplesInFile div CdrFH.NumChannels)*CdrFH.dt ;

     { Create vertical cursors }
     scDisplay.ClearVerticalCursors ;
     Cursors.C0 := scDisplay.AddVerticalCursor(AllChannels, clGreen, 't=0') ;
     Cursors.C1 := scDisplay.AddVerticalCursor(AllChannels, clGreen, '?y?t0') ;

     scDisplay.VerticalCursors[Cursors.C0] := 0 ;
     scDisplay.VerticalCursors[Cursors.C1] := scDisplay.NumPoints div 2  ;

     { Create a set of zero level cursors }
     scDisplay.ClearHorizontalCursors ;
     for ch := 0 to CDRFH.NumChannels-1 do begin
         Cursors.Base[ch] := scDisplay.AddHorizontalCursor(ch,clRed,True,'z' ) ;
         scDisplay.HorizontalCursors[ch] := Channel[ch].ADCZero ;
         end ;

     for ch := 0 to CdrFH.NumChannels-1 do begin
         scdisplay.HorizontalCursors[Cursors.Base[ch]] := Channel[ch].ADCZero ;
         end ;

     sbStartTime.Min := 0 ;
     sbStartTime.Max := (CdrFH.NumSamplesInFile div CdrFH.NumChannels) - 1 ;
     

     // Number of chart markers still available
     edNumMarkers.text := format('%d',[MaxMarkers-MarkerList.Count]);

     edTDisplay.LoLimit := CDRFH.dt*32 ;

     // Set display calibration bars
     ChangeDisplayGrid ;

     DisplayFromFile ;

     end ;


procedure TViewSigFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin

     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     end ;


procedure TViewSigFrm.edIdentChange(Sender: TObject);
{ -------------------------------------------
  Update identification string in file header
  -------------------------------------------}
begin
     { Update ident line if it is changed }
     CdrFH.IdentLine := edIdent.text ;
     SaveCDRHeader(CdrFH) ;
     end;


procedure TViewSigFrm.DisplayFromFile ;
// ------------------------------------------
// Display digitised signals stored on file
// ------------------------------------------
var

    NumScans : Integer ;
    NumScansPerBlock : Integer ;
    NumSamplesPerBlock : Integer ;
    NumPointsPerBlock : Integer ;
    NumSamplesPerBuf : Integer ;
    NumSamplesRead : Integer ;
    StartScan : Integer ;
    BlockCount : Integer ;
    NumPoints : Integer ;
    FilePointer : Integer ;
    iDisp : Integer ;
    Done : Boolean ;
    yMin : Array[0..EDRChannelLimit] of Integer ;
    yMax : Array[0..EDRChannelLimit] of Integer ;
    yMinAt : Array[0..EDRChannelLimit] of Integer ;
    yMaxAt : Array[0..EDRChannelLimit] of Integer ;
    Buf : Array[0..(256*(EDRChannelLimit+1)-1)] of SmallInt ;
    i,ch,y : Integer ;
    MarkerTime : Single ;
    MarkerAt : Integer ;
    TimeScale : Single ;
begin

     edIdent.Text := CDRFH.IdentLine ;

     // No. of multi-channel scans to be displayed
     NumScans := MaxInt( [Round(edTDisplay.Value/CDRFH.dt),1] ) ;

     // Size of display compression block
     NumScansPerBlock := MaxInt( [NumScans div MaxDisplayScans,1]) ;
     NumSamplesPerBlock := NumScansPerBlock*CDRFH.NumChannels ;
     // No. of display points per compression block
     NumPointsPerBlock := MinInt([NumScansPerBlock,2]) ;
     // Max. number of points in display
     scDisplay.MaxPoints := (NumScans div NumScansPerBlock)*NumPointsPerBlock ;

     // No. of samples in file I/O buffer
     NumSamplesPerBuf := CDRFH.NumChannels*256 ;

     // Find starting scan number
     StartScan := Round(edStartTime.Value/CDRFH.dt) ;
     scDisplay.XOffset := (StartScan*NumPointsPerBlock) div NumScansPerBlock ;

     scDisplay.TScale := (CDRFH.dt*NumScansPerBlock*Settings.TScale)/NumPointsPerBlock ;
     scDisplay.TUnits := Settings.TUnits ;

     // Move file pointer to start of data
     FilePointer := CDRFH.NumBytesInHeader + StartScan*CDRFH.NumChannels*2 ;
     FileSeek( CDRFH.FileHandle, FilePointer, 0 ) ;

     // Initialise counters
     BlockCount := NumScansPerBlock ;
     NumSamplesRead := NumSamplesPerBuf ;
     i := NumSamplesRead ;
     iDisp := 0 ;
     NumPoints := 0 ;
     Done := False ;

     // Read samples from file
     While not Done do begin

        // Initialise block
        if BlockCount >= NumScansPerBlock then begin
           for ch := 0 to CDRFH.NumChannels-1 do begin
               yMin[ch] := Channel[0].ADCMaxValue ;
               yMax[ch] := -Channel[0].ADCMaxValue -1 ;
               end ;
           BlockCount := 0 ;
           end ;

        // Load new buffer
        if i >= NumSamplesRead then begin
           NumSamplesRead := FileRead(CDRFH.FileHandle,Buf,NumSamplesPerBuf*2) div 2 ;
           i := 0 ;
           if NumSamplesRead <= 0 then Break ;
           end ;

        // Determine min. / max. value & order of samples within compression block
        for ch := 0 to CDRFH.NumChannels-1 do begin

            // Get A/D sample
            y := Buf[i] ;

            if y < yMin[ch] then begin
               yMin[ch] := y ;
               yMinAt[ch] := BlockCount ;
               end ;
            if y > yMax[ch] then begin
               yMax[ch] := y ;
               yMaxAt[ch] := BlockCount ;
               end ;
            Inc(i) ;
            end ;
        Inc(BlockCount) ;

        // When block complete ... write min./max. to display buffer
        if BlockCount >= NumScansPerBlock then begin

           // First point
           for ch := 0 to CDRFH.NumChannels-1 do begin
               if yMaxAt[ch] <= yMinAt[ch] then DisplayBuf^[iDisp] := yMax[ch]
                                           else DisplayBuf^[iDisp] := yMin[ch] ;
               Inc(iDisp) ;
               end ;
           Inc(NumPoints) ;

           // Second point
           if BlockCount > 1 then begin
              for ch := 0 to CDRFH.NumChannels-1 do begin
                  if yMaxAt[ch] >= yMinAt[ch] then DisplayBuf^[iDisp] := yMax[ch]
                                              else DisplayBuf^[iDisp] := yMin[ch] ;
                  Inc(iDisp) ;
                  end ;
              Inc(NumPoints) ;
              end ;

           end ;

        if NumPoints >= scDisplay.MaxPoints then Done := True ;

        end ;

     scDisplay.NumPoints := NumPoints ;

     scDisplay.xMin := 0 ;
     scDisplay.xMax := MaxInt([scDisplay.MaxPoints-1,1]) ;
     // Enable/disable display calibration grid

     scDisplay.SetDataBuf( DisplayBuf ) ;

     scDisplay.VerticalCursors[Cursors.C0] := ((TZeroScan*NumPointsPerBlock) div NumScansPerBlock)
                                              - scDisplay.xOffset ;

     scDisplay.VerticalCursors[Cursors.C1] := scDisplay.MaxPoints div 2 ;

     // Add markers (if any appear on display
     scDisplay.ClearMarkers ;
     TimeScale := scDisplay.TScale/Settings.TScale ;
     for i := 0 to MarkerList.Count-1 do begin
         MarkerTime := Single(MarkerList.Objects[i]) ;
         MarkerAt := Round(MarkerTime/TimeScale) - scDisplay.XOffset ;
         if (MarkerAt >= 0) and (MarkerAt < scDisplay.MaxPoints) then
            scDisplay.AddMarker( MarkerAt, MarkerList.Strings[i] );
         end ;

     scDisplay.Invalidate ;

     sbStartTime.LargeChange := Maxint( [NumScans div 4,1]) ;

     end ;



procedure TViewSigFrm.edTDisplayKeyPress(Sender: TObject; var Key: Char);
// -------------------------------
// Display window duration changed
// -------------------------------
begin
     if Key = #13 then begin
        DisplayFromFile ;
        Settings.DisplayDuration := edTDisplay.Value ;
        end ;
     end;


procedure TViewSigFrm.edStartTimeKeyPress(Sender: TObject; var Key: Char);
// ---------------------------------
// Display window start time changed
// ---------------------------------
begin
     if Key = #13 then begin
        sbStartTime.Position := Round(edStartTime.Value/CDRFH.dt) ;
        DisplayFromFile ;
        end ;
     end;


procedure TViewSigFrm.sbStartTimeChange(Sender: TObject);
// ------------------------------------------
// Display window position scroll bar changed
// ------------------------------------------
begin
    edStartTime.Value := CDRFH.dt*(sbStartTime.Position) ;
    DisplayFromFile ;
    end;


procedure TViewSigFrm.scDisplayCursorChange(Sender: TObject);
// -----------------------------------------
// Respond to display cursor position change
// -----------------------------------------
var
   ch,Cursor1Pos,Cursor0Pos : Integer ;
begin

      { Time zero cursor }
      Cursor0Pos := scDisplay.VerticalCursors[Cursors.C0] ;
      if Cursor0Pos >= 0 then begin
         TZeroScan := Round((scDisplay.XOffset + Cursor0Pos)*scDisplay.TScale/(CDRFH.dt*Settings.TScale)) ;
         end ;

      { Read out cursor }
      Cursor1Pos := scDisplay.VerticalCursors[Cursors.C1] ;

      Channel[0].CursorTime := (Cursor1Pos + scDisplay.XOffset)*scDisplay.TScale
                                - (TZeroScan*CDRFH.dt*Settings.TScale);

      for ch := 0 to scDisplay.NumChannels-1 do begin

          { Get signal baseline cursor }
          if Settings.FixedZeroLevels then begin
             if scDisplay.HorizontalCursors[ch] <> Channel[ch].ADCZero then
                scDisplay.HorizontalCursors[ch] := Channel[ch].ADCZero ;
             end
          else begin
             Channel[ch].ADCZero := scDisplay.HorizontalCursors[ch] ;
             end ;

          { Signal level at cursor }
          Channel[ch].CursorValue := Channel[ch].ADCScale *
                                    ( DisplayBuf^[(Cursor1Pos*scDisplay.NumChannels)
                                      + Channel[ch].ChannelOffset] - Channel[ch].ADCZero ) ;
          end ;

      { Update channel descriptors with any changes to display }
      for ch := 0 to scDisplay.NumChannels-1 do begin
          Channel[Ch].InUse := scDisplay.ChanVisible[ch] ;
          Channel[Ch].yMin := scDisplay.YMin[Ch] ;
          Channel[Ch].yMax := scDisplay.YMax[Ch] ;
          end ;
      Channel[0].xMin := scDisplay.xMin ;
      Channel[0].xMax := scDisplay.xMax ;

      TScopeDisplay(Sender).CursorChangeInProgress := False ;

      end;


procedure TViewSigFrm.rbTUnitsSecondsClick(Sender: TObject);
// ---------------------------------
// Set time readout units to seconds
// ---------------------------------
begin
    Settings.TUnits := 's' ;
    Settings.TSCale := 1.0 ;
    edStartTime.Scale := Settings.TSCale ;
    edStartTime.Units := Settings.TUnits ;
    edTDisplay.Scale := Settings.TSCale ;
    edTDisplay.Units := Settings.TUnits ;
    DisplayFromFile ;
    end;

procedure TViewSigFrm.rbTUnitsMsecsClick(Sender: TObject);
// ---------------------------------
// Set time readout units to seconds
// ---------------------------------
begin
    Settings.TUnits := 'ms' ;
    Settings.TSCale := 1000.0 ;
    edStartTime.Scale := Settings.TSCale ;
    edStartTime.Units := Settings.TUnits ;
    edTDisplay.Scale := Settings.TSCale ;
    edTDisplay.Units := Settings.TUnits ;

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

     if MarkerList.Count < MaxMarkers then begin

          TimeScale := scDisplay.TScale/Settings.TScale ;

          // Mark replay
          MarkerTime := (scDisplay.VerticalCursors[Cursors.C1] + scDisplay.XOffset)*TimeScale ;
          // Plot marker on chart
          MarkerList.AddObject( EdMarker.text, TObject(MarkerTime) ) ;
          scDisplay.AddMarker( Round(MarkerTime/TimeScale) - scDisplay.XOffset,
                               EdMarker.text );

          end ;
     edNumMarkers.text := format('%d',[MaxMarkers-MarkerList.Count]);
     end;


procedure TViewSigFrm.rbTUnitsMinutesClick(Sender: TObject);
// ---------------------------------
// Set time readout units to minutes
// ---------------------------------
begin
    Settings.TUnits := 'm' ;
    Settings.TSCale := 1.0/60.0 ;
    edStartTime.Scale := Settings.TSCale ;
    edStartTime.Units := Settings.TUnits ;
    edTDisplay.Scale := Settings.TSCale ;
    edTDisplay.Units := Settings.TUnits ;
    DisplayFromFile ;
    end;

procedure TViewSigFrm.scDisplayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ----------------------
// Set channel zero level
// ----------------------
begin
     if (Button = mbRight) and (scDisplay.ActiveHorizontalCursor >=0) then begin
        // If right-mouse button down, display zero baseline level selection dialog box
        ZeroFrm.ChSel := scDisplay.ActiveHorizontalCursor ;
        ZeroFrm.ZeroLevel := Channel[ZeroFrm.ChSel].ADCZero ;
        ZeroFrm.ChanName := Channel[ZeroFrm.ChSel].ADCName ;
        ZeroFrm.NewZeroAt := Round(scDisplay.ScreenCoordToX( ZeroFrm.ChSel, X )) ;
        ZeroFrm.Left := ViewSigFrm.Left + Main.Left + 10 + scDisplay.Left + X;
        ZeroFrm.Top := ViewSigFrm.Top + Main.Top + 10 + scDisplay.Top + Y ;
        ZeroFrm.ShowModal ;
        Channel[ZeroFrm.ChSel].ADCZero := ZeroFrm.ZeroLevel ;
        Channel[ZeroFrm.ChSel].ADCZero := Max(-Channel[ZeroFrm.ChSel].ADCMaxValue-1,ZeroFrm.ZeroLevel) ;
        Channel[ZeroFrm.ChSel].ADCZero := Min(Channel[ZeroFrm.ChSel].ADCMaxValue,ZeroFrm.ZeroLevel) ;
        Channel[ZeroFrm.ChSel].ADCZeroAt := -1 ;
        SaveCDRHeader( CDRfH ) ;
        scDisplay.HorizontalCursors[ZeroFrm.ChSel] := Channel[ZeroFrm.ChSel].ADCZero ;
        end
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
    Settings.TUnits := 'm' ;
    Settings.TSCale := 1.0/60.0 ;
    edStartTime.Scale := Settings.TSCale ;
    edStartTime.Units := Settings.TUnits ;
    edTDisplay.Scale := Settings.TSCale ;
    edTDisplay.Units := Settings.TUnits ;
    DisplayFromFile ;
    end;

procedure TViewSigFrm.rbTDisplayUnitsSecsClick(Sender: TObject);
// ---------------------------------
// Set time readout units to seconds
// ---------------------------------
begin
    Settings.TUnits := 's' ;
    Settings.TSCale := 1.0 ;
    edStartTime.Scale := Settings.TSCale ;
    edStartTime.Units := Settings.TUnits ;
    edTDisplay.Scale := Settings.TSCale ;
    edTDisplay.Units := Settings.TUnits ;
    DisplayFromFile ;
    end;


procedure TViewSigFrm.bTDisplayDoubleClick(Sender: TObject);
// -------------------------------
// Double display window duration
// -------------------------------
begin
     edTDisplay.Value := edTDisplay.Value*2.0 ;
     DisplayFromFile ;
     Settings.DisplayDuration := edTDisplay.Value ;
     end;


procedure TViewSigFrm.bTDisplayHalfClick(Sender: TObject);
// -------------------------------
// Halve display window duration
// -------------------------------
begin
     edTDisplay.Value := edTDisplay.Value*0.5 ;
     DisplayFromFile ;
     Settings.DisplayDuration := edTDisplay.Value ;
     end;


procedure TViewSigFrm.rbTDisplayUnitsMSecsClick(Sender: TObject);
// --------------------------------------
// Set time readout units to milliseconds
// --------------------------------------
begin
    Settings.TUnits := 'ms' ;
    Settings.TSCale := 1000.0 ;
    edStartTime.Scale := Settings.TSCale ;
    edStartTime.Units := Settings.TUnits ;
    edTDisplay.Scale := Settings.TSCale ;
    edTDisplay.Units := Settings.TUnits ;
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
    NumBytesPerScan,NumScansInBuf,NumScansToDo,NumScansToRead : Integer ;
    FilePointer : Integer ;
    Buf : PSmallIntArrayDyn ;
begin

      // Initialise
      for ch := 0 to CDRFH.NumChannels-1 do Sum[ch] := 0.0 ;
      NumAvg := 0 ;
      NumBytesPerScan := CDRFH.NumChannels*2 ;
      GetMem( Buf, NumBytesPerScan*NumScansPerBuf ) ;

      // No of scans to average
      TScan := Round((scDisplay.XOffset + scDisplay.VerticalCursors[Cursors.C1])
                      *scDisplay.TScale/(CDRFH.dt*Settings.TScale)) ;
      NumScansToDo := Abs(TScan - TZeroScan) + 1 ;
      // Start at scan
      iScan := Min(TScan,TZeroScan) ;

      // Add scans to summation array

      while NumScansToDo > 0 do begin

          // Read data into buffer from file
          FilePointer := CDRFH.NumBytesInHeader + iScan*CDRFH.NumChannels*2 ;
          FileSeek( CDRFH.FileHandle, FilePointer, 0 ) ;
          NumScansToRead := Min(NumScansPerBuf,NumScansToDo) ;
          NumScansInBuf := FileRead(CDRFH.FileHandle,Buf^,NumBytesPerScan*NumScansToRead)
                           div NumBytesPerScan ;

          // Add buffer data to sum
          j := 0 ;
          for i := 0 to NumScansInBuf-1 do begin
              for ch := 0 to CDRFH.NumChannels-1 do begin
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
                          [TZeroScan*CDRFH.dt*Settings.TScale,
                           TScan*CDRFH.dt*Settings.TScale])) ;
      meCursor.Lines.Add(format( 'No. Samples=  %d ',[NumAvg])) ;
      for ch := 0 to CDRFH.NumChannels-1 do begin
          { Add to readout list }
          Avg := Channel[ch].ADCScale*
                 ( (Sum[ch]/Max(NumAvg,1)) - Channel[ch].ADCZero) ;
          meCursor.Lines.Add(format( '%s= %.4g %s',
                                     [Channel[ch].ADCName,
                                      Avg,
                                      Channel[ch].ADCUnits] ) );
          end ;

      FreeMem( Buf ) ;

     end;

procedure TViewSigFrm.ckFixedZeroLevelsClick(Sender: TObject);
// --------------------------------
// Enable/Disable fixed zero levels
// --------------------------------
begin
     Settings.FixedZeroLevels := ckFixedZeroLevels.Checked ;
     end;

end.
