unit PageView;
// --------------------------------------------------
// WinEDR - Multi-line page display module
// (c) John Dempster, University of Strathclyde, 2003
// --------------------------------------------------
// 12.12.02
// 26.02.03 Display can now compress data. Long duration lines now possible
// 12.06.03 Display now updated automatically when channel changed
// 24.6.03 .... No. horizontal/vertical grid lines changeable
// 03.03.04 ... Whole record printing now works correctly
// 10.07.08 ... Display point compression now works correctly
//              when ADCMaxValue for EDR file differs from selected interface.
//              Display Y range for all channels now updates correctly when one
//              channel changed
// 28.08.12 ... Zero Level popup and Fixed Zero level option added

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScopeDisplay, ValidatedEdit,
  global, fileio, RangeEdit, math ;

const
     MaxLines = 16 ;
     MaxDisplayPoints = 1000 ;

type
  TPageViewFrm = class(TForm)
    ControlsGrp: TGroupBox;
    GroupBox2: TGroupBox;
    cbChannel: TComboBox;
    Label1: TLabel;
    edStartTime: TValidatedEdit;
    Label2: TLabel;
    scDisplay: TScopeDisplay;
    sbDisplay: TScrollBar;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    edLinesPerPage: TValidatedEdit;
    lbDisplayPoints: TLabel;
    edLineDuration: TValidatedEdit;
    Label4: TLabel;
    edIdent: TEdit;
    ckShowLineTimes: TCheckBox;
    ckShowZeroLevels: TCheckBox;
    ckFixedZeroLevels: TCheckBox;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbDisplayChange(Sender: TObject);
    procedure edStartTimeKeyPress(Sender: TObject; var Key: Char);
    procedure edLinesPerPageKeyPress(Sender: TObject; var Key: Char);
    procedure edLineDurationKeyPress(Sender: TObject; var Key: Char);
    procedure scDisplayCursorChange(Sender: TObject);
    procedure edIdentChange(Sender: TObject);
    procedure ckShowZeroLevelsClick(Sender: TObject);
    procedure ckShowLineTimesClick(Sender: TObject);
    procedure cbChannelChange(Sender: TObject);
    procedure ckFixedZeroLevelsClick(Sender: TObject);
    procedure scDisplayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    ADC : ^TSmallIntArray ;
    DispBuf : ^TSmallIntArray ;
    BuffersAllocated : Boolean ;
    CompressionBlockSize : Integer ; // No. sample points in compression block
    NumPointsPerLine : Integer ;     // No. sample points per line
    procedure HeapBuffers( Operation : THeapBufferOp ) ;
    procedure DisplayPage ;
  public
    { Public declarations }
    procedure NewFile ;
    procedure PrintDisplay ;
    procedure CopyImageToClipboard ;
    procedure ChangeDisplayGrid ;
    procedure ZoomIn( ChanNum : Integer ) ;
    procedure ZoomOut( ChanNum : Integer ) ;

    end ;

var
  PageViewFrm: TPageViewFrm;

implementation

uses Mdiform, PrintPageView, Printers , Zero;

{$R *.dfm}

procedure TPageViewFrm.HeapBuffers( Operation : THeapBufferOp ) ;
{ -----------------------------------------------
  Allocate/deallocation dynamic buffers from heap
  -----------------------------------------------}
begin
     case Operation of
          Allocate : begin
             if not BuffersAllocated then begin
                New(ADC) ;
                New(DispBuf) ;
                BuffersAllocated := True ;
                end ;
             end ;
          Deallocate : begin
             if BuffersAllocated then begin
                Dispose(ADC) ;
                Dispose(DispBuf) ;
                BuffersAllocated := False ;
                end ;
             end ;
          end ;
     end ;


procedure TPageViewFrm.FormShow(Sender: TObject);
// --------------------------------------------
// Initialisations when form is first displayed
// --------------------------------------------
var
    ch : Integer ;
begin

     Top := 20 ;
     Left := 20 ;

     // Allocate buffers used within form
     HeapBuffers( Allocate ) ;

     // Fill channel selection list
     cbChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do
         cbChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
     cbChannel.ItemIndex := 0 ;

     // Get settings
     edLinesPerPage.Value := Settings.PageViewLinesPerPage ;
     edLineDuration.Value := Settings.PageViewLineDuration ;

     ckFixedZeroLevels.Checked := Settings.FixedZeroLevels ;

     // Set sizes of controls
     Resize ;

     // Set up display
     NewFile ;

     end;


procedure TPageViewFrm.NewFile ;
// --------------------------------------
// Update controls when data file changed
// --------------------------------------
var
     ch : Integer ;
     Keep : Integer ;
begin

     // Fill channel selection list
     Keep := cbChannel.ItemIndex ;
     cbChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do
         cbChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
     cbChannel.ItemIndex := Min(Keep,CdrFH.NumChannels-1) ;

     CompressionBlockSize := Max(Round(edLineDuration.Value/CDRFH.dt)
                                 div MaxDisplayPoints,1) ;
     NumPointsPerLine := (Round(edLineDuration.Value/CDRFH.dt) div CompressionBlockSize)
                         *CompressionBlockSize ;
     edLineDuration.Value := NumPointsPerLine*CDRFH.dt ;
     edLineDuration.LoLimit := 50.0*CDRFH.dt ;

     // Set upper limit of page start
     edStartTime.HiLimit := CdrFH.RecordDuration ;
     sbDisplay.Max := Round( CdrFH.RecordDuration/CdrFH.dt ) ;
     sbDisplay.LargeChange := NumPointsPerLine ;
     scDisplay.TScale := CdrFH.dt*Settings.TScale*CompressionBlockSize /
                         Min(CompressionBlockSize,2);

     // Update global settings
     Settings.PageViewLinesPerPage := Round(edLinesPerPage.Value) ;
     Settings.PageViewLineDuration := Round(edLineDuration.Value) ;

     // Set up signal display area }
     scDisplay.MaxADCValue := Channel[cbChannel.ItemIndex].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[cbChannel.ItemIndex].ADCMaxValue - 1 ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;
     scDisplay.DisableChannelVisibilityButton := True ;

     // Set no. of display points to twice number of points per line
     // if compression is in use
     scDisplay.MaxPoints := Min(NumPointsPerLine*CompressionBlockSize,2 ) ;
     scDisplay.NumPoints := scDisplay.MaxPoints ;
     scDisplay.NumChannels := Round(edLinesPerPage.Value) ;
     scDisplay.xMin := 0 ;
     scDisplay.xMax := scDisplay.NumPoints - 1  ;
     scDisplay.SetDataBuf( DispBuf ) ;

     //  Set display scaling information
     // (Each line encoded as a channel within the display)

     for ch := 0 to scDisplay.NumChannels-1 do begin
         scDisplay.ChanUnits[ch] := Channel[cbChannel.ItemIndex].ADCUnits ;
         scDisplay.ChanName[ch] := '' ;
         scDisplay.yMin[ch] := Channel[cbChannel.ItemIndex].yMin ;
         scDisplay.yMax[ch] := Channel[cbChannel.ItemIndex].yMax ;
         scDisplay.ChanScale[ch] := Channel[cbChannel.ItemIndex].ADCScale ;
         scDisplay.ChanUnits[ch] := Channel[cbChannel.ItemIndex].ADCUnits ;
         scDisplay.ChanZero[ch] := Channel[cbChannel.ItemIndex].ADCZero ;
         scDisplay.ChanOffsets[ch] := ch ;
         scDisplay.ChanColor[ch] := clBlue ;
         scDisplay.ChanVisible[ch] := True ;
         end ;

     scDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDisplay.TUnits := Settings.TUnits ;

     // Enable/disable zero level cursors
     scDisplay.ClearHorizontalCursors ;
     if ckShowZeroLevels.Checked then begin
        for ch := 0 to scDisplay.NumChannels-1 do begin
            scDisplay.AddHorizontalCursor(ch,clGray,True, 'z') ;
            end ;
        end ;

     // Update file identification information line
     edIdent.Text := CdrFH.IdentLine ;

     DisplayPage ;

     end ;


procedure TPageViewFrm.FormResize(Sender: TObject);
// -------------------------------------------------------
// Update control positions/sizes when window size changed
// -------------------------------------------------------
begin

     // Set vertical positions/heights
     ControlsGrp.Height := ClientHeight - ControlsGrp.Top - 5 ;
     sbDisplay.Top := ClientHeight - sbDisplay.Height - 20 ;
     scDisplay.Height := sbDisplay.Top - scDisplay.Top ;

     // Set horizontal positions/widths
     scDisplay.Width := ClientWidth - scDisplay.Left - 5 ;
     sbDisplay.Left := scDisplay.Left ;
     sbDisplay.Width := scDisplay.Width ;

     edIdent.Width := ClientWidth - edIdent.Left - 5 ;

     end;


procedure TPageViewFrm.DisplayPage ;
// -----------------------------------------------
// Display selected digitised signal block on page
// -----------------------------------------------
const
    NumScansPerBuffer = 256 ;
var
    i,j : Integer ;
    iSrc : Integer ;              // Source index
    iDest : Integer ;             // Destination index
    Line : Integer ;              // Line counter
    StartAtScan : Integer ;       // Starting scan for line
    StartReadAtScan : Integer ;   // Current starting scan of data in buffer
    NumSamplesPerBuffer : Integer ; // No. A/D samples in A/D buffer
    NumPointsPerLine : Integer ;  // No. of A/D samples per line
    NumLines : Integer ;          // No. of lines per page
    t : single ;                  // Time
    Done : Boolean ;              // Terminate loop flag

    CompressionCount : Integer ;  // Compression buffer position counter
    MinY : Integer ;              // Minimum A/D sample value in compression block
    MinYAt : Integer ;            // Index of minimum A/D sample value
    MaxY : Integer ;              // Maximum A/D sample value in compression block
    MaxYAt : Integer ;            // Index of maximum A/D sample value
begin


    NumLines := Round(edLinesPerPage.Value) ;

    CompressionBlockSize := Max( Round(edLineDuration.Value/CDRFH.dt)
                                 div MaxDisplayPoints,1) ;
    NumPointsPerLine := (Round(edLineDuration.Value/CDRFH.dt) div CompressionBlockSize)
                         *CompressionBlockSize ;
    edLineDuration.Value := NumPointsPerLine*CDRFH.dt ;


    // Clear page display buffer
    for iDest := 0 to High(DispBuf^) do DispBuf^[iDest] := 0 ;

    // Add each line to be displayed into a scDisplay channel
    // ------------------------------------------------------

    StartAtScan := Round( edStartTime.Value/CDRFH.dt ) ;
    for Line := 0 to NumLines-1 do begin

        // Offset within display buffer for this line
        iDest := Line ;

        // Initialise file buffer read
        StartReadAtScan := StartAtScan ;
        NumSamplesPerBuffer := NumScansPerBuffer*CDRFH.NumChannels ;
        iSrc :=  NumSamplesPerBuffer ; // (Forces buffer read)

        // Initialise compression block
        CompressionCount := 0 ;
        MinY := Channel[0].ADCMaxValue ;
        MaxY := -Channel[0].ADCMaxValue-1 ;

        // Copy this to display buffer
        // ---------------------------

        i := 0 ;
        Done := False ;
        While not Done do begin

            // Read signal from data file into buffer
            if iSrc >= NumSamplesPerBuffer then begin
               if ReadCDRBuffer(CdrFH,StartReadAtScan,ADC^,NumScansPerBuffer)
                  <> NumScansPerBuffer then Done := True ;
               iSrc := Channel[cbChannel.ItemIndex].ChannelOffset ;
               StartReadAtScan := StartReadAtScan + NumScansPerBuffer ;
               end ;

            if CompressionBlockSize = 1 then begin
               // If no compression ...
               // Just copy A/D sample into display buffer
               DispBuf^[iDest] := ADC^[iSrc] ;
               iSrc := iSrc + CDRFH.NumChannels ;
               iDest := iDest + NumLines ;
               end
            else begin
               // If compression required ...
               // Extract and plot min./max. points from compression block
               if ADC^[iSrc] < MinY then begin
                  MinY := ADC^[iSrc] ;
                  MinYAt := iSrc ;
                  end ;
               if ADC^[iSrc] > MaxY then begin
                  MaxY := ADC^[iSrc] ;
                  MaxYAt := iSrc ;
                  end ;
               iSrc := iSrc + CDRFH.NumChannels ;
               Inc(CompressionCount) ;

               // On completion of a compression block
               // copy min./max. values to display buffer
               if CompressionCount >= CompressionBlockSize then begin
                  if MinYAt < MaxYAt then begin
                     DispBuf^[iDest] := MinY ;
                     iDest := iDest + NumLines ;
                     DispBuf^[iDest] := MaxY ;
                     iDest := iDest + NumLines ;
                     end
                  else begin
                     DispBuf^[iDest] := MaxY ;
                     iDest := iDest + NumLines ;
                     DispBuf^[iDest] := MinY ;
                     iDest := iDest + NumLines ;
                     end ;
                  CompressionCount := 0 ;
                  MinY := Channel[0].ADCMaxValue ;
                  MaxY := -Channel[0].ADCMaxValue-1 ;
                  end ;
               end ;

            Inc(i) ;
            if i >= NumPointsPerLine then Done := True ;

            end ;

        // Increment to next line
        StartAtScan := StartAtScan + NumPointsPerLine ;

        end ;

    // Update horizontal cursors
    if ckShowZeroLevels.Checked then begin
       for Line := 0 to scDisplay.NumChannels-1 do begin
           scDisplay.HorizontalCursors[Line] := Channel[cbChannel.ItemIndex].ADCZero ;
           end ;
       end ;

    // Add times to beginning of each

    t := edStartTime.Value ;
    for Line := 0 to scDisplay.NumChannels-1 do begin
        if ckShowLineTimes.Checked then begin
           scDisplay.ChanName[Line] := format('t=%.3gs',[t]) ;
           t := t + NumPointsPerLine*CDRfh.dt ;
           end
        else scDisplay.ChanName[Line] := '' ;
       end ;

    scDisplay.MaxPoints := (NumPointsPerLine div CompressionBlockSize)
                           *Min(CompressionBlockSize,2);
    scDisplay.NumPoints := scDisplay.MaxPoints ;
    scDisplay.xMin := 0 ;
    scDisplay.xMax := scDisplay.NumPoints - 1  ;
    scDisplay.TScale := CdrFH.dt*Settings.TScale*CompressionBlockSize /
                         Min(CompressionBlockSize,2);

    scDisplay.Invalidate ;

    end ;




procedure TPageViewFrm.sbDisplayChange(Sender: TObject);
// --------------------------------------
// Update display when slider bar changed
// --------------------------------------
begin
     edStartTime.Value := sbDisplay.Position*CDRfh.dt ;
     DisplayPage ;
     end;


procedure TPageViewFrm.edStartTimeKeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then DisplayPage ;
     end;


procedure TPageViewFrm.edLinesPerPageKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then NewFile ;
     end;

procedure TPageViewFrm.edLineDurationKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then NewFile ;
     end;


procedure TPageViewFrm.scDisplayCursorChange(Sender: TObject);
// ----------------------------------------
// Update display when zoom/cursors changed
// ----------------------------------------

var
     i,ch : Integer ;
     YChanged : Boolean ;
begin

     // Detect any changes in limits of vertical display region
     YChanged := False ;
     for ch := 0 to scDisplay.NumChannels-1 do begin

          { Get signal baseline cursor }
          if Settings.FixedZeroLevels then begin
             if scDisplay.HorizontalCursors[ch] <> Channel[cbChannel.ItemIndex].ADCZero then
                scDisplay.HorizontalCursors[ch] := Channel[cbChannel.ItemIndex].ADCZero ;
             end
          else begin
             Channel[cbChannel.ItemIndex].ADCZero := scDisplay.HorizontalCursors[ch] ;
             end ;

         if scDisplay.yMin[ch] <> Channel[cbChannel.ItemIndex].yMin then begin
            Channel[cbChannel.ItemIndex].yMin := scDisplay.yMin[ch] ;
            YChanged := True ;
            end ;
         if scDisplay.yMax[ch] <> Channel[cbChannel.ItemIndex].yMax then begin
            Channel[cbChannel.ItemIndex].yMax := scDisplay.yMax[ch] ;
            YChanged := True ;
            end ;
         if YChanged then Break ;
         end ;

     // Update all lines with same display region
     if YChanged then begin
        for ch := 0 to scDisplay.NumChannels-1 do begin
            scDisplay.yMin[ch] := Channel[cbChannel.ItemIndex].yMin ;
            scDisplay.yMax[ch] := Channel[cbChannel.ItemIndex].yMax ;
            end ;
        scDisplay.Invalidate ;
        end ;

     // Zero levels
     if ckShowZeroLevels.Checked then begin
        // Detect any changes in horizontal zero level cursor
        YChanged := False ;
        for ch := 0 to scDisplay.NumChannels-1 do
            if scDisplay.HorizontalCursors[ch]
               <> Channel[cbChannel.ItemIndex].ADCZero then begin
               Channel[cbChannel.ItemIndex].ADCZero := scDisplay.HorizontalCursors[ch] ;
               YChanged := True ;
               Break ;
               end ;
        if YChanged then begin
           for i := 0 to scDisplay.NumChannels-1 do begin
               scDisplay.HorizontalCursors[i] := Channel[cbChannel.ItemIndex].ADCZero ;
               scDisplay.ChanZero[i] := Channel[cbChannel.ItemIndex].ADCZero ;
               end ;
           scDisplay.Invalidate ;      
           end ;
        end ;

     end;


procedure TPageViewFrm.edIdentChange(Sender: TObject);
{ -------------------------------------------
  Update identification string in file header
  -------------------------------------------}
begin
     { Update ident line if it is changed }
     //CdrFH.IdentLine := edIdent.text ;
     //SaveCDRHeader(CdrFH) ;
     end;


procedure TPageViewFrm.ckShowZeroLevelsClick(Sender: TObject);
// ------------------------------
// Show zero level cursor changed
// ------------------------------
begin
     // Request new file to enable/disable cursors
     NewFile ;
     end;


procedure TPageViewFrm.ckShowLineTimesClick(Sender: TObject);
// ---------------------------------
// Show line times check box changed
// ---------------------------------
begin
     DisplayPage ;
     end;

procedure TPageViewFrm.PrintDisplay ;
{ ------------------------
  Print displayed page(s)
  ------------------------ }
var
   iPage,NumPages : Integer ;
   PageDuration : Single ;
   PageStartTime : Single ;
begin

     // Duration of each displayed line
     PageDuration := edLinesPerPage.Value*edLineDuration.Value ;

     { Print transition }
     PrintPageViewFrm.Destination := dePrinter ;
     PrintPageViewFrm.Display := scDisplay ;

     // Set default page display range (current page)
     PrintPageViewFrm.StartTime := edStartTime.Value ;
     PrintPageViewFrm.EndTime := edStartTime.Value + PageDuration - CDRFH.dt ;

     // Display printing settings dialog
     PrintPageViewFrm.ShowModal ;

     if PrintPageViewFrm.ModalResult = mrOK then begin

        Printer.Title := 'WinEDR' ;

        NumPages := 1 + Trunc((PrintPageViewFrm.EndTime - PrintPageViewFrm.StartTime)/PageDuration );
        iPage := 1 ;
        PageStartTime := PrintPageViewFrm.StartTime ;
        while PageStartTime <= PrintPageViewFrm.EndTime do begin

             // Load and display signals on page
             edStartTime.Value := PageStartTime ;
             DisplayPage ;

             // Report printing status
             Main.StatusBar.SimpleText := format(
             'Page View : Printing page %d/%d',
             [iPage,NumPages] ) ;

             // Print page
             PrintPageViewFrm.Display.ClearPrinterTitle ;
             PrintPageViewFrm.Display.AddPrinterTitleLine(
             format('File : %s (Page %d/%d, T=%.4g s)',
             [cdrFH.FileName,iPage,NumPages,edStartTime.Value] )) ;
             PrintPageViewFrm.Display.AddPrinterTitleLine( CdrFH.IdentLine ) ;
             PrintPageViewFrm.Display.Print ;

             Inc(iPage) ;
             PageStartTime := PageStartTime + PageDuration ;

             end ;
        end ;

     // Report printing status
     Main.StatusBar.SimpleText := format(
     'Page View : Printing complete, %d pages sent to printer.',[iPage-1] ) ;


    end ;

procedure TPageViewFrm.CopyImageToClipboard ;
{ ----------------------------------------
  Copy displayed page to Windows clipboard
  ---------------------------------------- }
var
   PageDuration : Single ;
begin

     // Duration of each displayed line
     PageDuration := edLinesPerPage.Value*edLineDuration.Value ;

     { Print transition }
     PrintPageViewFrm.Destination := deClipboard ;
     PrintPageViewFrm.Display := scDisplay ;

     // Set default page display range (current page)
     PrintPageViewFrm.StartTime := edStartTime.Value ;
     PrintPageViewFrm.EndTime := edStartTime.Value + PageDuration - CDRFH.dt ;

     PrintPageViewFrm.ShowModal ;

     if PrintPageViewFrm.ModalResult = mrOK then begin
        PrintPageViewFrm.Display.ClearPrinterTitle ;
        PrintPageViewFrm.Display.CopyImageToClipboard ;
        end ;

    end ;


procedure TPageViewFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin
     scDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDisplay.Invalidate ;
     end ;


procedure TPageViewFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin
     // Free buffers used within form
     HeapBuffers( Deallocate ) ;

     Action := caFree ;
     end;


procedure TPageViewFrm.cbChannelChange(Sender: TObject);
// ------------------------------------
// Channel selected for display changed
// ------------------------------------
begin
     DisplayPage ;
     end;

procedure TPageViewFrm.ZoomOut(
          ChanNum : Integer ) ;
// ------------------------------------
// Magnify selected A/D channel display
// ------------------------------------
var
    i : Integer ;
begin

     if ChanNum <> cbChannel.ItemIndex then Exit ;

     for i := 0 to scDisplay.NumChannels-1 do begin
         scDisplay.YZoom( i, 50.0 ) ;
         end ;

     end ;


procedure TPageViewFrm.ZoomIn( ChanNum : Integer ) ;
// ------------------------------------
// Reduce selected A/D channel display
// ------------------------------------
var
    i : Integer ;
begin

     if ChanNum <> cbChannel.ItemIndex then Exit ;

     for i := 0 to scDisplay.NumChannels-1 do begin
         scDisplay.YZoom( i, -50.0 ) ;
         end ;

     end ;


procedure TPageViewFrm.ckFixedZeroLevelsClick(Sender: TObject);
// --------------------------------
// Enable/Disable fixed zero levels
// --------------------------------
begin
     Settings.FixedZeroLevels := ckFixedZeroLevels.Checked ;
     end;

procedure TPageViewFrm.scDisplayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ----------------------
// Set channel zero level
// ----------------------
var
    ch : Integer ;
begin
     if (Button = mbRight) and (scDisplay.ActiveHorizontalCursor >=0) then begin
        // If right-mouse button down, display zero baseline level selection dialog box
        ZeroFrm.ChSel := cbChannel.ItemIndex ;
        ZeroFrm.ZeroLevel := Channel[ZeroFrm.ChSel].ADCZero ;
        ZeroFrm.ChanName := Channel[ZeroFrm.ChSel].ADCName ;
        ZeroFrm.NewZeroAt := Round(scDisplay.ScreenCoordToX( ZeroFrm.ChSel, X )) ;
        ZeroFrm.Left := PageViewFrm.Left + Main.Left + 10 + scDisplay.Left + X;
        ZeroFrm.Top := PageViewFrm.Top + Main.Top + 10 + scDisplay.Top + Y ;
        ZeroFrm.ShowModal ;
        Channel[ZeroFrm.ChSel].ADCZero := ZeroFrm.ZeroLevel ;
        Channel[ZeroFrm.ChSel].ADCZero := Max(-Channel[ZeroFrm.ChSel].ADCMaxValue-1,ZeroFrm.ZeroLevel) ;
        Channel[ZeroFrm.ChSel].ADCZero := Min(Channel[ZeroFrm.ChSel].ADCMaxValue,ZeroFrm.ZeroLevel) ;
        Channel[ZeroFrm.ChSel].ADCZeroAt := -1 ;
        SaveCDRHeader( CDRfH ) ;
        for ch := 0 to scDisplay.NumChannels-1 do
            scDisplay.HorizontalCursors[ch] := Channel[ZeroFrm.ChSel].ADCZero ;
        end
     end;


procedure TPageViewFrm.FormActivate(Sender: TObject);
begin
     ckFixedZeroLevels.Checked := Settings.FixedZeroLevels ;
     end;

end.
