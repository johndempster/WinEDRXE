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
// 07.08.15 ... Min/Max compression of large array signal arrays now handled by ScopeDisplay.pas

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScopeDisplay, ValidatedEdit,
  global, fileio, RangeEdit, math, SESLabIO ;

const
     MaxLines = 16 ;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    DispBuf : PSmallIntArray ;
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

procedure TPageViewFrm.FormShow(Sender: TObject);
// --------------------------------------------
// Initialisations when form is first displayed
// --------------------------------------------
var
    ch : Integer ;
begin

     Top := 20 ;
     Left := 20 ;

     // Fill channel selection list
     cbChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do
         cbChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
     cbChannel.ItemIndex := 0 ;

     // Get settings
     edLinesPerPage.Value := Settings.PageViewLinesPerPage ;
     edLineDuration.Value := Settings.PageViewLineDuration ;

     ckFixedZeroLevels.Checked := Settings.FixedZeroLevels ;

     edLineDuration.Value := 1.0 ;

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

     edLineDuration.LoLimit := 50.0*CDRFH.dt ;
     edLineDuration.HiLimit := CdrFH.RecordDuration ;

     // Set upper limit of page start
     edStartTime.HiLimit := CdrFH.RecordDuration ;
     sbDisplay.Max := Round( CdrFH.RecordDuration/CdrFH.dt ) ;
     sbDisplay.LargeChange := Round(edLineDuration.Value/CDRFH.dt) ;
     scDisplay.TScale := CdrFH.dt ;

     // Update global settings
     Settings.PageViewLinesPerPage := Round(edLinesPerPage.Value) ;
     Settings.PageViewLineDuration := Round(edLineDuration.Value) ;

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
var
    i,j,k,ch : Integer ;
    Line : Integer ;              // Line counter
    StartScan : Integer ;       // Starting scan for line
    NumPointsPerLine : Integer ;  // No. of A/D samples per line
    NumLines : Integer ;          // No. of lines per page
    t : single ;                  // Time
    NumScans,MaxScans : Integer ;
    FilePointer : Int64 ;
    ADC : PSmallIntArray ;
begin

    NumLines := Round(edLinesPerPage.Value) ;

    // No. of multi-channel scans to be displayed
    MaxScans := CDRFH.NumSamplesInFile div CDRFH.NumChannels ;
    edLineDuration.Value := Min( edLineDuration.Value, Max(1.0,CDRFH.RecordDuration));
    scDisplay.MaxPoints := Round(edLineDuration.Value/CDRFH.dt) ;
    NumScans := Max( Round(edLineDuration.Value/CDRFH.dt),1 ) ;
    scDisplay.NumPoints := NumScans ;
    scDisplay.xMin := 0 ;
    scDisplay.xMax := scDisplay.NumPoints - 1  ;
    scDisplay.TScale := CdrFH.dt ;
    scDisplay.NumChannels := Round(edLinesPerPage.Value) ;

    // Allocate memory buffer
    if DispBuf <> Nil then FreeMem(DispBuf) ;
    DispBuf := GetMemory( scDisplay.MaxPoints*scDisplay.NumChannels*2) ;
    scDisplay.SetDataBuf( DispBuf ) ;

    ADC := GetMemory( NumScans*CDRFH.NumChannels*2 ) ;

     // Set up signal display area }
     scDisplay.MaxADCValue := Channel[cbChannel.ItemIndex].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[cbChannel.ItemIndex].ADCMaxValue - 1 ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;
     scDisplay.DisableChannelVisibilityButton := True ;

     scDisplay.xMin := 0 ;
     scDisplay.xMax := scDisplay.NumPoints - 1  ;

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

    // Add each line to be displayed into a scDisplay channel
    // ------------------------------------------------------

    StartScan := Round( edStartTime.Value/CDRFH.dt ) ;
    for Line := 0 to NumLines-1 do begin

        // Read data from file
        FilePointer := CDRFH.NumBytesInHeader + StartScan*CDRFH.NumChannels*2 ;
        FileSeek( CDRFH.FileHandle, FilePointer, 0 ) ;
        for i := 0 to (NumScans*CDRFH.NumChannels)-1 do ADC^[i] := 0 ;
        FileRead(CDRFH.FileHandle,ADC^,NumScans*CDRFH.NumChannels*2) ;

        j := Channel[cbChannel.ItemIndex].ChannelOffset ;
        k := Line ;
        for i := 0 to NumScans-1 do begin
            DispBuf[k] := ADC^[j] ;
            j := j + CDRFH.NumChannels ;
            k := k + NumLines ;
            end;

        // Increment to next line
        StartScan := StartScan + NumScans ;

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
           scDisplay.ChanName[Line] := format('t=%.4gs',[t]) ;
           t := t + scDisplay.MaxPoints*CDRfh.dt ;
           end
        else scDisplay.ChanName[Line] := '   ' ;
       end ;

    scDisplay.Invalidate ;
    FreeMem( ADC ) ;

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
     if Key = #13 then DisplayPage ;
     end;

procedure TPageViewFrm.edLineDurationKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then DisplayPage ;
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
             Channel[cbChannel.ItemIndex].ADCZero := Round(scDisplay.HorizontalCursors[ch]) ;
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
               Channel[cbChannel.ItemIndex].ADCZero := Round(scDisplay.HorizontalCursors[ch]) ;
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

        // Report printing status
        Main.StatusBar.SimpleText := format(
        'Page View : Printing complete, %d pages sent to printer.',[iPage-1] ) ;

        end ;

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

     Action := caFree ;
     end;


procedure TPageViewFrm.FormCreate(Sender: TObject);
begin
    DispBuf := Nil ;
    end;

procedure TPageViewFrm.FormDestroy(Sender: TObject);
begin
    if DispBuf <> Nil then FreeMem(DispBuf) ;
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
        ZeroFrm.ZeroLevel := Round(Channel[ZeroFrm.ChSel].ADCZero) ;
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
