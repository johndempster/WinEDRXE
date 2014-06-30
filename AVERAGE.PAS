unit Average;
{ ==========================================================================
  WinWCP - Signal averaging module (c) J Dempster, 1996, All Rights Reserved
  ========================================================================== }
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Global, Shared, FileIo;

type
  TAvgFrm = class(TForm)
    RecordGrp: TGroupBox;
    Label2: TLabel;
    edRecordNum: TEdit;
    cbRecordType: TComboBox;
    ckBadRecord: TCheckBox;
    sbRecordNum: TScrollBar;
    pbDisplay: TPaintBox;
    AnalysisGrp: TGroupBox;
    Label5: TLabel;
    Label7: TLabel;
    edRange: TEdit;
    bDoAverages: TButton;
    cbRecTypeToBeAveraged: TComboBox;
    bCancel: TButton;
    EdBlockSize: TEdit;
    Label6: TLabel;
    lbTMax: TLabel;
    lbTMin: TLabel;
    Timer: TTimer;
    bAbort: TButton;
    cbAlignMode: TComboBox;
    Label8: TLabel;
    lbCursor0: TLabel;
    lbCursor1: TLabel;
    edProgress: TEdit;
    procedure TimerTimer(Sender: TObject);
    procedure sbRecordNumChange(Sender: TObject);
    procedure pbDisplayMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbDisplayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bCancelClick(Sender: TObject);
    procedure bDoAveragesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure pbDisplayPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ckBadRecordClick(Sender: TObject);
    procedure cbRecordTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure DisplayRecord ;
    procedure CreateAverages ;
    procedure HeapBuffers( Operation : THeapBufferOp ) ;
  public
    { Public declarations }
  end;


var
  AvgFrm: TAvgFrm;

implementation

uses MDIform ;

{$R *.DFM}
const
     NoAlignment = 0 ;
     OnPositiveRise = 1 ;
     OnNegativeRise = 2 ;
type
    TCursorState = ( Cursor0, Cursor1, NoCursor ) ;
    TAlignMode = ( PositivePeaks,NegativePeaks,AbsPeaks ) ;
    TState = ( DoRecord, DoAverages, EndOfAveraging, Idle) ;
    TAverageJob = record
                Running : Boolean ;
                StartAt : LongInt ;
                EndAt : LongInt ;
                BlockSize : LongInt ;
                EndOfBlock : LongInt ;
                NumAveraged : LongInt ;
                RecordNum : LongInt ;
                end ;

var
   State : TState ;
   CursorState : TCursorState ;
   CurCh : LongInt ;
   MoveCursor : Boolean ;
   AverageJob : TAverageJob ;
   CursorChannel : TChannel ;
   Sum : ^TSingleArray ;
   ADC : ^TIntArray ;
   RecHeader : ^TRecHeader ;
   LastUsedRH : ^TRecHeader ;
   AlignAt : array[0..ChannelLimit] of LongInt ;
   BuffersAllocated : boolean ;{ Indicates if memory buffers have been allocated }


procedure TAvgFrm.HeapBuffers( Operation : THeapBufferOp ) ;
{ -----------------------------------------------
  Allocate/deallocation dynamic buffers from heap
  -----------------------------------------------}
begin
     case Operation of
          Allocate : begin
             if not BuffersAllocated then begin
                New(Sum) ;
                New(ADC) ;
                New(RecHeader) ;
                New(LastUsedRH) ;
                BuffersAllocated := True ;
                end ;
             end ;
          Deallocate : begin
             if BuffersAllocated then begin
                Dispose(LastUsedRH) ;
                Dispose(RecHeader) ;
                Dispose(ADC) ;
                Dispose(Sum) ;
                BuffersAllocated := False ;
                end ;
             end ;
          end ;
     end ;


procedure TAvgFrm.FormCreate(Sender: TObject);
begin
     { Disable "Average Signals" item in "Analysis" menu }
     Main.SignalAverager.enabled := false ;
     BuffersAllocated := False ;
     end;


procedure TAvgFrm.TimerTimer(Sender: TObject);
begin
     { Execute any requested operations }
     case State of
          DoRecord : begin
                pbDisplay.canvas.FillRect( pbDisplay.canvas.ClipRect ) ;
                DisplayRecord ;
                State := Idle ;
                end ;
          DoAverages : begin
                Screen.Cursor := crHourGlass ;
                CreateAverages ;
                end ;
          EndOfAveraging : begin
                SaveHeader( AvgfH ) ;
                State := Idle ;
                Screen.Cursor := crDefault ;
                bAbort.enabled := False ;
                bDoAverages.enabled := True ;
                bCancel.enabled := True ;
                AvgFrm.Close ;
                end ;
          Idle : begin
                if Screen.Cursor <> crDefault then Screen.Cursor := crDefault ;
                end ;
          end ;

     end;


procedure TAvgFrm.sbRecordNumChange(Sender: TObject);
{ ----------------------------------------------------------
  Request a new record to be displayed when slider bar moved
  ----------------------------------------------------------}
begin
     State := DoRecord ;
     end;


procedure TAvgFrm.pbDisplayMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Const
     Margin = 4 ;
var
   OldIndex : Integer  ;
begin
     { Make sure the the record number box has the focus
       to avoid unintended effect if arrow keys are used to move the cursors }
     edRecordNum.Setfocus ;

     if not MoveCursor then begin

          { If not in the cursor move mode, check to see if the mouse
            is over any of the cursors and change its icon accordingly }

          CursorState := NoCursor ;
          pbDisplay.Cursor := crDefault ;

          { Is mouse over cursor 0 ? }
          if OverVerticalCursor(X,CursorChannel.Cursor0,CursorChannel ) then begin
             CursorState := Cursor0  ;
             pbDisplay.Cursor := crSizeWE ;
             end ;

          { Is mouse over cursor 1 ? }
          if OverVerticalCursor(X,CursorChannel.Cursor1,CursorChannel ) then begin
             CursorState := Cursor1  ;
             pbDisplay.Cursor := crSizeWE ;
             end ;

          end
      else begin

          { If in Move Cursor mode, move selected cursor to new position }

          case CursorState of
            { Move cursor 0 }
            Cursor0 : begin
                VerticalCursorScale( X,CursorChannel.Cursor0,OldIndex,CursorChannel);
                MoveVerticalCursor(pbDisplay,CursorChannel.Cursor0,OldIndex,
                                   CursorChannel,lbCursor0) ;
                end ;
            { Move cursor 1 }
            Cursor1 : begin
                VerticalCursorScale( X,CursorChannel.Cursor1,OldIndex,CursorChannel);
                MoveVerticalCursor(pbDisplay,CursorChannel.Cursor1,OldIndex,
                                   CursorChannel,lbCursor1) ;
                end ;
            end ;
          end ;
      end ;


procedure TAvgFrm.pbDisplayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     MoveCursor := False ;
     end;


procedure TAvgFrm.pbDisplayMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     MoveCursor := True ;
     end;


procedure TAvgFrm.DisplayRecord ;
{ ===============================
  Display digitised signal record
  ===============================}
var
   i,j,MaxCh,ch,ChOffset,Rec : LongInt ;
   x,y,dx : single ;
   xPix,yPix,nOffScreen,Temp0,Temp1 : Integer ;
   row,col : LongInt ;
begin
     if  RawfH.NumRecords > 0 then begin
          sbRecordNum.Max := RawfH.NumRecords ;
          sbRecordNum.Min := 1 ;
          sbRecordNum.Enabled := True ;
          RawfH.RecordNum := SbRecordNum.position ;

          { Read record data from file }
          GetRecord( RawfH, RecHeader^, RawfH.RecordNum, ADC^ ) ;

          InitializeDisplay( Channel, RawfH.NumChannels, RecHeader^,
                             lbTMin,lbTMax, pbDisplay) ;

          Temp0 := CursorChannel.Cursor0 ;
          Temp1 := CursorChannel.Cursor1 ;
          CursorChannel := Channel[0] ;
          CursorChannel.Cursor0 := Temp0 ;
          CursorChannel.Cursor1 := Temp1 ;

          CursorChannel.Top := pbDisplay.top ;
          CursorChannel.Bottom := pbDisplay.Top + pbDisplay.Height ;
          CursorChannel.color := clRed ;

          { Erase display }
          EraseDisplay(pbDisplay) ;

          for ch := 0 to RawfH.NumChannels-1 do begin
              if Channel[ch].InUse then begin
                 ChOffset := Channel[ch].ChannelOffset ;
                 dx := Channel[ch].xScale ;
                 x := -Channel[ch].xScale*Channel[ch].xMin + Channel[ch].Left ;
                 nOffScreen := 0 ;
                 for i := 0 to RawfH.NumSamples-1 do begin

                     y := ADC^[(i*RawfH.NumChannels) + ChOffset ] ;
                     xPix := Trunc(x) ;
                     yPix := Trunc(Channel[ch].Bottom
                             - Channel[ch].yScale*(y - Channel[ch].yMin));

                     if (xPix < Channel[ch].Left)
                        or (Channel[ch].Right < xPix )
                        or (yPix < Channel[ch].Top)
                        or (Channel[ch].Bottom < yPix ) then begin
                           xPix := MinInt( [ MaxInt( [xPix,Channel[ch].Left]),
                                       Channel[ch].Right] ) ;
                           yPix := MinInt( [ MaxInt( [yPix,Channel[ch].Top]),
                                       Channel[ch].Bottom] ) ;
                           nOffScreen := nOffScreen + 1 ;
                        end
                     else nOffScreen := 0 ;

                     if (nOffScreen > 1) or (i=0) then
                         pbDisplay.canvas.moveto(xPix,yPix)
                     else pbDisplay.canvas.lineto(xPix,yPix);

                     x := x + dx ;
                     end ;

                 { Display Channel Name }
                 pbDisplay.Canvas.TextOut( Channel[ch].Left,
                      (Channel[ch].Top + Channel[ch].Bottom) div 2,
                       ' ' + Channel[ch].ADCName ) ;

                 { Draw cursors }
                 CursorChannel.color := clRed ;
                 DrawCursor( pbDisplay,CursorChannel.Cursor0,CursorChannel,lbCursor0 ) ;
                 DrawCursor(pbDisplay,CursorChannel.Cursor1,CursorChannel,lbCursor1 ) ;
                 end ;
            end ;

          edRecordNum.text := format('Rec. %d/%d', [sbRecordNum.position,
                                                    RawfH.NumRecords]);

          { Show whether record has been rejected by operator }
          if RecHeader^.Status = 'ACCEPTED' then ckBadRecord.checked := False
                                              else ckBadRecord.checked := True ;
          { Show type of record }
          if cbRecordType.items.indexOf(RecHeader^.RecType) >= 0 then
             cbRecordType.ItemIndex :=
                      cbRecordType.items.indexOf(RecHeader^.RecType);
          ch := 0 ;

          end ;
     end ;


procedure TAvgFrm.bCancelClick(Sender: TObject);
{ --------------
  Cancel Button
  -------------}
begin
     AvgFrm.Close ;
     end;


procedure TAvgFrm.bDoAveragesClick(Sender: TObject);
{ ------------------------------
  Request averages to be created
  ------------------------------}
var
   iExt,OldHandle,Temp : LongInt ;
begin
     bDoAverages.Enabled := False ;
     bCancel.Enabled := False ;
     bAbort.Enabled := True ;

     GetIntRangeFromEditBox(edRange,AverageJob.StartAt,AverageJob.EndAt
                            ,1, RawfH.NumRecords) ;
     AverageJob.BlockSize := Trunc( GetFromEditBox(edBlockSize,1,1,
                                    RawfH.NumRecords,'%.0f','') ) ;
     AverageJob.NumAveraged := 0 ;
     AverageJob.RecordNum := AverageJob.StartAt ;
     AverageJob.EndOfBlock := AverageJob.StartAt + AverageJob.BlockSize ;

     { Create averages file }

     { Copy details from original file header (except file handle!!!!) }
     OldHandle := AvgFH.FileHandle ;
     AvgfH := RawfH ;
     AvgFH.FileHandle := OldHandle ;

     { The averages data file name has the same name as the original file
       but with the extension .avg }
     AvgfH.FileName := ReplaceFileEnding( RawfH.FileName, '.avg' ) ;
     { Create file to hold averages }
     if AvgFH.FileHandle >= 0 then FileClose(  AvgFH.FileHandle ) ;
     AvgfH.FileHandle := FileCreate( AvgfH.FileName ) ;
     { Save header block and request}
     if AvgfH.FileHandle >= 0 then begin
           AvgfH.NumRecords := 0 ;
           SaveHeader( AvgfH ) ;
           State := DoAverages ;
           end
     else MessageDlg( 'FileCreate Error ='+ IntToStr(AvgfH.FileHandle),
             mtWarning, [mbOK], 0 ) ;

     end;


procedure TAvgFrm.CreateAverages ;
{ ===================================================
  Create a file containing averaged signal records
  ===================================================}
var
   i,j,jFrom,jTo,iFrom,MaxCh,ch : LongInt ;
   Peak,PeakAt,HalfPeak,HalfPeakAt,Shift : LongInt ;

   PeakPositive,PeakPositiveAt : LongInt ;
   PeakNegative,PeakNegativeAt : LongInt ;
   iStart,iEnd : LongInt ;
   iY,i0,i1 : LongInt ;
   ChOffset : Integer ;

begin

     { If no records in buffer yet ... Initialise averaging array }
     if AverageJob.NumAveraged = 0 then begin
        for i := 0 to (RawFH.NumChannels*RawFH.NumSamples)-1 do Sum^[i] := 0. ;
        end ;
     { Read record data from file }
     GetRecord( RawfH, RecHeader^, AverageJob.RecordNum, ADC^ ) ;

     { If record is of the right type and is ACCEPTED for use
       ... add it to average }
     if (RecHeader^.Status = 'ACCEPTED') and
        ( (RecHeader^.RecType = cbRecTypeToBeAveraged.text ) or
          ( cbRecTypeToBeAveraged.text = 'ALL') ) then begin

         { Keep the record header to use for averaged record }
         LastUsedRH^ := RecHeader^ ;

         { Add each channel to record average }
         for ch := 0 to RawFH.NumChannels-1 do begin

            ChOffset := Channel[ch].ChannelOffset ;

            { Subtract zero level }
            for i := 0 to RawFH.NumSamples-1 do begin
                j := i*RawFH.NumChannels + ChOffset ;
                ADC^[j] := ADC^[j] - Channel[ch].ADCZero ;
                end ;

            { Get range of samples to be averaged from positions of cursors 0 and 1 }
            iStart := MinInt( [ CursorChannel.Cursor0,CursorChannel.Cursor1] ) ;
            iEnd :=   MaxInt( [ CursorChannel.Cursor0,CursorChannel.Cursor1] ) ;

            if cbAlignMode.ItemIndex = NoAlignment then begin
                { No re-alignment }
                Shift := 0 ;
                end
            else begin
                { If in one of the re-alignment mode ... determine how
                  much the signal should be shifted to align the mid-point
                  of its rising phase with that of the average }

                { Find peaks within cursor 0-1 region}
                PeakPositive := MinADCValue*2 ;
                PeakNegative := MaxADCValue*2 ;
                for i := iStart to iEnd do begin
                    j := i*RawFH.NumChannels + ChOffset ;
                    iY := ADC^[j] ;
                    if iY > PeakPositive then begin
                       PeakPositive := iY ;
                       PeakPositiveAt := i ;
                       end ;
                    if iY < PeakNegative then begin
                       PeakNegative := iY ;
                       PeakNegativeAt := i ;
                       end ;
                    end ;

                if cbAlignMode.ItemIndex = OnPositiveRise then begin
                   PeakAt := PeakPositiveAt ;
                   Peak := Abs(PeakPositive) ;
                   end
                else if cbAlignMode.ItemIndex = OnNegativeRise then begin
                   PeakAt := PeakNegativeAt ;
                   Peak := Abs(PeakNegative) ;
                   end ;

                HalfPeak := Peak div 2 ;
                iY := Peak ;
                HalfPeakAt := PeakAt ;
                while (iY >= HalfPeak) and (HalfPeakAt > iStart) do begin
                      j := HalfPeakAt*RawFH.NumChannels + ChOffset ;
                      iY := Abs(ADC^[j]) ;
                      HalfPeakAt := HalfPeakAt - 1;
                      end ;

                if AverageJob.NumAveraged = 0 then AlignAt[ch] := HalfPeakAt ;
                Shift := HalfPeakAt - AlignAt[ch] ;
                end ;

            { Add signal to average. (Note how signal is scaled by
              binary->real scaling factor. This allows averaging of
              records with different gains) }
            for i := 0 to RawFH.NumSamples-1 do begin
                iFrom := MaxInt( [MinInt( [i + Shift,RawFH.NumSamples-1] ),0 ]) ;
                jFrom := iFrom*RawFH.NumChannels + ChOffset ;
                jTo := i*RawFH.NumChannels + ChOffset ;
                Sum^[jTo] := Sum^[jTo] + ADC^[jFrom]*Channel[ch].ADCScale ;
                end ;
            end ;

         AverageJob.NumAveraged :=  AverageJob.NumAveraged + 1 ;
         end ;

     { If a block of averages has been acquired write it to average file }
     if (AverageJob.RecordNum >= AverageJob.EndOfBlock) or
        (AverageJob.RecordNum >= AverageJob.EndAt) then begin

        { Compute averaged record }
        if AverageJob.NumAveraged > 0 then begin
           for ch := 0 to RawFH.NumChannels-1 do begin
               ChOffset := Channel[ch].ChannelOffset ;
               for i := 0 to RawFH.NumSamples-1 do begin
                   j := i*RawFH.NumChannels + ChOffset ;
                   ADC^[j] := Trunc( Sum^[j] /
                                 (AverageJob.NumAveraged*Channel[ch].ADCScale) )
                                 + Channel[ch].ADCZero ;
                   end ;
               end ;
           end
        else begin
           for ch := 0 to RawFH.NumChannels-1 do begin
               ChOffset := Channel[ch].ChannelOffset ;
               for i := 0 to RawFH.NumSamples-1 do begin
                   j := i*RawFH.NumChannels + ChOffset ;
                   ADC^[j] := Channel[ch].ADCZero ;
                   end ;
               end ;
           end ;

        { Save record to averages file  }
        AvgfH.NumRecords := AvgfH.NumRecords + 1 ;
        LastUsedRH^.Number := AvgfH.NumRecords ;
        PutRecord( AvgfH, LastUsedRH^, AvgfH.NumRecords, ADC^ ) ;
        AverageJob.EndofBlock := AverageJob.EndOfBlock + AverageJob.BlockSize ;
        AverageJob.NumAveraged := 0 ;
        end ;

     edProgress.text := format( '%d/%d', [AverageJob.RecordNum,AverageJob.EndAt]);

     AverageJob.RecordNum := AverageJob.RecordNum + 1;
     { Terminate the job if that was the last record }

     if AverageJob.RecordNum > AverageJob.EndAt then begin
        AverageJob.RecordNum := AverageJob.EndAt ;
        { Request end-of-averaging processes }
        State := EndofAveraging ;
        end ;
     end ;


procedure TAvgFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    { Turn off event scheduling timer }
     Timer.enabled := false ;
     { Dispose of buffers }
     HeapBuffers( Deallocate ) ;

     if (AvgFH.NumRecords > 0) and (AvgFH.FileName <> '') then begin
        Main.ShowAveraged.visible := True ;
        Main.ShowAveraged.Enabled := True ;
        Main.ShowAveraged.Click ;
        end ;

     { Enable "Average Signals" item in "Analysis" menu }
     Main.SignalAverager.enabled := true ;

     Action :=caFree ;
     end;


procedure TAvgFrm.FormShow(Sender: TObject);
{ ---------------------------------------------------------
  Initialise controls and create buffers when form is shown
  ---------------------------------------------------------}
var
   ch : LongInt ;
begin
     { Create buffers }
     HeapBuffers( Allocate ) ;

     { Create record type selector box }
     cbRecordType.items := RecordTypes ;
     cbRecordType.items.delete(0) ; {Remove 'ALL' item}

     { Create time to be averaged selector box }
     cbRecTypeToBeAveraged.items := RecordTypes ;
     if cbRecTypeToBeAveraged.itemIndex < 0 then cbRecTypeToBeAveraged.itemIndex := 0 ;

     { Set up range of records to be displayed in averaging setup display }
     if RawfH.Numrecords > 0 then begin
        sbRecordNum.Max := RawfH.NumRecords ;
        sbRecordNum.Min := 1 ;
        sbRecordNum.Enabled := True ;
        sbRecordNum.Position := 1 ;
        edRange.text := format(' 1 - %d ', [RawFH.NumRecords]) ;
        edBlockSize.text := format(' %d ', [RawfH.NumRecords] ) ;
        end
     else begin
          edRange.text := 'None' ;
          edBlockSize.text := 'None' ;
        end ;

     { Initialise average alignment mode combo box }
     if cbAlignMode.ItemIndex < 0 then cbAlignMode.ItemIndex := 0 ;

     { Set initial position of alignment cursors }
     CursorChannel.Cursor0 := 1 ;
     CursorChannel.Cursor1 := RawFH.NumSamples - 2 ;

     { Set button states }
     bAbort.enabled := False ;
     bDoAverages.enabled := True ;
     bCancel.enabled := True ;
     Timer.Enabled := True ;
     end;


procedure TAvgFrm.pbDisplayPaint(Sender: TObject);
begin
     if State = Idle then State := DoRecord ;
     end;

procedure TAvgFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{ ------------------------
  Function key processing
  -----------------------}
type
    TAction = (MoveCursor,ChangeRecord,None) ;
var
   Action : TAction ;
   Step,OldPosition : Integer ;
   NewRecord : LongInt ;
begin

        case key of
          VK_LEFT : begin
             Action := MoveCursor ;
             Step := -1 ;
             end ;
          VK_RIGHT : begin
             Action := MoveCursor ;
             Step := 1 ;
             end ;
          VK_PRIOR : begin
             Action := ChangeRecord ;
             Step := -1 ;
             end ;
          VK_NEXT : begin
             Action := ChangeRecord ;
             Step := 1 ;
             end ;
          else Action := None ;
          end ;

        case Action of
             { Move vertical display cursor }
             MoveCursor : begin
                 Case CursorState of
                      Cursor0 : begin
                           OldPosition := CursorChannel.Cursor0 ;
                           CursorChannel.Cursor0 := OldPosition + Step ;
                           MoveVerticalCursor( pbDisplay,CursorChannel.Cursor0,
                                               OldPosition,CursorChannel,lbCursor0) ;
                           end ;
                      Cursor1 : begin
                           OldPosition := CursorChannel.Cursor1  ;
                           CursorChannel.Cursor1 := OldPosition + Step ;
                           MoveVerticalCursor( pbDisplay,CursorChannel.Cursor1,
                                               OldPosition,CursorChannel,lbCursor1 ) ;
                           end ;
                      end ;
                 end ;
             { Change record currently displayed }
             ChangeRecord : begin
                 NewRecord := MinInt([MaxInt([FH.RecordNum + Step,1]),FH.NumRecords]) ;
                 sbRecordNum.Position := NewRecord ;
                 State := DoRecord ;
                 end ;
             end ;
     end ;

procedure TAvgFrm.ckBadRecordClick(Sender: TObject);
{ ------------------------------------------------
  Save new record ACCEPTED/REJECTED status to file
  ------------------------------------------------}
begin
     if ckBadRecord.checked then RecHeader^.Status := 'REJECTED'
                            else RecHeader^.Status := 'ACCEPTED' ;
     PutRecordHeaderOnly( RawfH, RecHeader^, RawfH.RecordNum ) ;
     end;


procedure TAvgFrm.cbRecordTypeChange(Sender: TObject);
{ -----------------------------
  Save new record type to file
  ----------------------------}
begin
     RecHeader^.RecType := cbRecordType.text ;
     PutRecordHeaderOnly( RawfH, RecHeader^, RawfH.RecordNum ) ;
     end;



end.
