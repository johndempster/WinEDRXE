unit exportUnit;
{ ================================================================
  WinWCP (c) J. Dempster, University of Strathclyde, 1998-99
  Data file export module
  ================================================================
  5/2/00
  28/2/00 ... Change File Name now works, channel captions updated
  9/2/02 ... V2.2.6 ASCII text export added
  26/2/02 ... V2.3.0 ASCII text export now works properly
              Progress reported to main status bar
  14/08/02 ... Bug which prevented export when output file did not exist fixed  (V2.3.4)
  1/12/03 .... Export now uses TADCDataFile component
  19/04/03 ... Modified for use by WinWCP
  22/04/04 ... IGOR export added
  19/10/06 ... Now exports average and leak-subtracted as well as raw records
  18/12/06 ... Export to IGOR Binary Wave format files now works
  01/02/06 ... Exports to EDR files now scaled correctly
               A/D Voltage range differences stored within WCP records
               stored in .ChannelGain
  05.09.07 ... Bugs in export file naming fixed.
               Files can now be exported to folders with '.' in name
               new function used CreateExportFileName
  27.01.08 ... Sampling interval for export file now derived from record header
               of selected series of records. If sampling interval varies within
               exported series, a warning message is displayed
  30.05.08 ... FP Errors when RH.ADCVoltageRange = 0 fixed
  05.08.13 ... MAT file export added. Compiled under Delphi XE
  11.02.15 ... No. of exportable channels increased to 32
  04.06.15 ... Multiple files can be exported. Format now selected from drop-down liat.
  09.06.15 ... Replaces export.pas module in WinEDR.
  25.08.15 ... Long export file names no longer split across two lines
               No. of samples in exported EDR files now correct.
  }
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RangeEdit, global, ADCDataFile, maths, strutils, SESLabIO, MatFileWriterUnit, math, UITypes ;

type
  TExportFrm = class(TForm)
    GroupBox8: TGroupBox;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    ChannelsGrp: TGroupBox;
    ckCh0: TCheckBox;
    ckCh1: TCheckBox;
    ckCh2: TCheckBox;
    ckCh3: TCheckBox;
    ckCh4: TCheckBox;
    ckCh5: TCheckBox;
    ckCh6: TCheckBox;
    ckCh7: TCheckBox;
    GroupBox3: TGroupBox;
    GroupBox2: TGroupBox;
    bChangeName: TButton;
    bOK: TButton;
    bCancel: TButton;
    ExportFile: TADCDataFile;
    ckCombineRecords: TCheckBox;
    ckCh8: TCheckBox;
    ckCh9: TCheckBox;
    ckCh10: TCheckBox;
    ckCh11: TCheckBox;
    ckCh12: TCheckBox;
    ckCh13: TCheckBox;
    ckCh14: TCheckBox;
    ckCh15: TCheckBox;
    ckCh16: TCheckBox;
    ckCh17: TCheckBox;
    ckCh18: TCheckBox;
    ckCh19: TCheckBox;
    ckCh20: TCheckBox;
    ckCh21: TCheckBox;
    ckCh22: TCheckBox;
    ckCh23: TCheckBox;
    ckCh24: TCheckBox;
    ckCh25: TCheckBox;
    ckCh26: TCheckBox;
    ckCh27: TCheckBox;
    ckCh28: TCheckBox;
    ckCh29: TCheckBox;
    ckCh30: TCheckBox;
    ckCh31: TCheckBox;
    meFileList: TMemo;
    cbExportFormat: TComboBox;
    OpenDialog: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure bChangeNameClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure cbExportFormatChange(Sender: TObject);
  private
    { Private declarations }
    ExportFileName : string ;
    ExportExtension : Array[0..100] of String ;
    KeepFileName : String ;

    procedure SetChannel( CheckBox : TCheckBox ; ch : Integer ) ;
    function CreateExportFileName(FileName : string ) : String ;
    procedure ExportToFile( FileName : string ) ;
    procedure ExportToIGORFile( FileName : string ) ;
    procedure ExportToMATFile( FileName : string ) ;
    function UseChannel( chan : Integer ) : Boolean ;
    procedure UpdateChannelSelectionList ;


  public
    { Public declarations }
  end;

var
  ExportFrm: TExportFrm;

implementation

uses Mdiform, EDRFileUnit;

{$R *.DFM}


procedure TExportFrm.FormShow(Sender: TObject);
// ------------------------------
// Initialise form when displayed
// ------------------------------
begin

     Top := Main.Top + 60 ;
     Left := Main.Left + 20 ;

    KeepFileName := EDRFIle.CDRFH.FileName ;

     // Set block of EDR file to be exported
     edRange.LoLimit := 0.0 ;
     edRange.LoValue := 0.0 ;
     edRange.Units := 's' ;
     edRange.HiLimit := (EDRFIle.CdrFH.NumSamplesInFile div EDRFIle.CdrFH.NumChannels -1)*EDRFIle.cdrFH.dt ;
     edRange.HiValue := edRange.HiLimit ;

     // Export formats
     cbExportFormat.Clear ;
     cbExportFormat.Items.AddObject('Axon ABF V1.6',TObject(ftAxonABF)) ;
     ExportExtension[cbExportFormat.Items.Count-1] := '.abf' ;

     cbExportFormat.Items.AddObject('CED CFS',TObject(ftCFS)) ;
     ExportExtension[cbExportFormat.Items.Count-1] := '.cfs' ;

     cbExportFormat.Items.AddObject('ASCII Text',TObject(ftASC)) ;
     ExportExtension[cbExportFormat.Items.Count-1] := '.txt' ;

     cbExportFormat.Items.AddObject('Strathclyde WCP',TObject(ftWCP)) ;
     ExportExtension[cbExportFormat.Items.Count-1] := '.wcp' ;

     cbExportFormat.Items.AddObject('Strathclyde EDR',TObject(ftEDR)) ;
     ExportExtension[cbExportFormat.Items.Count-1] := '.edr' ;

     cbExportFormat.Items.AddObject('Wavemetrics IGOR IBW',TObject(ftIBW)) ;
     ExportExtension[cbExportFormat.Items.Count-1] := '.ibw' ;

     cbExportFormat.Items.AddObject('Matlab MAT',TObject(ftMAT)) ;
     ExportExtension[cbExportFormat.Items.Count-1] := '.mat' ;

     cbExportFormat.ItemIndex := 0 ;

     { Update O/P file name channel selection options }
     meFileList.Clear ;
     meFileList.Lines[0] := EDRFIle.CDRFH.FileName ;
     ckCombineRecords.Visible := False ;
     UpdateChannelSelectionList ;

     end;

procedure TExportFrm.SetChannel(
          CheckBox : TCheckBox ;
          ch : Integer
          ) ;
// ---------------------------
// Set channel selection state
// ---------------------------
begin
     if ch < EDRFIle.Cdrfh.NumChannels then begin
        CheckBox.Visible := True ;
        CheckBox.Checked := EDRFIle.Channel[ch].InUse ;
        CheckBox.Caption := EDRFIle.Channel[ch].ADCName ;
        end
     else CheckBox.Visible := False ;
     end ;


procedure TExportFrm.ExportToFile(
          FileName : string        // Name of file to export
          ) ;
// -------------------------------------------------
// Copy selected section of data file to export file
// -------------------------------------------------
const
   NumScansPerBuf = 512 ;
var
   StartAt,EndAt,ch,i,j : Integer ;
   InBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
   OutBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
   InScan : Integer ;
   OutScan : Integer ;
   NumScansToCopy : Integer ;
   NumScansToRead : Integer ;
   NumScansRead : Integer ;
   chOut : Integer ;
   Done : Boolean ;
   ExportType : TADCDataFileType ;
begin

     if rbAllRecords.Checked then begin
        StartAt := 0 ;
        EndAt := (EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels) -1 ;
        end
     else begin
        StartAt := Round(edRange.LoValue/EDRFIle.Cdrfh.dt) ;
        EndAt := Min(Round(edRange.HiValue/EDRFIle.Cdrfh.dt),(EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels) -1) ;
        end ;

     // Add record range to file name
     ExportFileName := CreateExportFileName(FileName) ;

     // If destination file already exists, allow user to abort
     if FileExists( ExportFileName ) then begin
        if MessageDlg( ExportFileName + ' exists! Overwrite?!',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit ;
         end ;

     // Export file type
     ExportType := TADCDataFileType(cbExportFormat.Items.objects[cbExportFormat.ItemIndex]);

     // Create empty export data file
     ExportFile.CreateDataFile( ExportFileName, ExportType ) ;

     // Set file parameters
     ExportFile.NumChannelsPerScan := EDRFIle.Cdrfh.NumChannels ;
     ExportFile.NumScansPerRecord := EndAt - StartAt + 1 ;
     ExportFile.MaxADCValue := EDRFile.Channel[0].ADCMaxValue ;
     ExportFile.MinADCValue := -EDRFile.Channel[0].ADCMaxValue -1 ;
     ExportFile.ScanInterval := EDRFIle.Cdrfh.dt ;
     ExportFile.IdentLine := EDRFIle.Cdrfh.IdentLine ;
     ExportFile.RecordNum := 1 ;
     ExportFile.ABFAcquisitionMode := ftGapFree ;

     chOut := 0 ;
     for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do if UseChannel(ch)then begin
         ExportFile.ChannelOffset[chOut] := chOut ;
         ExportFile.ChannelADCVoltageRange[chOut] := EDRFIle.Cdrfh.ADCVoltageRange ;
         ExportFile.ChannelName[chOut] := EDRFile.Channel[ch].ADCName ;
         ExportFile.ChannelUnits[chOut] := EDRFile.Channel[ch].ADCUnits ;
         ExportFile.ChannelScale[chOut] := EDRFile.Channel[ch].ADCSCale ;
         ExportFile.ChannelCalibrationFactor[chOut] := EDRFile.Channel[ch].ADCCalibrationFactor ;
         ExportFile.ChannelGain[chOut] := EDRFile.Channel[ch].ADCAmplifierGain ;
         Inc(chOut) ;
         end ;
     ExportFile.NumChannelsPerScan := chOut ;

     { Copy records }
     InScan := StartAt ;
     NumScansToCopy := EndAt - StartAt + 1 ;
     OutScan := 0 ;
     Done := False ;
     While not Done do begin

         // Read from buffer
         NumScansToRead := Min( NumScansToCopy,NumScansPerBuf ) ;
         NumScansRead := EDRFile.ReadBuffer( EDRFile.Cdrfh, InScan, InBuf, NumScansToRead ) ;
         if NumScansRead <= 0 then Done := True ;

         // Copy required channels
         j := 0 ;
         for i := 0 to NumScansRead-1 do begin
             for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do if UseChannel(ch) then begin
                 OutBuf[j] := InBuf[i*EDRFIle.Cdrfh.NumChannels+EDRFile.Channel[ch].ChannelOffset]
                              - Round(EDRFile.Channel[ch].ADCZero) ;
                 Inc(j) ;
                 end ;
             end ;

         // Write to export file
         //OutputDebugString(PChar(format('%d %d',[OutScan,NumScansRead])));
         ExportFile.SaveADCBuffer( OutScan, NumScansRead, OutBuf ) ;
         OutScan := OutScan + NumScansRead ;

         // Report progress
         Main.StatusBar.SimpleText := format(
         ' EXPORT: Exporting time points %d/%d to %s ',
         [InScan,EndAt,ExportFileName]) ;

         InScan := InScan + NumScansRead ;
         NumScansToCopy := NumScansToCopy - NumScansRead ;
         if NumScansToCopy <= 0 then Done := True ;

         end ;

     // Close export data file
     ExportFile.CloseDataFile ;

     // Final Report
     Main.StatusBar.SimpleText := format(
     ' EXPORT: %d time points exported to %s ',
     [EndAt-StartAt+1,ExportFileName]) ;
     EDRFile.WriteToLogFile( Main.StatusBar.SimpleText ) ;

     end;


procedure TExportFrm.ExportToIGORFile(
          FileName : string );       // Name of file to export ;
// -------------------------------------------------
// Export data to IGOR IBW file(s)
// -------------------------------------------------
const
   NumScansPerBuf = 256 ;
var
   StartAt,EndAt,ch,i,j : Integer ;
   InBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
   OutBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
   InScan : Integer ;
   OutScan : Integer ;
   NumScansToCopy : Integer ;
   NumScansToRead : Integer ;
   NumScansRead : Integer ;
   Done : Boolean ;
begin

     if rbAllRecords.Checked then begin
        StartAt := 0 ;
        EndAt := (EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels) -1 ;
        end
     else begin
        StartAt := Round(edRange.LoValue/EDRFIle.Cdrfh.dt) ;
        EndAt := Min(Round(edRange.HiValue/EDRFIle.Cdrfh.dt),(EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels) -1) ;
        end ;

     // If destination file already exists, allow user to abort
     if FileExists( ExportFileName ) then begin
        if MessageDlg( ExportFileName + ' exists! Overwrite?!',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit ;
         end ;

     for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do if UseChannel(ch) then begin

         // Create export file name
         ExportFileName := CreateExportFileName(FileName) ;
         ExportFileName := ANSIReplaceText( ExportFileName,
                                            '.ibw',
                                            format( '[%s].ibw',[EDRFile.Channel[ch].ADCName])) ;
         ExportFile.CreateDataFile( FileName, ftIBW ) ;

         // Set file parameters
         ExportFile.NumChannelsPerScan := 1 ;
         ExportFile.NumScansPerRecord := EndAt - StartAt + 1 ;
         ExportFile.MaxADCValue := EDRFile.Channel[0].ADCMaxValue ;
         ExportFile.MinADCValue := -EDRFile.Channel[0].ADCMaxValue -1 ;
         ExportFile.ScanInterval := EDRFIle.Cdrfh.dt ;
         ExportFile.IdentLine := EDRFIle.Cdrfh.IdentLine ;
         ExportFile.RecordNum := 1 ;
         ExportFile.ABFAcquisitionMode := ftGapFree ;
         ExportFile.NumChannelsPerScan := 1 ;

         ExportFile.ChannelOffset[0] := 0 ;
         ExportFile.ChannelADCVoltageRange[0] := EDRFIle.Cdrfh.ADCVoltageRange ;
         ExportFile.ChannelName[0] := EDRFile.Channel[ch].ADCName ;
         ExportFile.ChannelUnits[0] := EDRFile.Channel[ch].ADCUnits ;
         ExportFile.ChannelScale[0] := EDRFile.Channel[ch].ADCSCale ;
         ExportFile.ChannelCalibrationFactor[0] := EDRFile.Channel[ch].ADCCalibrationFactor ;
         ExportFile.ChannelGain[0] := EDRFile.Channel[ch].ADCAmplifierGain ;

         { Copy records }
         InScan := StartAt ;
         NumScansToCopy := EndAt - StartAt + 1 ;
         OutScan := 0 ;
         Done := False ;
         While not Done do begin

            // Read from buffer
            NumScansToRead := Min(NumScansToCopy,NumScansPerBuf) ;
            NumScansRead := EDRFile.ReadBuffer( EDRFile.Cdrfh, InScan, InBuf, NumScansToRead ) ;
            if NumScansRead <= 0 then Done := True ;

            // Copy required channel
            j := EDRFile.Channel[ch].ChannelOffset ;
            for i := 0 to NumScansRead-1 do begin
                OutBuf[i] := InBuf[j] ;
                j := j + EDRFIle.Cdrfh.NumChannels ;
                end ;

            // Write to export file
            ExportFile.SaveADCBuffer( OutScan, NumScansRead, OutBuf ) ;
            OutScan := OutScan + NumScansRead ;

            // Report progress
            Main.StatusBar.SimpleText := format(
            ' EXPORT: Exporting time points %d/%d to %s ',
            [InScan,EndAt,FileName]) ;

            InScan := InScan + NumScansRead ;
            NumScansToCopy := NumScansToCopy - NumScansRead ;
            if NumScansToCopy <= 0 then Done := True ;

            end ;

         // Close export data file
         ExportFile.CloseDataFile ;

         // Final Report
         Main.StatusBar.SimpleText := format(
         ' EXPORT: %d time points from %s to %s ',
         [EndAt-StartAt+1,FileName,ExportFileName]) ;
         EDRFile.WriteToLogFile( Main.StatusBar.SimpleText ) ;

         end ;



     end;


procedure TExportFrm.ExportToMATFile(
          FileName : string ) ;       // Name of file to export
// -------------------------------------------------
// Copy selected records to MATLab .MAT file
// -------------------------------------------------
const
    NumScansPerBuf = 100000 ;
var
   ch,i,j : Integer ;
   InBuf : PSmallIntArray ;       // Source data buffer
   YBuf : PDoubleArray ;      // Output data buffer
   TBuf : PDoubleArray ;      // Output data buffer
   StartAt,EndAt : Integer ;                 // Record counter
   NumChannelsExported : Integer ;
   Writer : TMATFileWriter ;
   NumScans,NumScansRead,NumScansToRead,NumScansToExport : Integer ;
begin

     if rbAllRecords.Checked then begin
        StartAt := 0 ;
        EndAt := (EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels) -1 ;
        end
     else begin
        StartAt := Round(edRange.LoValue/EDRFIle.Cdrfh.dt) ;
        EndAt := Min(Round(edRange.HiValue/EDRFIle.Cdrfh.dt),(EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels) -1) ;
        end ;

     // If destination file already exists, allow user to abort
     if FileExists( ExportFileName ) then begin
        if MessageDlg( ExportFileName + ' exists! Overwrite?!',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit ;
         end ;

     // Add record range to file name
     FileName := CreateExportFileName(ExportFileName) ;

     // Create buffers
     NumScansToExport := EndAt - StartAt + 1 ;
     GetMem( InBuf, NumScansPerBuf*EDRFIle.Cdrfh.NumChannels*2 ) ;
     GetMem( YBuf, NumScansToExport*EDRFIle.Cdrfh.NumChannels*8 ) ;
     GetMem( TBuf, NumScansToExport*8 ) ;

     // Open MAT file
     Writer := TMATFileWriter.Create();
     Writer.OpenMATFile( FileName ) ;
     Writer.WriteFileHeader;

     Try

     // Write time vector
     for i := 0 to NumScansToExport-1 do begin
         TBuf^[i] := i*EDRFIle.Cdrfh.dt ;
         end ;

     // Extract channels to copy to to YBuf
     NumScansToRead := NumScansToExport ;
     NumChannelsExported := 0 ;
     NumScansRead := 0 ;
     while NumScansToRead > 0 do begin
           NumScans := EDRFile.ReadBuffer( EDRFile.Cdrfh, StartAt, InBuf^,Min(NumScansToRead,NumScansPerBuf) ) ;
           NumChannelsExported := 0 ;
           for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do if UseChannel(ch) then begin
               j := NumChannelsExported*NumScansToExport + NumScansRead ;
               for i := 0 to NumScans-1 do begin
                   YBuf^[j] := (InBuf^[(i*EDRFIle.Cdrfh.NumChannels)+EDRFile.Channel[ch].ChannelOffset]
                                            - EDRFile.Channel[ch].ADCZero)*EDRFile.Channel[ch].ADCSCale ;
                   Inc(j) ;
                   end ;
               Inc(NumChannelsExported) ;
               end ;
           NumScansToRead := NumScansToRead - NumScans ;
           NumScansRead := NumScansRead + NumScans ;
           if NumScansRead = 0 then NumScansToRead := 0 ;
           StartAt := StartAt + NumScans ;
           end ;

     // Write to MAT file
     Writer.WriteDoubleMatrixHeader('T',NumScansRead,1);
     Writer.WriteDoubleMatrixValues( TBuf^,NumScansRead,1) ;
     Writer.WriteDoubleMatrixHeader('Y',NumScansRead,NumChannelsExported);
     Writer.WriteDoubleMatrixValues( YBuf^, NumScansRead,NumChannelsExported) ;

     // Final Report
     Main.StatusBar.SimpleText := format(
     ' EXPORT: %.5g - %.5g time points exported from %s to %s ',
     [StartAt*EDRFIle.Cdrfh.dt,EndAt*EDRFIle.Cdrfh.dt,FileName,ExportFileName]) ;
     EDRFile.WriteToLogFile( Main.StatusBar.SimpleText ) ;

     Finally
        // Free allocated buffers
        FreeMem( InBuf ) ;
        FreeMem( YBuf ) ;
        FreeMem( TBuf ) ;
        // Close export data file
        Writer.CloseMATFile;
        end ;

     end;


procedure TExportFrm.bChangeNameClick(Sender: TObject);
{ ------------------------
  Select files for export
  ------------------------ }
begin

     OpenDialog.DefaultExt := '.edr' ;
     OpenDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist,ofAllowMultiSelect] ;
     OpenDialog.Filter := ' Files (*' + OpenDialog.DefaultExt + ')|*' +
                            OpenDialog.DefaultExt + '|' ;

     OpenDialog.Title := 'Select Files to Export ' ;
     if EDRFile.DataDirectory <> '' then begin
        SetCurrentDir(EDRFile.DataDirectory);
        OpenDialog.InitialDir := EDRFile.DataDirectory ;
        end;

     if OpenDialog.Execute then begin
        meFileList.Lines.Assign(OpenDialog.Files);
        UpdateChannelSelectionList ;
        end;

     end;


function TExportFrm.CreateExportFileName(
         FileName : string
         ) : String ;
// ---------------------------------------------------
// Update control settings when export format changed
// ---------------------------------------------------
var
    StartAt,EndAt,ch : Integer ;
    s : string ;
begin

     ChangeFileExt( FileName, '.tmp' ) ;

     if rbAllRecords.Checked then begin
        StartAt := 0 ;
        EndAt := (EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels) -1 ;
        end
     else begin
        StartAt := Round(edRange.LoValue/EDRFIle.Cdrfh.dt) ;
        EndAt := Min(Round(edRange.HiValue/EDRFIle.Cdrfh.dt),(EDRFIle.Cdrfh.NumSamplesInFile div EDRFIle.Cdrfh.NumChannels) -1) ;
        s := s + format( '%.6g-%.6gs',[edRange.LoValue,edRange.HiValue]) ;
        end ;

     // Set file extension to .tmp to locate end of file later
     FileName := ChangeFileExt( FileName, '.tmp' ) ;

     // Add record range
     FileName := ANSIReplaceText( FileName,
                                  '.tmp',
                                 format( '%.6g-%.6gs',[StartAt*EDRFIle.Cdrfh.dt,EndAt*EDRFIle.Cdrfh.dt]) ) ;

     // Add channels for ASCII text export
     if TADCDataFileType(cbExportFormat.Items.objects[cbExportFormat.ItemIndex])=ftASC then begin
        s := '[' ;
        for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do if UseChannel(ch) then begin
            s := s + EDRFile.Channel[ch].ADCName + ',' ;
            end;
        s := LeftStr(s,Length(s)-1)+'].tmp' ;
        FileName := ANSIReplaceText( FileName,'.tmp',s);
        end;

     FileName := ChangeFileExt( FileName, ExportExtension[cbExportFormat.ItemIndex] ) ;

     Result := FileName ;

     end ;


procedure TExportFrm.bOKClick(Sender: TObject);
// ----------------------
// Export to output file
// ----------------------
var
    ExportType : TADCDataFileType ;
    FileName : string ;
    i : Integer ;
begin

    ExportType := TADCDataFileType(cbExportFormat.Items.objects[cbExportFormat.ItemIndex]);

    // Close currently open data file
    FileClose(EDRFIle.Cdrfh.FileHandle) ;
    EDRFIle.Cdrfh.FileHandle := -1 ;

    for i := 0 to meFileList.Lines.Count-1 do begin

        FileName := meFileList.Lines[i] ;
        EDRFile.LoadDataFiles(FileName) ;

        case ExportType of
             ftIBW : ExportToIGORFile(FileName) ;
             ftMAT : ExportToMATFile(FileName) ;
             else ExportToFile(FileName) ;
             end ;

        FileClose(EDRFIle.Cdrfh.FileHandle) ;
        EDRFIle.Cdrfh.FileHandle := -1 ;

        end;

    EDRFile.LoadDataFiles(KeepFileName) ;

    end ;


procedure TExportFrm.cbExportFormatChange(Sender: TObject);
// ---------------------
// Output format changed
// ---------------------
var
    ExportType : TADCDataFileType ;
begin

   ExportType := TADCDataFileType(cbExportFormat.Items.objects[cbExportFormat.ItemIndex]);
   case ExportType of
     ftMat : begin
       ckCombineRecords.Visible := true ;
       end;
     ftASC : begin
       ckCombineRecords.Visible := true ;
       end;
     else begin
       ckCombineRecords.Visible := False ;
       end;
     end ;
   end;


function TExportFrm.UseChannel( chan : Integer ) : Boolean ;
// ---------------------------------------------
// Return TRUE if channel is selected for export
// ---------------------------------------------
begin
    case Chan of
      0 : Result := ckCh0.Checked or (not ckCh0.Visible) ;
      1 : Result := ckCh1.Checked or (not ckCh1.Visible) ;
      2 : Result := ckCh2.Checked or (not ckCh2.Visible) ;
      3 : Result := ckCh3.Checked or (not ckCh3.Visible) ;
      4 : Result := ckCh4.Checked or (not ckCh4.Visible) ;
      5 : Result := ckCh5.Checked or (not ckCh5.Visible) ;
      6 : Result := ckCh6.Checked or (not ckCh6.Visible) ;
      7 : Result := ckCh7.Checked or (not ckCh7.Visible) ;
      8 : Result := ckCh8.Checked or (not ckCh8.Visible) ;
      9 : Result := ckCh9.Checked or (not ckCh9.Visible) ;
      10 : Result := ckCh10.Checked or (not ckCh10.Visible) ;
      11 : Result := ckCh11.Checked or (not ckCh11.Visible) ;
      12 : Result := ckCh12.Checked or (not ckCh12.Visible) ;
      13 : Result := ckCh13.Checked or (not ckCh13.Visible) ;
      14 : Result := ckCh14.Checked or (not ckCh14.Visible) ;
      15 : Result := ckCh15.Checked or (not ckCh15.Visible) ;
      16 : Result := ckCh16.Checked or (not ckCh16.Visible) ;
      17 : Result := ckCh17.Checked or (not ckCh17.Visible) ;
      18 : Result := ckCh18.Checked or (not ckCh18.Visible) ;
      19 : Result := ckCh19.Checked or (not ckCh19.Visible) ;
      20 : Result := ckCh20.Checked or (not ckCh20.Visible) ;
      21 : Result := ckCh21.Checked or (not ckCh21.Visible) ;
      22 : Result := ckCh22.Checked or (not ckCh22.Visible) ;
      23 : Result := ckCh23.Checked or (not ckCh23.Visible) ;
      24 : Result := ckCh24.Checked or (not ckCh24.Visible) ;
      25 : Result := ckCh25.Checked or (not ckCh25.Visible) ;
      26 : Result := ckCh26.Checked or (not ckCh26.Visible) ;
      27 : Result := ckCh27.Checked or (not ckCh27.Visible) ;
      28 : Result := ckCh28.Checked or (not ckCh28.Visible) ;
      29 : Result := ckCh29.Checked or (not ckCh29.Visible) ;
      30 : Result := ckCh30.Checked or (not ckCh30.Visible) ;
      31 : Result := ckCh31.Checked or (not ckCh31.Visible) ;
      Else Result := True ;
      end;
    end;


procedure TExportFrm.UpdateChannelSelectionList ;
// --------------------------------------
// Update list of channel selection boxes
// --------------------------------------
begin

     EDRFile.CloseDataFile ;
     EDRFile.LoadDataFiles( meFileList.Lines[0] ) ;

     SetChannel( ckCh0, 0 ) ;
     SetChannel( ckCh1, 1 ) ;
     SetChannel( ckCh2, 2 ) ;
     SetChannel( ckCh3, 3 ) ;
     SetChannel( ckCh4, 4 ) ;
     SetChannel( ckCh5, 5 ) ;
     SetChannel( ckCh6, 6 ) ;
     SetChannel( ckCh7, 7 ) ;
     SetChannel( ckCh8, 8 ) ;
     SetChannel( ckCh9, 9 ) ;
     SetChannel( ckCh10, 10 ) ;
     SetChannel( ckCh11, 11 ) ;
     SetChannel( ckCh12, 12 ) ;
     SetChannel( ckCh13, 13 ) ;
     SetChannel( ckCh14, 14 ) ;
     SetChannel( ckCh15, 15 ) ;
     SetChannel( ckCh16, 16 ) ;
     SetChannel( ckCh17, 17 ) ;
     SetChannel( ckCh18, 18 ) ;
     SetChannel( ckCh19, 19 ) ;
     SetChannel( ckCh20, 20 ) ;
     SetChannel( ckCh21, 21 ) ;
     SetChannel( ckCh22, 22 ) ;
     SetChannel( ckCh23, 23 ) ;
     SetChannel( ckCh24, 24 ) ;
     SetChannel( ckCh25, 25 ) ;
     SetChannel( ckCh26, 26 ) ;
     SetChannel( ckCh27, 27 ) ;
     SetChannel( ckCh28, 28 ) ;
     SetChannel( ckCh29, 29 ) ;
     SetChannel( ckCh30, 30 ) ;
     SetChannel( ckCh31, 31 ) ;

     EDRFile.CloseDataFile ;
     EDRFile.LoadDataFiles(KeepFileName) ;

     end ;

end.
