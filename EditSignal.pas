unit EditSignal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ValidatedEdit;

type
  TEditSignalDataFrm = class(TForm)
    bEditData: TButton;
    bCancel: TButton;
    gpEditOptions: TGroupBox;
    Label1: TLabel;
    edStartAtScan: TValidatedEdit;
    edNumScansToDelete: TValidatedEdit;
    bRestoreBackup: TButton;
    rbDeleteBlock: TRadioButton;
    rbShiftChannels: TRadioButton;
    edShiftChannelsBy: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bEditDataClick(Sender: TObject);
    procedure bRestoreBackupClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    StartAtScan : LongInt ;
    ScansToDelete : LongInt ;
  end;

var
  EditSignalDataFrm: TEditSignalDataFrm;

implementation

{$R *.dfm}

uses EDRFileUnit , MDIFORM, math ;

procedure TEditSignalDataFrm.bCancelClick(Sender: TObject);
// ----------
// Close form
// ----------
begin
    Close ;
end;

procedure TEditSignalDataFrm.bEditDataClick(Sender: TObject);
// --------------
// Edit data file
// --------------
const
     NumScansPerBuffer = 65536 ;
var
   i,PercentDone : Integer ;
   Done : Boolean ;
   iSample,StartAtSample,EndAtSample,NumSamples,iShift : Int64 ;
   FileReadPointer, FileWritePointer : Int64 ;
   nbf,NumBytesToRead,NumBytesInFile,NumBytes,BufSize : Int64 ;
   iBuf : pSmallIntArrayDyn ;
begin

    // Make backup of data file
    EDRFile.MakeBackupFile ;

   // Set starting file read & write positions
   // (Note shift up when channels are to be realigned
    StartAtSample := Round(edStartAtScan.Value) * EDRFile.CDRFH.Numchannels ;

    // Allocate memory buffer
    BufSize := NumScansPerBuffer*EDRFile.CDRFH.NumChannels*SizeOf(SmallInt) ;
    iBuf := AllocMem( BufSize ) ;

    // Find # bytes in file
    NumBytesInFile := EDRFile.CDRFH.NumSamplesInFile*SizeOf(SmallInt) + EDRFile.CDRFH.NumBytesInHeader;

    // Copy bytes
    iSample := StartAtSample ;
    EndAtSample := iSample + Round(edNumScansToDelete.Value)*EDRFile.CDRFH.Numchannels - 1 ;
    NumBytesToRead := (EDRFile.CDRFH.NumSamplesInFile - StartAtSample)*SizeOf(SmallInt)  ;

    iShift := Round(edShiftChannelsBy.Value)*SizeOf(SmallInt) ;
    if not rbShiftChannels.Checked then iShift := 0 ;

    FileReadPointer := StartAtSample*SizeOf(SmallInt) + EDRFile.CDRFH.NumBytesInHeader + iShift ;
    FileWritePointer := FileReadPointer  - iShift ;

    repeat

       // Samples to read and write
       NumBytes := Min( BufSize, NumBytesToRead ) ;
       NumSamples := NumBytes div SizeOf(SmallInt) ;
       FileSeek( EDRFile.CDRFH.FileHandle, FileReadPointer, 0 ) ;
       FileRead( EDRFIle.Cdrfh.FileHandle,iBuf^,NumBytes) ;

       // Delete (replace with zero level) selected segment of samples
       if rbDeleteBlock.Checked then
          begin
          for i := 0 to NumSamples-1 do
              begin
              if (iSample >= StartAtSample) and (iSample <= EndAtSample) then
                 begin
                 iBuf^[i] := EDRFile.Channel[iSample mod EDRFile.CDRFH.NumChannels].ADCZero ;
                 end;
              Inc(iSample) ;
              end;
          end;

       // Write buffer
       FileSeek( EDRFile.CDRFH.FileHandle, FileWritePointer, 0 ) ;
       FileWrite( EDRFIle.Cdrfh.FileHandle,iBuf^,NumBytes) ;

       // No. bytes still to do
       NumBytesToRead := NumBytesToRead - NumBytes ;
       FileReadPointer := FileReadPointer + NumBytes ;
       FileWritePointer := FileWritePointer + NumBytes ;

       PercentDone := Round( 100.0*(1.0 - NumBytesToRead/NumBytesInFile)) ;
       if (PercentDone mod 5) = 0 then
          begin
          Main.StatusBar.SimpleText := format( 'Editing %s %d%% done', [EDRFile.CDRFH.FileName,PercentDone] ) ;
          // Allow other events to be serviced
          Application.ProcessMessages ;
          end ;

       until NumBytesToRead <= 0 ;

    // Fill last multi-channel scan with 2nd last (if channels shifted)

    if Round(edShiftChannelsBy.Value) > 0 then
       begin

       // Read last 2 sets of multi-channel scans
       NumBytes := EDRFile.CDRFH.NumChannels*SizeOf(SmallInt)*2  ;
       FileReadPointer := NumBytesInFile - NumBytes ;
       FileRead( EDRFIle.Cdrfh.FileHandle,iBuf^,NumBytes) ;

       // Copy 2nd last into last scan
       for i := 0 to EDRFile.CDRFH.NumChannels-1 do
           begin
           iBuf^[i+EDRFile.CDRFH.NumChannels] := iBuf^[i] ;
           end;

       // Write back to file
       FileSeek( EDRFIle.Cdrfh.FileHandle, FileReadPointer, 0 ) ;
       FileWrite( EDRFIle.Cdrfh.FileHandle,iBuf^,NumBytes) ;

       end ;


    FreeMem( iBuf ) ;

    Main.UpdateViewSig ;

    Close ;



end;

procedure TEditSignalDataFrm.bRestoreBackupClick(Sender: TObject);
// ---------------------------------
// Restore original from backup file
// ---------------------------------

begin

    EDRFile.RestoreFromBackupFile ;

    Main.UpdateViewSig ;

end;

procedure TEditSignalDataFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ----------
// Close form
// ----------
begin
    Action := caFree ;
end;

procedure TEditSignalDataFrm.FormShow(Sender: TObject);
// ---------------------------------
// Init controls when form displayed
// ---------------------------------
begin
    edStartAtScan.Scale := EDRFile.CDRFH.dt ;
    edStartAtScan.Value := StartAtScan ;
    edNumScansToDelete.Scale := EDRFile.CDRFH.dt ;
    edNumScansToDelete.Value := 1 ;

    bRestoreBackup.Enabled := EDRFile.CDRFH.BackedUp ;

end;

end.
