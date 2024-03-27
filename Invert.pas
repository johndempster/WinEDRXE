unit Invert;
// ---------------------------------
// Invert signal in selected channel
// ---------------------------------
// 18.07.23 BlockStart now correctly starts at 0 rather than 1

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls;

type
  TInvertDlg = class(TForm)
    GroupBox1: TGroupBox;
    cbChannel: TComboBox;
    bOK: TButton;
    bCancel: TButton;
    prProgress: TProgressBar;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InvertDlg: TInvertDlg;

implementation

{$R *.DFM}

uses EDRFileUnit , MDIFORM;

procedure TInvertDlg.FormClose(Sender: TObject; var Action: TCloseAction);
// ------------------------------
// Procedures when form is closed
// ------------------------------
begin

     Action := caFree ;

    // Save form position to INI file
    EDRFile.SaveFormPosition( Self ) ;

end;


procedure TInvertDlg.FormShow(Sender: TObject);
//
// Initialisations when form is opened
//
var
   ch : Integer ;
begin

     // Fill channel selection list
     cbChannel.Clear ;
     for ch := 0 to EDRFile.CdrFH.NumChannels-1 do
         begin
         cbChannel.items.add( format('Ch.%d %s',[ch,EDRFile.Channel[ch].ADCName]) ) ;
         end ;
     cbChannel.ItemIndex := 0 ;

    prProgress.Min := 0 ;
    prProgress.Max := 1 ;
    prProgress.Position := 0 ;

     end;

procedure TInvertDlg.bOKClick(Sender: TObject);
{ --------------
  Invert signal
  -------------- }
const
     NumBlocksPerBuffer = 256 ;
var
   BlockPointer,NumBlocksInFile : LongInt ;
   ADC : Array[0..NumBlocksPerBuffer*(EDRChannelLimit+1)] of SmallInt ;
   j,i : Integer ;
   Done : Boolean ;
begin

    // No. of multi-channel sample groups in file
    NumBlocksInFile := EDRFile.CdrFH.NumSamplesInFile div EDRFile.CdrFH.NumChannels ;

    { Initialise progress bar }
    prProgress.Min := 1 ;
    prProgress.Max := NumBlocksInFile ;
    prProgress.Position := 1 ;

    BlockPointer := 0 ;
    Done := False ;
    while not Done do begin

       { Read a record from file and add its data to histogram }
       EDRFile.ReadBuffer(EDRFile.CdrFH,BlockPointer,ADC,NumBlocksPerBuffer) ;

       j := EDRFile.Channel[cbChannel.ItemIndex].ChannelOffset ;
        for i := 0 to NumBlocksPerBuffer-1 do
            begin
            ADC[j] := -ADC[j] ;
            j := j + EDRFile.CdrFH.NumChannels ;
            end ;

       { Write record back to file  }
       EDRFile.WriteBuffer(EDRFile.CdrFH,BlockPointer,ADC,NumBlocksPerBuffer) ;

       // Update progress bar
       prProgress.Position := BlockPointer ;

       // Increment to next record
       BlockPointer := BlockPointer + NumBlocksPerBuffer ;
       // Terminate loop
       if BlockPointer >= NumBlocksInFile then Done := True ;

       // Allow other events to be serviced
       Application.ProcessMessages ;

       end ;

    Main.UpdateViewSig ;

    Close ;

    end ;


end.
