unit Log;
{ ====================================================
  WinWCP - Display log file text (c) J. Dempster 1996
  ====================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Global, FileIo ;

type
  TLogFrm = class(TForm)
    mmText: TMemo;
    AddGrp: TGroupBox;
    meAdd: TMemo;
    bAddNote: TButton;
    procedure bCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bAddNoteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LogFrm: TLogFrm;

implementation

{$R *.DFM}
uses MdiForm ;


procedure TLogFrm.bCloseClick(Sender: TObject);
begin
     close ;
     end;

procedure TLogFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     { Enable "Inspect Log File" option of "File" menu}
     Main.InspectLogFile.enabled := true ;
     Action := caFree ;
     end;

procedure TLogFrm.FormActivate(Sender: TObject);
begin
     Caption := 'Log File ' + LogFileName ;
     CloseLogFile ;
     mmText.Lines.LoadFromFile( LogFileName ) ;
     OpenLogFile ;
     Resize ;
     end;

procedure TLogFrm.FormDeactivate(Sender: TObject);
begin
     CloseLogFile ;
     mmText.Lines.SaveToFile( LogFileName ) ;
     OpenLogFile ;
     end;


procedure TLogFrm.FormCreate(Sender: TObject);
begin
     { Disable "Inspect Log File" option of "File" menu}
     Main.InspectLogFile.enabled := false ;
     end;

procedure TLogFrm.FormResize(Sender: TObject);
begin
     mmText.Width := ClientWidth - mmText.Left - 5 ;
     mmText.Height := ClientHeight - mmText.Top - 5 ;

     mmText.Height := AddGrp.Top - mmText.Top - 2 ;
     AddGrp.Top := ClientHeight - AddGrp.Height - 5 ;
     AddGrp.Width := mmText.Width ;
     meAdd.Width := AddGrp.ClientWidth - meAdd.Left - 5 ;

     end;

procedure TLogFrm.bAddNoteClick(Sender: TObject);
// ---------------
// Add note to log
// ---------------
var
    i : Integer ;
begin

    // Add note to log file
    for i := 0 to meAdd.Lines.Count-1 do WriteToLogFile( meAdd.Lines[i] ) ;

    // CLear note
    meAdd.Lines.Clear ;

    // Update log file display
    CloseLogFile ;
    mmText.Lines.LoadFromFile( LogFileName ) ;
    OpenLogFile ;

    end;

end.
