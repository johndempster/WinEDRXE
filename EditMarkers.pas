unit EditMarkers;
// -------------------------------------
// CHART - Edit chart markers dialog box
// -------------------------------------
// 20.05.03
// 24.03.24 ... Form change to MDIChild and position saved to INI file

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, EDRFileUnit;

type
  TEditMarkersFrm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    Table: TStringGrid;
    bDeleteMarker: TButton;
    edIdent: TEdit;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bDeleteMarkerClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditMarkersFrm: TEditMarkersFrm;

implementation

uses MDIForm, maths, math , ViewSig, PageView;

{$R *.dfm}

procedure TEditMarkersFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// ------------------------------
// Procedures when form is closed
// ------------------------------
begin

     Action := caFree ;

    // Save form position to INI file
    EDRFile.SaveFormPosition( Self ) ;

     end;


procedure TEditMarkersFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
     t : Single ;
begin

     // File ident
     edIdent.Text := EDRFile.CDRFH.IdentLine ;

     if EDRFile.MarkerList.Count > 1 then begin
        Table.FixedRows := 1 ;
        Table.RowCount := EDRFile.MarkerList.Count + 1 ;
        end
     else begin
        Table.FixedRows := 0 ;
        Table.RowCount := 1 ;
        end ;

     Table.Cells[0,0] := 'Time (' + EDRFile.Settings.TUnits + ')' ;
     Table.Cells[1,0] := 'Text' ;
     for i := 0 to EDRFile.MarkerList.Count-1 do begin
         t := Single(EDRFile.MarkerList.Objects[i]) ;
         Table.Cells[0,i+1] := format( '%.4g',[t]) ;
         Table.Cells[1,i+1] := EDRFile.MarkerList.Strings[i] ;
         end ;

     end;


procedure TEditMarkersFrm.bOKClick(Sender: TObject);
// ------------------
// Update marker list
// ------------------
var
     i : Integer ;
     t : Single ;
begin

     EDRFile.MarkerList.Clear ;
     for i := 1 to Table.RowCount-1 do begin
         t := ExtractFloat( Table.Cells[0,i], -1.0 ) ;
         if t >= 0.0 then begin
            EDRFile.MarkerList.AddObject( Table.Cells[1,i], TObject(t) ) ;
            end ;
         end ;

     // Update ident. line
     if edIdent.Text <> EDRFile.CDRFH.IdentLine then begin
        EDRFile.CDRFH.IdentLine := edIdent.Text ;
        EDRFile.WriteToLogFile(' Ident. changed: ' +  EDRFile.CDRFH.IdentLine ) ;
        end ;

     // Save file header
     EDRFile.SaveHeader( EDRFile.CDRFH ) ;

     // Update display
     Main.UpdateViewSig ;

     end;


procedure TEditMarkersFrm.bDeleteMarkerClick(Sender: TObject);
// ----------------------
// Delete selected marker
// ----------------------
var
     i : Integer ;
begin
     for i := Table.Row to Table.RowCount-2 do begin
         Table.Cells[0,i] := Table.Cells[0,i+1] ;
         Table.Cells[1,i] := Table.Cells[1,i+1] ;
         end ;
     Table.RowCount := Table.RowCount - 1 ;

     end;

end.
