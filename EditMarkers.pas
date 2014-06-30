unit EditMarkers;
// -------------------------------------
// CHART - Edit chart markers dialog box
// -------------------------------------
// 20.05.03

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditMarkersFrm: TEditMarkersFrm;

implementation

uses MDIForm, shared, global, maths, fileio, math ;

{$R *.dfm}

procedure TEditMarkersFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
     t : Single ;
begin

     // File ident
     edIdent.Text := CDRFH.IdentLine ;

     if MarkerList.Count > 1 then begin
        Table.FixedRows := 1 ;
        Table.RowCount := MarkerList.Count + 1 ;
        end
     else begin
        Table.FixedRows := 0 ;
        Table.RowCount := 1 ;
        end ;

     Table.Cells[0,0] := 'Time (' + Settings.TUnits + ')' ;
     Table.Cells[1,0] := 'Text' ;
     for i := 0 to MarkerList.Count-1 do begin
         t := Single(MarkerList.Objects[i]) ;
         Table.Cells[0,i+1] := format( '%.4g',[t]) ;
         Table.Cells[1,i+1] := MarkerList.Strings[i] ;
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

     MarkerList.Clear ;
     for i := 1 to Table.RowCount-1 do begin
         t := ExtractFloat( Table.Cells[0,i], -1.0 ) ;
         if t >= 0.0 then begin
            MarkerList.AddObject( Table.Cells[1,i], TObject(t) ) ;
            end ;
         end ;

     // Update ident. line
     if edIdent.Text <> CDRFH.IdentLine then begin
        CDRFH.IdentLine := edIdent.Text ;
        WriteToLogFile(' Ident. changed: ' +  CDRFH.IdentLine ) ;
        end ;

     // Save file header
     SaveCDRHeader( CDRFH ) ;

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
