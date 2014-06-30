unit EventFilter;
// -------------------------------------------------
// WinEDR (c) J. Dempster, University of Strathclyde
// Event selection filter criteria form
// -------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ValEdit, ValidatedEdit;

const
    vEventNum = 0 ;
    vTime = 1 ;
    vInterval = 2 ;
    vFrequency = 3 ;
    vRate = 4 ;
    vPeak = 5 ;
    vTRise = 6 ;
    vTDecay50 = 7 ;
    MaxVar = 7 ;

type

  TFilter = record
    Use : Boolean ;
    Variable : Integer ;
    Include : Boolean ;
    LoLimit : Single ;
    HiLimit : Single ;
    end ;

  TEventFilterFrm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    cbVariable: TComboBox;
    GroupBox3: TGroupBox;
    rbInclude: TRadioButton;
    rbExclude: TRadioButton;
    bAdd: TButton;
    GroupBox4: TGroupBox;
    bDelete: TButton;
    bDeleteAll: TButton;
    meFilterList: TMemo;
    bOK: TButton;
    bCancel: TButton;
    GroupBox6: TGroupBox;
    edLoLimit: TValidatedEdit;
    Label2: TLabel;
    Label3: TLabel;
    edHiLimit: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bAddClick(Sender: TObject);
    procedure bDeleteAllClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Filters : Array[0..MaxVar] of TFilter ;
    VarNames : Array[0..MaxVar] of string ;
    procedure UpdateList(
              Filters : Array of TFilter ;
              meList : TMemo ) ;

  end;

var
  EventFilterFrm: TEventFilterFrm;

implementation

{$R *.DFM}

uses EventDetector ;

procedure TEventFilterFrm.UpdateList(
          Filters : Array of TFilter ;
          meList : TMemo ) ;
// ------------------------------
// Update list of filter criteria
// ------------------------------
var
    Action : string ;
    i : Integer ;
begin

    meList.Clear ;

    for i := 0 to High(Filters) do if Filters[i].Use then begin
        if Filters[i].Include then Action := '[+]'
                              else Action := '[-]' ;

        meList.Lines.Add(format('%.3g <=%s<= %.3g %s',
        [Filters[i].LoLimit,
         VarNames[Filters[i].Variable],
         Filters[i].HiLimit,
         Action] )) ;

        end ;
    end ;


procedure TEventFilterFrm.FormShow(Sender: TObject);
// ------------------------------------------
// Initialise controls on form when displayed
// ------------------------------------------
begin

     // X/Y Plot variables
     cbVariable.Clear ;
     cbVariable.Items.AddObject( VarNames[vEventNum], TObject(vEventNum) ) ;
     cbVariable.Items.AddObject( VarNames[vTime], TObject(vTime) ) ;
     cbVariable.Items.AddObject( VarNames[vInterval], TObject(vInterval) ) ;
     cbVariable.Items.AddObject( VarNames[vFrequency], TObject(vFrequency) ) ;
     cbVariable.Items.AddObject( VarNames[vRate], TObject(vRate) ) ;
     cbVariable.Items.AddObject( VarNames[vPeak], TObject(vPeak) ) ;
     cbVariable.Items.AddObject( VarNames[vTRise], TObject(vTRise) ) ;
     cbVariable.Items.AddObject( VarNames[vTDecay50], TObject(vTDecay50) ) ;
     cbVariable.ItemIndex := 0 ;

     UpdateList( Filters, meFilterList) ;

     end;

procedure TEventFilterFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     //Action := caFree ;
     end;


procedure TEventFilterFrm.bAddClick(Sender: TObject);
// ----------------------------------
// Add a new filter criterion to set
// ----------------------------------
var
     i : Integer ;
begin
     // Find next empty filter slot
     i := 0 ;
     while Filters[i].Use and (i<High(Filters)) do Inc(i) ;
     // Add filter criteria
     if not Filters[i].Use then begin
        Filters[i].Use := True ;
        Filters[i].Variable := Integer(cbVariable.Items.Objects[cbVariable.ItemIndex]) ;
        Filters[i].Include := rbInclude.Checked ;
        Filters[i].LoLimit := edLoLimit.Value ;
        Filters[i].HiLimit := edHiLimit.Value ;
        end
     else bAdd.Enabled := False ;

     // Update filter criteria list
     UpdateList( Filters, meFilterList) ;
     end;


procedure TEventFilterFrm.bDeleteAllClick(Sender: TObject);
// ---------------------------------
// Delete all filter criteria in set
// ---------------------------------
var
     i : Integer ;
begin
     for i := 0 to High(Filters) do Filters[i].Use := False ;
     UpdateList( Filters, meFilterList) ;
     bAdd.Enabled := True ;
     end;

end.
