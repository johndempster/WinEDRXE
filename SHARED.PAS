unit Shared;
{ =======================================================================
  Library of shared procedures and functions V1.0 7/1/95
  (c) J. Dempster, University of Strathclyde 1996-67. All Rights Reserved
  =======================================================================
  8/8/01 ExtractFloar moved to maths.pas
  14/8/01 CopyStringGrid modified
  25/9/6  ExtractInt now returns 0 when supplied with empty string}


interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Spin, Global, Grids, Printers, ClipBrd,
  maths ;


  function ExtractListOfFloats (
           const CBuf : string ;
           var Values : Array of Single ;
           PositiveOnly : Boolean
           ) : Integer ;
  function ExtractInt (
           CBuf : string
           ) : LongInt ;
  function VerifyInt(
           text : string ;
           LoLimit,HiLimit : LongInt
           ) : string ;
  procedure AppendFloat(
            var Dest : array of ANSIChar;
            Keyword : string ;
            Value : Extended
            ) ;
  procedure ReadFloat(
            const Source : array of ANSIChar;
            Keyword : string ;
            var Value : Single ) ;
  procedure AppendInt(
            var Dest : array of ANSIChar;
            Keyword : string ;
            Value : LongInt
            ) ;
  procedure ReadInt(
            const Source : array of ANSIChar;
            Keyword : string ;
            var Value : LongInt
            ) ;
  procedure AppendLogical(
            var Dest : array of ANSIChar;
            Keyword : string ;
            Value : Boolean ) ;
  procedure ReadLogical(
            const Source : array of ANSIChar;
            Keyword : string ;
            var Value : Boolean
            ) ;
  procedure AppendString(
            var Dest : Array of ANSIChar;
            Keyword,
            Value : string
            ) ;
  procedure ReadString(
            const Source : Array of ANSIChar;
            Keyword : string ;
            var Value : string
            ) ;

  procedure CopyStringToArray(
            var Dest : array of ANSIChar ;
            Source : string
            ) ;
  procedure CopyArrayToString(
            var Dest : string ;
            var Source : array of ANSIChar
            ) ;
  procedure FindParameter(
            const Source : array of ANSIChar ;
            Keyword : string ;
            var Parameter : string
            ) ;
  Function GetFromEditBox(
           var ed : TEdit ;
           Default, Min, Max : Single ;
           const FormatString, Units : string ;
           Scale : single
           ) : Single ;
  procedure GetIntRangeFromEditBox(
            var ed : TEdit ;
            var Lo,Hi : LongInt ;
            Min,Max : LongInt
            ) ;
  Procedure GetRangeFromEditBox(
            const ed : TEdit ;
            var LoValue,HiValue : Single ;
            Min,Max : Single ;
            const FormatString : String ;
            const Units : String
            ) ;
  function Contains(
           const Target,
           Buf : string
           ) : boolean ;

  function ReplaceFileEnding(
           FileName,
           Ending : string
           ) : string ;
  function ExtractFileNameOnly(
           FilePath : string
           ) : string ;

  procedure PrintHeaderAndFooter ;
  procedure PrintPageTitle(
            Canvas : TCanvas ;
            EqnType : TEqnType ;
            const Results : TStringGrid ;
            var YEndOfText : Integer
            ) ;

  procedure PrintStringGrid( const Table : TStringGrid ) ;
  procedure CopyStringGrid(
          const Table : TStringGrid ;
          UseSelection : Boolean ) ;

  function PrinterPointsToPixels( PointSize : Integer ) : Integer ;
  function PrinterCmToPixels( const Axis : string ; cm : single ) : Integer ;

  function GetChannelOffset( Chan, NumChannels : LongInt ) : Integer ;
  const
     MaxSingle = 1E38 ;

implementation

{uses plotlib ;}


function ExtractInt ( CBuf : string ) : longint ;
{ ---------------------------------------------------
  Extract a 32 bit integer number from a string which
  may contain additional non-numeric text
  ---------------------------------------------------}

Type
    TState = (RemoveLeadingWhiteSpace, ReadNumber) ;
var CNum : string ;
    i : integer ;
    Quit : Boolean ;
    State : TState ;

begin

     if CBuf = '' then begin
        Result := 0 ;
        Exit ;
        end ;

     CNum := '' ;
     i := 1;
     Quit := False ;
     State := RemoveLeadingWhiteSpace ;
     while not Quit do begin

           case State of

                { Ignore all non-numeric characters before number }
                RemoveLeadingWhiteSpace : begin
                   if CBuf[i] in ['0'..'9','+','-'] then State := ReadNumber
                                                    else i := i + 1 ;
                   end ;

                { Copy number into string CNum }
                ReadNumber : begin
                    {End copying when a non-numeric character
                    or the end of the string is encountered }
                    if CBuf[i] in ['0'..'9','E','e','+','-','.'] then begin
                       CNum := CNum + CBuf[i] ;
                       i := i + 1 ;
                       end
                    else Quit := True ;
                    end ;
                else end ;

           if i > Length(CBuf) then Quit := True ;
           end ;
     try


        ExtractInt := StrToInt( CNum ) ;
     except
        ExtractInt := 1 ;
        end ;
     end ;


function VerifyInt( text : string ; LoLimit,HiLimit : LongInt ) : string ;
{ -------------------------------------------------------------
  Ensure an ASCII edit field contains a value within set limits
  -------------------------------------------------------------}
var
   Value : LongInt ;
begin
     Value := ExtractInt( text ) ;
     if Value < LoLimit then Value := LoLimit ;
     If Value > HiLimit then Value := HiLimit ;
     VerifyInt := IntToStr( Value ) ;
     end ;


function ExtractListOfFloats ( const CBuf : string ;
                                var Values : Array of Single ;
                                PositiveOnly : Boolean ) : Integer ;
{ -------------------------------------------------------------
  Extract a series of floating point number from a string which
  may contain additional non-numeric text
  ---------------------------------------}

var
   CNum : string ;
   i,nValues : integer ;
   EndOfNumber : Boolean ;
begin
     nValues := 0 ;
     CNum := '' ;
     for i := 1 to length(CBuf) do begin

         { If character is numeric ... add it to number string }
         if PositiveOnly then begin
            { Minus sign is treated as a number separator }
            if CBuf[i] in ['0'..'9', 'E', 'e', '.' ] then begin
               CNum := CNum + CBuf[i] ;
               EndOfNumber := False ;
               end
            else EndOfNumber := True ;
            end
         else begin
            { Positive or negative numbers }
            if CBuf[i] in ['0'..'9', 'E', 'e', '.', '-' ] then begin
               CNum := CNum + CBuf[i] ;
               EndOfNumber := False ;
               end
            else EndOfNumber := True ;
            end ;

         { If all characters are finished ... check number }
         if i = length(CBuf) then EndOfNumber := True ;

         if (EndOfNumber) and (Length(CNum) > 0)
            and (nValues <= High(Values)) then begin
              try
                 Values[nValues] := StrToFloat( CNum ) ;
                 CNum := '' ;
                 Inc(nValues) ;
              except
                    on E : EConvertError do CNum := '' ;
                    end ;
              end ;
         end ;
     { Return number of values extracted }
     Result := nValues ;
     end ;


procedure AppendFloat( var Dest : Array of ANSIChar; Keyword : string ; Value : Extended ) ;
{ --------------------------------------------------------
  Append a floating point parameter line
  'Keyword' = 'Value' on to end of the header text array
  --------------------------------------------------------}
begin
     CopyStringToArray( Dest, Keyword ) ;
     CopyStringToArray( Dest, format( '%.6g',[Value] ) ) ;
     CopyStringToArray( Dest, chr(13) + chr(10) ) ;
     end ;


procedure ReadFloat( const Source : Array of ANSIChar; Keyword : string ; var Value : Single ) ;
var
   Parameter : string ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := ExtractFloat( Parameter, 1. ) ;
     end ;



procedure AppendInt( var Dest : Array of ANSIChar; Keyword : string ; Value : LongInt ) ;
{ -------------------------------------------------------
  Append a long integer point parameter line
  'Keyword' = 'Value' on to end of the header text array
  ------------------------------------------------------ }
begin
     CopyStringToArray( Dest, Keyword ) ;
     CopyStringToArray( Dest, InttoStr( Value ) ) ;
     CopyStringToArray( Dest, chr(13) + chr(10) ) ;
     end ;


procedure ReadInt( const Source : Array of ANSIChar; Keyword : string ; var Value : LongInt ) ;
var
   Parameter : string ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := ExtractInt( Parameter ) ;
     end ;

{ Append a text string parameter line
  'Keyword' = 'Value' on to end of the header text array}

procedure AppendString( var Dest : Array of ANSIChar; Keyword, Value : string ) ;
begin
CopyStringToArray( Dest, Keyword ) ;
CopyStringToArray( Dest, Value ) ;
CopyStringToArray( Dest, chr(13) + chr(10) ) ;
end ;

procedure ReadString( const Source : Array of ANSIChar; Keyword : string ; var Value : string ) ;
var
   Parameter : string ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := Parameter  ;
     end ;

{ Append a boolean True/False parameter line
  'Keyword' = 'Value' on to end of the header text array}

procedure AppendLogical( var Dest : Array of ANSIChar; Keyword : string ; Value : Boolean ) ;
begin
     CopyStringToArray( Dest, Keyword ) ;
     if Value = True then CopyStringToArray( Dest, 'T' )
                     else CopyStringToArray( Dest, 'F' )  ;
     CopyStringToArray( Dest, chr(13) + chr(10) ) ;
     end ;

procedure ReadLogical( const Source : Array of ANSIChar; Keyword : string ; var Value : Boolean ) ;
var
   Parameter : string ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if pos('T',Parameter) > 0 then Value := True
                               else Value := False ;
     end ;

{ Copy a string variable to character array
  NOTE. array MUST have been filled with 0 characters before
        using the function }

procedure CopyStringToArray( var Dest : array of ANSIChar ; Source : string ) ;
var
   i,j : Integer ;
begin

     { Find end of character array }
     j := 0 ;
     while (Dest[j] <> chr(0)) and (j < High(Dest) ) do j := j + 1 ;

     if (j + length(Source)) < High(Dest) then
     begin
          for i := 1 to length(Source) do
          begin
               Dest[j] := ANSIChar(Source[i]) ;
               j := j + 1 ;
               end ;
          end
     else HeaderArrayFull := True ;

     end ;


procedure CopyArrayToString(
          var Dest : string ;
          var Source : array of ANSIChar ) ;
var
   i : Integer ;
begin
     Dest := '' ;
     for i := 0 to High(Source) do begin
         Dest := Dest + Source[i] ;
         end ;
     end ;


procedure FindParameter( const Source : array of ANSIChar ;
                               Keyword : string ;
                               var Parameter : string ) ;
var
s,k : integer ;
Found : boolean ;
begin

     { Search for the string 'keyword' within the
       array 'Source' }

     s := 0 ;
     k := 1 ;
     Found := False ;
     while (not Found) and (s < High(Source)) do
     begin
          if Source[s] = ANSIChar(Keyword[k]) then
          begin
               k := k + 1 ;
               if k > length(Keyword) then Found := True
               end
               else k := 1;
         s := s + 1;
         end ;

    { Copy parameter value into string 'Parameter'
      to be returned to calling routine }

    Parameter := '' ;
    if Found then
    begin
        while (Source[s] <> chr(13)) and (s < High(Source)) do
        begin
             Parameter := Parameter + Source[s] ;
             s := s + 1
             end ;
        end ;
    end ;


Function GetFromEditBox( var ed : TEdit ;
                         Default, Min, Max : Single ;
                         const FormatString,Units : string ;
                         Scale : single ) : Single ;
{ --------------------------------------------------------------------
  Get a number from an edit box, ensure that it is within valid limits,
  and update the box with the value used.
  ed ... Edit box to get text from
  Default ... value to use if box does not contain valid data
  Min ... Minimum valid value
  Max ... Maximum valid value
  FormatString ... format used to update box
  Units ... units of value
  Scale ... Factor for scaling display units
  --------------------------------------------------------------------}
var
   Value : single ;
begin
     Value := ExtractFloat( ed.text, Default*Scale ) / Scale ;
     if Value < Min then Value := Abs(Value) ;
     if Value < Min then Value := Min ;
     if Value > Max then Value := Max ;
     ed.text := format( FormatString, [Value*Scale] ) + ' ' + Units ;
     Result := Value ;
     end ;


procedure GetIntRangeFromEditBox( var ed : TEdit ; var Lo,Hi : LongInt ;
                                  Min,Max : LongInt ) ;
var
   LoValue,HiValue : single ;
begin
     {if ed.text = '' then ed.text := format( ' %d-%d', [Lo,Hi]) ;}
     GetRangeFromEditBox( ed, LoValue,HiValue, Min, Max,'%.0f-%.0f','' ) ;
     Lo := Trunc( LoValue ) ;
     Hi := Trunc( HiValue ) ;
     end ;


procedure GetRangeFromEditBox( const ed : TEdit ;
                               var LoValue,HiValue : Single ;
                               Min,Max : Single ;
                               const FormatString : String ;
                               const Units : String ) ;
var
   Values : Array[0..10] of Single ;
   Temp : Single ;
   nValues : Integer ;
begin
     LoValue := Min ;
     HiValue := Max ;
     nValues := ExtractListofFloats( ed.text, Values, True ) ;
     if nValues >=1 then LoValue := Values[0] ;
     if nValues >=2 then HiValue := Values[1] ;
     if LoValue > HiValue then begin
        Temp := LoValue ;
        LoValue := HiValue ;
        HiValue := Temp ;
        end ;
     ed.text := format( FormatString, [LoValue,HiValue] ) + ' ' + Units ;
     end ;


function Contains( const Target,Buf : string ) : boolean ;
{ Determine whether the sub-string in 'Target' is contained in 'Buf'
  ... return True if it is. }
begin
     if Pos( UpperCase(Target), UpperCase(Buf) ) > 0 then Contains := True
                                                     else Contains := False ;
     end ;












function ReplaceFileEnding( FileName,Ending : string ) : string ;
{ -------------------------------------------------
  Replace the '.???' ending of FileName with Ending
  -------------------------------------------------}
var
   iExt : LongInt ;
begin
     iExt := pos( '.', FileName ) ;
     if iExt > 0 then begin
        Delete( FileName, iExt, 4 ) ;
        Insert( Ending, FileName, iExt ) ;

        end
     else FileName := FileName + Ending ;
     ReplaceFileEnding := FileName ;
     end ;


function ExtractFileNameOnly( FilePath : string ) : string ;
{ -----------------------------------------------------
  Extract file name (without extension) from file path
  ----------------------------------------------------}
var
   FileName : string ;
   FileExt : string[6] ;
begin
     FileName := ExtractFileName(FilePath) ;
     FileExt := ExtractFileExt(FileName) ;
     Delete( FileName,Pos(FileExt,FileName),Length(FileExt) ) ;
     ExtractFileNameOnly := FileName ;
     end ;


procedure PrintStringGrid( const Table : TStringGrid ) ;
{ -----------------------------------------------
  Print the contents of a string grid spreadsheet
  -----------------------------------------------}
var
   CharWidth,CharHeight,ColHeight,Row,Col,w : Integer ;
   PageLeft,PageTop,PageBottom,Line,ColLeft,PageNum,LastPage : Integer ;
   ColWidth : Array[0..20] of Integer ;
begin

     Screen.Cursor := crHourglass ;

     { Set print font and size }
     Printer.Canvas.font.name := Settings.Plot.FontName ;
     Printer.Canvas.font.Size := 10 ;

     CharWidth := Printer.canvas.TextWidth('X') ;
     CharHeight := Printer.canvas.TextHeight('X') ;
     PageTop := CharHeight*5 ;
     PageBottom := printer.PageHeight - PageTop ;
     PageLeft := CharWidth*8 ;

     Printer.BeginDoc ;

     { Calculate column widths of table}
     for col := 0 to Table.ColCount-1 do begin
         ColWidth[Col] := 0 ;
         for row := 0 to Table.RowCount-1 do begin
             w := Printer.canvas.TextWidth(Table.cells[Col,Row]) ;
             if ColWidth[Col] < w then ColWidth[Col] := w ;
             end ;
         end ;
     for col := 0 to Table.ColCount-1 do ColWidth[Col] := ColWidth[Col] +
                                           2*CharWidth ;

     ColHeight := (12*Printer.canvas.TextHeight(Table.cells[0,0])) div 10 ;

     { Calculate number of pages to be printed }
     LastPage := 0 ;
     PageNum := 1 ;
     Line := PageTop + ColHeight*3 ;
     for row := 0 to Table.RowCount-1 do begin
         if LastPage <> PageNum then begin
            Line := PageTop + ColHeight*3 ;
            LastPage := PageNum ;
            end ;
         Line := Line + ColHeight ;
         if Line > PageBottom then Inc(PageNum) ;
         end ;

     { Print table
       ===========}

     PageNum := -1 ;
     for row := 0 to Table.RowCount-1 do begin
         {Print header lines on each new page }
         if Printer.PageNumber <> PageNum then begin
            PageNum := Printer.PageNumber ;
            Line := PageTop ;
            printer.canvas.textout(PageLeft,Line, 'File ... ' + CdrfH.FileName
                                   + format(' ( Page %d of %d )',
                                            [PageNum,LastPage])) ;
            Line := Line + ColHeight ;
            printer.canvas.textout(PageLeft,Line, CDRfH.IdentLine) ;
            Line := Line + ColHeight*2 ;
            //NewPage := False ;
            end ;

         { Print row }
         ColLeft := PageLeft ;
         Printer.Canvas.Pen.Width := 1 ;
         for col := 0 to Table.ColCount-1 do begin
             printer.canvas.rectangle( ColLeft,Line,ColLeft+ColWidth[Col],
                                       Line+ColHeight ) ;
             printer.canvas.textout( ColLeft + CharWidth,
                                     Line + CharHeight div 10,
                                     Table.cells[Col,Row] ) ;
             ColLeft := ColLeft + ColWidth[Col] ;
             end ;

         { New page when line crosses bottom margin }
         Line := Line + ColHeight ;
         if Line > PageBottom then Printer.NewPage ;

         end ;

     Printer.EndDoc ;

     Screen.Cursor := crDefault ;

     end ;

procedure CopyStringGrid(
          const Table : TStringGrid ;
          UseSelection : Boolean ) ;
{ ---------------------------------------------------
  Copy the contents of a string grid to the clipboard
  --------------------------------------------------- }
var
   Row,Row0,Row1,Col,Col0,Col1,BufSize : Integer ;
   CopyBuf : PChar ;
begin

     if  ((Table.Selection.Bottom - Table.Selection.Top) > 1) or
         ((Table.Selection.Right - Table.Selection.Left) > 1) then
         UseSelection := True ;

     if UseSelection then begin
        Row0 := Table.Selection.Top ;
        Row1 := Table.Selection.Bottom ;
        Col0 := Table.Selection.Left ;
        Col1 := Table.Selection.Right ;
        end
     else begin
        Row0 := 0 ;
        Row1 := Table.RowCount-1 ;
        Col0 := 0 ;
        Col1 := Table.ColCount-1 ;
        end ;

     // Determine size of and allocate string buffer
     BufSize := 1 ;
     for Row := Row0 to Row1 do
         for Col := Col0 to Col1 do
             BufSize := BufSize + Length(Table.Cells[Col,Row]) + 2 ;
     CopyBuf := StrAlloc( BufSize ) ;

     // Open clipboard preventing others acceessing it
     Clipboard.Open ;

     try

       // Copy table into buffer
       StrCopy(CopyBuf,PChar('')) ;
       for Row := Row0 to Row1 do begin
           for Col := Col0 to Col1 do begin
               if Col < Col1 then StrCat(CopyBuf,PChar(Table.Cells[Col,Row]+#9))
                             else StrCat(CopyBuf,PChar(Table.Cells[Col,Row]+#13#10)) ;
               end ;
           end ;

       // Copy string buffer to clipboard
       ClipBoard.SetTextBuf( CopyBuf ) ;

     finally

       // Release clipboard
       Clipboard.Close ;
       { Dispose of buffers }
       StrDispose( CopyBuf ) ;
       end ;

     end ;


procedure PrintHeaderAndFooter ;
{ -----------------------------------------------------
  Printer standard header and footer for a printed page
  -----------------------------------------------------}
var
   KeepSize,xPix,yPix,LineHeight : Integer ;
begin

     { File name and title always in 12 point }
     KeepSize := Printer.Canvas.font.size ;

     Printer.Canvas.font.size := 12 ;
     LineHeight := (Printer.Canvas.TextHeight('X')*12) div 10 ;

     { Print file name }
     xPix := Printer.PageWidth div 10 ;
     yPix := Printer.PageHeight div 60 ;
     Printer.Canvas.TextOut(xPix,yPix, 'File ... ' + CdrFH.FileName ) ;

     { Print ident line }
     yPix := yPix + LineHeight ;
     Printer.Canvas.TextOut( xPix, yPix, CdrFH.IdentLine ) ;

     Printer.Canvas.font.size := KeepSize ;

     end ;


procedure PrintPageTitle(
          Canvas : TCanvas ;
          EqnType : TEqnType ;
          const Results : TStringGrid ;
          var YEndOfText : Integer
          ) ;
{ -----------------------------------------------------
  Print experiment identification and other information
  -----------------------------------------------------}
var
   xPix,yPix,LineHeight,Row : Integer ;
   OldFontName : String ;
   OldFontSize : Integer ;

begin
     { Save the current font settings }
     OldFontName := Canvas.Font.Name ;
     OldFontSize := Canvas.Font.Height ;

     { Select standard font name and size for text information }
     Canvas.Font.Name := 'Arial' ;
     Canvas.Font.Size := 10 ;

     //CharWidth := Canvas.TextWidth('X') ;
     LineHeight := (Canvas.TextHeight('X')*12) div 10 ;

     { Start printing a top-left of page }
     xPix := Printer.PageWidth div 10 ;
     yPix := Printer.PageHeight div 60 ;

     { File Name }
     Canvas.TextOut(xPix,yPix, 'File ... ' + CdrfH.FileName ) ;
     { Ident line }
     yPix := yPix + LineHeight ;
     Canvas.TextOut( xPix, yPix, CdrfH.IdentLine ) ;

     { If a curve has been fitted, print the best fit parameters }
     if EqnType <> None then begin
        for Row := 0 to Results.RowCount-1 do begin
            Canvas.TextOut( xPix, yPix, Results.Cells[0,Row] ) ;
            yPix := yPix + LineHeight ;
            end ;
        end ;

     { Return the vertical position of the bottom of the area used for text }
     YEndOfText := yPix + LineHeight ;

     { Restore the old font settings }
     Canvas.Font.Name := OldFontName ;
     Canvas.Font.Height := OldFontSize ;
     end ;


function PrinterPointsToPixels(
         PointSize : Integer
         ) : Integer ;
var
   PixelsPerInch : single ;
begin

     { Get height and width of page (in mm) and calculate
       the size of a pixel (in cm) }
     PixelsPerInch := GetDeviceCaps( printer.handle, LOGPIXELSX ) ;
     PrinterPointsToPixels := Trunc( (PointSize*PixelsPerInch) / 72. ) ;
     end ;


function PrinterCmToPixels(
         const Axis : string;
         cm : single
         ) : Integer ;
{ -------------------------------------------
  Convert from cm (on printer page) to pixels
  -------------------------------------------}
var
   PixelWidth,PixelHeight : single ;
begin
     { Get height and width of page (in mm) and calculate
       the size of a pixel (in cm) }
     if UpperCase(Axis) = 'H' then begin
        { Printer pixel width (mm) }
        PixelWidth := GetDeviceCaps( printer.handle, HORZSIZE ) ;
        Result := Trunc( ( 10. * cm * printer.pagewidth) / PixelWidth );
        end
     else begin
        { Printer pixel height (mm) }
        PixelHeight := GetDeviceCaps( printer.handle, VERTSIZE ) ;
        Result := Trunc( ( printer.pageheight * 10. * cm )/ PixelHeight ) ;
        end ;
     end ;



function GetChannelOffset( Chan, NumChannels : LongInt ) : Integer ;
begin
     Result := NumChannels - 1 - Chan ;
     end ;

end.

