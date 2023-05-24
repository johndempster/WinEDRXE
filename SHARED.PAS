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


  function ExtractInt (
           CBuf : string
           ) : LongInt ;
  function VerifyInt(
           text : string ;
           LoLimit,HiLimit : LongInt
           ) : string ;
{  procedure AppendFloat(
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
            ) ;}

{           var Dest : array of ANSIChar ;
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
            ) ; }

  function GetChannelOffset( Chan, NumChannels : LongInt ) : Integer ;
  const
     MaxSingle = 1E38 ;

implementation

{uses plotlib ; }


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




function GetChannelOffset( Chan, NumChannels : LongInt ) : Integer ;
begin
     Result := NumChannels - 1 - Chan ;
     end ;

end.

