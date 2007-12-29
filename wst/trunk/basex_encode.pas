{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
{$RANGECHECKS OFF}
unit basex_encode;

interface
uses
  SysUtils, wst_types;

type

  EBaseXException = class(Exception);
  EBase64Exception = class(EBaseXException);

  TBaseXOption = ( xoDecodeIgnoreIllegalChar );
  TBaseXOptions = set of TBaseXOption;

  function Base64Encode(const ALength : PtrInt; const AInBuffer) : string;overload;
  function Base64Encode(const AInBuffer : string) : string;overload;

  function Base64Decode(const AInBuffer : string; const AOptions : TBaseXOptions = [xoDecodeIgnoreIllegalChar]) : string;

resourcestring
  s_InvalidEncodedData = 'Invalid encoded data.';
  s_IllegalChar = 'Illegal character for that encoding : %s.';

implementation

const
  Base64_CHAR_TABLE : array[0..63] of char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  IM = 255; INVALID_MARKER = IM;
  Base64_CHAR_INDEX_TABLE : array[Byte] of Byte = (
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,62,IM,IM,IM,63,
    52,53,54,55,56,57,58,59,60,61,IM,IM,IM,00,IM,IM,
    IM,00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,
    15,16,17,18,19,20,21,22,23,24,25,IM,IM,IM,IM,IM,
    IM,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
    41,42,43,44,45,46,47,48,49,50,51,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,
    IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM,IM
  );
  Base64_CHAR_SET = ['A'..'Z','a'..'z','0'..'9','+','/'];

function Base64Encode(const ALength : PtrInt; const AInBuffer) : string;
var
  locBuffer : PByte;
  locCopied, locBlockCount, i, locAtualLen : PtrInt;
  locInQuantom : array[0..2] of Byte;
  locOutQuantom : array[0..3] of Char;
begin
  Result := '';
  if ( ALength > 0 ) then begin
    locBuffer := @AInBuffer;
    locBlockCount := ALength div 3;
    SetLength(Result,(locBlockCount + 1 ) * 4);
    locAtualLen := 0;
    for i := 1 to locBlockCount do begin
      Move(locBuffer^,locInQuantom[0],3);
      Inc(locBuffer,3);
      locOutQuantom[0] := Base64_CHAR_TABLE[( locInQuantom[0] shr 2 )];
      locOutQuantom[1] := Base64_CHAR_TABLE[( ( locInQuantom[0] and 3 ) shl 4 ) or ( locInQuantom[1] shr 4 )];
      locOutQuantom[2] := Base64_CHAR_TABLE[( ( locInQuantom[1] and 15 ) shl 2 ) or ( locInQuantom[2] shr 6 )];
      locOutQuantom[3] := Base64_CHAR_TABLE[( locInQuantom[2] and 63 )];
      Move(locOutQuantom[0],Result[locAtualLen + 1],4);
      Inc(locAtualLen,4);
    end;
    locCopied := ALength mod 3;
    if ( locCopied > 0 ) then begin
      case locCopied of
        1 :
          begin
            Move(locBuffer^,locInQuantom[0],1);
            locInQuantom[1] := 0;
            locOutQuantom[0] := Base64_CHAR_TABLE[( locInQuantom[0] shr 2 )];
            locOutQuantom[1] := Base64_CHAR_TABLE[( ( locInQuantom[0] and 3 ) shl 4 ) or ( locInQuantom[1] shr 4 )];
            locOutQuantom[2] := '=';
            locOutQuantom[3] := '=';
          end;
        2 :
          begin
            Move(locBuffer^,locInQuantom[0],2);
            locInQuantom[2] := 0;
            locOutQuantom[0] := Base64_CHAR_TABLE[( locInQuantom[0] shr 2 )];
            locOutQuantom[1] := Base64_CHAR_TABLE[( ( locInQuantom[0] and 3 ) shl 4 ) or ( locInQuantom[1] shr 4 )];
            locOutQuantom[2] := Base64_CHAR_TABLE[( ( locInQuantom[1] and 15 ) shl 2 ) or ( locInQuantom[2] shr 6 )];
            locOutQuantom[3] := '=';
          end;
      end;
      Move(locOutQuantom[0],Result[locAtualLen + 1],4);
      Inc(locAtualLen,4);
    end;
    SetLength(Result,locAtualLen);
  end;
end;

function Base64Encode(const AInBuffer : string) : string;
begin
  if ( Length(AInBuffer) = 0 ) then
    Result := ''
  else
    Result := Base64Encode(Length(AInBuffer),AInBuffer[1]);
end;

function Base64Decode(const AInBuffer : string; const AOptions : TBaseXOptions) : string;
var
  locBuffer : PByte;
  locInLen, locInIndex, i, locPadded : PtrInt;
  locOutQuantom : array[0..2] of Byte;
  locInQuantom : array[0..3] of Byte;
  ok : Boolean;
  locAtualLen : PtrInt;
  locInValue : Byte;
  locFailOnIllegalChar : Boolean;
begin
  if ( AInBuffer = '' ) then begin
    Result := '';
  end else begin
    locInIndex := 0;
    locAtualLen := 0;
    locPadded := 0;
    locInLen := Length(AInBuffer);
    SetLength(Result,locInLen);
    locBuffer := @(AInBuffer[1]);
    locFailOnIllegalChar := not ( xoDecodeIgnoreIllegalChar in AOptions );
    while ( locInIndex < locInLen ) do begin
      for i := 0 to 3 do begin
        ok := False;
        while ( locInIndex <= locInLen ) do begin
          locInValue := Base64_CHAR_INDEX_TABLE[locBuffer^];
          Inc(locBuffer);
          Inc(locInIndex);          
          if ( locInValue <> INVALID_MARKER ) then begin
            locInQuantom[i] := locInValue;
            if ( locBuffer^ = Byte('=') ) then begin
              Inc(locPadded);
            end;
            ok := True;
            Break;
          end else begin
            if locFailOnIllegalChar then
              raise EBase64Exception.Create(s_InvalidEncodedData);
          end;
        end;
        if not ok then
          raise EBase64Exception.CreateFmt(s_IllegalChar,[Char(locBuffer^)]);
      end;
      locOutQuantom[0] := ( locInQuantom[0] shl 2 ) or ( locInQuantom[1] shr 4 );
      locOutQuantom[1] := ( locInQuantom[1] shl 4 ) or ( locInQuantom[2] shr 2 );
      locOutQuantom[2] := ( locInQuantom[2] shl 6 ) or ( locInQuantom[3] );
      Move(locOutQuantom[0],Result[locAtualLen + 1],3 - locPadded);
      Inc(locAtualLen,3 - locPadded);
    end;
    SetLength(Result,locAtualLen);
  end;
end;

end.
