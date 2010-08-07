{ The unit is part of Lazarus Chelper package

  Copyright (C) 2010 Dmitry Boyarintsev skalogryz dot lists at gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit TextParsingUtils;

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

interface

type
  TCharSet = set of Char;

const
  EoLnChars      = [#10,#13];
  SpaceChars     = [#32,#9];
  InvsChars      = SpaceChars;
  WhiteSpaceChars = SpaceChars;
  SpaceEolnChars = EoLnChars+SpaceChars;
  NumericChars   = ['0'..'9'];
  AlphabetChars  = ['a'..'z','A'..'Z'];
  AlphaNumChars  = AlphabetChars+NumericChars;

function ScanWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
function ScanTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
function SkipToEoln(const s: AnsiString; var index: Integer): AnsiString;

// returns #10, #13, #10#13 or #13#10, if s[index] is end-of-line sequence
// otherwise returns empty string
function EolnStr(const s: AnsiString; index: Integer): String;

function IsSubStr(const sbs, s: AnsiString; index: Integer): Boolean;

// todo: not used?
function SkipCommentBlock(const s: AnsiString; var index: Integer; const closecmt: AnsiString): AnsiString;

function SkipLine(const s: AnsiString; var index: Integer): AnsiString;

implementation

function ScanWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
var
  i : Integer;
begin
  Result := '';
  if (index <= 0) or (index > length(s)) then Exit;
  for i := index to length(s) do
    if not (s[i] in ch) then begin
      if i = index then Result := ''
      else Result := Copy(s, index, i - index);
      index := i;
      Exit;
    end;
  Result := Copy(s, index, length(s) - index + 1);
  index := length(s) + 1;
end;

function ScanTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
var
  i : Integer;
begin
  Result := '';
  if (index <= 0) or (index > length(s)) then Exit;
  for i := index to length(s) do
    if (s[i] in ch) then begin
      if i = index then Result := ''
      else Result := Copy(s, index, i - index);
      index := i;
      Exit;
    end;
  Result := Copy(s, index, length(s) - index + 1);
  index := length(s) + 1;
end;

function EolnStr(const s: AnsiString; index: Integer): String;
begin
  if (index<=0) or (index>length(s)) or (not (s[index] in EoLnChars)) then
    Result:=''
  else begin
    if (index<length(s)) and (s[index+1] in EolnChars) and (s[index]<>s[index+1]) then
      Result:=Copy(s, index, 2)
    else
      Result:=s[index];
  end;
end;

function SkipToEoln(const s: AnsiString; var index: Integer): AnsiString;
begin
  Result := ScanTo(s, index, EoLnChars);
end;

function IsSubStr(const sbs, s: AnsiString; index: Integer): Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := false;
  if (sbs = '') or (length(sbs) > length(s) - index) then Exit;
  j := index;
  for i := 1 to length(sbs) do begin
    if sbs[i] <> s[j] then Exit;
    inc(j);
  end;
  Result := true;
end;

function SkipCommentBlock(const s: AnsiString; var index: Integer; const closecmt: AnsiString): AnsiString;
begin
  Result := '';
  if closecmt = '' then begin
    index := length(s) + 1;
    Exit;
  end;
  while index <= length(s) do begin
    Result := Result + ScanTo(s, index, [closecmt[1]]+EoLnChars);
    //if (index<=length(s)) and (s in EoLnChars(

    if IsSubStr(closecmt, s, index) then begin
      inc(index, length(closecmt));
      Exit;
    end else begin
      Result := Result + s[index];
      inc(index);
    end;
  end;
end;

function SkipLine(const s: AnsiString; var index: Integer): AnsiString;
begin
  Result:=ScanTo(s, index, EoLnChars);
  if (index<length(s)) and (s[index+1] in EoLnChars) and (s[index]<>s[index+1]) then
    inc(index);
  inc(index);
end;

end.

