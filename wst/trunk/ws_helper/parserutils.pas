{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit parserutils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  sNEW_LINE = {$ifndef Unix}#13#10{$else}#10{$endif};

  function IsStrEmpty(Const AStr : String):Boolean;
  function ExtractIdentifier(const AValue : string) : string ;
  
implementation

function IsStrEmpty(Const AStr : String):Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 );
end;

function ExtractIdentifier(const AValue : string) : string ;
var
  i, c : Integer;
  s : string;
begin
  Result := '';
  s := Trim(AValue);
  c := Length(s);
  if ( c > 0 ) then begin
    if not ( s[1] in ['A'..'Z', 'a'..'z', '_'] ) then begin
      Result := '_';
    end;
    for i := 1 to c do begin
      if ( s[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] ) then begin
        Result := Result + s[i];
      end else begin
        if ( Length(Result) > 0 ) and ( Result[Length(Result)] <> '_' ) then begin
          Result := Result + '_';
        end;
      end;
    end;
  end;
end;


end.

