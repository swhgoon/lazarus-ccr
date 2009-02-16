unit ObjCToPas;
{ * This file is part of ObjCParser tool 
  * Copyright (C) 2008-2009 by Dmitry Boyarintsev under the GNU LGPL
  * license version 2.0 or 2.1.  You should have received a copy of the
  * LGPL license along with at http://www.gnu.org/                                              
}
// the unit contains (should contain) ObjC to Pascal convertion utility routines
// todo: move all ObjCParserUtils functions here.

interface

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

uses
  ObjCParserTypes;

const
  ObjCDefaultParamDelim = '_';

function ObjCToPasMethodName(mtd: TClassMethodDef; CutLastDelims: Boolean = false; ParamDelim: AnsiChar = ObjCDefaultParamDelim): AnsiString;

implementation

function ObjCToPasMethodName(mtd: TClassMethodDef; CutLastDelims: Boolean; ParamDelim: AnsiChar): AnsiString;
var
  i   : Integer;
  obj : TObject;
begin
  Result := mtd._Name;
  for i := 0 to mtd.Items.Count - 1 do begin
    obj := mtd.Items[i];
    if not Assigned(obj) then Continue;
    if obj is TParamDescr then begin
      Result := Result + TParamDescr(obj)._Descr;
    end else if obj is TObjCParameterDef then
      Result := Result + ParamDelim;
  end;

  if CutLastDelims then begin
    i := length(Result);
    while (i > 0) and (Result[i] = ParamDelim) do dec(i);
    Result := Copy(Result, 1, i);
  end;
end;

end.
