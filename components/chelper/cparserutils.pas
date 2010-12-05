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
unit cparserutils;

interface

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

uses
  cparsertypes;

// is function declared, i.e. int f()
function isFunc(name: TNamePart): Boolean;

// probably an untyped function: fn ().
// the name of the function has been consumed by TYPE parsing, so ommited!
// so TNamepart doesn't contain any children
function isUnnamedFunc(name: TNamepart): Boolean;

// is pointer to a function declared, i.e. int (*f)()
function isPtrToFunc(name: TNamePart): Boolean;

// is function declared, returning a pointer to a function, i.e. int (* (f)(int i) )()
// pascal variant of this case:
// type
//   TRetFunc = function : Integer;
// function f(i: Integer): TRetFunc; // body or extern modifier must be present!!!
function isFuncRetFuncPtr(name: TNamePart): Boolean;

// is pointer to a function declared, returning a pointer to a function, i.e.: int (*(*f)(int i))()
// pascal variant of this case:
// type
//   TRetFunc = function : Integer;
// var
//   f : function (i: Integer): TRetFunc;
function isPtrToFuncRetFuncPtr(name: TNamePart): Boolean;

function GetFuncParam(name: TNamePart): TNamePart;

// is array variable:
//   int a[10], *a[10] (array of 10 integers, or array of 10 pointers to integer)
function isArray(name: TNamePart): Boolean;

function GetArrayPart(name: TNamePart): TNamePart;

// returns the variable/function name from the struct
function GetIdFromPart(name: TNamePart): AnsiString;

function GetIdPart(name: TNamePart): TNamePart;

function isNamePartPtrToFunc(part: TNamePart): Boolean; inline;

function isAnyBlock(part: TNamePart): Boolean;


type
  { TLineBreaker }

  TLineInfo = record
    linestart : Integer;
    lineend   : Integer;
  end;

  TLineBreaker = class(TObject)
  private
    fLines      : array of TLineInfo;
    flineCount  : Integer;
    procedure AddLine(const linestart, lineend: Integer);
  public
    procedure SetText(const AText: AnsiString);
    function LineNumber(Offset: Integer): Integer;
  end;

implementation

function isNamePartPtrToFunc(part: TNamePart): Boolean; inline;
begin
  Result:=Assigned(part) and (part.Kind=nk_Ref) and Assigned(part.owner) and (part.owner.kind=nk_Func);
end;

function isAnyBlock(part: TNamePart): Boolean;
begin
  Result:=Assigned(part) and ((part.Kind=nk_Block) or isAnyBlock(part.child));
end;

function isPtrToFunc(name: TNamePart): Boolean;
begin
  Result := Assigned(name) and (name.Kind=nk_Func) and Assigned(name.child) and
            (name.child.Kind=nk_Ref) and Assigned(name.child.child) and
            (name.child.child.Kind=nk_Ident);
end;

function SkipRefPart(name: TNamePart): TNamePart;
begin
  if Assigned(name) then begin
    if name.Kind=nk_Ref then Result:=name.child
    else Result:=name;
  end else
    Result:=nil;
end;

function isFunc(name: TNamePart): Boolean;
begin
  name:=SkipRefPart(name);
  Result:=Assigned(name) and (name.Kind=nk_Func) and Assigned(name.child) and (name.child.Kind=nk_Ident)
end;

function isUnnamedFunc(name: TNamepart): Boolean;
begin
  Result:=Assigned(name) and not Assigned(name.child) and (name.Kind=nk_Func);
end;

function isRetFuncPtr(name: TNamePart): Boolean;
begin
  Result:=Assigned(name) and Assigned(name.child) and
          (name.Kind=nk_Func) and (name.child.Kind=nk_Ref);
end;

function GetFuncParam(name:TNamePart):TNamePart;
begin
  while Assigned(name) and (name.Kind<>nk_Func) do name:=name.child;
  Result:=name;
end;

function isArray(name: TNamePart): Boolean;
begin
  Result:=(name.Kind=nk_Array)
          or (Assigned(name.child)
              and (name.child.Kind=nk_Array)
              and (name.Kind=nk_Ref));
end;

function isFuncRetFuncPtr(name: TNamePart): Boolean;
var
  p : TNamePart;
begin
  Result:=isRetFuncPtr(name);
  if Result then begin
    p:=name.child.child;
    Result:=Assigned(p) and Assigned(p.child)
            and (p.Kind=nk_Func)
            and (p.child.Kind=nk_Ident)
  end;
end;

function isPtrToFuncRetFuncPtr(name: TNamePart): Boolean;
var
  p : TNamePart;
begin
  Result:=isRetFuncPtr(name);
  if Result then begin
    p:=name.child.child;
    Result:=Assigned(p) and Assigned(p.child) and Assigned(p.child.child)
            and (p.Kind=nk_Func) and (p.child.Kind=nk_Ref)
            and (p.child.child.Kind=nk_Ident);
  end;
end;

function GetArrayPart(name:TNamePart):TNamePart;
begin
  if name.Kind=nk_Array then
    Result:=name
  else if (name.Kind=nk_Ref) and (Assigned(name.child)) and (name.child.Kind=nk_array) then
    Result:=name.child
  else
    Result:=nil;
end;

function GetIdFromPart(name: TNamePart): AnsiString;
begin
  while Assigned(name) and (name.Kind<>nk_Ident) do
    name:=name.child;
  if Assigned(name) then Result:=name.Id
  else Result:='';
end;

function GetIdPart(name: TNamePart): TNamePart;
begin
  Result:=nil;
  while Assigned(name) and (name.Kind<>nk_Ident) do
    name:=name.child;
  Result:=name;
end;

{ TLineBreaker }

procedure TLineBreaker.AddLine(const linestart,lineend:Integer);
begin
  if flineCount=length(fLines) then begin
    if fLineCount=0 then SetLength(fLines, 4)
    else SetLength(fLines, fLineCount*2)
  end;
  fLines[fLineCount].linestart:=linestart;
  fLines[fLineCount].lineend:=lineend;
  inc(fLineCount);
end;

procedure TLineBreaker.SetText(const AText: AnsiString);
var
  i : Integer;
  j : Integer;
begin
  flineCount:=0;
  i:=1;
  j:=1;
  while i<=length(AText) do begin
    if (AText[i] in [#10, #13]) then begin
      inc(i);
      if (i<=length(AText)) and (AText[i] in [#10, #13]) and (AText[i-1]<>Atext[i]) then
        inc(i);
      AddLine(j, i-1);
      j:=i;
    end else
      inc(i);
  end;
  if j<>i-1 then AddLine(j, i-1);
end;

function TLineBreaker.LineNumber(Offset:Integer):Integer;
var
  i : Integer;
begin
  for i:=0 to flineCount-1 do
    if (Offset>=fLines[i].linestart) and (Offset<=flines[i].lineend) then begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

end.
