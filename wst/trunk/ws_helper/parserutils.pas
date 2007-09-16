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
{$INCLUDE wst_global.inc}
unit parserutils;

interface

uses
  SysUtils, Classes
  {$IFNDEF FPC}, xmldom, wst_delphi_xml{$ELSE},DOM{$ENDIF}
  , cursor_intf, dom_cursors, xsd_consts
  ;

type
  TNotFoundAction = ( nfaNone, nfaRaiseException );

const
  sNEW_LINE = sLineBreak;

  function IsStrEmpty(Const AStr : String):Boolean;
  function ExtractIdentifier(const AValue : string) : string ;

  function IsReservedKeyWord(const AValue : string):Boolean ;

  procedure ExtractNameSpaceShortNamesNested(
          ANode         : TDOMNode;
          AResList      : TStrings;
    const ANameSpace    : WideString
  );
  function CreateQualifiedNameFilterStr(
    const AName        : WideString;
          APrefixList  : TStrings
  ) : string;
  function ExtractNameFromQName(const AQName : string):string ;
  procedure ExtractNameSpaceShortNames(
          AAttribCursor   : IObjectCursor;
          AResList        : TStrings;
    const ANameSpace      : WideString;
    const ANotFoundAction : TNotFoundAction;
    const AClearBefore    : Boolean;
    const AExceptionClass : ExceptClass
  );
  function AddNameSpace(const AValue: string; ANameSpaceList : TStrings): TStrings;
  procedure BuildNameSpaceList(AAttCursor : IObjectCursor; ANameSpaceList : TStrings);
  procedure ExplodeQName(const AQName : string; out ALocalName, ANameSpace : string) ;
  
  function wst_findCustomAttribute(
          AWsdlShortNames : TStrings;
          ANode      : TDOMNode;
    const AAttribute : string;
    out   AValue     : string
  ) : Boolean;
  function wst_findCustomAttributeXsd(
          AXsdShortNames : TStrings;
          ANode      : TDOMNode;
    const AAttribute : string;
    out   AValue     : string
  ) : Boolean;

implementation
uses StrUtils, rtti_filters;

const LANGAGE_TOKEN : array[0..107] of string = (
  'ABSTRACT', 'AND', 'ARRAY', 'AS', 'ASM',
  'BEGIN', 'BOOLEAN', 'BYTE',
  'CASE', 'CDECL', 'CHAR', 'CLASS', 'COMP', 'CONST', 'CONSTRUCTOR', 'CONTAINS', 'CURRENCY',
  'DEFAULT', 'DESTRUCTOR', 'DIV', 'DO', 'DOUBLE', 'DOWNTO', 'DYNAMIC',
  'END', 'EXPORT', 'EXPORTS', 'EXTERNAL',
  'FAR', 'FILE', 'FINALLY', 'FOR', 'FORWARD', 'FUNCTION', 'GOTO',
  'ELSE', 'EXCEPT', 'EXTENDED',
  'IF', 'IMPLEMENTATION', 'IMPLEMENTS', 'IN', 'INHERITED', 'INT64', 'INITIALIZATION',
    'INTEGER', 'INTERFACE', 'IS',
  'LABEL', 'LIBRARY', 'LOCAL', 'LONGINT', 'LONGWORD',
  'MOD', 'NEAR', 'NIL', 'NODEFAULT', 'NOT',
  'OBJECT', 'OF', 'OLEVARIANT', 'OR', 'OUT', 'OVERLOAD', 'OVERRIDE',
  'PACKAGE', 'PACKED', 'PASCAL', 'PCHAR', 'PRIVATE', 'PROCEDURE', 'PROGRAM', 'PUBLISHED',
  'RAISE', 'READ', 'REAL', 'RECORD', 'REGISTER', 'REINTRODUCE', 'REPEAT', 'REQUIRES', 'RESULT',
  'SAFECALL', 'SET', 'SHL', 'SHORTINT', 'SHR', 'SINGLE', 'SMALLINT', 'STDCALL', 'STORED',
  'THEN', 'TO', 'TRY', 'TYPE', 'UNIT', 'UNTIL', 'USES',
  'VAR', 'VARARGS', 'VARIANT', 'VIRTUAL', 'WHILE', 'WIDECHAR', 'WITH', 'WORD', 'WRITE', 'XOR'
);
const WST_RESERVED_TOKEN : array[0..1] of string = ( 'Item', 'Item' );
function IsReservedKeyWord(const AValue : string):Boolean ;
begin
  Result := AnsiMatchText(AValue,LANGAGE_TOKEN) or
            AnsiMatchText(AValue,WST_RESERVED_TOKEN);
end;

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

function ExtractNameFromQName(const AQName : string):string ;
var
  i : Integer;
begin
  Result := Trim(AQName);
  i := Pos(':',Result);
  if ( i > 0 ) then
    Result := Copy(Result,( i + 1 ), MaxInt);
end;

function CreateQualifiedNameFilterStr(
  const AName        : WideString;
        APrefixList  : TStrings
) : string;
var
  k : Integer;
  locStr : string;
  locWStr : WideString;
begin
  Result := '';
  if ( APrefixList.Count > 0 ) then begin
    for k := 0 to Pred(APrefixList.Count) do begin
      if IsStrEmpty(APrefixList[k]) then begin
        locWStr := ''
      end else begin
        locWStr := APrefixList[k] + ':';
      end;
      locWStr := locWStr + AName;
      locStr := s_NODE_NAME;
      Result := Result + ' or ' + locStr + ' = ' + QuotedStr(locWStr);
    end;
    if ( Length(Result) > 0 ) then begin
      Delete(Result,1,Length(' or'));
    end;
  end else begin
    Result := Format('%s = %s',[s_NODE_NAME,QuotedStr(AName)]);
  end;
end;

procedure ExtractNameSpaceShortNamesNested(
        ANode         : TDOMNode;
        AResList      : TStrings;
  const ANameSpace    : WideString
);
var
  nd : TDOMNode;
begin
  AResList.Clear();
  nd := ANode;
  while Assigned(nd) do begin
    if Assigned(nd.Attributes) and ( nd.Attributes.Length > 0 ) then begin
      ExtractNameSpaceShortNames(CreateAttributesCursor(nd,cetRttiNode),AResList,ANameSpace,nfaNone,False,nil);
    end;
    nd := nd.ParentNode;
  end;
end;

procedure ExtractNameSpaceShortNames(
        AAttribCursor   : IObjectCursor;
        AResList        : TStrings;
  const ANameSpace      : WideString;
  const ANotFoundAction : TNotFoundAction;
  const AClearBefore    : Boolean;
  const AExceptionClass : ExceptClass
);
var
  crs : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
  wStr : WideString;
  i : Integer;
  ec : ExceptClass;
begin
  if AClearBefore then begin
    AResList.Clear();
  end;
  AAttribCursor.Reset();
  crs := CreateCursorOn(AAttribCursor,ParseFilter(Format('%s=%s',[s_NODE_VALUE,QuotedStr(ANameSpace)]),TDOMNodeRttiExposer));
  crs.Reset();
  if crs.MoveNext() then begin
    repeat
      locObj := crs.GetCurrent() as TDOMNodeRttiExposer;
      wStr := Trim(locObj.NodeName);
      i := AnsiPos(s_xmlns + ':',wStr);
      if ( i > 0 ) then begin
        i := AnsiPos(':',wStr);
        AResList.Add(Copy(wStr,( i + 1 ), MaxInt));
      end else begin
        if ( AResList.IndexOf('') = -1 ) then
          AResList.Add('');
      end;
    until not crs.MoveNext();
  end else begin
    if ( ANotFoundAction = nfaRaiseException ) then begin
      if Assigned(AExceptionClass) then
        ec := AExceptionClass
      else
        ec := Exception;
      raise ec.CreateFmt('Namespace not found : "%s"',[ANameSpace]);
    end;
  end;
end;

function wst_findCustomAttribute(
        AWsdlShortNames : TStrings;
        ANode      : TDOMNode;
  const AAttribute : string;
  out   AValue     : string
) : Boolean;
var
  nd : TDOMNode;
  tmpCrs : IObjectCursor;
begin
  Result := False;
  tmpCrs := CreateCursorOn(
              CreateChildrenCursor(ANode,cetRttiNode),
              ParseFilter(CreateQualifiedNameFilterStr(s_document,AWsdlShortNames),TDOMNodeRttiExposer)
            );
  tmpCrs.Reset();
  if tmpCrs.MoveNext() then begin
    nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    if nd.HasChildNodes() then begin
      tmpCrs := CreateCursorOn(
                  CreateChildrenCursor(nd,cetRttiNode),
                  ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_customAttributes)]),TDOMNodeRttiExposer)
                );
      tmpCrs.Reset();
      if tmpCrs.MoveNext() then begin
        nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if ( nd.Attributes <> nil ) then begin
          nd := nd.Attributes.GetNamedItem(AAttribute);
          if Assigned(nd) then begin
            Result := True;
            AValue := nd.NodeValue;
          end;
        end;
      end;
    end;
  end;
end;

function wst_findCustomAttributeXsd(
        AXsdShortNames : TStrings;
        ANode      : TDOMNode;
  const AAttribute : string;
  out   AValue     : string
) : Boolean;
var
  nd : TDOMNode;
begin
  Result := False;
  if Assigned(ANode) and ( ANode.Attributes <> nil ) then begin
    nd := ANode.Attributes.GetNamedItem(AAttribute);
    if Assigned(nd) then begin
      Result := True;
      AValue := nd.NodeValue;
    end;
  end;
end;

procedure ExplodeQName(const AQName : string; out ALocalName, ANameSpace : string) ;
var
  i : PtrInt;
begin
  i := Pos(':',AQName);
  if ( i > 0 ) then begin
    ANameSpace := Copy(AQName,1,Pred(i));
    ALocalName := Copy(AQName,Succ(i),Length(AQName));
  end else begin
    ANameSpace := '';
    ALocalName := AQName;
  end;
end;

function AddNameSpace(const AValue: string; ANameSpaceList : TStrings): TStrings;
var
  i : PtrInt;
  s : string;
  ls : TStringList;
begin
  s := Trim(AValue);
  i := ANameSpaceList.IndexOf(s);
  if ( i < 0 ) then begin
    i := ANameSpaceList.Add(s);
    ls := TStringList.Create();
    ANameSpaceList.Objects[i] := ls;
    ls.Duplicates := dupIgnore;
    ls.Sorted := True;
    Result := ls;
  end else begin
    Result := ANameSpaceList.Objects[i] as TStrings;
  end;
end;

procedure BuildNameSpaceList(AAttCursor : IObjectCursor; ANameSpaceList : TStrings);
var
  locObj : TDOMNodeRttiExposer;
  locNameSpace, locNameSpaceShort : string;
  tmpXmlNs : string;
  found : Boolean;
begin
  if Assigned(AAttCursor) then begin
    tmpXmlNs := s_xmlns + ':';
    AAttCursor.Reset();
    while AAttCursor.MoveNext() do begin
      found := False;
      locObj := AAttCursor.GetCurrent() as TDOMNodeRttiExposer;
      if AnsiSameText(s_xmlns,locObj.NodeName) then begin
        found := True;
        locNameSpace := locObj.NodeValue;
        locNameSpaceShort := '';
      end else if AnsiStartsText(tmpXmlNs,locObj.NodeName) then begin
        found := True;
        locNameSpace := locObj.NodeValue;
        locNameSpaceShort := locObj.NodeName;
        locNameSpaceShort := Copy(locNameSpaceShort,Pos(':',locNameSpaceShort) + 1, Length(locNameSpaceShort));
      end;
      if found then
        AddNameSpace(locNameSpace,ANameSpaceList).Add(locNameSpaceShort);
    end;
  end;
end;


end.
