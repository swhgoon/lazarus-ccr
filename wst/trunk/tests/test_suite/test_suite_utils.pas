{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007, 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_suite_utils;

interface
uses
  Classes, SysUtils
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry, DOM, XmlRead, wst_fpc_xml
{$ELSE}
  ,TestFrameWork, xmldom, wst_delphi_xml
{$ENDIF}
  ,wst_types;

  function CompareNodes(const A,B : TDOMNode) : Boolean;overload;

implementation

function CompareNodes(const A,B : TDOMNode) : Boolean;overload;
var
  ca, cb : TDOMNode;
  i : PtrInt;
begin
  if ( A = nil ) and ( B = nil ) then begin
    Result := True;
  end else if ( A <> nil ) and ( B <> nil ) then begin
    Result := False;
    if ( A.NodeName = B.NodeName ) and
       ( A.NodeValue = B.NodeValue )
    then begin
      if ( ( A.FirstChild = nil ) and ( B.FirstChild = nil ) ) or
         ( ( A.FirstChild <> nil ) and ( B.FirstChild <> nil ) )
      then begin
        ca := a.FirstChild;
        cb := b.FirstChild;
        while ( ca <> nil ) do begin
          if not CompareNodes(ca,cb) then
            Exit;
          ca := ca.NextSibling;
          cb := cb.NextSibling;
        end;
        if ( ( A.Attributes = nil ) and ( B.Attributes = nil ) ) or
           ( ( A.Attributes <> nil ) and ( B.Attributes <> nil ) )
        then begin
          if ( A.Attributes <> nil ) then begin
            if ( A.Attributes.Length <> B.Attributes.Length ) then
              Exit;
            if ( A.Attributes.Length > 0 ) then begin
              for i := 0 to Pred(A.Attributes.Length) do begin
                if not CompareNodes(A.Attributes.Item[i],B.Attributes.GetNamedItem(A.Attributes.Item[i].NodeName)) then
                  Exit;
              end;
            end;
          end;
          Result := True;
        end;
      end;
    end;
  end else begin
    Result := False;
  end;
end;

end.
