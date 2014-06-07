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
  ,wst_types, dateutils;

const
  TestFilesPath = {$IFDEF WST_DELPHI}'.' +{$ENDIF WST_DELPHI}'.' + PathDelim + 'files' + PathDelim;
  EPSILON_DATE = 0.00004;
  
type

  { TWstBaseTest }

  TWstBaseTest = class(TTestCase)
  protected
    procedure CheckEquals(expected, actual: TByteDynArray; msg: string = ''; const AStrict : Boolean = True); overload;
{$IFDEF FPC}
    procedure CheckEquals(expected, actual: Int64; msg: string = ''; const AStrict : Boolean = True); overload;
    procedure CheckEquals(expected, actual: QWord; msg: string = ''; const AStrict : Boolean = True); overload;
    procedure CheckEquals(expected, actual: Currency; msg: string = ''); overload;
{$ENDIF FPC}
{$IFDEF WST_DELPHI}
    procedure CheckEquals(expected, actual: Word; msg: string = ''); overload;
    procedure CheckEquals(expected, actual: Byte; msg: string = ''); overload;
{$ENDIF WST_DELPHI}
  end;

  function CompareNodes(const A,B : TDOMNode) : Boolean;overload;
  function wstExpandLocalFileName(const AFileName : string) : string;
  function DumpMemory(AMem : Pointer; const ALength : PtrInt) : ansistring;
  function StringToByteArray(const AValue : TBinaryString) : TByteDynArray;

  function RandomRange(const AFrom, ATo : Integer) : Integer ;overload;
  function RandomRange(const AFrom, ATo : Int64) : Int64 ; overload;

implementation

//{$IFDEF FPC}
 // {$IF not Defined(RandomRange)}
    function RandomRange(const AFrom, ATo : Integer) : Integer ;
    var
      a : Integer;
    begin
      if ( AFrom <= ATo ) then
        a := AFrom
      else
        a := ATo;
      Result := a + Random(Abs(ATo - AFrom));
    end;
//  {$IFEND}
//{$ENDIF}

function RandomRange(const AFrom, ATo : Int64) : Int64 ;
var
  a : Int64;
begin
  if ( AFrom <= ATo ) then
    a := AFrom
  else
    a := ATo;
  Result := a + Random(Abs(ATo - AFrom));
end;

function StringToByteArray(const AValue : TBinaryString) : TByteDynArray;
begin
  SetLength(Result,Length(AValue));
  Move(Pointer(AValue)^,Pointer(Result)^,Length(Result));
end;

function wstExpandLocalFileName(const AFileName : string) : string;
begin
  Result := ExtractFilePath(ParamStr(0)) + AFileName;
end;

function DumpMemory(AMem : Pointer; const ALength : PtrInt) : ansistring;
var
  i : PtrInt;
begin
  Result := '';
  for i := 0 to Pred(ALength) do begin
    Result := Result + '[' + IntToStr(Ord(PAnsiChar(AMem)^)) + ']';
    Inc(PAnsiChar(AMem));
   end;
end;

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

{ TWstBaseTest }

{$IFDEF FPC}
procedure TWstBaseTest.CheckEquals(expected, actual: Int64; msg: string;
  const AStrict: Boolean);
begin
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg{$IFDEF WST_DELPHI}, CallerAddr{$ENDIF WST_DELPHI});
end;

procedure TWstBaseTest.CheckEquals(expected, actual: QWord; msg: string;
  const AStrict: Boolean);
begin
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg{$IFDEF WST_DELPHI}, CallerAddr{$ENDIF WST_DELPHI});
end;

procedure TWstBaseTest.CheckEquals(expected, actual : Currency; msg : string);
begin
  if (expected <> actual) then
    FailNotEquals(CurrToStr(expected), CurrToStr(actual), msg{$IFDEF WST_DELPHI}, CallerAddr{$ENDIF WST_DELPHI});
end;
{$ENDIF FPC}

procedure TWstBaseTest.CheckEquals(expected, actual: TByteDynArray;
  msg: string; const AStrict: Boolean
);
begin
  if ( expected = nil ) then begin
    Check(actual = nil, msg);
  end else begin
    CheckEquals(Length(expected),Length(actual),msg);
    if ( Length(expected) > 0 ) then
      Check(CompareMem(Pointer(expected), Pointer(actual),Length(expected)),msg);
  end;
end;

{$IFDEF WST_DELPHI}
procedure TWstBaseTest.CheckEquals(expected, actual: Word; msg: string);
begin
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg, CallerAddr);
end;

procedure TWstBaseTest.CheckEquals(expected, actual: Byte; msg: string);
begin
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg,CallerAddr);
end;
{$ENDIF WST_DELPHI}

end.
