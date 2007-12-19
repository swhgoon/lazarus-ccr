{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit testformatter_unit;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry,
{$ENDIF}
{$IFNDEF FPC}
  TestFrameWork, ActiveX,
{$ENDIF}
  TypInfo,
  base_service_intf, wst_types, server_service_intf, service_intf;

type

  TTestEnum = ( teOne, teTwo, teThree, teFour );

  TArrayOfStringRemotableSample = class(TArrayOfStringRemotable)
  end;

  { TClass_A }

  TClass_A = class(TBaseComplexRemotable)
  private
    FVal_32S: LongInt;
    FVal_Bool: Boolean;
    FVal_Enum: TTestEnum;
    FVal_String: string;
  Published
    property Val_32S : LongInt Read FVal_32S Write FVal_32S;
    property Val_Enum : TTestEnum Read FVal_Enum Write FVal_Enum;
    property Val_Bool : Boolean Read FVal_Bool Write FVal_Bool;
    property Val_String : string Read FVal_String Write FVal_String;
  End;

  { TClass_B }

  TClass_B = class(TBaseComplexRemotable)
  private
    FNonStored: Integer;
    FObjProp: TClass_A;
    FVal_32S: LongInt;
    FVal_Bool: Boolean;
    FVal_Enum: TTestEnum;
    FVal_String: string;
    procedure SetObjProp(const AValue: TClass_A);
  Public
    constructor Create();override;
    destructor Destroy();override;
  Published
    property Val_32S : LongInt Read FVal_32S Write FVal_32S;
    property Val_Enum : TTestEnum Read FVal_Enum Write FVal_Enum;
    property Val_Bool : Boolean Read FVal_Bool Write FVal_Bool;
    property Val_String : string Read FVal_String Write FVal_String;
    property ObjProp : TClass_A Read FObjProp Write SetObjProp stored True;
    property NonStored : Integer Read FNonStored Write FNonStored stored false;
  End;
  
  { TClass_Int }

  TClass_Int = class(TBaseComplexRemotable)
  private
    FVal_16S: SmallInt;
    FVal_16U: Word;
    FVal_32S: LongInt;
    FVal_32U: LongWord;
    FVal_64S: Int64;
    FVal_64U: QWord;
    FVal_8S: ShortInt;
    FVal_8U: Byte;
  Published
    property Val_8U : Byte Read FVal_8U Write FVal_8U;
      property Val_8S : ShortInt Read FVal_8S Write FVal_8S;
    property Val_16U : Word Read FVal_16U Write FVal_16U;
      property Val_16S : SmallInt Read FVal_16S Write FVal_16S;
    property Val_32U : LongWord Read FVal_32U Write FVal_32U;
      property Val_32S : LongInt Read FVal_32S Write FVal_32S;
    property Val_64U : QWord Read FVal_64U Write FVal_64U;
      property Val_64S : Int64 Read FVal_64S Write FVal_64S;
  End;

  T_ComplexInt64SContent = class(TComplexInt64SContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexInt64UContent = class(TComplexInt64UContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  { T_ComplexInt32SContent }

  T_ComplexInt32SContent = class(TComplexInt32SContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexInt32UContent = class(TComplexInt32UContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  T_ComplexInt16SContent = class(TComplexInt16SContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  T_ComplexInt16UContent = class(TComplexInt16UContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexInt8SContent = class(TComplexInt8SContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexInt8UContent = class(TComplexInt8UContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  T_ComplexFloatExtendedContent = class(TComplexFloatExtendedContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexFloatDoubleContent = class(TComplexFloatDoubleContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexStringContent = class(TComplexStringContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  { TClass_CplxSimpleContent }

  TClass_CplxSimpleContent = class(TBaseComplexRemotable)
  private
    FElt_Exemple: string;
    FVal_CplxDouble: T_ComplexFloatDoubleContent;
    FVal_CplxInt16S: T_ComplexInt16SContent;
    FVal_CplxInt16U: T_ComplexInt16UContent;
    FVal_CplxInt32S: T_ComplexInt32SContent;
    FVal_CplxInt32U: T_ComplexInt32UContent;
    FVal_CplxExtended : T_ComplexFloatExtendedContent;
    FVal_CplxInt64S: T_ComplexInt64SContent;
    FVal_CplxInt64U: T_ComplexInt64UContent;
    FVal_CplxInt8S: T_ComplexInt8SContent;
    FVal_CplxInt8U: T_ComplexInt8UContent;
    FVal_CplxString: T_ComplexStringContent;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Val_CplxInt64S : T_ComplexInt64SContent read FVal_CplxInt64S write FVal_CplxInt64S;
    property Val_CplxInt64U : T_ComplexInt64UContent read FVal_CplxInt64U write FVal_CplxInt64U;
    
    property Val_CplxInt32S : T_ComplexInt32SContent read FVal_CplxInt32S write FVal_CplxInt32S;
    property Val_CplxInt32U : T_ComplexInt32UContent read FVal_CplxInt32U write FVal_CplxInt32U;

    property Val_CplxInt16U : T_ComplexInt16UContent read FVal_CplxInt16U write FVal_CplxInt16U;
    property Val_CplxInt16S : T_ComplexInt16SContent read FVal_CplxInt16S write FVal_CplxInt16S;
    
    property Val_CplxInt8U : T_ComplexInt8UContent read FVal_CplxInt8U write FVal_CplxInt8U;
    property Val_CplxInt8S : T_ComplexInt8SContent read FVal_CplxInt8S write FVal_CplxInt8S;

    property Val_CplxExtended : T_ComplexFloatExtendedContent read FVal_CplxExtended write FVal_CplxExtended;
    property Val_CplxDouble : T_ComplexFloatDoubleContent read FVal_CplxDouble write FVal_CplxDouble;
    property Val_CplxString : T_ComplexStringContent read FVal_CplxString write FVal_CplxString;

    property Elt_Exemple : string read FElt_Exemple write FElt_Exemple;
  end;
  
  { TClass_Enum }

  TClass_Enum = class(TBaseComplexRemotable)
  private
    FVal_Bool: Boolean;
    FVal_Enum: TTestEnum;
    FVal_String: string;
  Published
    property Val_Enum : TTestEnum Read FVal_Enum Write FVal_Enum;
    property Val_Bool : Boolean Read FVal_Bool Write FVal_Bool;
    property Val_String : string Read FVal_String Write FVal_String;
  End;

  { TClass_Float }

  TClass_Float = class(TBaseComplexRemotable)
  private
    FVal_Currency: Currency;
    FVal_Double: Double;
    FVal_Extended: Extended;
    FVal_Single: Single;
  Published
    property Val_Single : Single Read FVal_Single Write FVal_Single;
    property Val_Double : Double Read FVal_Double Write FVal_Double;
    property Val_Extended : Extended Read FVal_Extended Write FVal_Extended;
    property Val_Currency : Currency Read FVal_Currency Write FVal_Currency;
  End;

  TEmbeddedArrayOfStringRemotable = class(TArrayOfStringRemotable);

  TTestSmallRecord = record
    fieldSmallint : Smallint;
    fieldWord : Word;
    fieldString : string;
  end;
  
  TTestRecord = record
    fieldByte : Byte;
    fieldShortInt : ShortInt;
    fieldSmallint : Smallint;
    fieldWord : Word;
    fieldInteger : Integer;
    fieldLongWord : LongWord;
    fieldInt64 : Int64;
    fieldQWord : QWord;
    fieldComp : Comp;
    fieldSingle : Single;
    fieldDouble : Double;
    fieldExtended : Extended;
    fieldCurrency : Currency;
    fieldBoolean : Boolean;
    fieldString : string;
    fieldRecord : TTestSmallRecord;
  end;
  
  { TTestFormatterSimpleType }

  TTestFormatterSimpleType= class(TTestCase)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;virtual;abstract;
    function Support_ComplextType_with_SimpleContent():Boolean;virtual;
    function Support_nil():Boolean;virtual;
  published
    procedure Test_Int_8;
    procedure Test_Int_8_ScopeData;
    procedure Test_Int_16;
    procedure Test_Int_32;
    procedure Test_Int_64;

    procedure Test_Single_4;
    procedure Test_Double_8;
    procedure Test_Currency_8;
    procedure Test_Extended_10;

    procedure Test_String;
    procedure Test_Bool;
    procedure Test_Enum;
  end;
  
  { TTestFormatter }

  TTestFormatter = class(TTestFormatterSimpleType)
  protected
    class function GetFormaterName() : string;virtual;abstract;
  published
    procedure Test_Int_WithClass;

    procedure Test_Float_WithClass;

    procedure Test_Enum_Bool_String_WithClass;

    procedure Test_CplxInt64SimpleContent_WithClass;
    procedure Test_CplxInt32SimpleContent_WithClass;
    procedure Test_CplxInt16SimpleContent_WithClass;
    procedure Test_CplxInt8SimpleContent_WithClass;

    procedure Test_CplxFloatExtendedSimpleContent_WithClass;
    procedure Test_CplxStringSimpleContent_WithClass;

    procedure Test_Object();
    procedure Test_Object_Nil();
    procedure Test_StringArray();
    procedure Test_StringArray_Embedded();
    procedure Test_StringArrayZeroLength();
    procedure Test_BooleanArray();

    procedure Test_Int8UArray();
    procedure Test_Int8SArray();

    procedure Test_Int16SArray();
    procedure Test_Int16UArray();

    procedure Test_Int32UArray();
    procedure Test_Int32SArray();

    procedure Test_Int64SArray();
    procedure Test_Int64UArray();

    procedure Test_FloatSingleArray();
    procedure Test_FloatDoubleArray();
    procedure Test_FloatExtendedArray();
    procedure Test_FloatCurrencyArray();

    procedure  Test_ComplexInt32S();

    procedure Test_Record_simple();
    procedure Test_Record_nested();

    procedure test_GetScopeItemNames();
    procedure test_GetFormaterName();
  end;

  { TTestBinaryFormatter }

  TTestBinaryFormatter= class(TTestFormatter)
  protected
    class function GetFormaterName() : string;override;
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  published
    procedure test_WriteBuffer();
  end;

  { TTestBinaryFormatterAttributes }

  TTestBinaryFormatterAttributes= class(TTestFormatterSimpleType)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  { TTestSOAPFormatter }

  TTestSOAPFormatter= class(TTestFormatter)
  protected
    class function GetFormaterName() : string;override;
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  published
    procedure test_WriteBuffer();
  end;

  { TTestSOAPFormatterAttributes }

  TTestSOAPFormatterAttributes = class(TTestFormatterSimpleType)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  { TTestXmlRpcFormatterAttributes }

  TTestXmlRpcFormatterAttributes = class(TTestFormatterSimpleType)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  TTestXmlRpcFormatter= class(TTestFormatter)
  protected
    class function GetFormaterName() : string;override;
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
    function Support_ComplextType_with_SimpleContent():Boolean;override;
    function Support_nil():Boolean;override;
  published
    procedure test_WriteBuffer();
  end;
  
  { TTestArray }

  TTestArray= class(TTestCase)
  published
    procedure Test_StringArray();
    procedure Test_BooleanArray();
    
    procedure Test_Int8UArray();
    procedure Test_Int8SArray();
    
    procedure Test_Int16SArray();
    procedure Test_Int16UArray();
    
    procedure Test_Int32UArray();
    procedure Test_Int32SArray();
    
    procedure Test_Int64SArray();
    procedure Test_Int64UArray();
    
    procedure Test_FloatSingleArray();
    procedure Test_FloatDoubleArray();
    procedure Test_FloatExtendedArray();
    procedure Test_FloatCurrencyArray();
  end;

  { TTest_TBaseComplexRemotable }

  TTest_TBaseComplexRemotable= class(TTestCase)
  published
    procedure Test_Assign();
  end;

  { TTest_SoapFormatterExceptionBlock }

  TTest_SoapFormatterExceptionBlock = class(TTestCase)
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
    function CreateFormatter():IFormatterResponse;
    function CreateFormatterClient():IFormatterClient;
  published
    procedure ExceptBlock_server();
    procedure ExceptBlock_client();
  end;

  { TTest_XmlRpcFormatterExceptionBlock }

  TTest_XmlRpcFormatterExceptionBlock = class(TTestCase)
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
    function CreateFormatter():IFormatterResponse;
    function CreateFormatterClient():IFormatterClient;
  published
    procedure ExceptBlock_server();
    procedure ExceptBlock_client();
  end;

  { TTest_BinaryFormatterExceptionBlock }

  TTest_BinaryFormatterExceptionBlock = class(TTestCase)
  protected
    function CreateFormatter():IFormatterResponse;
    function CreateFormatterClient():IFormatterClient;
  published
    procedure ExceptBlock_server();
    procedure ExceptBlock_client();
  end;
  
  { TTest_TStringBufferRemotable }

  TTest_TStringBufferRemotable = class(TTestCase)
  published
    procedure Assign();
  end;
  
  
implementation
uses base_binary_formatter, base_soap_formatter, base_xmlrpc_formatter, record_rtti,
     Math, imp_utils
{$IFNDEF FPC}
     , xmldom, wst_delphi_xml
{$ENDIF}
{$IFDEF FPC}
     , DOM, XMLRead, wst_fpc_xml
{$ENDIF}
     , server_service_soap, soap_formatter,
     server_service_xmlrpc, xmlrpc_formatter,
     binary_streamer, server_binary_formatter, binary_formatter;

function CompareNodes(const A,B : PDataBuffer) : Boolean;overload;forward;

function CompareObjectBuffers(const A,B : PObjectBuffer) : Boolean;overload;
var
  ca, cb : PObjectBufferItem;
  ok : Boolean;
begin
  if ( A = nil ) and ( B = nil ) then begin
    Result := True
  end else if ( A <> nil ) and ( B <> nil ) then begin
    if ( A^.NilObject = B^.NilObject ) and
       ( A^.Count = B^.Count ) and
       ( CompareNodes(A^.InnerData,B^.InnerData) )
    then begin
      if ( A^.Count > 0 ) then begin
        ca := A^.Head;
        cb := B^.Head;
        while Assigned(ca) do begin
          if not CompareNodes(ca^.Data,cb^.Data) then
            Break;
          ca := ca^.Next;
          cb := cb^.Next;
        end;
        ok := ( ca = nil );
      end else begin
        ok := True;
      end;
    end else begin
      ok := False;
    end;
    if ok then
      Result := CompareObjectBuffers(A^.Attributes,B^.Attributes);
  end else begin
    Result := False;
  end;
end;

function CompareObjectBuffers(const A,B : PArrayBuffer) : Boolean;overload;
var
  i : Integer;
  ok : Boolean;
begin
  if ( A = nil ) and ( B = nil ) then begin
    Result := ok
  end else if ( A <> nil ) and ( B <> nil ) then begin
    if ( A^.Count = B^.Count ) then begin
      ok := True;
      if ( A^.Count > 0 ) then begin
        for i := 0 to Pred(A^.Count) do begin
          if not CompareNodes(A^.Items^[i],B^.Items^[i]) then begin
            ok := False;
            Break;
          end;
        end;
      end;
      if ok then
        ok := CompareObjectBuffers(A^.Attributes,B^.Attributes);
    end else begin
      ok := False;
    end;
  end else begin
    Result := ok;
  end;
  Result := ok;
end;

function CompareNodes(const A,B : PDataBuffer) : Boolean;overload;
var
  ca, cb : PObjectBufferItem;
  i : PtrInt;
  ok : Boolean;
begin
  if ( A = nil ) and ( B = nil ) then begin
    ok := True;
  end else if ( A <> nil ) and ( B <> nil ) then begin
    ok := False;
    if ( A^.DataType = B^.DataType ) and
       ( A^.Name = B^.Name )
    then begin
      case A^.DataType of
        dtInt8U,dtInt8S   : ok := ( A^.Int8U = A^.Int8U );
        dtInt16U,dtInt16S : ok := ( A^.Int16U = A^.Int16U );
        dtInt32U,dtInt32S : ok := ( A^.Int32U = A^.Int32U );
        dtInt64U,dtInt64S : ok := ( A^.Int64U = A^.Int64U );
        dtBool            : ok := ( A^.BoolData = A^.BoolData );
        dtEnum            : ok := ( A^.EnumData = A^.EnumData );
        dtSingle          : ok := ( A^.SingleData = A^.SingleData );
        dtDouble          : ok := ( A^.DoubleData = A^.DoubleData );
        dtExtended        : ok := ( A^.ExtendedData = A^.ExtendedData );
        dtCurrency        : ok := ( A^.CurrencyData = A^.CurrencyData );
        dtString          : ok := ( A^.StrData = A^.StrData );
        dtObject          : ok := CompareObjectBuffers(A^.ObjectData,B^.ObjectData);
        dtArray           : ok := CompareObjectBuffers(A^.ArrayData,B^.ArrayData);
      end;
    end;
  end else begin
    ok := False;
  end;
  Result := ok;
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
                if not CompareNodes(A.Attributes.Item[i],B.Attributes.Item[i]) then
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

function RandomValue(const AMaxlen: Integer): ansistring;
var
  k : Integer;
begin
  SetLength(Result,AMaxlen);
  for k := 1 to AMaxlen do begin
    Result[k] := Char((Random(Ord(High(Char)))));
  end;
end;

function TTestFormatterSimpleType.Support_ComplextType_with_SimpleContent( ): Boolean;
begin
  Result := True;
end;

function TTestFormatterSimpleType.Support_nil(): Boolean;
begin
  Result := True;
end;

procedure TTestFormatterSimpleType.Test_Int_8;
const VAL_1 = 12; VAL_2 = -10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : Byte; intVal_S : ShortInt;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_U',TypeInfo(Byte),intVal_U);
      f.Put('intVal_S',TypeInfo(ShortInt),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.xml');
    intVal_U := 0;
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_U';
      f.Get(TypeInfo(Byte),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(ShortInt),x,intVal_S);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_U);
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Int_8_ScopeData;
const VAL_1 = 12; VAL_2 = -10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : Byte; intVal_S : ShortInt;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Int));
    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.PutScopeInnerValue(TypeInfo(Byte),intVal_U);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_U := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      f.GetScopeInnerValue(TypeInfo(Byte),intVal_U);
    f.EndScopeRead();
    CheckEquals(VAL_1,intVal_U);
    ///
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));
    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.PutScopeInnerValue(TypeInfo(ShortInt),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      f.GetScopeInnerValue(TypeInfo(ShortInt),intVal_S);
    f.EndScopeRead();
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Int_16;
const VAL_1 = 1210; VAL_2 : SmallInt = -1012;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : Word; intVal_S : SmallInt;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_U',TypeInfo(Word),intVal_U);
      f.Put('intVal_S',TypeInfo(SmallInt),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_U := 0;
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_U';
      f.Get(TypeInfo(Word),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(SmallInt),x,intVal_S);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_U);
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Int_32;
const VAL_1 = 121076; VAL_2 : LongInt = -101276;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : LongWord; intVal_S : LongInt;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_U',TypeInfo(LongWord),intVal_U);
      f.Put('intVal_S',TypeInfo(LongInt),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_U := 0;
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_U';
      f.Get(TypeInfo(LongWord),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(LongInt),x,intVal_S);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_U);
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Int_64;
const VAL_1 = 121076; VAL_2 : Int64 = -101276;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : QWord; intVal_S : Int64;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_U',TypeInfo(QWord),intVal_U);
      f.Put('intVal_S',TypeInfo(Int64),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_U := 0;
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_U';
      f.Get(TypeInfo(QWord),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(Int64),x,intVal_S);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_U);
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Single_4;
const VAL_1 : single = 12.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  tmpVal : Single;
begin
  s := Nil;
  Try
    tmpVal := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('tmpVal',TypeInfo(Single),tmpVal);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);s.SaveToFile(ClassName + '.Test_Single_4.xml');
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'tmpVal';
      f.Get(TypeInfo(Single),x,tmpVal);
    f.EndScopeRead();

    CheckEquals(VAL_1,tmpVal);//,0.00001);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Double_8;
const VAL_1 : Double = 12.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  tmpVal : Double;
begin
  s := Nil;
  Try
    tmpVal := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('tmpVal',TypeInfo(Double),tmpVal);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);s.SaveToFile(ClassName + '.Test_Double_8.xml');
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'tmpVal';
      f.Get(TypeInfo(Double),x,tmpVal);
    f.EndScopeRead();

    CheckEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Currency_8;
const VAL_1 : Currency = 12.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  tmpVal : Currency;
begin
  s := Nil;
  Try
    tmpVal := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('tmpVal',TypeInfo(Currency),tmpVal);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);s.SaveToFile(ClassName + '.Test_Currency_8.xml');
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'tmpVal';
      f.Get(TypeInfo(Currency),x,tmpVal);
    f.EndScopeRead();

    CheckEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Extended_10;
const VAL_1 : Extended = 12.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  tmpVal : Extended;
begin
  s := Nil;
  Try
    tmpVal := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('tmpVal',TypeInfo(Extended),tmpVal);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);s.SaveToFile(ClassName + '.Test_Extended_10.xml');
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'tmpVal';
      f.Get(TypeInfo(Extended),x,tmpVal);
    f.EndScopeRead();

    CheckEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_String;
const VAL_1 = 'AzErTy'; VAL_2 = 'QwErTy';
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_1 : string; intVal_3 : string;
begin
  s := Nil;
  Try
    intVal_1 := VAL_1;
    intVal_3 := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_1',TypeInfo(string),intVal_1);
      f.Put('intVal_3',TypeInfo(string),intVal_3);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_1 := '';
    intVal_3 := 'yyyyyyyy';

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_1';
      f.Get(TypeInfo(string),x,intVal_1);
      x := 'intVal_3';
      f.Get(TypeInfo(string),x,intVal_3);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_1);
    CheckEquals(VAL_2,intVal_3);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Bool;
const VAL_1 = True; VAL_2 = False;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_1 : Boolean; intVal_3 : Boolean;
begin
  s := Nil;
  Try
    intVal_1 := VAL_1;
    intVal_3 := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_1',TypeInfo(Boolean),intVal_1);
      f.Put('intVal_3',TypeInfo(Boolean),intVal_3);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_1 := False;
    intVal_3 := True;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_1';
      f.Get(TypeInfo(Boolean),x,intVal_1);
      x := 'intVal_3';
      f.Get(TypeInfo(Boolean),x,intVal_3);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_1);
    CheckEquals(VAL_2,intVal_3);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Enum;
const VAL_1 = teTwo; VAL_2 = teFour;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_1 : TTestEnum; intVal_3 : TTestEnum;
begin
  s := Nil;
  Try
    intVal_1 := VAL_1;
    intVal_3 := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Enum));
      f.Put('intVal_1',TypeInfo(TTestEnum),intVal_1);
      f.Put('intVal_3',TypeInfo(TTestEnum),intVal_3);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_1 := teOne;
    intVal_3 := teOne;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_1';
      f.Get(TypeInfo(TTestEnum),x,intVal_1);
      x := 'intVal_3';
      f.Get(TypeInfo(TTestEnum),x,intVal_3);
    f.EndScopeRead();

    CheckEquals(Ord(VAL_1),Ord(intVal_1));
    CheckEquals(Ord(VAL_2),Ord(intVal_3));
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Int_WithClass;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_Int;
  x : string;
begin
  s := Nil;
  a := TClass_Int.Create();
  Try
    a.Val_8U := 8;
      a.Val_8S := -8;
    a.Val_16U := 16;
      a.Val_16S := -16;
    a.Val_32U := 32;
      a.Val_32S := -32;
    a.Val_64U := 64;
      a.Val_64S := -64;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_Int),a);
    f.EndScope();
    
    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '_test_int_withclass.xml');
    FreeAndNil(a);
    
    a := TClass_Int.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_Int),x,a);
    f.EndScopeRead();
    
    CheckEquals(8,a.Val_8U);
      CheckEquals(-8,a.Val_8S);
    CheckEquals(16,a.Val_16U);
      CheckEquals(-16,a.Val_16S);
    CheckEquals(32,a.Val_32U);
      CheckEquals(-32,a.Val_32S);
    CheckEquals(64,a.Val_64U);
      CheckEquals(-64,a.Val_64S);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Float_WithClass;
const VAL_CUR : Currency = 8.8;
      VAL_DBL : Double = 8.8;
      VAL_SGL : Single = 4.4;
      VAL_EXT : Extended = 10.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_Float;
  x : string;
begin
  s := Nil;
  a := TClass_Float.Create();
  Try
    a.Val_Currency := VAL_CUR;
    a.Val_Double := VAL_DBL;
    a.Val_Extended := VAL_EXT;
    a.Val_Single := VAL_SGL;

    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('o1',TypeInfo(TClass_Float),a);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    a := TClass_Float.Create();
    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'o1';
      f.Get(TypeInfo(TClass_Float),x,a);
    f.EndScopeRead();

    CheckEquals(VAL_SGL,a.Val_Single);
    CheckEquals(VAL_DBL,a.Val_Double);
    CheckEquals(VAL_CUR,a.Val_Currency);
    CheckEquals(VAL_EXT,a.Val_Extended);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Enum_Bool_String_WithClass;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_Enum;
  x : string;
begin
  s := Nil;
  a := TClass_Enum.Create();
  Try
    a.Val_Bool := True;
    a.Val_Enum := teThree;
    a.Val_String := 'atou';
    f := CreateFormatter(TypeInfo(TClass_Enum));

    f.BeginObject('Root',TypeInfo(TClass_Enum));
      f.Put('o1',TypeInfo(TClass_Enum),a);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    a := TClass_Enum.Create();
    f := CreateFormatter(TypeInfo(TClass_Enum));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Enum));
      x := 'o1';
      f.Get(TypeInfo(TClass_Enum),x,a);
    f.EndScopeRead();

    CheckEquals(True,a.Val_Bool);
    CheckEquals(Ord(teThree),Ord(a.Val_Enum));
    CheckEquals('atou',a.Val_String);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_CplxInt64SimpleContent_WithClass;
const VAL_S = -12; VAL_U = 10; VAL_X = 121;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexInt64SContentRemotable;
  nu : TComplexInt64UContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;
    
  s := nil;
  ns := TComplexInt64SContentRemotable.Create();
  nu := TComplexInt64UContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxInt64S.Value := VAL_S;
    a.Val_CplxInt64S.StrSimpleAtt_Exemple := VAL_STR_S;
    a.Val_CplxInt64S.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt64S.BoolSimpleAtt_Exemple := True;
    a.Elt_Exemple := VAL_STR_X;
    ns.Value := VAL_S;

    a.Val_CplxInt64U.Value := VAL_U;
    a.Val_CplxInt64U.StrSimpleAtt_Exemple := VAL_STR_U;
    a.Val_CplxInt64U.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt64U.BoolSimpleAtt_Exemple := False;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexInt64SContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexInt64UContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexInt64SContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexInt64UContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxInt64S.Value);
    CheckEquals(VAL_X,a.Val_CplxInt64S.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_S,a.Val_CplxInt64S.StrSimpleAtt_Exemple);
    CheckEquals(True,a.Val_CplxInt64S.BoolSimpleAtt_Exemple);
    CheckEquals(VAL_STR_X,a.Elt_Exemple);

    CheckEquals(VAL_U,a.Val_CplxInt64U.Value);
    CheckEquals(VAL_X,a.Val_CplxInt64U.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_U,a.Val_CplxInt64U.StrSimpleAtt_Exemple);
    CheckEquals(False,a.Val_CplxInt64U.BoolSimpleAtt_Exemple);
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxInt32SimpleContent_WithClass;
const VAL_S = -12; VAL_U = 10; VAL_X = 1210;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexInt32SContentRemotable;
  nu : TComplexInt32UContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexInt32SContentRemotable.Create();
  nu := TComplexInt32UContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxInt32S.Value := VAL_S;
    a.Val_CplxInt32S.StrSimpleAtt_Exemple := VAL_STR_S;
    a.Val_CplxInt32S.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt32S.BoolSimpleAtt_Exemple := True;
    a.Elt_Exemple := VAL_STR_X;
    ns.Value := VAL_S;

    a.Val_CplxInt32U.Value := VAL_U;
    a.Val_CplxInt32U.StrSimpleAtt_Exemple := VAL_STR_U;
    a.Val_CplxInt32U.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt32U.BoolSimpleAtt_Exemple := False;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexInt32SContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexInt32UContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexInt32SContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexInt32UContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxInt32S.Value);
    CheckEquals(VAL_X,a.Val_CplxInt32S.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_S,a.Val_CplxInt32S.StrSimpleAtt_Exemple);
    CheckEquals(True,a.Val_CplxInt32S.BoolSimpleAtt_Exemple);
    CheckEquals(VAL_STR_X,a.Elt_Exemple);
    
    CheckEquals(VAL_U,a.Val_CplxInt32U.Value);
    CheckEquals(VAL_X,a.Val_CplxInt32U.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_U,a.Val_CplxInt32U.StrSimpleAtt_Exemple);
    CheckEquals(False,a.Val_CplxInt32U.BoolSimpleAtt_Exemple);
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxInt16SimpleContent_WithClass;
const VAL_S = -12; VAL_U = 10; VAL_X = 1210;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexInt16SContentRemotable;
  nu : TComplexInt16UContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexInt16SContentRemotable.Create();
  nu := TComplexInt16UContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxInt16S.Value := VAL_S;
    a.Val_CplxInt16S.StrSimpleAtt_Exemple := VAL_STR_S;
    a.Val_CplxInt16S.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt16S.BoolSimpleAtt_Exemple := True;
    a.Elt_Exemple := VAL_STR_X;
    ns.Value := VAL_S;

    a.Val_CplxInt16U.Value := VAL_U;
    a.Val_CplxInt16U.StrSimpleAtt_Exemple := VAL_STR_U;
    a.Val_CplxInt16U.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt16U.BoolSimpleAtt_Exemple := False;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexInt16SContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexInt16UContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexInt16SContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexInt16UContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxInt16S.Value);
    CheckEquals(VAL_X,a.Val_CplxInt16S.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_S,a.Val_CplxInt16S.StrSimpleAtt_Exemple);
    CheckEquals(True,a.Val_CplxInt16S.BoolSimpleAtt_Exemple);
    CheckEquals(VAL_STR_X,a.Elt_Exemple);

    CheckEquals(VAL_U,a.Val_CplxInt16U.Value);
    CheckEquals(VAL_X,a.Val_CplxInt16U.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_U,a.Val_CplxInt16U.StrSimpleAtt_Exemple);
    CheckEquals(False,a.Val_CplxInt16U.BoolSimpleAtt_Exemple);
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxInt8SimpleContent_WithClass;
const VAL_S = -12; VAL_U = 10; VAL_X = 121;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexInt8SContentRemotable;
  nu : TComplexInt8UContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexInt8SContentRemotable.Create();
  nu := TComplexInt8UContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxInt8S.Value := VAL_S;
    a.Val_CplxInt8S.StrSimpleAtt_Exemple := VAL_STR_S;
    a.Val_CplxInt8S.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt8S.BoolSimpleAtt_Exemple := True;
    a.Elt_Exemple := VAL_STR_X;
    ns.Value := VAL_S;

    a.Val_CplxInt8U.Value := VAL_U;
    a.Val_CplxInt8U.StrSimpleAtt_Exemple := VAL_STR_U;
    a.Val_CplxInt8U.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt8U.BoolSimpleAtt_Exemple := False;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexInt8SContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexInt8UContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexInt8SContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexInt8UContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxInt8S.Value);
    CheckEquals(VAL_X,a.Val_CplxInt8S.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_S,a.Val_CplxInt8S.StrSimpleAtt_Exemple);
    CheckEquals(True,a.Val_CplxInt8S.BoolSimpleAtt_Exemple);
    CheckEquals(VAL_STR_X,a.Elt_Exemple);

    CheckEquals(VAL_U,a.Val_CplxInt8U.Value);
    CheckEquals(VAL_X,a.Val_CplxInt8U.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_U,a.Val_CplxInt8U.StrSimpleAtt_Exemple);
    CheckEquals(False,a.Val_CplxInt8U.BoolSimpleAtt_Exemple);
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxFloatExtendedSimpleContent_WithClass;
const VAL_S : Extended = -12.10; VAL_U : Double = 10.76; VAL_X = 1210.76;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexFloatExtendedContentRemotable;
  nu : TComplexFloatDoubleContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexFloatExtendedContentRemotable.Create();
  nu := TComplexFloatDoubleContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxExtended := T_ComplexFloatExtendedContent.Create();
    a.Val_CplxExtended.Value := VAL_S;
    a.Val_CplxDouble := T_ComplexFloatDoubleContent.Create();
    a.Val_CplxDouble.Value := VAL_U;
    
    a.Val_CplxInt32S.Free();
    a.Val_CplxInt32S := nil;
    a.Val_CplxInt32U.Free();
    a.Val_CplxInt32U := nil;

    ns.Value := VAL_S;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexFloatExtendedContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexFloatDoubleContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    a.Val_CplxInt32S.Free();
    a.Val_CplxInt32S := nil;
    a.Val_CplxInt32U.Free();
    a.Val_CplxInt32U := nil;
    a.Val_CplxExtended := T_ComplexFloatExtendedContent.Create();
    a.Val_CplxDouble := T_ComplexFloatDoubleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexFloatExtendedContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexFloatDoubleContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxExtended.Value,'VAL_S <> a.Val_CplxExtended.Value');
    CheckEquals(VAL_U,a.Val_CplxDouble.Value,'VAL_S <> a.Val_CplxDouble.Value');
    CheckEquals(VAL_S,ns.Value,'VAL_S <> ns.Value');
    CheckEquals(VAL_U,nu.Value,'VAL_U <> nu.Value');
    CheckNull(a.Val_CplxInt32S,'a.Val_CplxInt32S <> nil');
    CheckNull(a.Val_CplxInt32U,'a.Val_CplxInt32U <> nil');
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxStringSimpleContent_WithClass;
const VAL_S = 'web services toolkit';
      VAL_STR_S = 'Test Attribute S';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexStringContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexStringContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxString := T_ComplexStringContent.Create();
    a.Val_CplxString.Value := VAL_S;
    a.Val_CplxInt32S.Free();
    a.Val_CplxInt32S := nil;
    a.Val_CplxInt32U.Free();
    a.Val_CplxInt32U := nil;

    ns.Value := VAL_STR_S;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexStringContentRemotable),ns);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    a.Val_CplxInt32S.Free();
    a.Val_CplxInt32S := nil;
    a.Val_CplxInt32U.Free();
    a.Val_CplxInt32U := nil;
    a.Val_CplxString := T_ComplexStringContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexStringContentRemotable),x,ns);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxString.Value,'VAL_S <> a.Val_CplxString.Value');
    CheckEquals(VAL_STR_S,ns.Value,'VAL_STR_S <> ns.Value');
  finally
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Object();
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_B;
  x : string;
begin
  s := Nil;
  a := TClass_B.Create();
  Try
    a.Val_Bool := False;
    a.Val_Enum := teThree;
    a.Val_String := '123';
    a.ObjProp.Val_String := '456';
    a.ObjProp.Val_Enum := teFour;
    a.ObjProp.Val_Bool := True;
    a.ObjProp.Val_32S := 121076;
    a.NonStored := 121076;
    
    f := CreateFormatter(TypeInfo(TClass_B));

    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('o1',TypeInfo(TClass_B),a);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    a := TClass_B.Create();
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'o1';
      f.Get(TypeInfo(TClass_B),x,a);
    f.EndScopeRead();

    CheckEquals(False,a.Val_Bool);
    CheckEquals(Ord(teThree),Ord(a.Val_Enum));
    CheckEquals('123',a.Val_String);
    
    CheckEquals(True,a.ObjProp.Val_Bool);
    CheckEquals(Ord(teFour),Ord(a.ObjProp.Val_Enum));
    CheckEquals('456',a.ObjProp.Val_String);
    CheckEquals(121076,a.ObjProp.Val_32S);
    
    CheckEquals(0,a.NonStored);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Object_Nil();
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_B;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  a := nil;
  try
    f := CreateFormatter(TypeInfo(TClass_B));

    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('o1',TypeInfo(TClass_B),a);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    a := nil;
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'o1';
      f.Get(TypeInfo(TClass_B),x,a);
    f.EndScopeRead();

    CheckNull(a);
  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_StringArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of string = ('AzErTy','QwErTy','123456','','1');
var
  a : TArrayOfStringRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfStringRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length, 'Length 1');

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfStringRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.Test_StringArray.xml');
    FreeAndNil(a);
    a := TArrayOfStringRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfStringRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length, 'Length 2');

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_StringArray_Embedded();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of string = ('AzErTy','QwErTy','123456','','1');
var
  a : TArrayOfStringRemotable;
  b : TEmbeddedArrayOfStringRemotable;
  i, intVal : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  b := nil;
  a := TArrayOfStringRemotable.Create();
  try
    b := TEmbeddedArrayOfStringRemotable.Create();
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length, 'Length 1');

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    b.SetLength(AR_LEN);
    CheckEquals(AR_LEN,b.Length);

    for i := 0 to Pred(AR_LEN) do begin
      a[i] := VAL_AR[i];
      b[i] := VAL_AR[Pred(AR_LEN)-i];
    end;

    intVal := 1210;
    
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfStringRemotable),a);
      f.Put('x',TypeInfo(Integer),intVal);
      f.Put('b',TypeInfo(TEmbeddedArrayOfStringRemotable),b);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.XML');
    FreeAndNil(a);
    FreeAndNil(b);
    intVal := 0;
    a := TArrayOfStringRemotable.Create();
    b := TEmbeddedArrayOfStringRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    b.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfStringRemotable),x,a);
      x := 'x';
      f.Get(TypeInfo(Integer),x,intVal);
      x := 'b';
      f.Get(TypeInfo(TEmbeddedArrayOfStringRemotable),x,b);
    f.EndScopeRead();
    CheckEquals(1210,intVal, 'IntVal');
    CheckEquals(AR_LEN,a.Length, 'Length 2 a');
    CheckEquals(AR_LEN,b.Length,'Length 2 b');

    for i := 0 to Pred(AR_LEN) do begin
      CheckEquals(VAL_AR[i],a[i]);
      CheckEquals(VAL_AR[Pred(AR_LEN)-i],b[i]);
    end;

  finally
    b.Free();
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_StringArrayZeroLength();
var
  a : TArrayOfStringRemotable;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfStringRemotable.Create();
  try
    CheckEquals(0,a.Length);

    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfStringRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfStringRemotable),x,a);
    f.EndScopeRead();
    CheckNotNull(a);
    CheckEquals(0,a.Length);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_BooleanArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Boolean = (True,True,False,True,False);
var
  a : TArrayOfBooleanRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfBooleanRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfBooleanRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfBooleanRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfBooleanRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int8UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Byte = (12,34,100,200,180);
var
  a : TArrayOfInt8URemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt8URemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt8URemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt8URemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt8URemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int8SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of ShortInt = (-12,-34,100,120,110);
var
  a : TArrayOfInt8SRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt8SRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt8SRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt8SRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt8SRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int16SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of SmallInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt16SRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt16SRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt16SRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt16SRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt16SRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int16UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Word = (12,34,100,200,180);
var
  a : TArrayOfInt16URemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt16URemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt16URemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt16URemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt16URemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int32UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of LongWord = (12,34,100,200,180);
var
  a : TArrayOfInt32URemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt32URemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt32URemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt32URemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt32URemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(True,VAL_AR[i] = a[i],Format('VAL_AR[%d] = a[%d]',[i,i]));

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int32SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of LongInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt32SRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt32SRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt32SRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt32SRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt32SRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int64SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Int64 = (-12,-34,100,200,180);
var
  a : TArrayOfInt64SRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt64SRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt64SRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt64SRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt64SRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int64UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of QWord = (12,34,100,200,180);
var
  a : TArrayOfInt64URemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt64URemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt64URemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt64URemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt64URemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_FloatSingleArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Single = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatSingleRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfFloatSingleRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfFloatSingleRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfFloatSingleRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatSingleRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_FloatDoubleArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Double = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatDoubleRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfFloatDoubleRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfFloatDoubleRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfFloatDoubleRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatDoubleRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_FloatExtendedArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Extended = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatExtendedRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfFloatExtendedRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfFloatExtendedRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfFloatExtendedRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatExtendedRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_FloatCurrencyArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Currency = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatCurrencyRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfFloatCurrencyRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfFloatCurrencyRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfFloatCurrencyRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatCurrencyRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_ComplexInt32S();
const VAL_1 = 121076; VAL_2 : LongInt = -101276;
var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  a, b : TComplexInt32SContentRemotable;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  a := nil;
  b := nil;
  try
    a := TComplexInt32SContentRemotable.Create();
    b := TComplexInt32SContentRemotable.Create();
    a.Value := VAL_1;
    b.Value := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('a',TypeInfo(TComplexInt32SContentRemotable),a);
      f.Put('b',TypeInfo(TComplexInt32SContentRemotable),b);
    f.EndScope();
    FreeAndNil(a);FreeAndNil(b);
    s := TMemoryStream.Create();
    f.SaveToStream(s);

    a := TComplexInt32SContentRemotable.Create();
    b := TComplexInt32SContentRemotable.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'a';
      f.Get(TypeInfo(TComplexInt32SContentRemotable),x,a);
      x := 'b';
      f.Get(TypeInfo(TComplexInt32SContentRemotable),x,b);
    f.EndScopeRead();

    CheckEquals(VAL_1,a.Value);
    CheckEquals(VAL_2,b.Value);
  finally
    s.Free();
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTestFormatter.Test_Record_simple();
const VAL_1 : Integer = 12; VAL_2 : Integer = -76; VAL_3 = 'wst record sample';
var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  a : TTestSmallRecord;
begin
  s := nil;
  try
    a.fieldWord := VAL_1;
    a.fieldSmallint := VAL_2;
    a.fieldString := VAL_3;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('a',TypeInfo(TTestSmallRecord),a);
    f.EndScope();
    a.fieldWord := 0;
    a.fieldSmallint := 0;
    a.fieldString := '';
    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.Test_Record_simple.xml');

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'a';
      f.Get(TypeInfo(TTestSmallRecord),x,a);
    f.EndScopeRead();

    CheckEquals(VAL_1,a.fieldWord);
    CheckEquals(VAL_2,a.fieldSmallint);
    CheckEquals(VAL_3,a.fieldString);
  finally
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Record_nested();
const
  VAL_EPSILON = 0.0001;
  VAL_EMPTY_RECORD : TTestRecord = (
    fieldByte : 0;
    fieldShortInt :  0;
    fieldSmallint : 0;
    fieldWord : 0;
    fieldInteger : 0;
    fieldLongWord : 0;
    fieldInt64 : 0;
    fieldQWord : 0;
    fieldComp : 0;
    fieldSingle : 0;
    fieldDouble : 0;
    fieldExtended : 0;
    fieldCurrency : 0;
    fieldBoolean : False;
    fieldString : '';
    fieldRecord :  ( fieldSmallint : 0; fieldWord : 0; fieldString : '');
  );
  VAL_RECORD : TTestRecord = (
    fieldByte : 12;
    fieldShortInt :  -10;
    fieldSmallint : 76;
    fieldWord : 34;
    fieldInteger : -45;
    fieldLongWord : 567;
    fieldInt64 : 8910;
    fieldQWord : 111213;
    fieldComp : 141516;
    fieldSingle : 1718;
    fieldDouble : -1819;
    fieldExtended : 2021;
    fieldCurrency : -2122;
    fieldBoolean : True;
    fieldString : 'sample record string 0123456789';
    fieldRecord :  ( fieldSmallint : 10; fieldWord : 11; fieldString : 'azertyqwerty');
  );
var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  a : TTestRecord;
begin
  s := nil;
  try
    a := VAL_RECORD;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('a',TypeInfo(TTestRecord),a);
    f.EndScope();
    a := VAL_EMPTY_RECORD;
    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.Test_Record_nested.xml');

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'a';
      f.Get(TypeInfo(TTestRecord),x,a);
    f.EndScopeRead();

    CheckEquals(VAL_RECORD.fieldBoolean,a.fieldBoolean,'fieldBoolean');
    CheckEquals(VAL_RECORD.fieldByte,a.fieldByte,'fieldByte');
{$IFDEF HAS_COMP}
    CheckEquals(VAL_RECORD.fieldComp,a.fieldComp,'fieldComp');
{$ENDIF}
    Check(IsZero(VAL_RECORD.fieldCurrency-a.fieldCurrency,VAL_EPSILON),'fieldCurrency');
    Check(IsZero(VAL_RECORD.fieldExtended-a.fieldExtended,VAL_EPSILON),'fieldExtended');
    CheckEquals(VAL_RECORD.fieldInt64,a.fieldInt64,'fieldInt64');
    CheckEquals(VAL_RECORD.fieldInteger,a.fieldInteger,'fieldInteger');
    Check(VAL_RECORD.fieldLongWord = a.fieldLongWord,'fieldLongWord');
{$IFDEF HAS_QWORD}
    CheckEquals(VAL_RECORD.fieldQWord,a.fieldQWord,'fieldQWord');
{$ENDIF}
    CheckEquals(VAL_RECORD.fieldRecord.fieldSmallint,a.fieldRecord.fieldSmallint,'fieldSmallint');
    CheckEquals(VAL_RECORD.fieldRecord.fieldString,a.fieldRecord.fieldString,'fieldString');
    CheckEquals(VAL_RECORD.fieldRecord.fieldWord,a.fieldRecord.fieldWord,'fieldWord');
    CheckEquals(VAL_RECORD.fieldShortInt,a.fieldShortInt,'fieldShortInt');
    Check(IsZero(VAL_RECORD.fieldSingle-a.fieldSingle,VAL_EPSILON),'fieldSingle');
    CheckEquals(VAL_RECORD.fieldSmallint,a.fieldSmallint,'fieldSmallint');
    CheckEquals(VAL_RECORD.fieldString,a.fieldString,'fieldString');
    CheckEquals(VAL_RECORD.fieldWord,a.fieldWord,'fieldWord');
  finally
    s.Free();
  end;
end;

procedure TTestFormatter.test_GetScopeItemNames();
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a, b : TClass_A;
  x : string;
  ls : TStringList;
  intv : TArrayOfStringRemotableSample;
begin
  ls := nil;
  s := Nil;
  b := nil;
  intv := nil;
  a := TClass_A.Create();
  try
    a.Val_Bool := False;
    a.Val_Enum := teThree;
    a.Val_String := '123';
    a.Val_32S := 55;
    b := TClass_A.Create();
    intv := TArrayOfStringRemotableSample.Create();
    intv.SetLength(3);
    intv[0] := 'wst';
    intv[1] := 'azerty';
    intv[2] := 'qwerty';

    f := CreateFormatter(TypeInfo(TClass_A));

    f.BeginObject('Root',TypeInfo(TClass_A));
      f.Put('a',TypeInfo(TClass_A),a);
      f.Put('b',TypeInfo(TClass_A),b);
      f.Put('intv',TypeInfo(TArrayOfStringRemotable),intv);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    FreeAndNil(b);
    FreeAndNil(intv);

    ls := TStringList.Create();
    f := CreateFormatter(TypeInfo(TClass_A));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_A));
      CheckEquals(3, f.GetScopeItemNames(ls), 'GetScopeItemNames.Count(Root)');
      Check( ls.IndexOf('a') >= 0 );
      Check( ls.IndexOf('b') >= 0 );
      Check( ls.IndexOf('intv') >= 0 );
      x := 'a';
      f.BeginObjectRead(x,TypeInfo(TClass_A));
        CheckEquals(4, f.GetScopeItemNames(ls), 'GetScopeItemNames.Count(a)');
        Check( ls.IndexOf('Val_Bool') >= 0 );
        Check( ls.IndexOf('Val_Enum') >= 0 );
        Check( ls.IndexOf('Val_String') >= 0 );
        Check( ls.IndexOf('Val_32S') >= 0 );
      f.EndScopeRead();

      x := 'b';
      f.BeginObjectRead(x,TypeInfo(TClass_A));
        CheckEquals(4, f.GetScopeItemNames(ls), 'GetScopeItemNames.Count(b)');
        Check( ls.IndexOf('Val_Bool') >= 0 );
        Check( ls.IndexOf('Val_Enum') >= 0 );
        Check( ls.IndexOf('Val_String') >= 0 );
        Check( ls.IndexOf('Val_32S') >= 0 );
      f.EndScopeRead();

      x := 'intv';
      f.BeginArrayRead(x,TypeInfo(TArrayOfStringRemotableSample),asScoped,'OI');
        CheckEquals(3, f.GetScopeItemNames(ls), 'GetScopeItemNames.Count(intv)');
        //Check( ls.IndexOf('OI') >= 0 );
      f.EndScopeRead();

    f.EndScopeRead();
  finally
    intv.Free();
    ls.Free();
    b.Free();;
    a.Free();
    s.Free();
  end;
end;


{ TTestBinaryFormatter }

function TTestBinaryFormatter.CreateFormatter(ARootType : PTypeInfo):IFormatterBase;
begin
  Result := TBaseBinaryFormatter.Create() as IFormatterBase;
  //Result.BeginObject('root',Nil);
end;

class function TTestBinaryFormatter.GetFormaterName(): string;
begin
  Result := 'wst-binary';
end;

procedure TTestBinaryFormatter.test_WriteBuffer();
var
  bw : IDataStore;
  br : IDataStoreReader;
  f : IFormatterBase;
  strm : TStringStream;
  a, b, tmp : PDataBuffer;
  locBuffer : string;
begin
  a := CreateObjBuffer(dtObject,'a',nil);
    CreateObjBuffer(dtString,'aa',a)^.StrData^.Data := 'val_aa';
    tmp := CreateObjBuffer(dtObject,'b',a);
      tmp := CreateObjBuffer(dtObject,'c',tmp);
        CreateObjBuffer(dtInt32U,'i',tmp)^.Int32S := 1210;
        CreateObjBuffer(dtString,'s',tmp)^.StrData^.Data := 's string sample';
  b := nil;
  strm := TStringStream.Create('');
  try
    bw := CreateBinaryWriter(strm);
    SaveObjectToStream(a,bw);
    strm.Position := 0;
    locBuffer := strm.DataString;

    f := TBaseBinaryFormatter.Create() as IFormatterBase;
    //f.BeginObject('Root',TypeInfo(TClass_A)); //done in the constructor!
      f.WriteBuffer(locBuffer);
    //f.EndScope();
    strm.Size := 0;
    f.SaveToStream(strm);
    strm.Position := 0;
    br := CreateBinaryReader(strm);
    b := LoadObjectFromStream(br);
    Check(CompareNodes(a,b^.ObjectData^.Head^.Data));
  finally
    strm.Free();
    ClearObj(a);
    ClearObj(b);
  end;
end;

{ TTestSOAPFormatter }

function TTestSOAPFormatter.CreateFormatter(ARootType : PTypeInfo):IFormatterBase;
begin
  Result := TSOAPBaseFormatter.Create() as IFormatterBase;
  Result.BeginObject('Env',ARootType)
end;

class function TTestSOAPFormatter.GetFormaterName(): string;
begin
  Result := 'SOAP';
end;

procedure TTestSOAPFormatter.test_WriteBuffer();
const
  s_XML_BUFFER =
    '<?xml version="1.0"?> ' +
    '<a aa="val_aa"> ' +
     ' <b> ' +
       ' <c cc="cc_val"> ' +
         ' <i>-76</i> ' +
         ' <s>wst record sample</s> ' +
       ' </c> ' +
     ' </b> ' +
    '</a>';
var
  f : IFormatterBase;
  strm : TMemoryStream;
  da, db : TXMLDocument;
begin
  f := TSOAPBaseFormatter.Create() as IFormatterBase;
  f.BeginObject('Root',TypeInfo(TClass_A));
    f.WriteBuffer(s_XML_BUFFER);
  f.EndScope();
  da := nil;
  db := nil;
  strm := TMemoryStream.Create();
  try
    f.SaveToStream(strm);
    strm.Position := 0;
    ReadXMLFile(da,strm);

    strm.Size := 0;
    strm.WriteBuffer(s_XML_BUFFER[1],Length(s_XML_BUFFER));
    strm.Position := 0;
    ReadXMLFile(db,strm);

    Check(CompareNodes(da.DocumentElement.FirstChild,db.DocumentElement));
  finally
    ReleaseDomNode(da);
    ReleaseDomNode(db);
    strm.Free();
  end;
end;

{ TClass_B }

procedure TClass_B.SetObjProp(const AValue: TClass_A);
begin
  FObjProp.Assign(AValue);
end;

constructor TClass_B.Create();
begin
  inherited Create();
  FObjProp := TClass_A.Create();
end;

destructor TClass_B.Destroy();
begin
  FreeAndNil(FObjProp);
  inherited Destroy();
end;

{ TTestArray }

procedure TTestArray.Test_StringArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of string = ('AzErTy','QwErTy','123456','','1');
var
  a : TArrayOfStringRemotable;
  i, j : Integer;
begin
  a := TArrayOfStringRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(ansistring))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(ansistring))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');
    
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_BooleanArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Boolean = (True,True,False,True,False);
var
  a : TArrayOfBooleanRemotable;
  i, j : Integer;
begin
  a := TArrayOfBooleanRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Boolean))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Boolean))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int8UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Byte = (12,34,100,200,180);
var
  a : TArrayOfInt8URemotable;
  i, j : Integer;
begin
  a := TArrayOfInt8URemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Byte))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Byte))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int8SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of ShortInt = (-12,-34,100,120,110);
var
  a : TArrayOfInt8SRemotable;
  i, j : Integer;
begin
  a := TArrayOfInt8SRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(ShortInt))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(ShortInt))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int16SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of SmallInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt16SRemotable;
  i, j : Integer;
begin
  a := TArrayOfInt16SRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(SmallInt))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(SmallInt))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int16UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Word = (12,4,100,200,180);
var
  a : TArrayOfInt16URemotable;
  i, j : Integer;
begin
  a := TArrayOfInt16URemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Word))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Word))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int32UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of LongWord = (12,4,100,200,180);
var
  a : TArrayOfInt32URemotable;
  i, j : Integer;
begin
  a := TArrayOfInt32URemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(LongWord))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(LongWord))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int32SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of LongInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt32SRemotable;
  i, j : Integer;
begin
  a := TArrayOfInt32SRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(LongInt))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(LongInt))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int64SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Int64 = (-12,-34,100,200,180);
var
  a : TArrayOfInt64SRemotable;
  i, j : Integer;
begin
  a := TArrayOfInt64SRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Int64))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Int64))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int64UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of QWord = (12,4,100,200,180);
var
  a : TArrayOfInt64URemotable;
  i, j : Integer;
begin
  a := TArrayOfInt64URemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(QWord))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(QWord))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_FloatSingleArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Single = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatSingleRemotable;
  i, j : Integer;
begin
  a := TArrayOfFloatSingleRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Single))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Single))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_FloatDoubleArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Double = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatDoubleRemotable;
  i, j : Integer;
begin
  a := TArrayOfFloatDoubleRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Double))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Double))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_FloatExtendedArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Extended = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatExtendedRemotable;
  i, j : Integer;
begin
  a := TArrayOfFloatExtendedRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Extended))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Extended))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_FloatCurrencyArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Currency = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatCurrencyRemotable;
  i, j : Integer;
begin
  a := TArrayOfFloatCurrencyRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Currency))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Currency))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

{ TTest_TBaseComplexRemotable }

procedure TTest_TBaseComplexRemotable.Test_Assign();
Var
  a,b : TClass_B;
begin
  b := nil;
  a := TClass_B.Create();
  Try
    a.Val_Bool := False;
    a.Val_Enum := teThree;
    a.Val_String := '123';
    a.ObjProp.Val_String := '456';
    a.ObjProp.Val_Enum := teFour;
    a.ObjProp.Val_Bool := True;
    a.ObjProp.Val_32S := 121076;
    a.NonStored := 121076;

    b := TClass_B.Create();
    
    b.Assign(a);

    CheckEquals(a.Val_Bool,b.Val_Bool);
    CheckEquals(Ord(a.Val_Enum),Ord(b.Val_Enum));
    CheckEquals(a.Val_String,b.Val_String);

    CheckEquals(a.ObjProp.Val_Bool,b.ObjProp.Val_Bool);
    CheckEquals(Ord(a.ObjProp.Val_Enum),Ord(b.ObjProp.Val_Enum));
    CheckEquals(a.ObjProp.Val_String,a.ObjProp.Val_String);
  Finally
    a.Free();
    b.Free();
  End;
end;

{ TTestSOAPFormatterAttributes }

function TTestSOAPFormatterAttributes.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := TSOAPBaseFormatter.Create() as IFormatterBase;
  Result.BeginObject('Env',ARootType);
  Result.SetSerializationStyle(ssAttibuteSerialization);
end;

{ TTestBinaryFormatterAttributes }

function TTestBinaryFormatterAttributes.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := TSOAPBaseFormatter.Create() as IFormatterBase;
  Result.BeginObject('Env',ARootType);
  Result.SetSerializationStyle(ssAttibuteSerialization);
end;

{ TClass_CplxSimpleContent }

constructor TClass_CplxSimpleContent.Create();
begin
  FVal_CplxInt64S := T_ComplexInt64SContent.Create();
    FVal_CplxInt64U := T_ComplexInt64UContent.Create();
  FVal_CplxInt32S := T_ComplexInt32SContent.Create();
    FVal_CplxInt32U := T_ComplexInt32UContent.Create();
  FVal_CplxInt16S := T_ComplexInt16SContent.Create();
    FVal_CplxInt16U := T_ComplexInt16UContent.Create();
  FVal_CplxInt8S := T_ComplexInt8SContent.Create();
    FVal_CplxInt8U := T_ComplexInt8UContent.Create();
end;

destructor TClass_CplxSimpleContent.Destroy();
begin
  FreeAndNil(FVal_CplxInt64S);
    FreeAndNil(FVal_CplxInt64U);
  FreeAndNil(FVal_CplxInt32U);
    FreeAndNil(FVal_CplxInt32S);
  FreeAndNil(FVal_CplxInt16U);
    FreeAndNil(FVal_CplxInt16S);
  FreeAndNil(FVal_CplxInt8U);
    FreeAndNil(FVal_CplxInt8S);
  inherited Destroy();
end;

{ TTestXmlRpcFormatterAttributes }

function TTestXmlRpcFormatterAttributes.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := TXmlRpcBaseFormatter.Create() as IFormatterBase;
  //Result.BeginObject('Env',ARootType)
end;

{ TTestXmlRpcFormatter }

function TTestXmlRpcFormatter.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := TXmlRpcBaseFormatter.Create() as IFormatterBase;
end;

class function TTestXmlRpcFormatter.GetFormaterName(): string;
begin
  Result := 'XMLRPC';
end;

function TTestXmlRpcFormatter.Support_ComplextType_with_SimpleContent(): Boolean;
begin
  Result := False;
end;

function TTestXmlRpcFormatter.Support_nil(): Boolean;
begin
  Result := False;
end;

procedure TTestXmlRpcFormatter.test_WriteBuffer();
const
  s_XML_BUFFER =
    '<?xml version="1.0"?> ' +
    '<a aa="val_aa"> ' +
     ' <b> ' +
       ' <c cc="cc_val"> ' +
         ' <i>-76</i> ' +
         ' <s>wst record sample</s> ' +
       ' </c> ' +
     ' </b> ' +
    '</a>';
var
  f : IFormatterBase;
  strm : TMemoryStream;
  da, db : TXMLDocument;
begin
  f := TXmlRpcBaseFormatter.Create() as IFormatterBase;
  f.BeginObject('Root',TypeInfo(TClass_A));
    f.WriteBuffer(s_XML_BUFFER);
  f.EndScope();
  da := nil;
  db := nil;
  strm := TMemoryStream.Create();
  try
    f.SaveToStream(strm);
    strm.Position := 0;
    ReadXMLFile(da,strm);

    strm.Size := 0;
    strm.WriteBuffer(s_XML_BUFFER[1],Length(s_XML_BUFFER));
    strm.Position := 0;
    ReadXMLFile(db,strm);

    Check(CompareNodes(da.DocumentElement.FirstChild,db.DocumentElement));
  finally
    ReleaseDomNode(da);
    ReleaseDomNode(db);
    strm.Free();
  end;
end;

{ TTest_SoapFormatterExceptionBlock }

function TTest_SoapFormatterExceptionBlock.CreateFormatter() : IFormatterResponse;
begin
  Result := server_service_soap.TSOAPFormatter.Create() as IFormatterResponse;
end;

function TTest_SoapFormatterExceptionBlock.CreateFormatterClient() : IFormatterClient;
begin
  Result := soap_formatter.TSOAPFormatter.Create() as IFormatterClient;
end;

function FindAttributeByValueInNode(
  const AAttValue : string;
  const ANode     : TDOMNode;
  out   AResAtt   : string
):boolean;
Var
  i,c : Integer;
begin
  AResAtt := '';
  if Assigned(ANode) and
     Assigned(ANode.Attributes) and
     ( ANode.Attributes.Length > 0 )
  then begin
    c := Pred(ANode.Attributes.Length);
    For i := 0 To c Do Begin
      If AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) Then Begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      End;
    End;
  end;
  Result := False;
end;

procedure TTest_SoapFormatterExceptionBlock.ExceptBlock_server();
const
  VAL_CODE = 'Server.CustomCode.Test'; VAL_MSG = 'This is a sample exception message.';
var
  f : IFormatterResponse;
  strm : TMemoryStream;

  envNd : TDOMElement;
  bdyNd, fltNd, hdrNd, tmpNode : TDOMNode;
  nsShortName,eltName, msgBuff : string;
  doc : TXMLDocument;
begin
  f := CreateFormatter();
  f.BeginExceptionList(VAL_CODE,VAL_MSG);
  f.EndExceptionList();
  strm := TMemoryStream.Create();
  try
    f.SaveToStream(strm);strm.SaveToFile('TTest_SoapFormatterExceptionBlock.ExceptBlock.xml');
    strm.Position := 0;
    ReadXMLFile(doc,strm);
    if FindAttributeByValueInNode(sSOAP_ENV,doc.DocumentElement,nsShortName) or
       FindAttributeByValueInNode('"' + sSOAP_ENV + '"',doc.DocumentElement,nsShortName)
    then begin
      nsShortName := Copy(nsShortName,1 + Pos(':',nsShortName),MaxInt);
      if not IsStrEmpty(nsShortName) then
        nsShortName := nsShortName + ':';
    end else begin
      nsShortName := '';
    end;
    eltName := nsShortName + sENVELOPE;
    envNd := doc.DocumentElement;
    if not SameText(eltName,envNd.NodeName) then
      check(False,Format('XML root node must be "Envelope", found : "%s"',[envNd.NodeName + ':::' + nsShortName]));

    bdyNd := envNd.FirstChild;
    if not Assigned(bdyNd) then
      check(False,'Node not found : "Body".');

    eltName := nsShortName + 'Body';
    if not SameText(bdyNd.NodeName,eltName) then begin
      check(False,'Node not found : "Body".');
    end;

    bdyNd := envNd.FirstChild;
    If Not Assigned(bdyNd) Then
      check(False,'Node not found : "Body"');
    If Not SameText(bdyNd.NodeName,eltName) Then
      bdyNd := bdyNd.NextSibling;
    If Not Assigned(bdyNd) Then
      Check(False,'Node not found : "Body"');
    If Not Assigned(bdyNd.FirstChild) Then
      Check(False,'Response Node not found');
    eltName := nsShortName + 'Fault';
    if SameText(eltName,bdyNd.FirstChild.NodeName) then begin
      fltNd := bdyNd.FirstChild;
        eltName := 'faultcode';
        tmpNode := FindNode(fltNd,eltName);
        if not Assigned(tmpNode) then
          Check(False,Format('"%s" Node not found.',[eltName]));
        if tmpNode.HasChildNodes then
          msgBuff := tmpNode.FirstChild.NodeValue
        else
          msgBuff := tmpNode.NodeValue;
        CheckEquals(VAL_CODE,msgBuff,eltName);

        eltName := 'faultstring';
        tmpNode := FindNode(fltNd,eltName);
        if not Assigned(tmpNode) then
          Check(False,Format('"%s" Node not found.',[eltName]));
        if tmpNode.HasChildNodes then
          msgBuff := tmpNode.FirstChild.NodeValue
        else
          msgBuff := tmpNode.NodeValue;
        CheckEquals(VAL_MSG,msgBuff,eltName);
    end;
  finally
    FreeAndNil(strm);
  end;
end;

procedure TTest_SoapFormatterExceptionBlock.ExceptBlock_client();
const
  VAL_CODE = 'Server.CustomCode.Test'; VAL_MSG = 'This is a sample exception message.';
  VAL_STREAM =
    '<?xml version="1.0"?> '+
    ' <SOAP-ENV:Envelope ' +
        ' xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
        ' xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" ' +
        ' xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"> ' +
     ' <SOAP-ENV:Body> '+
       ' <SOAP-ENV:Fault> '+
         ' <faultcode>' + VAL_CODE + '</faultcode> '+
         ' <faultstring>' + VAL_MSG +'</faultstring> '+
       ' </SOAP-ENV:Fault> '+
     ' </SOAP-ENV:Body> '+
    ' </SOAP-ENV:Envelope>';
var
  f : IFormatterClient;
  strm : TStringStream;
  excpt_code, excpt_msg : string;
begin
  excpt_code := '';
  excpt_msg := '';
  f := CreateFormatterClient();
  strm := TStringStream.Create(VAL_STREAM);
  try
    strm.Position := 0;
    f.LoadFromStream(strm);
    try
      f.BeginCallRead(nil);
      Check(False,'BeginCallRead() should raise an exception.');
    except
      on e : ESOAPException do begin
        excpt_code := e.FaultCode;
        excpt_msg := e.FaultString;
      end;
    end;
    CheckEquals(VAL_CODE,excpt_code,'faultCode');
    CheckEquals(VAL_MSG,excpt_msg,'faultString');
  finally
    FreeAndNil(strm);
  end;
end;

{$IFDEF WST_RECORD_RTTI}
function __TTestSmallRecord_TYPEINFO_FUNC__() : PTypeInfo;
var
  p : ^TTestSmallRecord;
  r : TTestSmallRecord;
begin
  p := @r;
  Result := MakeRawTypeInfo(
    'TTestSmallRecord',
    SizeOf(TTestSmallRecord),
    [ PtrUInt(@(p^.fieldSmallint)) - PtrUInt(p), PtrUInt(@(p^.fieldWord)) - PtrUInt(p), PtrUInt(@(p^.fieldString)) - PtrUInt(p) ],
    [ TypeInfo(SmallInt), TypeInfo(Word), TypeInfo(String) ]
  );
end;
{$ENDIF WST_RECORD_RTTI}

{$IFDEF WST_RECORD_RTTI}
function __TTestRecord_TYPEINFO_FUNC__() : PTypeInfo;
var
  p : ^TTestRecord;
  r : TTestRecord;
begin
  p := @r;
  Result := MakeRawTypeInfo(
    'TTestRecord',
    SizeOf(TTestRecord),
    [ PtrUInt(@(p^.fieldByte)) - PtrUInt(p), PtrUInt(@(p^.fieldShortInt)) - PtrUInt(p), PtrUInt(@(p^.fieldSmallint)) - PtrUInt(p), PtrUInt(@(p^.fieldWord)) - PtrUInt(p), PtrUInt(@(p^.fieldInteger)) - PtrUInt(p), PtrUInt(@(p^.fieldLongWord)) - PtrUInt(p), PtrUInt(@(p^.fieldInt64)) - PtrUInt(p), PtrUInt(@(p^.fieldQWord)) - PtrUInt(p), PtrUInt(@(p^.fieldComp)) - PtrUInt(p), PtrUInt(@(p^.fieldSingle)) - PtrUInt(p), PtrUInt(@(p^.fieldDouble)) - PtrUInt(p), PtrUInt(@(p^.fieldExtended)) - PtrUInt(p), PtrUInt(@(p^.fieldCurrency)) - PtrUInt(p), PtrUInt(@(p^.fieldBoolean)) - PtrUInt(p), PtrUInt(@(p^.fieldString)) - PtrUInt(p), PtrUInt(@(p^.fieldRecord)) - PtrUInt(p) ],
    [ TypeInfo(Byte), TypeInfo(ShortInt), TypeInfo(SmallInt), TypeInfo(Word), TypeInfo(Integer), TypeInfo(LongWord), TypeInfo(Int64), TypeInfo(QWord), TypeInfo(Comp), TypeInfo(Single), TypeInfo(Double), TypeInfo(Extended), TypeInfo(Currency), TypeInfo(Boolean), TypeInfo(String), TypeInfo(TTestSmallRecord) ]
  );
end;
{$ENDIF WST_RECORD_RTTI}

procedure TTest_SoapFormatterExceptionBlock.SetUp();
begin
  inherited;
{$IFNDEF FPC}
  CoInitialize(nil);
{$ENDIF}
end;

procedure TTest_SoapFormatterExceptionBlock.TearDown();
begin
{$IFNDEF FPC}
  CoUninitialize();
{$ENDIF}
  inherited;
end;

{ TTest_XmlRpcFormatterExceptionBlock }

procedure TTest_XmlRpcFormatterExceptionBlock.SetUp();
begin
  inherited;
{$IFNDEF FPC}
  CoInitialize(nil);
{$ENDIF}
end;

procedure TTest_XmlRpcFormatterExceptionBlock.TearDown();
begin
{$IFNDEF FPC}
  CoUninitialize();
{$ENDIF}
  inherited;
end;

function TTest_XmlRpcFormatterExceptionBlock.CreateFormatter() : IFormatterResponse;
begin
  Result := server_service_xmlrpc.TXmlRpcFormatter.Create() as IFormatterResponse;
end;

function TTest_XmlRpcFormatterExceptionBlock.CreateFormatterClient() : IFormatterClient;
begin
  Result := xmlrpc_formatter.TXmlRpcFormatter.Create() as IFormatterClient;
end;

procedure TTest_XmlRpcFormatterExceptionBlock.ExceptBlock_server();
  function loc_FindNode(AScope : TDOMNode; const ANodeName: string): TDOMNode;
  var
    memberNode, tmpNode : TDOMNode;
    i : Integer;
    chilNodes : TDOMNodeList;
    nodeFound : Boolean;
  begin
    Result := nil;
    if AScope.HasChildNodes() then begin
      nodeFound := False;
      memberNode := AScope.FirstChild;
      while ( not nodeFound ) and ( memberNode <> nil ) do begin
        if memberNode.HasChildNodes() then begin
          chilNodes := memberNode.ChildNodes;
          for i := 0 to Pred(GetNodeListCount(chilNodes)) do begin
            tmpNode := chilNodes.Item[i];
            if AnsiSameText(sNAME,tmpNode.NodeName) and
               ( tmpNode.FirstChild <> nil ) and
               AnsiSameText(ANodeName,tmpNode.FirstChild.NodeValue)
            then begin
              nodeFound := True;
              Break;
            end;
          end;
          if nodeFound then begin
            tmpNode := FindNode(memberNode,sVALUE);
            if ( tmpNode <> nil ) and ( tmpNode.FirstChild <> nil ) then begin
              Result := tmpNode.FirstChild;
              Break;
            end;
          end;
        end;
        memberNode := memberNode.NextSibling;
      end;
    end;
  end;

const VAL_CODE = '1210'; VAL_MSG = 'This is a sample exception message.';
var
  f : IFormatterResponse;
  strm : TMemoryStream;
  callNode : TDOMElement;
  faultNode, faultStruct, tmpNode : TDOMNode;
  doc : TXMLDocument;
  eltName : string;
  excpt_Obj : EXmlRpcException;
  excpt_code, excpt_msg : string;
begin
  f := CreateFormatter();
  f.BeginExceptionList(VAL_CODE,VAL_MSG);
  f.EndExceptionList();
  strm := TMemoryStream.Create();
  try
    f.SaveToStream(strm);strm.SaveToFile('TTest_XmlRpcFormatterExceptionBlock.ExceptBlock.xml');
    strm.Position := 0;
    ReadXMLFile(doc,strm);
    callNode := doc.DocumentElement;
    if not SameText(base_xmlrpc_formatter.sMETHOD_RESPONSE,callNode.NodeName) then
      Check(False,Format('XML root node must be "%s".',[base_xmlrpc_formatter.sMETHOD_RESPONSE]));

      faultNode := FindNode(callNode,base_xmlrpc_formatter.sFAULT);
      if ( faultNode = nil ) then begin
        Check(False,Format('Invalid XmlRPC response message, "%s" or "%s" are not present.',[base_xmlrpc_formatter.sPARAMS,base_xmlrpc_formatter.sFAULT]));
      end;
      tmpNode := FindNode(faultNode,base_xmlrpc_formatter.sVALUE);
      if ( tmpNode = nil ) then begin
        Check(False,Format('Invalid XmlRPC fault response message, "%s"  is not present.',[base_xmlrpc_formatter.sVALUE]));
      end;
      faultStruct := FindNode(tmpNode,XmlRpcDataTypeNames[xdtStruct]);
      if ( faultStruct = nil ) then begin
        Check(False,Format('Invalid XmlRPC fault response message, "%s"  is not present.',[XmlRpcDataTypeNames[xdtStruct]]));
      end;
      tmpNode := loc_FindNode(faultStruct,base_xmlrpc_formatter.sFAULT_CODE);
      if ( tmpNode = nil ) then begin
        Check(False,Format('Invalid XmlRPC fault response message, "%s"  is not present.',[base_xmlrpc_formatter.sFAULT_CODE]));
      end;
      excpt_code := tmpNode.FirstChild.NodeValue;
      CheckEquals(VAL_CODE,excpt_code,base_xmlrpc_formatter.sFAULT_STRING);
      tmpNode := loc_FindNode(faultStruct,base_xmlrpc_formatter.sFAULT_STRING);
      if ( tmpNode = nil ) then begin
        Check(False,Format('Invalid XmlRPC fault response message, "%s"  is not present.',[base_xmlrpc_formatter.sFAULT_STRING]));
      end;
      excpt_msg := tmpNode.FirstChild.NodeValue;
      CheckEquals(VAL_MSG,excpt_msg,base_xmlrpc_formatter.sFAULT_STRING);
  finally
    FreeAndNil(strm);
  end;
end;

procedure TTest_XmlRpcFormatterExceptionBlock.ExceptBlock_client();
const
  VAL_CODE = '1210'; VAL_MSG = 'This is a sample exception message.';
  VAL_STREAM =
'<?xml version="1.0"?> ' +
' <methodResponse> ' +
 ' <fault> ' +
   ' <value> ' +
     ' <struct> ' +
       ' <member> ' +
         ' <name>faultCode</name> ' +
         ' <value> ' +
           ' <int>' + VAL_CODE + '</int> ' +
         ' </value> ' +
       ' </member> ' +
       ' <member> ' +
         ' <name>faultString</name> ' +
         ' <value> ' +
           ' <string>' + VAL_MSG + '</string> ' +
         ' </value> ' +
       ' </member> ' +
     ' </struct> ' +
   ' </value> ' +
 ' </fault>  ' +
' </methodResponse>';
var
  f : IFormatterClient;
  strm : TStringStream;
  excpt_code, excpt_msg : string;
begin
  excpt_code := '';
  excpt_msg := '';
  f := CreateFormatterClient();
  strm := TStringStream.Create(VAL_STREAM);
  try
    strm.Position := 0;
    f.LoadFromStream(strm);
    try
      f.BeginCallRead(nil);
      Check(False,'BeginCallRead() should raise an exception.');
    except
      on e : EXmlRpcException do begin
        excpt_code := e.FaultCode;
        excpt_msg := e.FaultString;
      end;
    end;
    CheckEquals(VAL_CODE,excpt_code,'faultCode');
    CheckEquals(VAL_MSG,excpt_msg,'faultString');
  finally
    FreeAndNil(strm);
  end;
end;

{ TTest_BinaryFormatterExceptionBlock }

function TTest_BinaryFormatterExceptionBlock.CreateFormatter() : IFormatterResponse;
begin
  Result := server_binary_formatter.TBinaryFormatter.Create() as IFormatterResponse;
end;

function TTest_BinaryFormatterExceptionBlock.CreateFormatterClient() : IFormatterClient;
begin
  Result := binary_formatter.TBinaryFormatter.Create() as IFormatterClient;
end;

function loc_FindObj(const AOwner: PDataBuffer; const AName : TDataName) : PDataBuffer;
Var
  p : PObjectBufferItem;
Begin
  Assert(AOwner^.DataType >= dtObject);
  Result := Nil;
   p:= AOwner^.ObjectData^.Head;
  While Assigned(p) Do Begin
    If AnsiSameText(AName,p^.Data^.Name) Then Begin
      Result := p^.Data;
      Exit;
    End;
    p := p^.Next;
  End;
End;

procedure TTest_BinaryFormatterExceptionBlock.ExceptBlock_server();
const VAL_CODE = '1210'; VAL_MSG = 'This is a sample exception message.';
var
  f : IFormatterResponse;
  strm : TMemoryStream;
  root, bodyNode, faultNode, tmpNode : PDataBuffer;
  excpt_code, excpt_msg : string;
begin
  root := nil;
  f := CreateFormatter();
  f.BeginExceptionList(VAL_CODE,VAL_MSG);
  f.EndExceptionList();
  strm := TMemoryStream.Create();
  try
    f.SaveToStream(strm);
    strm.Position := 0;
    root := LoadObjectFromStream(CreateBinaryReader(strm));
    Check(Assigned(root));
    CheckEquals(Ord(dtObject), Ord(root^.DataType),'root^.DataType');
    Check(Assigned(root^.ObjectData),'root^.ObjectData');
    CheckEquals(False,root^.ObjectData^.NilObject,'root^.NilObject');
    Check(root^.ObjectData^.Count > 0, 'root^.Count');
      bodyNode := root^.ObjectData^.Head^.Data;
      Check(Assigned(bodyNode),'body');
      CheckEquals(Ord(dtObject), Ord(bodyNode^.DataType),'body.DataType');
      CheckEquals(False,bodyNode^.ObjectData^.NilObject,'body.NilObject');
      Check(bodyNode^.ObjectData^.Count > 0, 'body.Count');

        faultNode := bodyNode^.ObjectData^.Head^.Data;
        Check(Assigned(faultNode),'fault');
        CheckEquals(Ord(dtObject), Ord(faultNode^.DataType),'fault.DataType');
        CheckEquals(False,faultNode^.ObjectData^.NilObject,'fault.NilObject');
        Check(faultNode^.ObjectData^.Count > 0, 'fault.Count');

        tmpNode := loc_FindObj(faultNode,'faultcode');
        Check(Assigned(tmpNode),'faultcode');
        CheckEquals(Ord(dtString), Ord(tmpNode^.DataType),'faultcode.DataType');
        excpt_code := tmpNode^.StrData^.Data;
        CheckEquals(VAL_CODE,excpt_code,'faultCode');
        
        tmpNode := loc_FindObj(faultNode,'faultstring');
        Check(Assigned(tmpNode),'faultstring');
        CheckEquals(Ord(dtString), Ord(tmpNode^.DataType),'faultstring.DataType');
        excpt_msg := tmpNode^.StrData^.Data;
        CheckEquals(VAL_MSG,excpt_msg,'faultString');
  finally
    FreeAndNil(strm);
    ClearObj(root);
  end;
end;

procedure TTest_BinaryFormatterExceptionBlock.ExceptBlock_client();
const
  VAL_CODE = '1210'; VAL_MSG = 'This is a sample exception message.';
var
  f : IFormatterClient;
  strm : TMemoryStream;
  root, bodyNode, faultNode, tmpNode : PDataBuffer;
  excpt_code, excpt_msg : string;
  locStore : IDataStore;
begin
  excpt_code := '';
  excpt_msg := '';
  root := CreateObjBuffer(dtObject,'ROOT');
  try
      bodyNode := CreateObjBuffer(dtObject,'Body',root);
        faultNode := CreateObjBuffer(dtObject,'Fault',bodyNode);
          CreateObjBuffer(dtString,'faultCode',faultNode)^.StrData^.Data := VAL_CODE;
          CreateObjBuffer(dtString,'faultString',faultNode)^.StrData^.Data := VAL_MSG;
    f := CreateFormatterClient();
    strm := TMemoryStream.Create();
    try
      locStore := CreateBinaryWriter(strm);
      SaveObjectToStream(root,locStore);
      locStore := nil;
      strm.Position := 0;
      f.LoadFromStream(strm);
      try
        f.BeginCallRead(nil);
        Check(False,'BeginCallRead() should raise an exception.');
      except
        on e : EBinaryException do begin
          excpt_code := e.FaultCode;
          excpt_msg := e.FaultString;
        end;
      end;
      CheckEquals(VAL_CODE,excpt_code,'faultCode');
      CheckEquals(VAL_MSG,excpt_msg,'faultString');
    finally
      FreeAndNil(strm);
    end;
  finally
    ClearObj(root);
  end;
end;

procedure TTestFormatter.test_GetFormaterName();
var
  f : IFormatterBase;
begin
  f := CreateFormatter(TypeInfo(TClass_A));
  CheckEquals(Self.GetFormaterName(),f.GetFormatName());
end;

{ TTest_TStringBufferRemotable }

procedure TTest_TStringBufferRemotable.Assign();
const ITER = 100;
var
  a, b : TStringBufferRemotable;
  i : Integer;
begin
  b := nil;
  a := TStringBufferRemotable.Create();
  try
    b := TStringBufferRemotable.Create();
    CheckEquals(a.Data,b.Data);
    for i := 0 to ITER do begin
      a.Data := RandomValue(i);
      b.Assign(a);
      CheckEquals(a.Data,b.Data);
    end;
    a.Data := '';
    b.Assign(a);
    CheckEquals(a.Data,b.Data);
  finally
    b.Free();
    a.Free();
  end;
end;

initialization
  RegisterStdTypes();
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TTestEnum),'TTestEnum').RegisterExternalPropertyName('teOne', '1');
    GetTypeRegistry().ItemByTypeInfo[TypeInfo(TTestEnum)].RegisterExternalPropertyName('teThree', 'Three-external-name');
    
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_Int),'TClass_Int').RegisterExternalPropertyName('Val_8U','U8');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_Enum),'TClass_Enum');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_A),'TClass_A');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_B),'TClass_B');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_Float),'TClass_Float');

  GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexInt32SContent),'T_ComplexInt32SContent');
    GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexInt32UContent),'T_ComplexInt32UContent');

  GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexInt16SContent),'T_ComplexInt16SContent');
    GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexInt16UContent),'T_ComplexInt16UContent');

  GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexFloatExtendedContent),'T_ComplexFloatExtendedContent');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexFloatDoubleContent),'T_ComplexFloatDoubleContent');

  TClass_CplxSimpleContent.RegisterAttributeProperty('Elt_Exemple');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_CplxSimpleContent),'TClass_CplxSimpleContent').RegisterExternalPropertyName('Elt_Exemple', 'published');

  with GetTypeRegistry().Register(sWST_BASE_NS,TypeInfo(TEmbeddedArrayOfStringRemotable),'TEmbeddedArrayOfStringRemotable') do begin
    RegisterExternalPropertyName(sARRAY_ITEM,'abc');
    RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  end;
  with GetTypeRegistry().Register(sWST_BASE_NS,TypeInfo(TArrayOfStringRemotableSample),'TArrayOfStringRemotableSample') do begin
    RegisterExternalPropertyName(sARRAY_ITEM,'OI');
    RegisterExternalPropertyName(sARRAY_STYLE,sScoped);
  end;

  GetTypeRegistry().Register(sWST_BASE_NS,TypeInfo(TTestSmallRecord),'TTestSmallRecord').RegisterExternalPropertyName('__FIELDS__','fieldSmallint;fieldWord;fieldString');
{$IFNDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TTestSmallRecord)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(TypeInfo(TTestSmallRecord)),GetTypeRegistry().ItemByTypeInfo[TypeInfo(TTestSmallRecord)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
{$IFDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TTestSmallRecord)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(__TTestSmallRecord_TYPEINFO_FUNC__()),GetTypeRegistry().ItemByTypeInfo[TypeInfo(TTestSmallRecord)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}

  GetTypeRegistry().Register(sWST_BASE_NS,TypeInfo(TTestRecord),'TTestRecord').RegisterExternalPropertyName('__FIELDS__','fieldByte;fieldShortInt;fieldSmallint;fieldWord;fieldInteger;fieldLongWord;fieldInt64;fieldQWord;fieldComp;fieldSingle;fieldDouble;fieldExtended;fieldCurrency;fieldBoolean;fieldString;fieldRecord');
{$IFNDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TTestRecord)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(TypeInfo(TTestRecord)),GetTypeRegistry().ItemByTypeInfo[TypeInfo(TTestRecord)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
{$IFDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TTestRecord)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(__TTestRecord_TYPEINFO_FUNC__()),GetTypeRegistry().ItemByTypeInfo[TypeInfo(TTestRecord)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
  RegisterAttributeProperty(TypeInfo(TTestSmallRecord),'fieldWord');
  RegisterAttributeProperty(TypeInfo(TTestRecord),'fieldWord');

{$IFDEF FPC}
  RegisterTest(TTestArray);
  RegisterTest(TTestSOAPFormatter);
  RegisterTest(TTestBinaryFormatter);
  RegisterTest(TTest_TBaseComplexRemotable);
  RegisterTest(TTestSOAPFormatterAttributes);
  RegisterTest(TTestBinaryFormatterAttributes);

  RegisterTest(TTestXmlRpcFormatterAttributes);
  RegisterTest(TTestXmlRpcFormatter);
  RegisterTest(TTest_SoapFormatterExceptionBlock);
  RegisterTest(TTest_XmlRpcFormatterExceptionBlock);
  RegisterTest(TTest_BinaryFormatterExceptionBlock);
  RegisterTest(TTest_TStringBufferRemotable);
{$ELSE}
  RegisterTest(TTestArray.Suite);
  RegisterTest(TTestSOAPFormatter.Suite);
  RegisterTest(TTestBinaryFormatter.Suite);
  RegisterTest(TTest_TBaseComplexRemotable.Suite);
  RegisterTest(TTestSOAPFormatterAttributes.Suite);
  RegisterTest(TTestBinaryFormatterAttributes.Suite);

  RegisterTest(TTestXmlRpcFormatterAttributes.Suite);
  RegisterTest(TTestXmlRpcFormatter.Suite);
  RegisterTest(TTest_SoapFormatterExceptionBlock.Suite);
  RegisterTest(TTest_XmlRpcFormatterExceptionBlock.Suite);
  RegisterTest(TTest_BinaryFormatterExceptionBlock.Suite);
  RegisterTest(TTest_TStringBufferRemotable.Suite);
{$ENDIF}


end.
