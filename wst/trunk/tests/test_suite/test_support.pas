{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_support;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  TypInfo,
  wst_types, base_service_intf, imp_utils, test_suite_utils, date_utils;

type

  TTestEnum = ( teOne, teTwo, teThree, teFour );
  
  { TClass_B }

  TClass_B = class(TBaseComplexRemotable)
  private
    FVal_32S: LongInt;
    FVal_32U: LongWord;
    FVal_64S: Int64;
    FVal_64U: QWord;
    FVal_8S: ShortInt;
    FVal_8U: Byte;
    FVal_16S: SmallInt;
    FVal_16U: Word;
    FVal_Bool: Boolean;
    FVal_Enum: TTestEnum;
    FVal_String: string;
  Published
    property Val_Enum : TTestEnum Read FVal_Enum Write FVal_Enum;
    property Val_Bool : Boolean Read FVal_Bool Write FVal_Bool;
    property Val_String : string Read FVal_String Write FVal_String;

    property Val_8U : Byte Read FVal_8U Write FVal_8U;
      property Val_8S : ShortInt Read FVal_8S Write FVal_8S;
    property Val_16U : Word Read FVal_16U Write FVal_16U;
      property Val_16S : SmallInt Read FVal_16S Write FVal_16S;
    property Val_32U : LongWord Read FVal_32U Write FVal_32U;
      property Val_32S : LongInt Read FVal_32S Write FVal_32S;
    property Val_64U : QWord Read FVal_64U Write FVal_64U;
      property Val_64S : Int64 Read FVal_64S Write FVal_64S;
  End;
  
  { TClass_A }

  TClass_A = class(TBaseComplexRemotable)
  private
    FVal_32S: LongInt;
    FVal_32U: LongWord;
    FVal_64S: Int64;
    FVal_64U: QWord;
    FVal_8S: ShortInt;
    FVal_8U: Byte;
    FVal_16S: SmallInt;
    FVal_16U: Word;
    FVal_Bool: Boolean;
    FVal_Enum: TTestEnum;
    FVal_Obj: TClass_B;
    FVal_String: string;
    FVal_StringArray: TArrayOfStringRemotable;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  Published
    property Val_Enum : TTestEnum Read FVal_Enum Write FVal_Enum;
    property Val_Bool : Boolean Read FVal_Bool Write FVal_Bool;
    property Val_String : string Read FVal_String Write FVal_String;

    property Val_8U : Byte Read FVal_8U Write FVal_8U;
      property Val_8S : ShortInt Read FVal_8S Write FVal_8S;
    property Val_16U : Word Read FVal_16U Write FVal_16U;
      property Val_16S : SmallInt Read FVal_16S Write FVal_16S;
    property Val_32U : LongWord Read FVal_32U Write FVal_32U;
      property Val_32S : LongInt Read FVal_32S Write FVal_32S;
    property Val_64U : QWord Read FVal_64U Write FVal_64U;
      property Val_64S : Int64 Read FVal_64S Write FVal_64S;
      
    property Val_Obj : TClass_B read FVal_Obj write FVal_Obj;
    property Val_StringArray : TArrayOfStringRemotable read FVal_StringArray write FVal_StringArray;
  End;

  TArrayOfClass_A = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TClass_A;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TClass_A Read GetItem;Default;
  end;

  { TTest_TBaseComplexRemotable }

  TTest_TBaseComplexRemotable = class(TWstBaseTest)
  protected
    procedure Compare(const a,b : TClass_A);overload;
    procedure Compare(const a,b : TClass_B);overload;
  published
    procedure test_Assign();
    procedure Equal();
  end;
  
  { TTest_TBaseArrayRemotable }

  TTest_TBaseArrayRemotable = class(TWstBaseTest)
  protected
    class function CreateArray() : TBaseArrayRemotable;virtual;abstract;
    class function GetTypeInfo() : PTypeInfo;virtual;abstract;
  published
    procedure Length_procs();
    procedure GetItemTypeInfo();
  end;
  
  { TTest_TArrayOfStringRemotable }

  TTest_TArrayOfStringRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
    procedure Equal();
  end;

  { TTest_TArrayOfBooleanRemotable }

  TTest_TArrayOfBooleanRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;

  { TTest_TArrayOfInt8URemotable }

  TTest_TArrayOfInt8URemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;
  
  { TTest_TArrayOfInt8SRemotable }

  TTest_TArrayOfInt8SRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;
  
  { TTest_TArrayOfInt16SRemotable }

  TTest_TArrayOfInt16SRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;

  { TTest_TArrayOfInt16URemotable }

  TTest_TArrayOfInt16URemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;
  
  { TTest_TArrayOfInt32URemotable }

  TTest_TArrayOfInt32URemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;
  
  { TTest_TArrayOfInt32SRemotable }

  TTest_TArrayOfInt32SRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;

  { TTest_TArrayOfInt64SRemotable }

  TTest_TArrayOfInt64SRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;

  { TTest_TArrayOfInt64URemotable }

  TTest_TArrayOfInt64URemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;
  
  { TTest_TArrayOfFloatSingleRemotable }

  TTest_TArrayOfFloatSingleRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;

  { TTest_TArrayOfFloatDoubleRemotable }

  TTest_TArrayOfFloatDoubleRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;

  { TTest_TArrayOfFloatExtendedRemotable }

  TTest_TArrayOfFloatExtendedRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;
  
  { TTest_TArrayOfFloatCurrencyRemotable }

  TTest_TArrayOfFloatCurrencyRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
  published
    procedure test_Assign();
    procedure GetItemAndSetItem();
  end;
  
  { TTest_TBaseObjectArrayRemotable }

  TTest_TBaseObjectArrayRemotable = class(TTest_TBaseArrayRemotable)
  protected
    class function CreateArray() : TBaseArrayRemotable;override;
    class function GetTypeInfo() : PTypeInfo;override;
    class procedure FillRandomItem(const AItem : TBaseRemotable);
    procedure CompareItem(const A,B : TBaseRemotable);
    procedure Compare(const a,b : TClass_A);overload;
    procedure Compare(const a,b : TClass_B);overload;
  published
    procedure test_Assign();
    procedure Equal();
  end;
  
  { TTest_TDateTimeRemotable }

  TTest_TDateTimeRemotable = class(TWstBaseTest)
  published
    procedure FormatDate();
    procedure FormatDate_ZERO();
    procedure ParseDate();
    procedure Assign();
    procedure Equal();
    procedure AsDate();
    procedure AsUTCDate();
    procedure HourOffset();
    procedure HourOffset_invalid_values();
    procedure MinuteOffset();
    procedure MinuteOffset_invalid_values();
    procedure Year();
  end;

  { TTest_TDateTimeRemotable }

  TTest_TDateRemotable = class(TWstBaseTest)
  published
    procedure FormatDate();
    procedure FormatDate_ZERO();
    procedure ParseDate();
    procedure Assign();
    procedure Equal();
    procedure AsDate();
    procedure AsUTCDate();
    procedure HourOffset();
    procedure HourOffset_invalid_values();
    procedure MinuteOffset();
    procedure MinuteOffset_invalid_values();
    procedure Year();
  end; 
    
  { TTest_TDurationRemotable }

  TTest_TDurationRemotable = class(TTestCase)
  published
    procedure Clear();
    procedure AsString_empty();
    procedure AsString_not_empty();
    procedure AsString_date_only();
    procedure AsString_time_only();
    procedure Parse_non_empty();
    procedure Parse_time_only();
    procedure Parse_zero();
    procedure parse_negative();
    procedure parse_invalid_1();
    procedure parse_invalid_2();
    procedure parse_invalid_3();
    procedure parse_invalid_4();
    procedure parse_invalid_5();
    procedure parse_empty();
  end;

  { TTest_TTimeRemotable }

  TTest_TTimeRemotable = class(TWstBaseTest)
  protected
{$IFDEF FPC}
    class procedure CheckEquals(expected, actual: TTimeRec; msg: string = ''); overload;
{$ENDIF FPC}
{$IFDEF WST_DELPHI}
    procedure CheckEquals(expected, actual: TTimeRec; msg: string = ''); overload;
{$ENDIF WST_DELPHI}
  published
    procedure ToStr();
    procedure Parse();
    procedure Parse_millisecond();
    procedure Parse_offset_1();
    procedure Parse_offset_2();
    procedure Data;
    procedure Equal();
    procedure Assign();
    procedure Clear();
  end;
  
  { TTest_TStringBufferRemotable }

  TTest_TStringBufferRemotable = class(TTestCase)
  published
    procedure test_Assign();
    procedure Equal();
  end;

  { TTest_TAbstractEncodedStringRemotable }

  TTest_TAbstractEncodedStringRemotable = class(TWstBaseTest)
  protected
    class function CreateObject() : TAbstractEncodedStringRemotable; virtual; abstract;
    class function EncodeData(const AValue : TByteDynArray) : string; overload; virtual; abstract;
    class function EncodeData(const AValue : TBinaryString) : string; overload;
  published
    procedure test_Assign();
    procedure Equal();
    procedure SetBinaryData();
    procedure SetEncodedString();
    procedure LoadFromStream();
    procedure LoadFromFile();
    procedure SaveToStream();
    procedure SaveToFile();
  end;

  { TTest_TBase64StringRemotable }

  TTest_TBase64StringRemotable = class(TTest_TAbstractEncodedStringRemotable)
  protected
    class function CreateObject() : TAbstractEncodedStringRemotable; override;
    class function EncodeData(const AValue : TByteDynArray) : string; override;
  end;

  { TTest_TBase16StringRemotable }

  TTest_TBase16StringRemotable = class(TTest_TAbstractEncodedStringRemotable)
  protected
    class function CreateObject() : TAbstractEncodedStringRemotable; override;
    class function EncodeData(const AValue : TByteDynArray) : string; override;
  end;

  { TTest_TBase64StringExtRemotable }

  { TTest_TAbstractEncodedStringExtRemotable }

  TTest_TAbstractEncodedStringExtRemotable = class(TWstBaseTest)
  protected
    class function CreateObject() : TAbstractEncodedStringExtRemotable; virtual; abstract;
    class function EncodeData(const AValue : TByteDynArray) : string; overload; virtual; abstract;
    class function EncodeData(const AValue : TBinaryString) : string; overload;
  published
    procedure Equal();
    procedure test_Assign();
    procedure SetBinaryData();
    procedure SetEncodedString();
    procedure LoadFromStream();
    procedure LoadFromFile();
    procedure SaveToStream();
    procedure SaveToFile();
  end;
  
  TTest_TBase64StringExtRemotable = class(TTest_TAbstractEncodedStringExtRemotable)
  protected
    class function CreateObject() : TAbstractEncodedStringExtRemotable; override;
    class function EncodeData(const AValue : TByteDynArray) : string; override;
  end;

  { TTest_TBase16StringExtRemotable }

  TTest_TBase16StringExtRemotable = class(TTest_TAbstractEncodedStringExtRemotable)
  protected
    class function CreateObject() : TAbstractEncodedStringExtRemotable; override;
    class function EncodeData(const AValue : TByteDynArray) : string; override;
  end;

  { TClass_A_CollectionRemotable }

  TClass_A_CollectionRemotable = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex : PtrInt) : TClass_A;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TClass_A;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : PtrInt): TClass_A;{$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:PtrInt] : TClass_A read GetItem;default;
  end;
  
  { TTest_TObjectCollectionRemotable }

  TTest_TObjectCollectionRemotable = class(TTestCase)
  published
    procedure GetItemTypeInfo();
    procedure Add();
    procedure Delete();
    procedure Equal();
    procedure test_Assign();
    procedure Exchange();
    procedure IndexOf();
  end;
  
  { TTest_Procedures }

  TTest_Procedures = class(TWstBaseTest)
  published
    procedure test_LoadBufferFromStream();
    procedure test_LoadBufferFromFile();
  end;
  
implementation
uses Math, basex_encode, DateUtils;

function RandomValue(const AMaxlen: Integer): TBinaryString;
var
  k : Integer;
begin
  SetLength(Result,AMaxlen);
  for k := 1 to AMaxlen do begin
    Result[k] := AnsiChar((Random(Ord(High(AnsiChar)))));
  end;
end;

function RandomBytesValue(const AMaxlen: Integer): TByteDynArray;
var
  k : Integer;
begin
  SetLength(Result,AMaxlen);
  for k := 0 to ( AMaxlen - 1 ) do begin
    Result[k] := RandomRange(Low(Byte),High(Byte));
  end;
end;

{ TArrayOfClass_A }

function TArrayOfClass_A.GetItem(AIndex: Integer): TClass_A;
begin
  Result := Inherited GetItem(AIndex) As TClass_A;
end;

class function TArrayOfClass_A.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TClass_A;
end;

{ TTest_TBaseArrayRemotable }

procedure TTest_TBaseArrayRemotable.Length_procs();
const ITER : Integer = 1000;
var
  localObj : TBaseArrayRemotable;
  i : Integer;
  ok : Boolean;
begin
  localObj := CreateArray();
  try
    CheckEquals(0,localObj.Length);
    CheckEquals(0,localObj.Length);

    localObj.SetLength(0);
    CheckEquals(0,localObj.Length);

    ok := False;
    try
      localObj.SetLength(-10);
    except
      on e : EBaseRemoteException do
        ok := True;
    end;
    CheckEquals(True,ok);

    localObj.SetLength(ITER);
      CheckEquals(ITER,localObj.Length);

    for i := 0 to ITER do begin
      localObj.SetLength(i);
      CheckEquals(i,localObj.Length);
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

procedure TTest_TBaseArrayRemotable.GetItemTypeInfo();
var
  localObj : TBaseArrayRemotable;
  a, b : PTypeInfo;
begin
  localObj := CreateArray();
  try
    a := GetTypeInfo();
    b := localObj.GetItemTypeInfo();
    CheckEquals(a^.Name,b^.Name);
    CheckEquals(Ord(a^.Kind),Ord(b^.Kind));
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfStringRemotable }

class function TTest_TArrayOfStringRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfStringRemotable.Create();
end;

class function TTest_TArrayOfStringRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(String);
end;

procedure TTest_TArrayOfStringRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfStringRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfStringRemotable.Create();
  try
    b := TArrayOfStringRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := RandomValue(Random(500));
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],b[k]);
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(b[k],a[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfStringRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfStringRemotable;
  i, j, k : Integer;
  a : array of string;
begin
  localObj := TArrayOfStringRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := RandomValue(Random(500));
        end;
        
        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

procedure TTest_TArrayOfStringRemotable.Equal();
const ITER : Integer = 100;
var
  a, b : TArrayOfStringRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfStringRemotable.Create();
  try
    b := TArrayOfStringRemotable.Create();

    CheckEquals(True,a.Equal(b));
    CheckEquals(True,b.Equal(a));

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      b.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := RandomValue(1 + Random(500));
          CheckEquals(False,a.Equal(b));
          CheckEquals(False,b.Equal(a));
          b[k] := a[k];
          CheckEquals(True,a.Equal(b));
          CheckEquals(True,b.Equal(a));
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

{ TTest_TBaseComplexRemotable }

procedure TTest_TBaseComplexRemotable.Compare(const a, b: TClass_A);
var
  i, c : Integer;
begin
  CheckEquals(True, a.Val_64S = b.Val_64S);
  CheckEquals(True, a.Val_64U = b.Val_64U);

  CheckEquals(a.Val_32S,b.Val_32S);
  CheckEquals(a.Val_32U,b.Val_32U);
  
  CheckEquals(a.Val_16S,b.Val_16S);
  CheckEquals(a.Val_16U,b.Val_16U);
  
  CheckEquals(a.Val_8S,b.Val_8S);
  CheckEquals(a.Val_8U,b.Val_8U);
  
  CheckEquals(a.Val_String,b.Val_String);
  CheckEquals(a.Val_Bool,b.Val_Bool);
  CheckEquals(Ord(a.Val_Enum),Ord(b.Val_Enum));
  
  Compare(a.Val_Obj,b.Val_Obj);
  Check(
    ( ( a.Val_StringArray <> nil ) and ( b.Val_StringArray <> nil ) ) or
    ( ( a.Val_StringArray = nil ) and ( b.Val_StringArray = nil ) )
  );
  if ( a.Val_StringArray <> nil ) then begin
    c := a.Val_StringArray.Length;
    for i := 0 to Pred(c) do begin
      CheckEquals(a.Val_StringArray[i],b.Val_StringArray[i]);
    end;
  end;
end;

procedure TTest_TBaseComplexRemotable.Compare(const a, b: TClass_B);
begin
  CheckEquals(a.Val_64S,b.Val_64S);
  CheckEquals(a.Val_64U,b.Val_64U);

  CheckEquals(a.Val_32S,b.Val_32S);
  CheckEquals(a.Val_32U,b.Val_32U);

  CheckEquals(a.Val_16S,b.Val_16S);
  CheckEquals(a.Val_16U,b.Val_16U);

  CheckEquals(a.Val_8S,b.Val_8S);
  CheckEquals(a.Val_8U,b.Val_8U);

  CheckEquals(a.Val_String,b.Val_String);
  CheckEquals(a.Val_Bool,b.Val_Bool);
  CheckEquals(Ord(a.Val_Enum),Ord(b.Val_Enum));
end;

procedure TTest_TBaseComplexRemotable.test_Assign();
const ITER = 100;
var
  a, b : TClass_A;
  i : Integer;
begin
  a := TClass_A.Create();
  try
    b := TClass_A.Create();
    b.Assign(a);
    Compare(a,b);
    
    a.Val_64S := Random(1210);
    a.Val_64U := Random(1210);
    
    a.Val_32S := Random(1210);
    a.Val_32U := Random(1210);

    a.Val_16S := Random(1210);
    a.Val_16U := Random(1210);

    a.Val_8S := Random(123);
    a.Val_8U := Random(123);
    
    a.Val_Enum := teThree;
    a.Val_Bool := True;
    a.Val_String := RandomValue(100);
    
    a.Val_Obj.Val_64S := Random(1210);
    a.Val_Obj.Val_64U := Random(1210);

    a.Val_Obj.Val_32S := Random(1210);
    a.Val_Obj.Val_32U := Random(1210);

    a.Val_Obj.Val_16S := Random(1210);
    a.Val_Obj.Val_16U := Random(1210);

    a.Val_Obj.Val_8S := Random(123);
    a.Val_Obj.Val_8U := Random(123);

    a.Val_Obj.Val_Enum := teTwo;
    a.Val_Obj.Val_Bool := True;
    a.Val_Obj.Val_String := RandomValue(250);
    
    a.Val_StringArray.SetLength(ITER);
    for i := 0 to Pred(ITER) do begin
      a.Val_StringArray[i] := RandomValue(Random(123));
    end;
    
    b.Assign(a);
    Compare(a,b);
  finally
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

procedure TTest_TBaseComplexRemotable.Equal();
const ITER = 100;
var
  a, b : TClass_A;
  cc : TClass_B;
  i : Integer;
begin
  b:= nil;
  cc := nil;
  a := TClass_A.Create();
  try
    b := TClass_A.Create();
    CheckEquals(True,a.Equal(b));
    CheckEquals(True,b.Equal(a));
    CheckEquals(False,a.Equal(nil));
    
    cc := TClass_B.Create();
    CheckEquals(False,a.Equal(cc));
    CheckEquals(False,cc.Equal(a));
    
    a.Val_64S := Random(1210);
    a.Val_64U := Random(1210);
      b.Val_64S := a.Val_64S;
      b.Val_64U := a.Val_64U;

    CheckEquals(True,a.Equal(b));
    CheckEquals(True,b.Equal(a));

    a.Val_32S := Random(1210);
    a.Val_32U := Random(1210);

    a.Val_16S := Random(1210);
    a.Val_16U := Random(1210);

    a.Val_8S := Random(123);
    a.Val_8U := Random(123);

    a.Val_Enum := teThree;
    a.Val_Bool := True;
    a.Val_String := RandomValue(100);

    a.Val_Obj.Val_64S := Random(1210);
    a.Val_Obj.Val_64U := Random(1210);

    a.Val_Obj.Val_32S := Random(1210);
    a.Val_Obj.Val_32U := Random(1210);

    a.Val_Obj.Val_16S := Random(1210);
    a.Val_Obj.Val_16U := Random(1210);

    a.Val_Obj.Val_8S := Random(123);
    a.Val_Obj.Val_8U := Random(123);

    a.Val_Obj.Val_Enum := teTwo;
    a.Val_Obj.Val_Bool := True;
    a.Val_Obj.Val_String := RandomValue(250);

    a.Val_StringArray.SetLength(ITER);
    for i := 0 to Pred(ITER) do begin
      a.Val_StringArray[i] := RandomValue(Random(123));
    end;
    CheckEquals(False,a.Equal(b));
    CheckEquals(False,b.Equal(a));

    b.Assign(a);
    CheckEquals(True,a.Equal(b));
    CheckEquals(True,b.Equal(a));
  finally
    FreeAndNil(cc);
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

{ TClass_A }

constructor TClass_A.Create();
begin
  inherited;
  FVal_Obj := TClass_B.Create();
  FVal_StringArray := TArrayOfStringRemotable.Create();
end;

procedure TClass_A.FreeObjectProperties();
begin
  FreeAndNil(FVal_StringArray);
  FreeAndNil(FVal_Obj);
  inherited FreeObjectProperties();
end;

{ TTest_TBaseObjectArrayRemotable }

class function TTest_TBaseObjectArrayRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfClass_A.Create();
end;

class function TTest_TBaseObjectArrayRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(TClass_A);
end;

class procedure TTest_TBaseObjectArrayRemotable.FillRandomItem(const AItem: TBaseRemotable);
var
  a : TClass_A;
  i, c : Integer;
begin
 Randomize();
  a := AItem as TClass_A;

    a.Val_64S := Random(1210);
    a.Val_64U := Random(1210);

    a.Val_32S := Random(1210);
    a.Val_32U := Random(1210);

    a.Val_16S := Random(1210);
    a.Val_16U := Random(1210);

    a.Val_8S := Random(123);
    a.Val_8U := Random(123);

    a.Val_Enum := teThree;
    a.Val_Bool := True;
    a.Val_String := RandomValue(100);

    a.Val_Obj.Val_64S := Random(1210);
    a.Val_Obj.Val_64U := Random(1210);

    a.Val_Obj.Val_32S := Random(1210);
    a.Val_Obj.Val_32U := Random(1210);

    a.Val_Obj.Val_16S := Random(1210);
    a.Val_Obj.Val_16U := Random(1210);

    a.Val_Obj.Val_8S := Random(123);
    a.Val_Obj.Val_8U := Random(123);

    a.Val_Obj.Val_Enum := teTwo;
    a.Val_Obj.Val_Bool := True;
    a.Val_Obj.Val_String := RandomValue(250);

    c := Random(200);
    a.Val_StringArray.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        a.Val_StringArray[i] := RandomValue(Random(500));
      end;
    end;
end;

procedure TTest_TBaseObjectArrayRemotable.CompareItem(const A, B: TBaseRemotable);
begin
  Compare(A as TClass_A, B as TClass_A);
end;

procedure TTest_TBaseObjectArrayRemotable.Compare(const a, b: TClass_A);
var
  i, c : Integer;
begin
  CheckEquals(a.Val_64S,b.Val_64S);
  CheckEquals(a.Val_64U,b.Val_64U);

  CheckEquals(a.Val_32S,b.Val_32S);
  CheckEquals(a.Val_32U,b.Val_32U);

  CheckEquals(a.Val_16S,b.Val_16S);
  CheckEquals(a.Val_16U,b.Val_16U);

  CheckEquals(a.Val_8S,b.Val_8S);
  CheckEquals(a.Val_8U,b.Val_8U);

  CheckEquals(a.Val_String,b.Val_String);
  CheckEquals(a.Val_Bool,b.Val_Bool);
  CheckEquals(Ord(a.Val_Enum),Ord(b.Val_Enum));

  Compare(a.Val_Obj,b.Val_Obj);
  Check(
    ( ( a.Val_StringArray <> nil ) and ( b.Val_StringArray <> nil ) ) or
    ( ( a.Val_StringArray = nil ) and ( b.Val_StringArray = nil ) )
  );
  if ( a.Val_StringArray <> nil ) then begin
    c := a.Val_StringArray.Length;
    for i := 0 to Pred(c) do begin
      CheckEquals(a.Val_StringArray[i],b.Val_StringArray[i]);
    end;
  end;
end;

procedure TTest_TBaseObjectArrayRemotable.Compare(const a, b: TClass_B);
begin
  CheckEquals(a.Val_64S,b.Val_64S);
  CheckEquals(a.Val_64U,b.Val_64U);

  CheckEquals(a.Val_32S,b.Val_32S);
  CheckEquals(a.Val_32U,b.Val_32U);

  CheckEquals(a.Val_16S,b.Val_16S);
  CheckEquals(a.Val_16U,b.Val_16U);

  CheckEquals(a.Val_8S,b.Val_8S);
  CheckEquals(a.Val_8U,b.Val_8U);

  CheckEquals(a.Val_String,b.Val_String);
  CheckEquals(a.Val_Bool,b.Val_Bool);
  CheckEquals(Ord(a.Val_Enum),Ord(b.Val_Enum));
end;

procedure TTest_TBaseObjectArrayRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TBaseObjectArrayRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := CreateArray() as TBaseObjectArrayRemotable;
  try
    b := CreateArray() as TBaseObjectArrayRemotable;
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(20);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          FillRandomItem(a.Item[k]);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CompareItem(a[k],b[k]);
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CompareItem(b[k],a[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TBaseObjectArrayRemotable.Equal();
const ITER : Integer = 100;
var
  a, b : TBaseObjectArrayRemotable;
  aa : TClass_A;
  i, j, k : Integer;
begin
  Randomize();
  aa := nil;
  a := CreateArray() as TBaseObjectArrayRemotable;
  try
    b := CreateArray() as TBaseObjectArrayRemotable;
    aa := TClass_A.Create();
    CheckEquals(True,a.Equal(b));
    CheckEquals(True,b.Equal(a));
    CheckEquals(False,a.Equal(nil));
    CheckEquals(False,a.Equal(aa));
    CheckEquals(False,aa.Equal(a));

    for i := 1 to ITER do begin
      Randomize();
      j := Random(20);
      FreeAndNil(a);
      FreeAndNil(b);
      a := CreateArray() as TBaseObjectArrayRemotable;
      b := CreateArray() as TBaseObjectArrayRemotable;
      a.SetLength(j);
      b.SetLength(j);
      if ( j > 0  ) then begin
        TClass_A(b.Item[0]).Val_String := 'azertyqwerty';
        for k := 0 to Pred(j) do begin
          //FillRandomItem(a.Item[k]);
          //FillRandomItem(b.Item[k]);
          //CheckEquals(False,b.Equal(a), '1111');
          //CheckEquals(False,a.Equal(b), '2222');
          b.Item[k].Assign(a.Item[k]);
          CheckEquals(True,a.Equal(b));
          CheckEquals(True,b.Equal(a));
        end;
      end;
    end;
  finally
    FreeAndNil(aa);
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;


{ TTest_TArrayOfBooleanRemotable }

class function TTest_TArrayOfBooleanRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result:= TArrayOfBooleanRemotable.Create();
end;

class function TTest_TArrayOfBooleanRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Boolean);
end;

procedure TTest_TArrayOfBooleanRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfBooleanRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfBooleanRemotable.Create();
  try
    b := TArrayOfBooleanRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := ( ( k mod 3 ) = 0 );
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],b[k]);
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(b[k],a[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfBooleanRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfBooleanRemotable;
  i, j, k : Integer;
  a : array of Boolean;
begin
  localObj := TArrayOfBooleanRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := ( ( k mod 5 ) = 1 );
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfInt8URemotable }

class function TTest_TArrayOfInt8URemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfInt8URemotable.Create();
end;

class function TTest_TArrayOfInt8URemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Byte);
end;

procedure TTest_TArrayOfInt8URemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfInt8URemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfInt8URemotable.Create();
  try
    b := TArrayOfInt8URemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := Random(High(Byte) - 1);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],b[k]);
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(b[k],a[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfInt8URemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt8URemotable;
  i, j, k : Integer;
  a : array of Byte;
begin
  Randomize();
  localObj := TArrayOfInt8URemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := Random(High(Byte) - 1 );
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfInt8SRemotable }

class function TTest_TArrayOfInt8SRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfInt8SRemotable.Create();
end;

class function TTest_TArrayOfInt8SRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result :=  TypeInfo(ShortInt);
end;

procedure TTest_TArrayOfInt8SRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfInt8SRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfInt8SRemotable.Create();
  try
    b := TArrayOfInt8SRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(ShortInt) - 1)
          else
            a[k] := -Random(High(ShortInt) - 2);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],b[k]);
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(b[k],a[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfInt8SRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt8SRemotable;
  i, j, k : Integer;
  a : array of ShortInt;
begin
  Randomize();
  localObj := TArrayOfInt8SRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := Random(High(ShortInt) -1);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfInt16SRemotable }

class function TTest_TArrayOfInt16SRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfInt16SRemotable.Create();
end;

class function TTest_TArrayOfInt16SRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(SmallInt);
end;

procedure TTest_TArrayOfInt16SRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfInt16SRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfInt16SRemotable.Create();
  try
    b := TArrayOfInt16SRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(SmallInt) -1)
          else
            a[k] := -Random(High(SmallInt) - 2);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],b[k]);
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(b[k],a[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfInt16SRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt16SRemotable;
  i, j, k : Integer;
  a : array of SmallInt;
begin
  Randomize();
  localObj := TArrayOfInt16SRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(SmallInt) -1 )
          else
            a[k] := -Random(High(SmallInt) -2);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfInt16URemotable }

class function TTest_TArrayOfInt16URemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfInt16URemotable.Create();
end;

class function TTest_TArrayOfInt16URemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Word);
end;

procedure TTest_TArrayOfInt16URemotable.test_Assign();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt16URemotable;
  i, j, k : Integer;
  a : array of Word;
begin
  Randomize();
  localObj := TArrayOfInt16URemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := Random(High(Word)-1);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

procedure TTest_TArrayOfInt16URemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt16URemotable;
  i, j, k : Integer;
  a : array of Word;
begin
  Randomize();
  localObj := TArrayOfInt16URemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := Random(High(Word)-1);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfInt32URemotable }

class function TTest_TArrayOfInt32URemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfInt32URemotable.Create();
end;

class function TTest_TArrayOfInt32URemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(LongWord);
end;

procedure TTest_TArrayOfInt32URemotable.test_Assign();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt32URemotable;
  i, j, k : Integer;
  a : array of LongWord;
begin
  Randomize();
  localObj := TArrayOfInt32URemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := Random(High(Word)-2);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

procedure TTest_TArrayOfInt32URemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt32URemotable;
  i, j, k : Integer;
  a : array of LongWord;
begin
  Randomize();
  localObj := TArrayOfInt32URemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := Random(High(Word)-1);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfInt32SRemotable }

class function TTest_TArrayOfInt32SRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfInt32SRemotable.Create();
end;

class function TTest_TArrayOfInt32SRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(LongInt);
end;

procedure TTest_TArrayOfInt32SRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfInt32SRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfInt32SRemotable.Create();
  try
    b := TArrayOfInt32SRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(LongInt) -1)
          else
            a[k] := -Random(High(LongInt) - 2);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],b[k]);
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          CheckEquals(b[k],a[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfInt32SRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt32SRemotable;
  i, j, k : Integer;
  a : array of LongInt;
begin
  Randomize();
  localObj := TArrayOfInt32SRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(LongInt) -1 )
          else
            a[k] := -Random(High(LongInt) -2);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          CheckEquals(a[k],localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfInt64SRemotable }

class function TTest_TArrayOfInt64SRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfInt64SRemotable.Create();
end;

class function TTest_TArrayOfInt64SRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Int64);
end;

procedure TTest_TArrayOfInt64SRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfInt64SRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfInt64SRemotable.Create();
  try
    b := TArrayOfInt64SRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Int64) -1)
          else
            a[k] := -Random(High(Int64) - 2);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(a[k] = b[k]);
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(b[k] = a[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfInt64SRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt64SRemotable;
  i, j, k : Integer;
  a : array of Int64;
begin
  Randomize();
  localObj := TArrayOfInt64SRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Int64) -1 )
          else
            a[k] := -Random(High(Int64) -2);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          Check(a[k] = localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfInt64URemotable }

class function TTest_TArrayOfInt64URemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfInt64URemotable.Create();
end;

class function TTest_TArrayOfInt64URemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(QWord);
end;

procedure TTest_TArrayOfInt64URemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfInt64URemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfInt64URemotable.Create();
  try
    b := TArrayOfInt64URemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := Random(High(Integer) -1);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(a[k] = b[k]);
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(b[k] = a[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfInt64URemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfInt64URemotable;
  i, j, k : Integer;
  a : array of QWord;
begin
  Randomize();
  localObj := TArrayOfInt64URemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          a[k] := Random(High(Integer) -1 );
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          Check(a[k] = localObj[k]);
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfFloatSingleRemotable }

class function TTest_TArrayOfFloatSingleRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfFloatSingleRemotable.Create();
end;

class function TTest_TArrayOfFloatSingleRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Single);
end;

procedure TTest_TArrayOfFloatSingleRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfFloatSingleRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfFloatSingleRemotable.Create();
  try
    b := TArrayOfFloatSingleRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Int64) -1)
          else
            a[k] := -Random(High(Int64) - 2);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(SameValue(a[k],b[k]));
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(SameValue(b[k],a[k]));
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfFloatSingleRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfFloatSingleRemotable;
  i, j, k : Integer;
  a : array of Single;
begin
  Randomize();
  localObj := TArrayOfFloatSingleRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Int64) -1 )
          else
            a[k] := -Random(High(Int64) -2);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          Check(SameValue(a[k], localObj[k]));
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfFloatDoubleRemotable }

class function TTest_TArrayOfFloatDoubleRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfFloatDoubleRemotable.Create();
end;

class function TTest_TArrayOfFloatDoubleRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Double);
end;

procedure TTest_TArrayOfFloatDoubleRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfFloatDoubleRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfFloatDoubleRemotable.Create();
  try
    b := TArrayOfFloatDoubleRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Int64) -1)
          else
            a[k] := -Random(High(Int64) - 2);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(SameValue(a[k],b[k]));
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(SameValue(b[k],a[k]));
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfFloatDoubleRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfFloatDoubleRemotable;
  i, j, k : Integer;
  a : array of Double;
begin
  Randomize();
  localObj := TArrayOfFloatDoubleRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Int64) -1 )
          else
            a[k] := -Random(High(Int64) -2);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          Check(SameValue(a[k], localObj[k]));
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfFloatExtendedRemotable }

class function TTest_TArrayOfFloatExtendedRemotable.CreateArray(): TBaseArrayRemotable;
begin
  Result := TArrayOfFloatExtendedRemotable.Create();
end;

class function TTest_TArrayOfFloatExtendedRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Extended);
end;

procedure TTest_TArrayOfFloatExtendedRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfFloatExtendedRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfFloatExtendedRemotable.Create();
  try
    b := TArrayOfFloatExtendedRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Int64) -1)
          else
            a[k] := -Random(High(Int64) - 2);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(SameValue(a[k],b[k]));
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(SameValue(b[k],a[k]));
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfFloatExtendedRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfFloatExtendedRemotable;
  i, j, k : Integer;
  a : array of Extended;
begin
  Randomize();
  localObj := TArrayOfFloatExtendedRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Int64) -1 )
          else
            a[k] := -Random(High(Int64) -2);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          Check(SameValue(a[k], localObj[k]));
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TArrayOfFloatCurrencyRemotable }

class function TTest_TArrayOfFloatCurrencyRemotable.CreateArray( ): TBaseArrayRemotable;
begin
  Result := TArrayOfFloatCurrencyRemotable.Create();
end;

class function TTest_TArrayOfFloatCurrencyRemotable.GetTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Currency);
end;

procedure TTest_TArrayOfFloatCurrencyRemotable.test_Assign();
const ITER : Integer = 100;
var
  a, b : TArrayOfFloatCurrencyRemotable;
  i, j, k : Integer;
begin
  Randomize();
  a := TArrayOfFloatCurrencyRemotable.Create();
  try
    b := TArrayOfFloatCurrencyRemotable.Create();
    a.Assign(nil);

    for i := 1 to ITER do begin
      j := Random(ITER);
      a.SetLength(j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Integer) -1)
          else
            a[k] := -Random(High(Integer) - 2);
        end;
      end;
      b.Assign(a);
      CheckEquals(a.Length,b.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(SameValue(a[k],b[k],0.0001));
        end;
      end;

      a.SetLength(0);
      a.Assign(b);
      CheckEquals(b.Length,a.Length, 'Length');
      if ( j > 0 ) then begin
        for k := 0 to Pred(j) do begin
          Check(SameValue(b[k],a[k],0.0001));
        end;
      end;
    end;
  finally
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;

procedure TTest_TArrayOfFloatCurrencyRemotable.GetItemAndSetItem();
const ITER : Integer = 100;
var
  localObj : TArrayOfFloatCurrencyRemotable;
  i, j, k : Integer;
  a : array of Currency;
begin
  Randomize();
  localObj := TArrayOfFloatCurrencyRemotable.Create() ;
  try
    for i := 1 to ITER do begin
      j := Random(ITER);
      SetLength(a,j);
      if ( j > 0  ) then begin
        for k := 0 to Pred(j) do begin
          if ( ( k mod 2 ) = 0 ) then
            a[k] := Random(High(Integer) -1 )
          else
            a[k] := -Random(High(Integer) -2);
        end;

        localObj.SetLength(j);
        for k := 0 to Pred(j) do begin
          localObj[k] := a[k];
        end;
        for k := 0 to Pred(j) do begin
          Check(SameValue(a[k], localObj[k], 0.0001));
        end;
      end;
    end;
  finally
    FreeAndNil(localObj);
  end;
end;

{ TTest_TDateTimeRemotable }

procedure TTest_TDateTimeRemotable.FormatDate();
const
  sDATE_1 = '1976-10-12T23:34:56';
  sDATE_2 = '0987-06-12T20:34:56';
var
  d : TDateTime;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  d := EncodeDate(1976,10,12) + EncodeTime(23,34,56,0);
  CheckEquals(sDATE_1, Copy(TDateTimeRemotable.ToStr(d),1,Length(sDATE_1)));

  d := EncodeDate(987,06,12) - EncodeTime(20,34,56,0);
  CheckEquals(sDATE_2, Copy(TDateTimeRemotable.ToStr(d),1,Length(sDATE_2)));
end;

procedure TTest_TDateTimeRemotable.ParseDate();
var
  s : string;
  objd : TDateTimeRemotable;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56';
  d := TDateTimeRemotable.Parse(s);
  DecodeDateTime(d.Date,y,m,dy, hh,mn,ss,ssss);
    CheckEquals(1976,y,'Year');
    CheckEquals(10,m,'Month');
    CheckEquals(12,dy,'Day');
    CheckEquals(23,hh,'Hour');
    CheckEquals(34,mn,'Minute');
    CheckEquals(56,ss,'Second');

  objd := TDateTimeRemotable.Create();
  try
    objd.AsDate := d.Date;
    CheckEquals(1976,objd.Year,'Year');
    CheckEquals(10,objd.Month,'Month');
    CheckEquals(12,objd.Day,'Day');
    CheckEquals(23,objd.Hour,'Hour');
    CheckEquals(34,objd.Minute,'Minute');
    CheckEquals(56,objd.Second,'Second');
  finally
    FreeAndNil(objd);
  end;
end;

procedure TTest_TDateTimeRemotable.Assign();
var
  a, b : TDateTimeRemotable;
begin
  b := nil;
  a := TDateTimeRemotable.Create();
  try
    b := TDateTimeRemotable.Create();
    Check(IsZero(a.AsDate - b.AsDate));

    a.AsDate := Now();
    b.Assign(a);
    Check(IsZero(a.AsDate - b.AsDate));

    a.AsDate := Now() + 1;
    a.Assign(b);
    Check(IsZero(a.AsDate - b.AsDate));
  finally
    b.Free();
    a.Free();
  end;
end;

procedure TTest_TDateTimeRemotable.Equal();
var
  a, b : TDateTimeRemotable;
  c : TClass_A;
begin
  c := nil;
  b := nil;
  a := TDateTimeRemotable.Create();
  try
    b := TDateTimeRemotable.Create();
    c := TClass_A.Create();
    
    CheckEquals(False,a.Equal(nil));
    CheckEquals(False,a.Equal(c));
    
    a.AsDate := Now();
    b.AsDate := a.AsDate;
    CheckEquals(True,a.Equal(b));
    CheckEquals(True,b.Equal(a));

    a.AsDate := a.AsDate + 1;
    CheckEquals(False,a.Equal(b));
    CheckEquals(False,b.Equal(a));
  finally
    c.Free();
    b.Free();
    a.Free();
  end;
end;

procedure TTest_TDateTimeRemotable.FormatDate_ZERO();
const sDATE = '1899-12-30T00:00:00';
var
  d : TDateTime;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  d := 0;
  CheckEquals(sDATE, Copy(TDateTimeRemotable.ToStr(d),1,Length(sDATE)));
end;

procedure TTest_TDateTimeRemotable.AsDate();
var
  d : TDateTime;
  locObj : TDateTimeRemotable;
begin
  d := EncodeDateTime(1976,10,12,13,14,15,0);
  locObj := TDateTimeRemotable.Create();
  try
    locObj.AsDate := d;
    CheckEquals(d, locObj.AsDate);
    locObj.HourOffset := 4;
    locObj.MinuteOffset := 5;
    CheckEquals(d, locObj.AsDate);

    // test while (Hour|Minute)Offset is not null
    locObj.HourOffset := 4;
    locObj.MinuteOffset := 5;
    locObj.AsDate := d;
    CheckEquals(d, locObj.AsDate);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateTimeRemotable.AsUTCDate();
var
  d, dd : TDateTime;
  locObj : TDateTimeRemotable;
begin
  d := EncodeDateTime(1976,10,12,13,14,15,0);
  locObj := TDateTimeRemotable.Create();
  try
    locObj.AsUTCDate := d;
    CheckEquals(d, locObj.AsUTCDate);
    locObj.HourOffset := 4;
    locObj.MinuteOffset := 5;
    dd := date_utils.IncHour(d,-locObj.HourOffset);
    dd := date_utils.IncMinute(dd,-locObj.MinuteOffset);
    CheckEquals(dd, locObj.AsUTCDate);

    // test while (Hour|Minute)Offset is not null
    locObj.AsUTCDate := dd;
    CheckEquals(dd, locObj.AsUTCDate);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateTimeRemotable.HourOffset();
var
  locObj : TDateTimeRemotable;
begin
  locObj := TDateTimeRemotable.Create();
  try
    locObj.HourOffset := -5;
      CheckEquals(-5, locObj.HourOffset);
    locObj.HourOffset := 0;
      CheckEquals(0, locObj.HourOffset);
    locObj.HourOffset := 1;
      CheckEquals(1, locObj.HourOffset);
    locObj.HourOffset := 2;
      CheckEquals(2, locObj.HourOffset);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateTimeRemotable.MinuteOffset();
var
  locObj : TDateTimeRemotable;
begin
  locObj := TDateTimeRemotable.Create();
  try
    locObj.MinuteOffset := -54;
      CheckEquals(-54, locObj.MinuteOffset);
    locObj.MinuteOffset := 0;
      CheckEquals(0, locObj.MinuteOffset);
    locObj.MinuteOffset := 20;
      CheckEquals(20, locObj.MinuteOffset);
    locObj.MinuteOffset := 56;
      CheckEquals(56, locObj.MinuteOffset);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateTimeRemotable.HourOffset_invalid_values();
var
  locObj : TDateTimeRemotable;

  procedure check_invalid_value(const AValue : ShortInt);
  var
    ok : Boolean;
  begin
    try
      locObj.HourOffset := AValue;
      ok := False;
    except
      ok := True;
    end;
    Check(ok, Format('"%d" is not a valid hour offset',[AValue]));
  end;

begin
  locObj := TDateTimeRemotable.Create();
  try
    check_invalid_value(-50);
    check_invalid_value(-24);
    check_invalid_value(-15);
    check_invalid_value(15);
    check_invalid_value(24);
    check_invalid_value(50);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateTimeRemotable.MinuteOffset_invalid_values();
var
  locObj : TDateTimeRemotable;

  procedure check_invalid_value(const AValue : ShortInt);
  var
    ok : Boolean;
  begin
    try
      locObj.MinuteOffset := AValue;
      ok := False;
    except
      ok := True;
    end;
    Check(ok, Format('"%d" is not a valid minute offset',[AValue]));
  end;

begin
  locObj := TDateTimeRemotable.Create();
  try
    check_invalid_value(-60);
    check_invalid_value(-74);
    check_invalid_value(-85);
    check_invalid_value(65);
    check_invalid_value(74);
    check_invalid_value(80);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateTimeRemotable.Year();
var
  locObj : TDateTimeRemotable;
begin
  locObj := TDateTimeRemotable.Create();
  try
    locObj.AsDate := EncodeDate(1976,10,12);
      CheckEquals(1976, locObj.Year);
    locObj.AsDate := EncodeDate(2000,10,12);
      CheckEquals(2000, locObj.Year);
    locObj.AsDate := EncodeDate(2,10,12);
      CheckEquals(2, locObj.Year);
    {locObj.AsDate := EncodeDate(-1976,10,12);
      CheckEquals(-1976, locObj.Year);}
  finally
    locObj.Free();
  end;
end;

{ TTest_TDateRemotable }

procedure TTest_TDateRemotable.FormatDate();
const
  sDATE_1 = '1976-10-12';
  sDATE_2 = '0987-06-12';
var
  d : TDateTime;
begin
  d := EncodeDate(1976,10,12);
  CheckEquals(sDATE_1, Copy(TDateRemotable.ToStr(d),1,Length(sDATE_1)));

  d := EncodeDate(987,06,12);
  CheckEquals(sDATE_2, Copy(TDateRemotable.ToStr(d),1,Length(sDATE_2)));
end;

procedure TTest_TDateRemotable.ParseDate();
var
  s : string;
  objd : TDateRemotable;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  s := '1976-10-12';
  d := TDateRemotable.Parse(s);
  DecodeDateTime(d.Date,y,m,dy, hh,mn,ss,ssss);
    CheckEquals(1976,y,'Year');
    CheckEquals(10,m,'Month');
    CheckEquals(12,dy,'Day');

  objd := TDateRemotable.Create();
  try
    objd.AsDate := d.Date;
    CheckEquals(1976,objd.Year,'Year');
    CheckEquals(10,objd.Month,'Month');
    CheckEquals(12,objd.Day,'Day');
  finally
    FreeAndNil(objd);
  end;
end;

procedure TTest_TDateRemotable.Assign();
var
  a, b : TDateRemotable;
begin
  b := nil;
  a := TDateRemotable.Create();
  try
    b := TDateRemotable.Create();
    Check(IsZero(a.AsDate - b.AsDate));

    a.AsDate := Date();
    b.Assign(a);
    Check(IsZero(a.AsDate - b.AsDate));

    a.AsDate := Date() + 1;
    a.Assign(b);
    Check(IsZero(a.AsDate - b.AsDate));
  finally
    b.Free();
    a.Free();
  end;
end;

procedure TTest_TDateRemotable.Equal();
var
  a, b : TDateRemotable;
  c : TClass_A;
begin
  c := nil;
  b := nil;
  a := TDateRemotable.Create();
  try
    b := TDateRemotable.Create();
    c := TClass_A.Create();
    
    CheckEquals(False,a.Equal(nil));
    CheckEquals(False,a.Equal(c));
    
    a.AsDate := Date();
    b.AsDate := a.AsDate;
    CheckEquals(True,a.Equal(b));
    CheckEquals(True,b.Equal(a));

    a.AsDate := a.AsDate + 1;
    CheckEquals(False,a.Equal(b));
    CheckEquals(False,b.Equal(a));
  finally
    c.Free();
    b.Free();
    a.Free();
  end;
end;

procedure TTest_TDateRemotable.FormatDate_ZERO();
const sDATE = '1899-12-30';
var
  d : TDateTime;
begin
  d := 0;
  CheckEquals(sDATE, Copy(TDateRemotable.ToStr(d),1,Length(sDATE)));
end;

procedure TTest_TDateRemotable.AsDate();
var
  d : TDateTime;
  locObj : TDateRemotable;
begin
  d := EncodeDate(1976,10,12);
  locObj := TDateRemotable.Create();
  try
    locObj.AsDate := d;
    CheckEquals(d, locObj.AsDate);
    locObj.HourOffset := 4;
    locObj.MinuteOffset := 5;
    CheckEquals(d, locObj.AsDate);

    // test while (Hour|Minute)Offset is not null
    locObj.HourOffset := 4;
    locObj.MinuteOffset := 5;
    locObj.AsDate := d;
    CheckEquals(d, locObj.AsDate);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateRemotable.AsUTCDate();
var
  d, dd : TDateTime;
  locObj : TDateRemotable;
begin
  d := EncodeDate(1976,10,12);
  locObj := TDateRemotable.Create();
  try
    locObj.AsUTCDate := d;
    CheckEquals(d, locObj.AsUTCDate);
    locObj.HourOffset := 4;
    locObj.MinuteOffset := 5;
    dd := date_utils.IncHour(d,-locObj.HourOffset);
    dd := date_utils.IncMinute(dd,-locObj.MinuteOffset);
    CheckEquals(dd, locObj.AsUTCDate);

    // test while (Hour|Minute)Offset is not null
    locObj.AsUTCDate := dd;
    CheckEquals(dd, locObj.AsUTCDate,EPSILON_DATE);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateRemotable.HourOffset();
var
  locObj : TDateRemotable;
begin
  locObj := TDateRemotable.Create();
  try
    locObj.HourOffset := -5;
      CheckEquals(-5, locObj.HourOffset);
    locObj.HourOffset := 0;
      CheckEquals(0, locObj.HourOffset);
    locObj.HourOffset := 1;
      CheckEquals(1, locObj.HourOffset);
    locObj.HourOffset := 2;
      CheckEquals(2, locObj.HourOffset);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateRemotable.MinuteOffset();
var
  locObj : TDateRemotable;
begin
  locObj := TDateRemotable.Create();
  try
    locObj.MinuteOffset := -54;
      CheckEquals(-54, locObj.MinuteOffset);
    locObj.MinuteOffset := 0;
      CheckEquals(0, locObj.MinuteOffset);
    locObj.MinuteOffset := 20;
      CheckEquals(20, locObj.MinuteOffset);
    locObj.MinuteOffset := 56;
      CheckEquals(56, locObj.MinuteOffset);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateRemotable.HourOffset_invalid_values();
var
  locObj : TDateRemotable;

  procedure check_invalid_value(const AValue : ShortInt);
  var
    ok : Boolean;
  begin
    try
      locObj.HourOffset := AValue;
      ok := False;
    except
      ok := True;
    end;
    Check(ok, Format('"%d" is not a valid hour offset',[AValue]));
  end;

begin
  locObj := TDateRemotable.Create();
  try
    check_invalid_value(-50);
    check_invalid_value(-24);
    check_invalid_value(-15);
    check_invalid_value(15);
    check_invalid_value(24);
    check_invalid_value(50);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateRemotable.MinuteOffset_invalid_values();
var
  locObj : TDateRemotable;

  procedure check_invalid_value(const AValue : ShortInt);
  var
    ok : Boolean;
  begin
    try
      locObj.MinuteOffset := AValue;
      ok := False;
    except
      ok := True;
    end;
    Check(ok, Format('"%d" is not a valid minute offset',[AValue]));
  end;

begin
  locObj := TDateRemotable.Create();
  try
    check_invalid_value(-60);
    check_invalid_value(-74);
    check_invalid_value(-85);
    check_invalid_value(65);
    check_invalid_value(74);
    check_invalid_value(80);
  finally
    locObj.Free();
  end;
end;

procedure TTest_TDateRemotable.Year();
var
  locObj : TDateRemotable;
begin
  locObj := TDateRemotable.Create();
  try
    locObj.AsDate := EncodeDate(1976,10,12);
      CheckEquals(1976, locObj.Year);
    locObj.AsDate := EncodeDate(2000,10,12);
      CheckEquals(2000, locObj.Year);
    locObj.AsDate := EncodeDate(2,10,12);
      CheckEquals(2, locObj.Year);
    {locObj.AsDate := EncodeDate(-1976,10,12);
      CheckEquals(-1976, locObj.Year);}
  finally
    locObj.Free();
  end;
end;

{ TTest_TDurationRemotable }

procedure TTest_TDurationRemotable.Clear();
var
  x : TDurationRemotable;
begin
  x := TDurationRemotable.Create();
  try
    x.Negative := True;
    x.Year := 1;
    x.Month := 2;
    x.Day := 3;
    x.Hour := 4;
    x.Minute := 5;
    x.Second := 6;
    x.FractionalSecond := 7;
    x.Clear();
    CheckEquals(False,x.Negative);
    CheckEquals(0,x.Year);
    CheckEquals(0,x.Month);
    CheckEquals(0,x.Day);
    CheckEquals(0,x.Hour);
    CheckEquals(0,x.Minute);
    CheckEquals(0,x.Second);
    CheckEquals(0,x.FractionalSecond);
  finally
    x.Free();
  end;
end;

procedure TTest_TDurationRemotable.AsString_empty();
var
  x : TDurationRemotable;
begin
  x := TDurationRemotable.Create();
  try
    CheckEquals('P0Y', x.AsString);
    x.Negative := True;
    CheckEquals('P0Y', x.AsString);
  finally
    x.Free();
  end;
end;

procedure TTest_TDurationRemotable.AsString_not_empty();
var
  x : TDurationRemotable;
begin
  x := TDurationRemotable.Create();
  try
    x.Year := 1;
    x.Month := 2;
    x.Day := 3;
    x.Hour := 4;
    x.Minute := 5;
    x.Second := 6;
    CheckEquals('P1Y2M3DT4H5M6S',x.AsString);
    x.FractionalSecond := 7;
    CheckEquals('P1Y2M3DT4H5M6.7S',x.AsString);
    x.Negative := True;
    CheckEquals('-P1Y2M3DT4H5M6.7S',x.AsString);
  finally
    x.Free();
  end;
end;

procedure TTest_TDurationRemotable.AsString_date_only();
var
  x : TDurationRemotable;
begin
  x := TDurationRemotable.Create();
  try
    x.Year := 1;
      CheckEquals('P1Y', x.AsString);
      x.Month := 2;
        CheckEquals('P1Y2M', x.AsString);
        x.Day := 3;
          CheckEquals('P1Y2M3D', x.AsString);
        x.Negative := True;
          CheckEquals('-P1Y2M3D', x.AsString);

    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Month := 12;
      CheckEquals('P12M',x.AsString);

    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Day := 34;
      CheckEquals('P34D',x.AsString);
      
    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Month := 12;
    x.Day := 3;
      CheckEquals('P12M3D',x.AsString);

    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Year := 2;
    x.Month := 34;
      CheckEquals('P2Y34M',x.AsString);

    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Year := 12;
    x.Day := 56;
      CheckEquals('P12Y56D',x.AsString);
  finally
    x.Free();
  end;
end;

procedure TTest_TDurationRemotable.AsString_time_only();
var
  x : TDurationRemotable;
begin
  x := TDurationRemotable.Create();
  try
    x.Hour := 1;
      CheckEquals('PT1H', x.AsString);
      x.Minute := 2;
        CheckEquals('PT1H2M', x.AsString);
        x.Second := 3;
          CheckEquals('PT1H2M3S', x.AsString);
          x.FractionalSecond := 4;
            CheckEquals('PT1H2M3.4S', x.AsString);
            x.Negative := True;
              CheckEquals('-PT1H2M3.4S', x.AsString);

    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Minute := 12;
      CheckEquals('PT12M',x.AsString);

    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Second := 34;
      CheckEquals('PT34S',x.AsString);

    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Minute := 12;
    x.Second := 3;
      CheckEquals('PT12M3S',x.AsString);

    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Hour := 2;
    x.Minute := 34;
      CheckEquals('PT2H34M',x.AsString);

    FreeAndNil(x);
    x := TDurationRemotable.Create();
    x.Hour := 12;
    x.Second := 56;
      CheckEquals('PT12H56S',x.AsString);
  finally
    x.Free();
  end;
end;

procedure TTest_TDurationRemotable.Parse_non_empty();
var
  x : TDurationRec;
begin
  x := TDurationRemotable.Parse('P1Y2M3DT4H5M6S');
    CheckEquals(False,x.Negative);
    CheckEquals(1,x.Year);
    CheckEquals(2,x.Month);
    CheckEquals(3,x.Day);
    CheckEquals(4,x.Hour);
    CheckEquals(5,x.Minute);
    CheckEquals(6,x.Second);
    CheckEquals(0,x.FractionalSecond);

  x := TDurationRemotable.Parse('-P1Y2M3DT4H5M6S');
    CheckEquals(True,x.Negative);
    CheckEquals(1,x.Year);
    CheckEquals(2,x.Month);
    CheckEquals(3,x.Day);
    CheckEquals(4,x.Hour);
    CheckEquals(5,x.Minute);
    CheckEquals(6,x.Second);
    CheckEquals(0,x.FractionalSecond);

  x := TDurationRemotable.Parse('P1Y2M3DT4H5M6.7S');
    CheckEquals(False,x.Negative);
    CheckEquals(1,x.Year);
    CheckEquals(2,x.Month);
    CheckEquals(3,x.Day);
    CheckEquals(4,x.Hour);
    CheckEquals(5,x.Minute);
    CheckEquals(6,x.Second);
    CheckEquals(7,x.FractionalSecond);

  x := TDurationRemotable.Parse('-P1Y2M3DT4H5M6.7S');
    CheckEquals(True,x.Negative);
    CheckEquals(1,x.Year);
    CheckEquals(2,x.Month);
    CheckEquals(3,x.Day);
    CheckEquals(4,x.Hour);
    CheckEquals(5,x.Minute);
    CheckEquals(6,x.Second);
    CheckEquals(7,x.FractionalSecond);
end;

procedure TTest_TDurationRemotable.Parse_time_only();
var
  x : TDurationRec;
begin
  x := TDurationRemotable.Parse('PT1H2M3.4S');
    CheckEquals(False,x.Negative);
    CheckEquals(0,x.Year);
    CheckEquals(0,x.Month);
    CheckEquals(0,x.Day);
    CheckEquals(1,x.Hour);
    CheckEquals(2,x.Minute);
    CheckEquals(3,x.Second);
    CheckEquals(4,x.FractionalSecond);

  x := TDurationRemotable.Parse('-PT1H2M3.4S');
    CheckEquals(True,x.Negative);
    CheckEquals(0,x.Year);
    CheckEquals(0,x.Month);
    CheckEquals(0,x.Day);
    CheckEquals(1,x.Hour);
    CheckEquals(2,x.Minute);
    CheckEquals(3,x.Second);
    CheckEquals(4,x.FractionalSecond);

  x := TDurationRemotable.Parse('PT1H');
    CheckEquals(False,x.Negative);
    CheckEquals(0,x.Year);
    CheckEquals(0,x.Month);
    CheckEquals(0,x.Day);
    CheckEquals(1,x.Hour);
    CheckEquals(0,x.Minute);
    CheckEquals(0,x.Second);
    CheckEquals(0,x.FractionalSecond);

  x := TDurationRemotable.Parse('PT1S');
    CheckEquals(False,x.Negative);
    CheckEquals(0,x.Year);
    CheckEquals(0,x.Month);
    CheckEquals(0,x.Day);
    CheckEquals(0,x.Hour);
    CheckEquals(0,x.Minute);
    CheckEquals(1,x.Second);
    CheckEquals(0,x.FractionalSecond);
end;

procedure TTest_TDurationRemotable.Parse_zero();
var
  x : TDurationRec;
begin
  x := TDurationRemotable.Parse('P0Y');
  CheckEquals(False,x.Negative);
  CheckEquals(0,x.Year);
  CheckEquals(0,x.Month);
  CheckEquals(0,x.Day);
  CheckEquals(0,x.Hour);
  CheckEquals(0,x.Minute);
  CheckEquals(0,x.Second);
  CheckEquals(0,x.FractionalSecond);
end;

procedure TTest_TDurationRemotable.parse_negative();
var
  x : TDurationRec;
begin
  x := TDurationRemotable.Parse('-P3YT4S');
  CheckEquals(True,x.Negative);
  CheckEquals(3,x.Year);
  CheckEquals(0,x.Month);
  CheckEquals(0,x.Day);
  CheckEquals(0,x.Hour);
  CheckEquals(0,x.Minute);
  CheckEquals(4,x.Second);
  CheckEquals(0,x.FractionalSecond);
end;

procedure TTest_TDurationRemotable.parse_invalid_1();
const S_EXPR = 'P-1347M';
var
  ok : Boolean;
begin
  ok := False;
  try
    TDurationRemotable.Parse(S_EXPR);
  except
    on e : EConvertError do
      ok := True;
  end;
  Check(ok, Format('Must fail with : "%s"',[S_EXPR]));
end;

procedure TTest_TDurationRemotable.parse_invalid_2();
const S_EXPR = 'P1Y2MT';
var
  ok : Boolean;
begin
  ok := False;
  try
    TDurationRemotable.Parse(S_EXPR);
  except
    on e : EConvertError do
      ok := True;
  end;
  Check(ok, Format('Must fail with : "%s"',[S_EXPR]));
end;

procedure TTest_TDurationRemotable.parse_invalid_3();
const S_EXPR = 'XOJDQJKJ';
var
  ok : Boolean;
begin
  ok := False;
  try
    TDurationRemotable.Parse(S_EXPR);
  except
    on e : EConvertError do
      ok := True;
  end;
  Check(ok, Format('Must fail with : "%s"',[S_EXPR]));
end;

procedure TTest_TDurationRemotable.parse_invalid_4();
const S_EXPR = 'P';
var
  ok : Boolean;
begin
  ok := False;
  try
    TDurationRemotable.Parse(S_EXPR);
  except
    on e : EConvertError do
      ok := True;
  end;
  Check(ok, Format('Must fail with : "%s"',[S_EXPR]));
end;

procedure TTest_TDurationRemotable.parse_invalid_5();
const S_EXPR = 'P45DH';
var
  ok : Boolean;
begin
  ok := False;
  try
    TDurationRemotable.Parse(S_EXPR);
  except
    on e : EConvertError do
      ok := True;
  end;
  Check(ok, Format('Must fail with : "%s"',[S_EXPR]));
end;

procedure TTest_TDurationRemotable.parse_empty();
const S_EXPR = '';
var
  ok : Boolean;
begin
  ok := False;
  try
    TDurationRemotable.Parse(S_EXPR);
  except
    on e : EConvertError do
      ok := True;
  end;
  Check(ok, Format('Must fail with : "%s"',[S_EXPR]));
end;

{ TTest_TTimeRemotable }
{$IFDEF WST_DELPHI}
procedure TTest_TTimeRemotable.CheckEquals(expected, actual: TTimeRec;msg: string);
begin
  CheckEquals(expected.Hour,actual.Hour,msg + ', Hour');
  CheckEquals(expected.Minute,actual.Minute,msg + ', Minute');
  CheckEquals(expected.Second,actual.Second,msg + ', Second');
  CheckEquals(expected.MilliSecond,actual.MilliSecond,msg + ', MilliSecond');
  CheckEquals(expected.HourOffset,actual.HourOffset,msg + ', HourOffset');
  CheckEquals(expected.MinuteOffset,actual.MinuteOffset,msg + ', MinuteOffset');
end;
{$ENDIF WST_DELPHI}

{$IFDEF FPC}
class procedure TTest_TTimeRemotable.CheckEquals(expected, actual: TTimeRec;msg: string);
begin
  CheckEquals(expected.Hour,actual.Hour,msg + ', Hour');
  CheckEquals(expected.Minute,actual.Minute,msg + ', Minute');
  CheckEquals(expected.Second,actual.Second,msg + ', Second');
  CheckEquals(expected.MilliSecond,actual.MilliSecond,msg + ', MilliSecond');
  CheckEquals(expected.HourOffset,actual.HourOffset,msg + ', HourOffset');
  CheckEquals(expected.MinuteOffset,actual.MinuteOffset,msg + ', MinuteOffset');
end;
{$ENDIF FPC}

procedure TTest_TTimeRemotable.ToStr();
const
  sVALUE_1 = '01:23:45.678';
  sVALUE_2 = '12:34:56';
  sVALUE_3 = '20:34:56';
var
  d : TTimeRec;
begin
  //hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  d := xsd_EncodeTime(01,23,45,678);
  CheckEquals(sVALUE_1, Copy(TTimeRemotable.ToStr(d),1,Length(sVALUE_1)));

  d := xsd_EncodeTime(12,34,56,0);
  CheckEquals(sVALUE_2, Copy(TTimeRemotable.ToStr(d),1,Length(sVALUE_2)));

  d := xsd_EncodeTime(20,34,56,0);
  CheckEquals(sVALUE_3, Copy(TTimeRemotable.ToStr(d),1,Length(sVALUE_3)));
end;

procedure TTest_TTimeRemotable.Parse();
var
  s : string;
  objd : TTimeRemotable;
  d : TTimeRec;
begin
  s := '23:34:56';
  d := TTimeRemotable.Parse(s);
    CheckEquals(d.Hour,23,'Hour');
    CheckEquals(d.Minute,34,'Minute');
    CheckEquals(d.Second,56,'Second');
    CheckEquals(d.MilliSecond,0,'MilliSecond');
    CheckEquals(d.HourOffset,0,'HourOffset');
    CheckEquals(d.MinuteOffset,0,'MinuteOffset');

  objd := TTimeRemotable.Create();
  try
    objd.Data := d;
    CheckEquals(objd.Hour,23,'Hour');
    CheckEquals(objd.Minute,34,'Minute');
    CheckEquals(objd.Second,56,'Second');
    CheckEquals(objd.MilliSecond,0,'MilliSecond');
    CheckEquals(objd.HourOffset,0,'HourOffset');
    CheckEquals(objd.MinuteOffset,0,'MinuteOffset');
  finally
    FreeAndNil(objd);
  end;
end;

procedure TTest_TTimeRemotable.Parse_millisecond();
var
  s : string;
  objd : TTimeRemotable;
  d : TTimeRec;
begin
  s := '23:34:56.780';
  d := TTimeRemotable.Parse(s);
    CheckEquals(d.Hour,23,'Hour');
    CheckEquals(d.Minute,34,'Minute');
    CheckEquals(d.Second,56,'Second');
    CheckEquals(d.MilliSecond,780,'MilliSecond');
    CheckEquals(d.HourOffset,0,'HourOffset');
    CheckEquals(d.MinuteOffset,0,'MinuteOffset');

  objd := TTimeRemotable.Create();
  try
    objd.Data := d;
    CheckEquals(objd.Hour,23,'Hour');
    CheckEquals(objd.Minute,34,'Minute');
    CheckEquals(objd.Second,56,'Second');
    CheckEquals(objd.MilliSecond,780,'MilliSecond');
    CheckEquals(objd.HourOffset,0,'HourOffset');
    CheckEquals(objd.MinuteOffset,0,'MinuteOffset');
  finally
    FreeAndNil(objd);
  end;
end;

procedure TTest_TTimeRemotable.Parse_offset_1();
var
  s : string;
  objd : TTimeRemotable;
  d : TTimeRec;
begin
  s := '23:34:56+01:27';
  d := TTimeRemotable.Parse(s);
    CheckEquals(d.Hour,23,'Hour');
    CheckEquals(d.Minute,34,'Minute');
    CheckEquals(d.Second,56,'Second');
    CheckEquals(d.MilliSecond,0,'MilliSecond');
    CheckEquals(d.HourOffset,1,'HourOffset');
    CheckEquals(d.MinuteOffset,27,'MinuteOffset');

  objd := TTimeRemotable.Create();
  try
    objd.Data := d;
    CheckEquals(objd.Hour,23,'Hour');
    CheckEquals(objd.Minute,34,'Minute');
    CheckEquals(objd.Second,56,'Second');
    CheckEquals(objd.MilliSecond,0,'MilliSecond');
    CheckEquals(objd.HourOffset,1,'HourOffset');
    CheckEquals(objd.MinuteOffset,27,'MinuteOffset');
  finally
    FreeAndNil(objd);
  end;
end;

procedure TTest_TTimeRemotable.Parse_offset_2();
var
  s : string;
  objd : TTimeRemotable;
  d : TTimeRec;
begin
  s := '23:34:56.800-01:27';
  d := TTimeRemotable.Parse(s);
    CheckEquals(d.Hour,23,'Hour');
    CheckEquals(d.Minute,34,'Minute');
    CheckEquals(d.Second,56,'Second');
    CheckEquals(d.MilliSecond,800,'MilliSecond');
    CheckEquals(d.HourOffset,-1,'HourOffset');
    CheckEquals(d.MinuteOffset,-27,'MinuteOffset');

  objd := TTimeRemotable.Create();
  try
    objd.Data := d;
    CheckEquals(objd.Hour,23,'Hour');
    CheckEquals(objd.Minute,34,'Minute');
    CheckEquals(objd.Second,56,'Second');
    CheckEquals(objd.MilliSecond,800,'MilliSecond');
    CheckEquals(objd.HourOffset,-1,'HourOffset');
    CheckEquals(objd.MinuteOffset,-27,'MinuteOffset');
  finally
    FreeAndNil(objd);
  end;
end;

procedure TTest_TTimeRemotable.Data;
var
  objd : TTimeRemotable;
  d : TTimeRec;
begin
  d := xsd_EncodeTime(1,2,3,4,5,6);
  objd := TTimeRemotable.Create();
  try
    objd.Data := d;
    CheckEquals(objd.Data,d);
  finally
    FreeAndNil(objd);
  end;
end;

procedure TTest_TTimeRemotable.Equal();
var
  a, b : TTimeRemotable;
begin
  b := nil;
  a := TTimeRemotable.Create();
  try
    b := TTimeRemotable.Create();
      Check(a.Equal(b));

    a.Data := xsd_EncodeTime(1,2,3,4,5,6);
      Check(not a.Equal(b));
      b.Data := xsd_EncodeTime(1,2,3,4,5,6);
        Check(a.Equal(b));
  finally
    b.Free();
    a.Free();
  end;
end;

procedure TTest_TTimeRemotable.Assign();
var
  a, b : TTimeRemotable;
begin
  b := nil;
  a := TTimeRemotable.Create();
  try
    b := TTimeRemotable.Create();
    b.Assign(a);
      CheckEquals(a.Data,b.Data);

    a.Data := xsd_EncodeTime(1,2,3,4,5,6);
    b.Assign(a);
      CheckEquals(a.Data,b.Data);
  finally
    b.Free();
    a.Free();
  end;
end;

procedure TTest_TTimeRemotable.Clear();
var
  a : TTimeRemotable;
begin
  a := TTimeRemotable.Create();
  try
    a.Clear();
      CheckEquals(a.Data,ZERO_TIME);

    a.Data := xsd_EncodeTime(1,2,3,4,5,6);
    a.Clear();
      CheckEquals(a.Data,ZERO_TIME);
  finally
    a.Free();
  end;
end;

{ TTest_TStringBufferRemotable }

procedure TTest_TStringBufferRemotable.test_Assign();
const ITER = 100;
var
  i : Integer;
  a, b : TStringBufferRemotable;
begin
  b := nil;
  a := TStringBufferRemotable.Create();
  try
    b := TStringBufferRemotable.Create();
    for i := 1 to ITER do begin
      a.Data := RandomValue(Random(500));
      b.Assign(a);
      CheckEquals(a.Data, b.Data);
    end;
  finally
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

procedure TTest_TStringBufferRemotable.Equal();
const ITER = 100;
var
  i : Integer;
  a, b : TStringBufferRemotable;
  c : TClass_A;
begin
  c := nil;
  b := nil;
  a := TStringBufferRemotable.Create();
  try
    b := TStringBufferRemotable.Create();
    CheckEquals(False, a.Equal(nil));
    c := TClass_A.Create();
    CheckEquals(False, a.Equal(c));
    a.Data := 'wst';
    b.Data := 'azerty';
    CheckEquals(False, a.Equal(b));
    CheckEquals(False, b.Equal(a));

    for i := 1 to ITER do begin
      a.Data := RandomValue(Random(500));
      b.Data := a.Data;
      CheckEquals(True, a.Equal(b));
      CheckEquals(True, b.Equal(a));
    end;
  finally
    FreeAndNil(c);
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

{ TTest_TAbstractEncodedStringRemotable }

class function TTest_TAbstractEncodedStringRemotable.EncodeData(const AValue: TBinaryString): string;
begin
  Result := EncodeData(StringToByteArray(AValue));
end;

procedure TTest_TAbstractEncodedStringRemotable.test_Assign();
const ITER = 100;
var
  i : Integer;
  a, b : TAbstractEncodedStringRemotable;
begin
  b := nil;
  a := CreateObject();
  try
    b := CreateObject();
    for i := 1 to ITER do begin
      a.BinaryData := RandomBytesValue(Random(500));
      b.Assign(a);
      CheckEquals(a.BinaryData, b.BinaryData);
      CheckEquals(a.EncodedString, b.EncodedString);
    end;
  finally
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

procedure TTest_TAbstractEncodedStringRemotable.Equal();
const ITER = 100;
var
  i : Integer;
  a, b : TAbstractEncodedStringRemotable;
  c : TClass_A;
begin
  c := nil;
  b := nil;
  a := CreateObject();
  try
    b := CreateObject();
    CheckEquals(False, a.Equal(nil));
    c := TClass_A.Create();
    CheckEquals(False, a.Equal(c));
    a.BinaryData := StringToByteArray('wst');
    b.BinaryData := StringToByteArray('azerty');
    CheckEquals(False, a.Equal(b));
    CheckEquals(False, b.Equal(a));

    for i := 1 to ITER do begin
      a.BinaryData := RandomBytesValue(Random(500));
      b.BinaryData := Copy(a.BinaryData);
      CheckEquals(True, a.Equal(b));
      CheckEquals(True, b.Equal(a));
    end;
  finally
    FreeAndNil(c);
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

procedure TTest_TAbstractEncodedStringRemotable.SetBinaryData();
const ITER = 100;
var
  i : Integer;
  a : TAbstractEncodedStringRemotable;
  s : TBinaryString;
  es : string;
begin
  a := CreateObject();
  try
    s := ''; es := EncodeData(s);
    a.BinaryData := StringToByteArray(s);
    CheckEquals(StringToByteArray(s),a.BinaryData, 'BinaryData 0');
    CheckEquals(es,a.EncodedString, 'EncodedString 0');
    CheckEquals(StringToByteArray(s),a.BinaryData, 'BinaryData 0.1');
    CheckEquals(es,a.EncodedString, 'EncodedString 0.1');

    for i := 1 to ITER do begin
      s := RandomValue(Random(500)); es := EncodeData(s);
      a.BinaryData := StringToByteArray(s);
      CheckEquals(StringToByteArray(s),a.BinaryData, 'BinaryData 1');
      CheckEquals(es,a.EncodedString, 'EncodedString 1');
      CheckEquals(StringToByteArray(s),a.BinaryData, 'BinaryData 2');
      CheckEquals(es,a.EncodedString, 'EncodedString 2');
    end;
  finally
    FreeAndNil(a);
  end;
end;

procedure TTest_TAbstractEncodedStringRemotable.SetEncodedString();
const ITER = 100;
var
  i : Integer;
  a : TAbstractEncodedStringRemotable;
  s : TBinaryString;
  es : string;
begin
  a := CreateObject();
  try
    s := ''; es := EncodeData(s);
    a.EncodedString := es;
    CheckEquals(StringToByteArray(s),a.BinaryData);
    CheckEquals(es,a.EncodedString);
    CheckEquals(StringToByteArray(s),a.BinaryData);
    CheckEquals(es,a.EncodedString);

    for i := 1 to ITER do begin
      s := RandomValue(Random(500)); es := EncodeData(s);
      a.EncodedString := es;
      CheckEquals(StringToByteArray(s),a.BinaryData);
      CheckEquals(es,a.EncodedString);
      CheckEquals(StringToByteArray(s),a.BinaryData);
      CheckEquals(es,a.EncodedString);
    end;
  finally
    FreeAndNil(a);
  end;
end;

procedure TTest_TAbstractEncodedStringRemotable.LoadFromStream();
var
  locLoadedBuffer : TAbstractEncodedStringRemotable;
  locBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TMemoryStream;
  i : PtrInt;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locLoadedBuffer := nil;
  locStream := TMemoryStream.Create();
  try
    locStream.Write(locBuffer[0],Length(locBuffer));
    locLoadedBuffer := CreateObject();
    locLoadedBuffer.LoadFromStream(locStream);
    CheckEquals( locBuffer, locLoadedBuffer.BinaryData );
  finally
    locLoadedBuffer.Free();
    locStream.Free();
  end;
end;

procedure TTest_TAbstractEncodedStringRemotable.LoadFromFile();
var
  locLoadedBuffer : TAbstractEncodedStringRemotable;
  locBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TMemoryStream;
  i : PtrInt;
  locFileName : string;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locLoadedBuffer := nil;
  locStream := TMemoryStream.Create();
  try
    locStream.Write(locBuffer[0],Length(locBuffer));
    locFileName := wstExpandLocalFileName('test_LoadBufferFromFile.bin');
    locStream.SaveToFile(locFileName);
    locLoadedBuffer := CreateObject();
    locLoadedBuffer.LoadFromFile(locFileName);
    CheckEquals( locBuffer, locLoadedBuffer.BinaryData );
  finally
    locLoadedBuffer.Free();
    locStream.Free();
  end;
end;

procedure TTest_TAbstractEncodedStringRemotable.SaveToStream();
var
  locObj : TAbstractEncodedStringRemotable;
  locBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TMemoryStream;
  i : PtrInt;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locObj := nil;
  locStream := TMemoryStream.Create();
  try
    locObj := CreateObject();
    locObj.BinaryData := Copy(locBuffer);
    locObj.SaveToStream(locStream);
    Check( locStream.Size = Length(locObj.BinaryData) );
    SetLength(locBuffer,locStream.Size);
    locStream.Position := 0;
    locStream.Read(locBuffer[0],Length(locBuffer));
    CheckEquals( locObj.BinaryData, locBuffer );
  finally
    locObj.Free();
    locStream.Free();
  end;
end;

procedure TTest_TAbstractEncodedStringRemotable.SaveToFile();
var
  locObj : TAbstractEncodedStringRemotable;
  locBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TFileStream;
  i : PtrInt;
  locFileName : string;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locStream := nil;
  locObj := CreateObject();
  try
    locObj.BinaryData := Copy(locBuffer);
    locFileName := wstExpandLocalFileName('test_LoadBufferFromFile.bin');
    DeleteFile(locFileName);
    locObj.SaveToFile(locFileName);
    Check(FileExists(locFileName));
    locStream := TFileStream.Create(locFileName,fmOpenRead);
    Check( locStream.Size = Length(locObj.BinaryData) );
    SetLength(locBuffer,locStream.Size);
    locStream.Position := 0;
    locStream.Read(locBuffer[0],Length(locBuffer));
    CheckEquals( locObj.BinaryData, locBuffer );
  finally
    locObj.Free();
    locStream.Free();
  end;
end;

{ TTest_TAbstractEncodedStringExtRemotable }

class function TTest_TAbstractEncodedStringExtRemotable.EncodeData(const AValue: TBinaryString): string;
begin
  Result := EncodeData(StringToByteArray(AValue));
end;

procedure TTest_TAbstractEncodedStringExtRemotable.Equal();
const ITER = 100;
var
  i : Integer;
  a, b : TAbstractEncodedStringExtRemotable;
  c : TClass_A;
begin
  c := nil;
  b := nil;
  a := CreateObject();
  try
    b := CreateObject();
    CheckEquals(False, a.Equal(nil));
    c := TClass_A.Create();
    CheckEquals(False, a.Equal(c));
    a.BinaryData := StringToByteArray('wst');
    b.BinaryData := StringToByteArray('azerty');
    CheckEquals(False, a.Equal(b));
    CheckEquals(False, b.Equal(a));

    for i := 1 to ITER do begin
      a.BinaryData := RandomBytesValue(Random(500));
      b.BinaryData := a.BinaryData;
      CheckEquals(True, a.Equal(b));
      CheckEquals(True, b.Equal(a));
    end;
  finally
    FreeAndNil(c);
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

procedure TTest_TAbstractEncodedStringExtRemotable.SetBinaryData();
const ITER = 100;
var
  i : Integer;
  a : TAbstractEncodedStringExtRemotable;
  s : TBinaryString;
  es : string;
begin
  a := CreateObject();
  try
    s := ''; es := EncodeData(s);
    a.BinaryData := StringToByteArray(s);
    CheckEquals(StringToByteArray(s),a.BinaryData);
    CheckEquals(es,a.EncodedString);
    CheckEquals(StringToByteArray(s),a.BinaryData);
    CheckEquals(es,a.EncodedString);

    for i := 1 to ITER do begin
      s := RandomValue(Random(500)); es := EncodeData(s);
      a.BinaryData := StringToByteArray(s);
      CheckEquals(StringToByteArray(s),a.BinaryData);
      CheckEquals(es,a.EncodedString);
      CheckEquals(StringToByteArray(s),a.BinaryData);
      CheckEquals(es,a.EncodedString);
    end;
  finally
    FreeAndNil(a);
  end;
end;

procedure TTest_TAbstractEncodedStringExtRemotable.SetEncodedString();
const ITER = 100;
var
  i : Integer;
  a : TAbstractEncodedStringExtRemotable;
  s : TBinaryString;
  es : string;
begin
  a := CreateObject();
  try
    s := ''; es := Base64Encode(s);
    a.EncodedString := es;
    CheckEquals(StringToByteArray(s),a.BinaryData);
    CheckEquals(es,a.EncodedString);
    CheckEquals(StringToByteArray(s),a.BinaryData);
    CheckEquals(es,a.EncodedString);

    for i := 1 to ITER do begin
      s := RandomValue(Random(500)); es := EncodeData(s);
      a.EncodedString := es;
      CheckEquals(StringToByteArray(s),a.BinaryData);
      CheckEquals(es,a.EncodedString);
      CheckEquals(StringToByteArray(s),a.BinaryData);
      CheckEquals(es,a.EncodedString);
    end;
  finally
    FreeAndNil(a);
  end;
end;

procedure TTest_TAbstractEncodedStringExtRemotable.LoadFromStream();
var
  locLoadedBuffer : TAbstractEncodedStringExtRemotable;
  locBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TMemoryStream;
  i : PtrInt;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locLoadedBuffer := nil;
  locStream := TMemoryStream.Create();
  try
    locStream.Write(locBuffer[0],Length(locBuffer));
    locLoadedBuffer := CreateObject();
    locLoadedBuffer.LoadFromStream(locStream);
    CheckEquals( locBuffer, locLoadedBuffer.BinaryData );
  finally
    locLoadedBuffer.Free();
    locStream.Free();
  end;
end;

procedure TTest_TAbstractEncodedStringExtRemotable.LoadFromFile();
var
  locLoadedBuffer : TAbstractEncodedStringExtRemotable;
  locBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TMemoryStream;
  i : PtrInt;
  locFileName : string;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locLoadedBuffer := nil;
  locStream := TMemoryStream.Create();
  try
    locStream.Write(locBuffer[0],Length(locBuffer));
    locFileName := wstExpandLocalFileName('test_LoadBufferFromFile.bin');
    locStream.SaveToFile(locFileName);
    locLoadedBuffer := CreateObject();
    locLoadedBuffer.LoadFromFile(locFileName);
    CheckEquals( locBuffer, locLoadedBuffer.BinaryData );
  finally
    locLoadedBuffer.Free();
    locStream.Free();
  end;
end;

procedure TTest_TAbstractEncodedStringExtRemotable.SaveToStream();
var
  locObj : TAbstractEncodedStringExtRemotable;
  locBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TMemoryStream;
  i : PtrInt;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locObj := nil;
  locStream := TMemoryStream.Create();
  try
    locObj := CreateObject();
    locObj.BinaryData := locBuffer;
    locObj.SaveToStream(locStream);
    Check( locStream.Size = Length(locObj.BinaryData) );
    SetLength(locBuffer,locStream.Size);
    locStream.Position := 0;
    locStream.Read(locBuffer[0],Length(locBuffer));
    CheckEquals( locObj.BinaryData, locBuffer );
  finally
    locObj.Free();
    locStream.Free();
  end;
end;

procedure TTest_TAbstractEncodedStringExtRemotable.SaveToFile();
var
  locObj : TAbstractEncodedStringExtRemotable;
  locBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TFileStream;
  i : PtrInt;
  locFileName : string;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locStream := nil;
  locObj := CreateObject();
  try
    locObj.BinaryData := Copy(locBuffer);
    locFileName := wstExpandLocalFileName('test_LoadBufferFromFile.bin');
    DeleteFile(locFileName);
    locObj.SaveToFile(locFileName);
    Check(FileExists(locFileName));
    locStream := TFileStream.Create(locFileName,fmOpenRead);
    Check( locStream.Size = Length(locObj.BinaryData) );
    SetLength(locBuffer,locStream.Size);
    locStream.Position := 0;
    locStream.Read(locBuffer[0],Length(locBuffer));
    CheckEquals( locObj.BinaryData, locBuffer );
  finally
    locObj.Free();
    locStream.Free();
  end;
end;

procedure TTest_TAbstractEncodedStringExtRemotable.test_Assign();
const ITER = 100;
var
  i : Integer;
  a, b : TAbstractEncodedStringExtRemotable;
begin
  b := nil;
  a := CreateObject();
  try
    b := CreateObject();
    for i := 1 to ITER do begin
      a.BinaryData := RandomBytesValue(Random(500));
      b.Assign(a);
      CheckEquals(a.BinaryData, b.BinaryData);
      CheckEquals(a.EncodedString, b.EncodedString);
    end;
  finally
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

{ TClass_A_CollectionRemotable }

function TClass_A_CollectionRemotable.GetItem(AIndex : PtrInt) : TClass_A;
begin
  Result := TClass_A(inherited Item[AIndex]);
end;

class function TClass_A_CollectionRemotable.GetItemClass() : TBaseRemotableClass;
begin
  Result := TClass_A;
end;

function TClass_A_CollectionRemotable.Add() : TClass_A;
begin
  Result := TClass_A(inherited Add());
end;

function TClass_A_CollectionRemotable.AddAt(const APosition : PtrInt) : TClass_A;
begin
  Result := TClass_A(inherited AddAt(APosition));
end;

{ TTest_TObjectCollectionRemotable }

procedure TTest_TObjectCollectionRemotable.GetItemTypeInfo();
begin
  CheckEquals(
    PtrUInt(TClass_A_CollectionRemotable.GetItemClass().ClassInfo),
    PtrUInt(TClass_A_CollectionRemotable.GetItemTypeInfo())
  );
end;

procedure TTest_TObjectCollectionRemotable.Add();
var
  ls : TClass_A_CollectionRemotable;
  aa,ab : TClass_A;
begin
  ls := TClass_A_CollectionRemotable.Create();
  try
    aa := ls.Add();
    CheckNotNull(aa);
      CheckEquals(1,ls.Length);
      CheckSame(aa, ls[0]);
    ab := ls.Add();
    CheckNotNull(ab);
      CheckEquals(2,ls.Length);
      CheckSame(ab, ls[1]);
  finally
    ls.Free();
  end;
end;

procedure TTest_TObjectCollectionRemotable.Delete();
var
  ls : TClass_A_CollectionRemotable;
  aa,ab : TClass_A;
  ok : Boolean;
begin
  ls := TClass_A_CollectionRemotable.Create();
  try
    ok := False;
    try
      ls.Delete(-112);
    except
      ok := True;
    end;
    Check(ok);

    ok := False;
    try
      ls.Delete(0);
    except
      ok := True;
    end;
    Check(ok);

    ok := False;
    try
      ls.Delete(112);
    except
      ok := True;
    end;
    Check(ok);

    ls.Add();
    ls.Delete(0);
      CheckEquals(0,ls.Length);

    ls.Add();
    ab := ls.Add();
    ls.Delete(0);
      CheckEquals(1,ls.Length);
      CheckSame(ab,ls[0]);

    FreeAndNil(ls);
    ls := TClass_A_CollectionRemotable.Create();
    aa := ls.Add();
    ls.Add();
    ls.Delete(1);
      CheckEquals(1,ls.Length);
      CheckSame(aa,ls[0]);
  finally
    ls.Free();
  end;
end;

procedure TTest_TObjectCollectionRemotable.Equal();
var
  a, b : TClass_A_CollectionRemotable;
begin
  b := nil;
  a := TClass_A_CollectionRemotable.Create();
  try
    b := TClass_A_CollectionRemotable.Create();
      Check(a.Equal(b));
      Check(b.Equal(a));
    a.Add().Val_16S := 1;
    a.Add().Val_16S := 2;
      Check(not a.Equal(nil));
      Check(a.Equal(a));
      Check(not a.Equal(b));
      Check(not b.Equal(a));

    b.Add().Val_16S := 1;
      Check(not a.Equal(b));
      Check(not b.Equal(a));
    b.Add().Val_16S := 2;
      Check(a.Equal(b));
      Check(b.Equal(a));
  finally
    b.Free();
    a.Free();
  end;
end;

procedure TTest_TObjectCollectionRemotable.test_Assign();

  procedure Check_List(Aa, Ab : TClass_A_CollectionRemotable);
  var
    k : PtrInt;
  begin
    if ( Aa = nil ) then begin
      CheckNull(Ab);
    end else begin
      CheckNotNull(Ab);
      CheckEquals(Aa.Length,Ab.Length);
      if ( Aa.Length > 0 ) then begin
        for k := 0 to Pred(Aa.Length) do begin
          Check(Aa[k].Equal(Ab[k]));
        end;
      end;
    end;
  end;
  
var
  a, b : TClass_A_CollectionRemotable;
begin
  b := nil;
  a := TClass_A_CollectionRemotable.Create();
  try
    b := TClass_A_CollectionRemotable.Create();
      Check_List(a,b);
    a.Add().Val_16S := 1;
    a.Add().Val_16S := 2;
    b.Assign(a);
      Check_List(a,b);

    b.Add().Val_16S := 3;
    a.Assign(b);
      Check_List(a,b);

    a.Clear();
    b.Assign(a);
      Check_List(a,b);
  finally
    b.Free();
    a.Free();
  end;
end;

procedure TTest_TObjectCollectionRemotable.Exchange();
var
  ls : TClass_A_CollectionRemotable;
  a, b, c : TClass_A;
begin
  ls := TClass_A_CollectionRemotable.Create();
  try
    a := ls.Add();
    ls.Exchange(0,0);
      CheckSame(a,ls[0]);
    b := ls.Add();
    ls.Exchange(0,1);
      CheckSame(a,ls[1]);
      CheckSame(b,ls[0]);
    c := ls.Add();
    ls.Exchange(0,2);
      CheckSame(c,ls[0]);
      CheckSame(b,ls[2]);
  finally
    ls.Free();
  end;
end;

procedure TTest_TObjectCollectionRemotable.IndexOf();
var
  ls : TClass_A_CollectionRemotable;
begin
  ls := TClass_A_CollectionRemotable.Create();
  try
    CheckEquals(-1, ls.IndexOf(nil));
    ls.Add();
      CheckEquals(-1, ls.IndexOf(nil));
      CheckEquals(0, ls.IndexOf(ls[0]));
    ls.Add();
      CheckEquals(-1, ls.IndexOf(nil));
      CheckEquals(0, ls.IndexOf(ls[0]));
      CheckEquals(1, ls.IndexOf(ls[1]));
    ls.Add();
      CheckEquals(-1, ls.IndexOf(nil));
      CheckEquals(0, ls.IndexOf(ls[0]));
      CheckEquals(1, ls.IndexOf(ls[1]));
      CheckEquals(2, ls.IndexOf(ls[2]));
  finally
    ls.Free();
  end;
end;

{ TTest_Procedures }

procedure TTest_Procedures.test_LoadBufferFromStream();
var
  locBuffer, locLoadedBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TMemoryStream;
  i : PtrInt;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locStream := TMemoryStream.Create();
  try
    locStream.Write(locBuffer[0],Length(locBuffer));
    locLoadedBuffer := LoadBufferFromStream(locStream);
    CheckEquals( locBuffer, locLoadedBuffer );
  finally
    locStream.Free();
  end;
end;

procedure TTest_Procedures.test_LoadBufferFromFile();
var
  locBuffer, locLoadedBuffer : TByteDynArray;
  pBytePtr : PByte;
  locStream : TMemoryStream;
  i : PtrInt;
  locFileName : string;
begin
  SetLength(locBuffer,255);
  pBytePtr := PByte(@(locBuffer[0]));
  for i := Low(locBuffer) to High(locBuffer) do begin
    pBytePtr^ := i;
    Inc(pBytePtr);
  end;
  locStream := TMemoryStream.Create();
  try
    locStream.Write(locBuffer[0],Length(locBuffer));
    locFileName := wstExpandLocalFileName('test_LoadBufferFromFile.bin');
    locStream.SaveToFile(locFileName);
    locLoadedBuffer := LoadBufferFromFile(locFileName);
    CheckEquals(locBuffer, locLoadedBuffer);
  finally
    locStream.Free();
  end;
end;

{ TTest_TBase64StringRemotable }

class function TTest_TBase64StringRemotable.CreateObject( ): TAbstractEncodedStringRemotable;
begin
  Result := TBase64StringRemotable.Create();
end;

class function TTest_TBase64StringRemotable.EncodeData(const AValue: TByteDynArray): string;
begin
  if ( Length(AValue) > 0 ) then
    Result := Base64Encode(Length(AValue),AValue[0])
  else
    Result := '';
end;

{ TTest_TBase16StringRemotable }

class function TTest_TBase16StringRemotable.CreateObject( ): TAbstractEncodedStringRemotable;
begin
  Result := TBase16StringRemotable.Create();
end;

class function TTest_TBase16StringRemotable.EncodeData(const AValue: TByteDynArray): string;
begin
  if ( Length(AValue) > 0 ) then
    Result := Base16Encode(AValue[0],Length(AValue))
  else
    Result := '';
end;

{ TTest_TBase64StringExtRemotable }

class function TTest_TBase64StringExtRemotable.CreateObject( ): TAbstractEncodedStringExtRemotable;
begin
  Result := TBase64StringExtRemotable.Create();
end;

class function TTest_TBase64StringExtRemotable.EncodeData(const AValue: TByteDynArray): string;
begin
  if ( Length(AValue) > 0 ) then
    Result := Base64Encode(Length(AValue),AValue[0])
  else
    Result := '';
end;


{ TTest_TBase16StringExtRemotable }

class function TTest_TBase16StringExtRemotable.CreateObject( ): TAbstractEncodedStringExtRemotable;
begin
  Result := TBase16StringExtRemotable.Create();
end;

class function TTest_TBase16StringExtRemotable.EncodeData(const AValue: TByteDynArray): string;
begin
  if ( Length(AValue) > 0 ) then
    Result := Base16Encode(AValue[0],Length(AValue))
  else
    Result := '';
end;

initialization
  RegisterTest('Support',TTest_TObjectCollectionRemotable.Suite);
  RegisterTest('Support',TTest_TBaseComplexRemotable.Suite);
  RegisterTest('Support',TTest_TStringBufferRemotable.Suite);
  RegisterTest('Support-Date',TTest_TDateTimeRemotable.Suite);
  RegisterTest('Support-Date',TTest_TDateRemotable.Suite);
  RegisterTest('Support-Date',TTest_TDurationRemotable.Suite);
  RegisterTest('Support-Date',TTest_TTimeRemotable.Suite);
  
  RegisterTest('Support',TTest_TArrayOfStringRemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfBooleanRemotable.Suite);
  
  RegisterTest('Support',TTest_TArrayOfInt8URemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfInt8SRemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfInt16SRemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfInt16URemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfInt32URemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfInt32SRemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfInt64SRemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfInt64URemotable.Suite);
  
  RegisterTest('Support',TTest_TArrayOfFloatSingleRemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfFloatDoubleRemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfFloatExtendedRemotable.Suite);
  RegisterTest('Support',TTest_TArrayOfFloatCurrencyRemotable.Suite);
  
  RegisterTest('Support',TTest_TBaseObjectArrayRemotable.Suite);

  RegisterTest('Support',TTest_TBase64StringRemotable.Suite);
  RegisterTest('Support',TTest_TBase64StringExtRemotable.Suite);
  RegisterTest('Support',TTest_TBase16StringRemotable.Suite);
  RegisterTest('Support',TTest_TBase16StringExtRemotable.Suite);
  
  RegisterTest('Support',TTest_Procedures.Suite);
  
end.

