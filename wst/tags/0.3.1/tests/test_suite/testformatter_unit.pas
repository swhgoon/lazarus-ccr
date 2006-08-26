{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit testformatter_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  TypInfo,
  base_service_intf;

type

  TTestEnum = ( teOne, teTwo, teThree, teFour );

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

  { TTestFormatter }

  TTestFormatter= class(TTestCase)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;virtual;abstract;
  published
    procedure Test_Int_8;
    procedure Test_Int_16;
    procedure Test_Int_32;
    procedure Test_Int_64;
    procedure Test_Int_WithClass;
    
    procedure Test_Single_4;
    procedure Test_Double_8;
    procedure Test_Currency_8;
    procedure Test_Extended_10;
    procedure Test_Float_WithClass;

    procedure Test_String;
    procedure Test_Bool;
    procedure Test_Enum;
    procedure Test_Enum_Bool_String_WithClass;
    
    procedure Test_Object();
    procedure Test_Object_Nil();
    procedure Test_StringArray();
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
  end;

  { TTestBinaryFormatter }

  TTestBinaryFormatter= class(TTestFormatter)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  { TTestBinaryFormatterAttributes }

  TTestBinaryFormatterAttributes= class(TTestBinaryFormatter)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  { TTestSOAPFormatter }

  TTestSOAPFormatter= class(TTestFormatter)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  { TTestSOAPFormatterAttributes }

  TTestSOAPFormatterAttributes = class(TTestSOAPFormatter)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
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

implementation
uses base_binary_formatter, base_soap_formatter;

procedure TTestFormatter.Test_Int_8;
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
    f.SaveToStream(s);
    intVal_U := 0;
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginScopeRead(x,TypeInfo(TClass_Int),stObject);
      x := 'intVal_U';
      f.Get(TypeInfo(Byte),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(ShortInt),x,intVal_S);
    f.EndScopeRead();

    AssertEquals(VAL_1,intVal_U);
    AssertEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Int_16;
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
    f.BeginScopeRead(x,TypeInfo(TClass_Int),stObject);
      x := 'intVal_U';
      f.Get(TypeInfo(Word),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(SmallInt),x,intVal_S);
    f.EndScopeRead();

    AssertEquals(VAL_1,intVal_U);
    AssertEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Int_32;
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
    f.BeginScopeRead(x,TypeInfo(TClass_Int),stObject);
      x := 'intVal_U';
      f.Get(TypeInfo(LongWord),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(LongInt),x,intVal_S);
    f.EndScopeRead();

    AssertEquals(VAL_1,intVal_U);
    AssertEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Int_64;
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
    f.BeginScopeRead(x,TypeInfo(TClass_Int),stObject);
      x := 'intVal_U';
      f.Get(TypeInfo(QWord),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(Int64),x,intVal_S);
    f.EndScopeRead();

    AssertEquals(VAL_1,intVal_U);
    AssertEquals(VAL_2,intVal_S);
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
    f.SaveToStream(s);
    FreeAndNil(a);
    
    a := TClass_Int.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginScopeRead(x,TypeInfo(TClass_Int),stObject);
      x := 'o1';
      f.Get(TypeInfo(TClass_Int),x,a);
    f.EndScopeRead();
    
    AssertEquals(8,a.Val_8U);
      AssertEquals(-8,a.Val_8S);
    AssertEquals(16,a.Val_16U);
      AssertEquals(-16,a.Val_16S);
    AssertEquals(32,a.Val_32U);
      AssertEquals(-32,a.Val_32S);
    AssertEquals(64,a.Val_64U);
      AssertEquals(-64,a.Val_64S);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Single_4;
const VAL_1 = 12.10;
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
    f.SaveToStream(s);
    tmpVal := 0;
    
    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginScopeRead(x,TypeInfo(TClass_Float),stObject);
      x := 'tmpVal';
      f.Get(TypeInfo(Single),x,tmpVal);
    f.EndScopeRead();

    AssertEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Double_8;
const VAL_1 = 12.10;
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
    f.SaveToStream(s);
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginScopeRead(x,TypeInfo(TClass_Float),stObject);
      x := 'tmpVal';
      f.Get(TypeInfo(Double),x,tmpVal);
    f.EndScopeRead();

    AssertEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Currency_8;
const VAL_1 = 12.10;
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
    f.SaveToStream(s);
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginScopeRead(x,TypeInfo(TClass_Float),stObject);
      x := 'tmpVal';
      f.Get(TypeInfo(Currency),x,tmpVal);
    f.EndScopeRead();

    AssertEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Extended_10;
const VAL_1 = 12.10;
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
    f.SaveToStream(s);
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginScopeRead(x,TypeInfo(TClass_Float),stObject);
      x := 'tmpVal';
      f.Get(TypeInfo(Extended),x,tmpVal);
    f.EndScopeRead();

    AssertEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Float_WithClass;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_Float;
  x : string;
begin
  s := Nil;
  a := TClass_Float.Create();
  Try
    a.Val_Currency := 8.8;
    a.Val_Double := 8.8;
    a.Val_Extended := 10.10;
    a.Val_Single := 4.4;

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
    f.BeginScopeRead(x,TypeInfo(TClass_Float),stObject);
      x := 'o1';
      f.Get(TypeInfo(TClass_Float),x,a);
    f.EndScopeRead();

    AssertEquals(4.4,a.Val_Single);
    AssertEquals(8.8,a.Val_Double);
    AssertEquals(8.8,a.Val_Currency);
    AssertEquals(10.10,a.Val_Extended);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_String;
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
    f.BeginScopeRead(x,TypeInfo(TClass_Int),stObject);
      x := 'intVal_1';
      f.Get(TypeInfo(string),x,intVal_1);
      x := 'intVal_3';
      f.Get(TypeInfo(string),x,intVal_3);
    f.EndScopeRead();

    AssertEquals(VAL_1,intVal_1);
    AssertEquals(VAL_2,intVal_3);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Bool;
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
    f.BeginScopeRead(x,TypeInfo(TClass_Int),stObject);
      x := 'intVal_1';
      f.Get(TypeInfo(Boolean),x,intVal_1);
      x := 'intVal_3';
      f.Get(TypeInfo(Boolean),x,intVal_3);
    f.EndScopeRead();

    AssertEquals(VAL_1,intVal_1);
    AssertEquals(VAL_2,intVal_3);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Enum;
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
    f.BeginScopeRead(x,TypeInfo(TClass_Int),stObject);
      x := 'intVal_1';
      f.Get(TypeInfo(TTestEnum),x,intVal_1);
      x := 'intVal_3';
      f.Get(TypeInfo(TTestEnum),x,intVal_3);
    f.EndScopeRead();

    AssertEquals(Ord(VAL_1),Ord(intVal_1));
    AssertEquals(Ord(VAL_2),Ord(intVal_3));
  Finally
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
    f.BeginScopeRead(x,TypeInfo(TClass_Enum),stObject);
      x := 'o1';
      f.Get(TypeInfo(TClass_Enum),x,a);
    f.EndScopeRead();

    AssertEquals(True,a.Val_Bool);
    AssertEquals(Ord(teThree),Ord(a.Val_Enum));
    AssertEquals('atou',a.Val_String);
  Finally
    a.Free();
    s.Free();
  End;
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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'o1';
      f.Get(TypeInfo(TClass_B),x,a);
    f.EndScopeRead();

    AssertEquals(False,a.Val_Bool);
    AssertEquals(Ord(teThree),Ord(a.Val_Enum));
    AssertEquals('123',a.Val_String);
    
    AssertEquals(True,a.ObjProp.Val_Bool);
    AssertEquals(Ord(teFour),Ord(a.ObjProp.Val_Enum));
    AssertEquals('456',a.ObjProp.Val_String);
    AssertEquals(121076,a.ObjProp.Val_32S);
    
    AssertEquals(0,a.NonStored);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Object_Nil();
begin
  Fail('Write me!');
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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);
    
    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfStringRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfStringRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfStringRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);
    
    for i := 0 to Pred(AR_LEN) do
      AssertEquals(VAL_AR[i],a[i]);

  finally
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
    AssertEquals(0,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfStringRemotable),x,a);
    f.EndScopeRead();
    AssertNotNull(a);
    AssertEquals(0,a.Length);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfBooleanRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertEquals(VAL_AR[i],a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt8URemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int8SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of ShortInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt8SRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt8SRemotable.Create();
  try
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt8SRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertEquals(VAL_AR[i],a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt16SRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertEquals(VAL_AR[i],a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt16URemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertEquals(VAL_AR[i],a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt32URemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertEquals(VAL_AR[i],a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt32SRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertEquals(VAL_AR[i],a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt64SRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertEquals(VAL_AR[i],a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt64URemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertTrue(VAL_AR[i]=a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatSingleRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertTrue(VAL_AR[i]=a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatDoubleRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertTrue(VAL_AR[i]=a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatExtendedRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertTrue(VAL_AR[i]=a[i]);

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
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    a.SetLength(AR_LEN);
    AssertEquals(AR_LEN,a.Length);

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
    f.BeginScopeRead(x,TypeInfo(TClass_B),stObject);
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatCurrencyRemotable),x,a);
    f.EndScopeRead();
    AssertEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      AssertTrue(VAL_AR[i]=a[i]);

  finally
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

{ TTestSOAPFormatter }

function TTestSOAPFormatter.CreateFormatter(ARootType : PTypeInfo):IFormatterBase;
begin
  Result := TSOAPBaseFormatter.Create() as IFormatterBase;
  Result.BeginObject('Env',ARootType)
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(ansistring))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(ansistring))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));
    
    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertEquals('Item',VAL_AR[j],a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(Boolean))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(Boolean))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertEquals('Item',VAL_AR[j],a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(Byte))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(Byte))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertEquals('Item',VAL_AR[j],a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int8SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of ShortInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt8SRemotable;
  i, j : Integer;
begin
  a := TArrayOfInt8SRemotable.Create();
  try
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(ShortInt))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(ShortInt))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertEquals('Item',VAL_AR[j],a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(SmallInt))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(SmallInt))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertEquals('Item',VAL_AR[j],a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(Word))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(Word))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertEquals('Item',VAL_AR[j],a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(LongWord))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(LongWord))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertEquals('Item',VAL_AR[j],a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(LongInt))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(LongInt))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertEquals('Item',VAL_AR[j],a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(Int64))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(Int64))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertEquals('Item',VAL_AR[j],a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(QWord))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(QWord))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertTrue('Item',VAL_AR[j]=a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(Single))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(Single))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertTrue('Item',VAL_AR[j]=a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(Double))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(Double))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertTrue('Item',VAL_AR[j]=a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(Extended))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(Extended))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertTrue('Item',VAL_AR[j]=a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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
    AssertEquals('TypeInfo',PTypeInfo(TypeInfo(Currency))^.Name,a.GetItemTypeInfo()^.Name);
    AssertEquals('TypeInfo',Ord(PTypeInfo(TypeInfo(Currency))^.Kind),Ord(a.GetItemTypeInfo()^.Kind));

    AssertEquals(0,a.Length);
    a.SetLength(0);
    AssertEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      AssertEquals('Length',i,a.Length);
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        AssertTrue('Item',VAL_AR[j]=a[j]);
    end;

    a.SetLength(0);
    AssertEquals(0,a.Length);
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

    AssertEquals(a.Val_Bool,b.Val_Bool);
    AssertEquals(Ord(a.Val_Enum),Ord(b.Val_Enum));
    AssertEquals(a.Val_String,b.Val_String);

    AssertEquals(a.ObjProp.Val_Bool,b.ObjProp.Val_Bool);
    AssertEquals(Ord(a.ObjProp.Val_Enum),Ord(b.ObjProp.Val_Enum));
    AssertEquals(a.ObjProp.Val_String,a.ObjProp.Val_String);
  Finally
    a.Free();
    b.Free();
  End;
end;

{ TTestSOAPFormatterAttributes }

function TTestSOAPFormatterAttributes.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := inherited CreateFormatter(ARootType);
  Result.SetSerializationStyle(ssAttibuteSerialization);
end;

{ TTestBinaryFormatterAttributes }

function TTestBinaryFormatterAttributes.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := inherited CreateFormatter(ARootType);
  Result.SetSerializationStyle(ssAttibuteSerialization);
end;

initialization
  RegisterStdTypes();
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TTestEnum),'TTestEnum');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_Int),'TClass_Int');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_Enum),'TClass_Enum');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_A),'TClass_A');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_B),'TClass_B');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_Float),'TClass_Float');


  RegisterTest(TTestArray);
  RegisterTest(TTestSOAPFormatter);
  RegisterTest(TTestBinaryFormatter);
  RegisterTest(TTest_TBaseComplexRemotable);
  RegisterTest(TTestSOAPFormatterAttributes);
  RegisterTest(TTestBinaryFormatterAttributes);
end.

