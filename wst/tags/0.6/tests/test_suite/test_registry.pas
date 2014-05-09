{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_registry;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  TypInfo,
  wst_types, base_service_intf;
  
const
  s_sample_namespace = 'org.wst.sample';
  
type

  { TClass_A }

  TClass_A = class(TBaseComplexRemotable)
  private
    FIntProp : Integer;
    FStrProp : string;
  published
    // StrProp is an attribute property in this class !
    property StrProp : string read FStrProp write FStrProp;
    property IntProp : Integer read FIntProp write FIntProp;
  end;

  { TClass_B }

  TClass_B = class(TBaseComplexRemotable)
  private
    FIntProp : Integer;
    FStrProp : string;
  published
    property StrProp : string read FStrProp write FStrProp;
    property IntProp : Integer read FIntProp write FIntProp;
  end;
  
  TClass_C = class(TBaseComplexRemotable)
  private
    FIntProp : Integer;
    FStrProp : string;
  published
    property StrProp : string read FStrProp write FStrProp;
    //IntProp is an attribute property
    property IntProp : Integer read FIntProp write FIntProp;
  end;
  
  { TTest_TTypeRegistry }

  TTest_TTypeRegistry = class(TTestCase)
  protected
  published
    procedure Register();
    procedure Register_with_declared_name();
    procedure isAttributeProperty();
    procedure register_external_prop();
    procedure synonym_procs();
  end;

implementation

{ TTest_TTypeRegistry }

procedure TTest_TTypeRegistry.Register();
var
  reg : TTypeRegistry;
  regItem0 : TTypeRegistryItem;
  c : PtrInt;
begin
  reg := TTypeRegistry.Create();
  try
    CheckEquals(0, reg.Count, 'Count');
    c := reg.Count;
    regItem0 := reg.Register(s_sample_namespace,TypeInfo(TClass_A));
    CheckEquals( ( c + 1 ), reg.Count, 'Count');
    CheckSame(regItem0,reg.Find(TypeInfo(TClass_A),True));
    CheckSame(regItem0,reg.ItemByTypeInfo[TypeInfo(TClass_A)]);
    Check(regItem0.DataType = TypeInfo(TClass_A),'Item.DataType');
    CheckEquals(TClass_A.ClassName,regItem0.DeclaredName);
    CheckEquals(s_sample_namespace,regItem0.NameSpace);
  finally
    reg.Free();
  end;
end;

procedure TTest_TTypeRegistry.Register_with_declared_name();
const s_declared_name = 'sample_declared_name';
var
  reg : TTypeRegistry;
  regItem0 : TTypeRegistryItem;
  c : PtrInt;
begin
  reg := TTypeRegistry.Create();
  try
    CheckEquals(0, reg.Count, 'Count');
    c := reg.Count;
    regItem0 := reg.Register(s_sample_namespace,TypeInfo(TClass_A),s_declared_name);
    CheckEquals( ( c + 1 ), reg.Count, 'Count');
    CheckSame(regItem0,reg.Find(TypeInfo(TClass_A),True));
    CheckSame(regItem0,reg.ItemByTypeInfo[TypeInfo(TClass_A)]);
    Check(regItem0.DataType = TypeInfo(TClass_A),'Item.DataType');
    CheckEquals(s_declared_name,regItem0.DeclaredName);
    CheckEquals(s_sample_namespace,regItem0.NameSpace);
  finally
    reg.Free();
  end;
end;

procedure TTest_TTypeRegistry.isAttributeProperty();
begin
  Check(TClass_A.IsAttributeProperty('StrProp'));
    Check(not TClass_A.IsAttributeProperty('IntProp'));
  Check(not TClass_B.IsAttributeProperty('StrProp'));
  Check(TClass_C.IsAttributeProperty('IntProp'));
    Check(not TClass_C.IsAttributeProperty('StrProp'));
end;

procedure TTest_TTypeRegistry.register_external_prop();
const s_ext_name = 'sample_external_name';
var
  reg : TTypeRegistry;
  regItem : TTypeRegistryItem;
begin
  reg := TTypeRegistry.Create();
  try
    regItem := reg.Register(s_sample_namespace,TypeInfo(TClass_A));
    regItem.RegisterExternalPropertyName('StrProp',s_ext_name);
    CheckEquals(s_ext_name,regItem.GetExternalPropertyName('StrProp'));
    CheckEquals('StrProp',regItem.GetInternalPropertyName(s_ext_name));
  finally
    reg.Free();
  end;
end;

procedure TTest_TTypeRegistry.synonym_procs();
const s_ext_name = 'sample_external_name';
var
  reg : TTypeRegistry;
  regItem : TTypeRegistryItem;
begin
  reg := TTypeRegistry.Create();
  try
    regItem := reg.Register(s_sample_namespace,TypeInfo(TClass_A));
    regItem.AddPascalSynonym(s_ext_name);
    Check(regItem.IsSynonym(s_ext_name));
    CheckSame(regItem, reg.Find(s_ext_name));
  finally
    reg.Free();
  end;
end;

initialization
  GetTypeRegistry().Register(s_sample_namespace,TypeInfo(TClass_A));
    TClass_A.RegisterAttributeProperty('StrProp');
  GetTypeRegistry().Register(s_sample_namespace,TypeInfo(TClass_B));
  GetTypeRegistry().Register(s_sample_namespace,TypeInfo(TClass_C));
    TClass_C.RegisterAttributeProperty('IntProp');
    
  RegisterTest('Registry',TTest_TTypeRegistry.Suite);
end.

