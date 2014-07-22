{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_utilities;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  TypInfo,
  base_service_intf, server_service_intf,
  library_imp_utils, parserutils, imp_utils;

type

  ITest = interface
    ['{61442DCF-0F6B-490F-AA33-FF856C07A757}']
    procedure SayHello();
    procedure DontPool();
  end;

  { TTestClass }

  TTestClass = class(TActivableServiceImplementation,IObjectControl,ITest)
  private
    FPooled : Boolean;
  protected
    procedure SayHello();
    function CanBePooled() : Boolean;override;
    procedure DontPool();
  public
    constructor Create();override;
  end;

  ISimple_A = interface
     ['{D015AD95-6062-4650-9B00-CF3004E9CA1A}']//['{4793180A-DAA4-4E50-9194-5EEEE851EBE3}']
  end;

  ISimple_B = interface
     ['{4793180A-DAA4-4E50-9194-5EEEE851EBE3}']
  end;

  TSimpleFactoryItem_A = class(TSimpleFactoryItem,IInterface,ISimple_A)
  end;

  TSimpleFactoryItem_B = class(TSimpleFactoryItem,IInterface,ISimple_B)
  end;

  { TTest_TIntfPoolItem }

  TTest_TIntfPoolItem = class(TTestCase)
  published
    procedure All();
  end;

  { TTest_TSimpleItemFactory }

  TTest_TSimpleItemFactory = class(TTestCase)
  published
    procedure CreateProc();
    procedure CreateInstance();
  end;
  
  { TTest_TIntfPool }

  TTest_TIntfPool= class(TTestCase)
  published
    procedure Create_ZEROS();
    procedure Create_NON_ZERO_MIN();
    procedure Release();
{$IFDEF WST_SEMAPHORE_TIMEOUT}
    procedure Release_NON();
{$ENDIF WST_SEMAPHORE_TIMEOUT}
    procedure Discard();
  end;
  
  { TTest_TSimpleItemFactoryEx }

  TTest_TSimpleItemFactoryEx = class(TTestCase)
  published
    procedure NOT_Pooled();
    procedure POOLED_Create_ZEROS();
    procedure POOLED_Release();
{$IFDEF WST_SEMAPHORE_TIMEOUT}
    procedure POOLED_Release_NON();
{$ENDIF WST_SEMAPHORE_TIMEOUT}
    procedure POOLED_Discard();
  end;

  { TTest_TImplementationFactory }

  TTest_TImplementationFactory = class(TTestCase)
  published
    procedure POOLED_Discard();
    procedure extension_empty();
    procedure extension_simple();
    procedure extension_array_empty();
    procedure extension_array_simple();
    procedure extension_duplicate();
  end;
  
  { TwstModuleNotLoad }

  TwstModuleNotLoad = class(TwstModule,IInterface,IwstModule)
  protected
    procedure Load(const ADoLoad : Boolean);override;
  end;

  { TTest_TwstModuleManager }

  TTest_TwstModuleManager = class(TTestCase)
  published
    procedure Get(const AFileName : string);
    procedure Clear();
    procedure GetCount();
    procedure GetItem(const AIndex : PtrInt);
  end;

  { TTest_Procs }

  TTest_Procs = class(TTestCase)
  published
    procedure test_GetToken();
{$IFNDEF WST_HAS_STRICT_DELIMITER}
    procedure test_SetListData();
{$ENDIF WST_HAS_STRICT_DELIMITER}
  end;

implementation

var
  ListToRelease : IInterfaceList;
procedure Finalize_ListToRelease();
var
  i : Integer;
begin
  if ( ListToRelease <> nil ) and ( ListToRelease.Count > 0 ) then begin
    for i := 0 to Pred(ListToRelease.Count) do
      ListToRelease[i]._Release();
    ListToRelease := nil;
  end;
end;

{ TTestClass }

procedure TTestClass.SayHello();
begin

end;

function TTestClass.CanBePooled() : Boolean;
begin
  Result := FPooled;
end;

procedure TTestClass.DontPool();
begin
  FPooled := False;
end;

constructor TTestClass.Create();
begin
  inherited Create();
  FPooled := True;
  _AddRef(); // not to allow the rtl to reuse the same memory for another instance of the same class!!
  ListToRelease.Add(Self as IInterface);
end;

{ TTest_TIntfPool }

procedure TTest_TIntfPool.Create_ZEROS();
var
  ok : Boolean;
  obj : TIntfPool;
begin
  ok := False;
  try
    obj := TIntfPool.Create(0,0,TSimpleItemFactory.Create(TTestClass));
  except
    ok := True;
  end;
  Check(ok);
end;

procedure TTest_TIntfPool.Create_NON_ZERO_MIN();
const MIN_A = Integer(1); MAX_A = Integer(5);
var
  obj : TIntfPool;
begin
  obj := TIntfPool.Create(MIN_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  CheckEquals(MIN_A,obj.Min);
  CheckEquals(MAX_A,obj.Max);
  CheckEquals(MIN_A,obj.GetInstancesCount());
  obj.Free();
end;

procedure TTest_TIntfPool.Release();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : TIntfPool;
  elt : ITest;
  i : Integer;
begin
  obj := TIntfPool.Create(MIN_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  for i := 0 to 300 do begin
    elt := obj.Get(0) as ITest;
    elt.SayHello();
    obj.Release(elt);
  end;
  
  FreeAndNil(obj);
  obj := TIntfPool.Create(MIN_B,MAX_A,TSimpleItemFactory.Create(TTestClass));
  for i := 0 to 300 do begin
    elt := obj.Get(0) as ITest;
    elt.SayHello();
    obj.Release(elt);
  end;
  
  FreeAndNil(obj);
  obj := TIntfPool.Create(MAX_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  for i := 0 to 300 do begin
    elt := obj.Get(0) as ITest;
    elt.SayHello();
    obj.Release(elt);
  end;
  FreeAndNil(obj);
end;

{$IFDEF WST_SEMAPHORE_TIMEOUT}
procedure TTest_TIntfPool.Release_NON();
const MIN_A = Integer(1); MAX_A = Integer(5);
var
  obj : TIntfPool;
  elt : ITest;
  i : Integer;
  ok : Boolean;
  il : IInterfaceList;
begin
  il := TInterfaceList.Create();
  obj := TIntfPool.Create(MIN_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  for i := 1 to MAX_A do begin
    elt := obj.Get(100) as ITest;
    elt.SayHello();
    il.Add(elt);
    //obj.Release(elt); do not release
  end;
  ok := False;
  try
    elt := obj.Get(100) as ITest;
  except
    ok := True;
  end;
  Check(ok);
  CheckEquals(MAX_A,obj.GetInstancesCount());
  for i := 0 to Pred(MAX_A) do begin
    obj.Release(il[0]);
    il.Delete(0);
  end;

  for i := 1 to 100 do begin
    elt := obj.Get(100) as ITest;
    elt.SayHello();
    il.Add(elt);
    obj.Release(elt);
  end;
  FreeAndNil(obj);
end;
{$ENDIF WST_SEMAPHORE_TIMEOUT}

procedure TTest_TIntfPool.Discard();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : TIntfPool;
  oldElt, elt : ITest;
begin
  obj := TIntfPool.Create(MIN_A,MIN_A,TSimpleItemFactory.Create(TTestClass));
  elt := obj.Get(10) as ITest;
  oldElt := elt;
  obj.Release(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt = elt);
  obj.Discard(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt <> elt );
  
  FreeAndNil(obj);oldElt := nil; elt := nil;
  obj := TIntfPool.Create(MIN_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  elt := obj.Get(10) as ITest;
  oldElt := elt;
  obj.Release(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt = elt);
  obj.Discard(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt <> elt );
  
  FreeAndNil(obj);oldElt := nil; elt := nil;
  obj := TIntfPool.Create(MIN_B,MIN_A,TSimpleItemFactory.Create(TTestClass));
  elt := obj.Get(10) as ITest;
  oldElt := elt;
  obj.Release(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt = elt);
  obj.Discard(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt <> elt );

  FreeAndNil(obj);oldElt := nil; elt := nil;
  obj := TIntfPool.Create(MIN_B,MAX_A,TSimpleItemFactory.Create(TTestClass));
  elt := obj.Get(10) as ITest;
  oldElt := elt;
  obj.Release(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt = elt);
  obj.Discard(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt <> elt );
  FreeAndNil(obj);
end;

{ TTest_TSimpleItemFactoryEx }

procedure TTest_TSimpleItemFactoryEx.NOT_Pooled();
var
  obj : IItemFactoryEx;
  elt : ITest;
  i : Integer;
begin
  obj := TSimpleItemFactoryEx.Create(TTestClass);
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
  end;
  
  obj := TSimpleItemFactoryEx.Create(TTestClass,'');
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
  end;
end;

procedure TTest_TSimpleItemFactoryEx.POOLED_Create_ZEROS();
var
  ok : Boolean;
  obj : IItemFactoryEx;
begin
  ok := False;
  try
    obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;Pooled=True',[0,0]));
  except
    ok := True;
  end;
  Check(ok);
end;

procedure TTest_TSimpleItemFactoryEx.POOLED_Release();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : IItemFactoryEx;
  elt : ITest;
  i : Integer;
begin
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;Pooled=True',[MIN_A,MAX_A]));
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    obj.ReleaseInstance(elt);
  end;

  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;Pooled=True',[MIN_B,MAX_A]));
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    obj.ReleaseInstance(elt);
  end;

  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;Pooled=True',[MAX_A,MAX_A]));
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    obj.ReleaseInstance(elt);
  end;
end;

{$IFDEF WST_SEMAPHORE_TIMEOUT}
procedure TTest_TSimpleItemFactoryEx.POOLED_Release_NON();
const MIN_A = Integer(1); MAX_A = Integer(5);
var
  obj : IItemFactoryEx;
  elt : ITest;
  i : Integer;
  ok : Boolean;
  il : IInterfaceList;
begin
  il := TInterfaceList.Create();
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MAX_A]));
  for i := 1 to MAX_A do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    il.Add(elt);
    //obj.Release(elt); do not release
  end;
  ok := False;
  try
    elt := obj.CreateInstance() as ITest;
  except
    ok := True;
  end;
  Check(ok);
  for i := 0 to Pred(MAX_A) do begin
    obj.ReleaseInstance(il[0]);
    il.Delete(0);
  end;

  for i := 1 to 100 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    il.Add(elt);
    obj.ReleaseInstance(elt);
  end;
end;
{$ENDIF WST_SEMAPHORE_TIMEOUT}

procedure TTest_TSimpleItemFactoryEx.POOLED_Discard();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : IItemFactoryEx;
  oldElt, elt : ITest;
begin
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MIN_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'1.1');
  obj.DiscardInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt, '1.2' );

  oldElt := nil; elt := nil;
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MAX_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'2.1');
  obj.DiscardInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt ,'2.2');

  oldElt := nil; elt := nil;
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_B,MIN_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'3.1');
  obj.DiscardInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt ,'3.2');

  oldElt := nil; elt := nil;
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_B,MAX_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'4.1');
  obj.DiscardInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt,'4.2');
end;

{ TTest_TImplementationFactory }

procedure TTest_TImplementationFactory.POOLED_Discard();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : IItemFactoryEx;
  oldElt, elt : ITest;
begin
  obj := TImplementationFactory.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MIN_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'1.1');
  elt.DontPool();
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt, '1.2' );

  oldElt := nil; elt := nil;
  obj := TImplementationFactory.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MAX_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'2.1');
  elt.DontPool();
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt ,'2.2');

  oldElt := nil; elt := nil;
  obj := TImplementationFactory.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_B,MIN_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'3.1');
  elt.DontPool();
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt ,'3.2');

  oldElt := nil; elt := nil;
  obj := TImplementationFactory.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_B,MAX_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'4.1');
  elt.DontPool();
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt,'4.2');
end;

procedure TTest_TImplementationFactory.extension_empty(); 
var
  obj : IServiceImplementationFactory;
  s : string;
begin
  obj := TImplementationFactory.Create(TTestClass);
  CheckEquals(False,obj.GetExtension(s));
  CheckEquals('',s)
end;

procedure TTest_TImplementationFactory.extension_simple(); 
var
  obj : IServiceImplementationFactory;
  s : string;
  pm : IPropertyManager;
begin
  obj := TImplementationFactory.Create(TTestClass);
  obj.RegisterExtension('a','');
  CheckEquals(True,obj.GetExtension(s));
  CheckEquals('a',s);
  
  obj := TImplementationFactory.Create(TTestClass);
  obj.RegisterExtension('a','a.val');
  CheckEquals(True,obj.GetExtension(s));
  CheckEquals('a',s);
  pm := obj.GetPropertyManager(sSERVICES_EXTENSIONS,False);
  Check((pm <> nil),'GetPropertyManager(sSERVICES_EXTENSIONS,False)');
  CheckEquals('a.val',pm.GetProperty('a'));
  obj.RegisterExtension('b','');
    CheckEquals(True,obj.GetExtension(s));
    CheckEquals('a;b',s);   
    CheckEquals('a.val',pm.GetProperty('a'));
    CheckEquals('',pm.GetProperty('b'));
  obj.RegisterExtension('c','123');
    CheckEquals(True,obj.GetExtension(s));
    CheckEquals('a;b;c',s);   
    CheckEquals('a.val',pm.GetProperty('a'));
    CheckEquals('',pm.GetProperty('b'));
    CheckEquals('123',pm.GetProperty('c'));
end;

procedure TTest_TImplementationFactory.extension_array_empty(); 
var
  obj : IServiceImplementationFactory;
  s : string;
begin
  obj := TImplementationFactory.Create(TTestClass);
  obj.RegisterExtension([]);
  CheckEquals(False,obj.GetExtension(s));
  CheckEquals('',s);
  
  obj := TImplementationFactory.Create(TTestClass);
  obj.RegisterExtension(['']);
  CheckEquals(False,obj.GetExtension(s));
  CheckEquals('',s);
  obj.RegisterExtension(['','']);
  CheckEquals(False,obj.GetExtension(s));
  CheckEquals('',s);
end;

procedure TTest_TImplementationFactory.extension_array_simple(); 
var
  obj : IServiceImplementationFactory;
  s : string;
  pm : IPropertyManager;
begin
  obj := TImplementationFactory.Create(TTestClass);
  obj.RegisterExtension(['a']);
  CheckEquals(True,obj.GetExtension(s));
  CheckEquals('a',s);
  
  obj := TImplementationFactory.Create(TTestClass);
  obj.RegisterExtension(['a']);
  CheckEquals(True,obj.GetExtension(s));
  CheckEquals('a',s);
  pm := obj.GetPropertyManager(sSERVICES_EXTENSIONS,False);
  Check((pm <> nil),'GetPropertyManager(sSERVICES_EXTENSIONS,False)');
  CheckEquals('',pm.GetProperty('a'));
  obj.RegisterExtension(['b']);
    CheckEquals(True,obj.GetExtension(s));
    CheckEquals('a;b',s);   
    CheckEquals('',pm.GetProperty('a'));
    CheckEquals('',pm.GetProperty('b'));
  obj.RegisterExtension(['c','123']);
    CheckEquals(True,obj.GetExtension(s));
    CheckEquals('a;b;c;123',s);   
    CheckEquals('',pm.GetProperty('a'));
    CheckEquals('',pm.GetProperty('b'));
    CheckEquals('',pm.GetProperty('c'));
    CheckEquals('',pm.GetProperty('123'));
end;

procedure TTest_TImplementationFactory.extension_duplicate(); 
var
  obj : IServiceImplementationFactory;
  s : string;
begin
  obj := TImplementationFactory.Create(TTestClass);
  obj.RegisterExtension('a','');
  CheckEquals(True,obj.GetExtension(s));
  CheckEquals('a',s);
  // Should not duplicate
  obj.RegisterExtension('a','');
  CheckEquals(True,obj.GetExtension(s));
  CheckEquals('a',s);

  obj.RegisterExtension('b','');
  CheckEquals(True,obj.GetExtension(s));
  CheckEquals('a;b',s);    
  
  obj.RegisterExtension('a','a.val');
  CheckEquals(True,obj.GetExtension(s));
  CheckEquals('a;b',s);
end;

{ TTest_TIntfPoolItem }

procedure TTest_TIntfPoolItem.All();
var
  i : IInterface;
  b : Boolean;
  a : TIntfPoolItem;
begin
  i := nil;
  b := False;
  a := TIntfPoolItem.Create(i,b);
  try
    Check(( i = a.Intf ),'Create() > Intf');
    CheckEquals(b,a.Used,'Create() > Used');
    b := not b;
    a.Used := b;
    CheckEquals(b,a.Used,'Used');
  finally
    FreeAndNil(a);
  end;
  a := nil;
  
  i := nil;
  b := True;
  a := TIntfPoolItem.Create(i,b);
  try
    Check(( i = a.Intf ),'Create() > Intf');
    CheckEquals(b,a.Used,'Create() > Used');
    b := not b;
    a.Used := b;
    CheckEquals(b,a.Used,'Used');
  finally
    FreeAndNil(a);
  end;
end;

{ TTest_TSimpleItemFactory }

procedure TTest_TSimpleItemFactory.CreateInstance();
var
  b, a : IItemFactory;
  itm : IInterface;
begin
  a := TSimpleItemFactory.Create(TSimpleFactoryItem_A);
    itm := a.CreateInstance();
    CheckEquals(True,Assigned(itm));
    CheckEquals(True,Supports(itm,ISimple_A));

    itm := a.CreateInstance();
    CheckEquals(True,Assigned(itm));
    CheckEquals(True,Supports(itm,ISimple_A));

  b := TSimpleItemFactory.Create(TSimpleFactoryItem_B);
    itm := b.CreateInstance();
    CheckEquals(True,Assigned(itm));
    CheckEquals(True,Supports(itm,ISimple_B));
    
    itm := b.CreateInstance();
    CheckEquals(True,Assigned(itm));
    CheckEquals(True,Supports(itm,ISimple_B));
end;

type

  { TSimpleItemFactoryCrack }

  TSimpleItemFactoryCrack = class(TSimpleItemFactory)
  public
    function GetItemClass() : TSimpleFactoryItemClass;
  end;

{ TSimpleItemFactoryCrack }

function TSimpleItemFactoryCrack.GetItemClass() : TSimpleFactoryItemClass;
begin
  Result := inherited GetItemClass();
end;

procedure TTest_TSimpleItemFactory.CreateProc();
var
  b : TSimpleItemFactoryCrack;
  ok : Boolean;
begin
  ok := False;
  try
    TSimpleItemFactory.Create(nil);
  except
    on e : EServiceConfigException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Create(nil)');
  
  b := TSimpleItemFactoryCrack.Create(TSimpleFactoryItem_A);
  CheckEquals(TSimpleFactoryItem_A,b.GetItemClass());
  FreeAndNil(b);
  
  b := TSimpleItemFactoryCrack.Create(TSimpleFactoryItem_B);
  CheckEquals(TSimpleFactoryItem_B,b.GetItemClass());
end;

{ TwstModuleNotLoad }

procedure TwstModuleNotLoad.Load(const ADoLoad: Boolean);
begin
  //;
end;

{ TTest_TwstModuleManager }

procedure TTest_TwstModuleManager.Get(const AFileName: string);
const C = 10;
var
  locObj : IwstModuleManager;
  locModule : IwstModule;
  i, j, k: Integer;
  ok : Boolean;
  locName : string;
begin
  locObj := TwstModuleManager.Create(TwstModuleNotLoad);

  for i := 0 to Pred(C) do begin
    locObj.Get(Format('lib_%d',[i]));
  end;

  for i := 0 to Pred(C) do begin
    ok := False;
    locName := Format('lib_%d',[i]);
    for j := 0 to Pred(locObj.GetCount()) do begin
      locModule := locObj.GetItem(j);
      if AnsiSameText(locName, locModule.GetFileName()) then begin
        ok := True;
        k := j + 1;
        Break;
      end;
    end;
    Check(ok);
    for j := k to Pred(locObj.GetCount()) do begin
      locModule := locObj.GetItem(j);
      if AnsiSameText(locName, locModule.GetFileName()) then begin
        Check(False,'Duplicated items : ' + locName);
      end;
    end;
  end;
end;

procedure TTest_TwstModuleManager.Clear();
const C = 12;
var
  locObj : IwstModuleManager;
  i : Integer;
begin
  locObj := TwstModuleManager.Create(TwstModuleNotLoad);
  locObj.Clear();
  CheckEquals(0,locObj.GetCount());
  for i := 0 to Pred(C) do begin
    locObj.Get(Format('lib_%d',[i]));
  end;
  CheckEquals(C,locObj.GetCount());
  locObj.Clear();
  CheckEquals(0,locObj.GetCount());
end;

procedure TTest_TwstModuleManager.GetCount();
const C = 10;
var
  locObj : IwstModuleManager;
  i : Integer;
begin
  locObj := TwstModuleManager.Create(TwstModuleNotLoad);
  CheckEquals(0,locObj.GetCount());
  CheckEquals(0,locObj.GetCount());
  for i := 0 to Pred(C) do begin
    CheckEquals(i,locObj.GetCount(),'before Add');
    locObj.Get(Format('lib_%d',[i]));
    CheckEquals(i + 1,locObj.GetCount(),'after Add');
  end;
  CheckEquals(C,locObj.GetCount());
end;

procedure TTest_TwstModuleManager.GetItem(const AIndex: PtrInt);
const C = 10;
var
  locObj : IwstModuleManager;
  locModule : IwstModule;
  i : Integer;
  ok : Boolean;
begin
  locObj := TwstModuleManager.Create(TwstModuleNotLoad);
  ok := False;
  try
    locObj.GetItem(0);
  except
    on e : Exception do begin
      ok := True;
    end;
  end;
  Check(ok);

  ok := False;
  try
    locObj.GetItem(1);
  except
    on e : Exception do begin
      ok := True;
    end;
  end;
  Check(ok);

  for i := 0 to Pred(C) do begin
    locObj.Get(Format('lib_%d',[i]));
  end;

  for i := 0 to Pred(C) do begin
    locModule := locObj.GetItem(i);
    CheckEquals(Format('lib_%d',[i]), locModule.GetFileName());
  end;
  
  ok := False;
  try
    locObj.GetItem(C + 1);
  except
    on e : Exception do begin
      ok := True;
    end;
  end;
  Check(ok);
end;

{ TTest_Procs }

procedure TTest_Procs.test_GetToken();

  procedure do_tests(const ADelimiter : string);
  var
    strBuffer : string;
  begin
    strBuffer := '';
      CheckEquals('', GetToken(strBuffer,ADelimiter));
      CheckEquals('',strBuffer);
    strBuffer := ADelimiter;
      CheckEquals('', GetToken(strBuffer,ADelimiter));
      CheckEquals('',strBuffer);
    strBuffer := Format('%s123',[ADelimiter]);
      CheckEquals('', GetToken(strBuffer,ADelimiter));
      CheckEquals('123',strBuffer);
    strBuffer := Format('%s123%s45',[ADelimiter,ADelimiter]);
      CheckEquals('', GetToken(strBuffer,ADelimiter));
      CheckEquals(Format('123%s45',[ADelimiter]),strBuffer);
      CheckEquals('123', GetToken(strBuffer,ADelimiter));
      CheckEquals('45',strBuffer);
      CheckEquals('45', GetToken(strBuffer,ADelimiter));
      CheckEquals('',strBuffer);

    strBuffer := Format('123',[ADelimiter,ADelimiter]);
      CheckEquals('123', GetToken(strBuffer,ADelimiter));
      CheckEquals('',strBuffer);
    strBuffer := Format('123%s45',[ADelimiter,ADelimiter]);
      CheckEquals('123', GetToken(strBuffer,ADelimiter));
      CheckEquals(Format('45',[ADelimiter]),strBuffer);
      CheckEquals('45', GetToken(strBuffer,ADelimiter));
      CheckEquals('',strBuffer);
  end;

begin
  do_tests(';');
  do_tests('##');
  do_tests('#<#');
end;

{$IFNDEF WST_HAS_STRICT_DELIMITER}
procedure TTest_Procs.test_SetListData();
var
  ls : TStringList;
begin
  ls := TStringList.Create();
  try
    SetListData(ls,'item1=azerty;item2=http: 123;item3=a b c');
    CheckEquals(3,ls.Count,'Items count');
    CheckEquals('item1',ls.Names[0]);
      CheckEquals('azerty',ls.ValueFromIndex[0]);
    CheckEquals('item2',ls.Names[1]);
      CheckEquals('http: 123',ls.ValueFromIndex[1]);
    CheckEquals('item3',ls.Names[2]);
      CheckEquals('a b c',ls.ValueFromIndex[2]);
  finally
    ls.Free();
  end;
end;
{$ENDIF WST_HAS_STRICT_DELIMITER}

initialization
  ListToRelease := TInterfaceList.Create();

  RegisterTest('Utilities',TTest_TIntfPool.Suite);
  RegisterTest('Utilities',TTest_TSimpleItemFactoryEx.Suite);
  RegisterTest('Utilities',TTest_TImplementationFactory.Suite);
  RegisterTest('Utilities',TTest_TIntfPoolItem.Suite);
  RegisterTest('Utilities',TTest_TImplementationFactory.Suite);
  RegisterTest('Utilities',TTest_TwstModuleManager.Suite);
  RegisterTest('Utilities',TTest_Procs.Suite);

finalization
  Finalize_ListToRelease();

end.
