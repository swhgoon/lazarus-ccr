{$INCLUDE wst_global.inc}
unit test_wst_cursors;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry,
{$ELSE}
  TestFrameWork, 
{$ENDIF}
  cursor_intf, wst_cursors, rtti_filters, base_service_intf;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}  
  
type

  { TClass_A }

  TClass_A = class(TBaseRemotable)
  private
    FIntProp: Integer;
  public
    constructor Create(AIntProp : Integer);
  published
    property IntProp : Integer read FIntProp;
  end;

  { TTClass_A_ArrayRemotable }

  TTClass_A_ArrayRemotable = class(TBaseObjectArrayRemotable)
  public
    class function GetItemClass():TBaseRemotableClass;override;
  end;

  { TTClass_A_CollectionRemotable }

  TTClass_A_CollectionRemotable = class(TObjectCollectionRemotable)
  public
    class function GetItemClass():TBaseRemotableClass;override;
  end;
  
  { TClass_B }

  TClass_B = class(TClass_A)
  private
    FIntProp_A: Integer;
    FIntProp_B: Integer;
  public
    constructor Create(AIntProp,AIntProp_A,AIntProp_B : Integer);
  published
    property IntProp_A : Integer read FIntProp_A;
    property IntProp_B : Integer read FIntProp_B;
  end;

  { TTClass_B_ArrayRemotable }

  TTClass_B_ArrayRemotable = class(TBaseObjectArrayRemotable)
  public
    class function GetItemClass():TBaseRemotableClass;override;
  end;
  
  { TBaseObjectArrayRemotableCursor_Test }

  TBaseObjectArrayRemotableCursor_Test = class(TTestCase)
  published
    procedure All();
  end;

  { TBaseObjectArrayRemotableFilterableCursor_Test }

  TBaseObjectArrayRemotableFilterableCursor_Test = class(TTestCase)
  published
    procedure All();
  end;

  { TObjectCollectionRemotableCursor_Test }

  TObjectCollectionRemotableCursor_Test = class(TTestCase)
  published
    procedure All();
  end;
  
  { TUtilsProcs_Test }

  TUtilsProcs_Test = class(TTestCase)
  published
    procedure test_Find_array();
    procedure test_Find_collection();
    procedure test_Filter_array();
    procedure test_Filter_collection();
  end;
  
implementation

{ TClass_A }

constructor TClass_A.Create(AIntProp: Integer);
begin
  FIntProp := AIntProp;
end;

{ TBaseObjectArrayRemotableCursor_Test }

procedure TBaseObjectArrayRemotableCursor_Test.All();
const O_COUNT = 100;
var
  x : IObjectCursor;
  ls : TBaseObjectArrayRemotable;
  i : Integer;
begin
  ls := TTClass_A_ArrayRemotable.Create();
  try
    x := TBaseObjectArrayRemotableCursor.Create(ls);
    x.Reset();
    CheckEquals(False,x.MoveNext());
    x.Reset();
    CheckEquals(False,x.MoveNext());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;
    
    ls.SetLength(1);
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());

    ls.SetLength(O_COUNT);
    for i := 0 to Pred(O_COUNT) do
      TClass_A(ls[i]).FIntProp := i;
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
    CheckEquals(False,x.MoveNext());
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
  finally
    ls.Free();
  end;
end;


{ TClass_B }

constructor TClass_B.Create(AIntProp, AIntProp_A, AIntProp_B: Integer);
begin
  inherited Create(AIntProp);
  FIntProp_A := AIntProp_A;
  FIntProp_B := AIntProp_B;
end;

{ TBaseObjectArrayRemotableFilterableCursor_Test }

procedure TBaseObjectArrayRemotableFilterableCursor_Test.All();
const O_COUNT = 100;
var
  x : IFilterableObjectCursor;
  ls : TBaseObjectArrayRemotable;
  i : Integer;
  f : IObjectFilter;
  fcr : TRttiFilterCreator;
begin
  fcr := nil;
  ls := TTClass_A_ArrayRemotable.Create();
  try
    x := TBaseObjectArrayRemotableFilterableCursor.Create(ls);
    CheckNull(x.GetFilter());
    x.Reset();
    CheckEquals(False,x.MoveNext());
    x.Reset();
    CheckEquals(False,x.MoveNext());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;

    ls.SetLength(1);
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());

    ls.SetLength(O_COUNT);
    for i := 0 to Pred(O_COUNT) do
      TClass_A(ls[i]).FIntProp := i;
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
    CheckEquals(False,x.MoveNext());
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
    
    FreeAndNil(ls);
    ls := TTClass_B_ArrayRemotable.Create();
    x := TBaseObjectArrayRemotableFilterableCursor.Create(ls);
    ls.SetLength(O_COUNT);
    for i := 0 to Pred(O_COUNT) do begin
      TClass_B(ls[i]).FIntProp := i;
      TClass_B(ls[i]).FIntProp_A := ( i mod 10 );
      TClass_B(ls[i]).FIntProp_B := ( i mod ( ( i + 1 ) * 2 ) ) ;
    end;

    fcr := TRttiFilterCreator.Create(TClass_B);
    fcr.AddCondition('IntProp',nfoEqual,-1,fcOr);//
    f := TRttiObjectFilter.Create(fcr.Root,clrFreeObjects) as IObjectFilter;
    //fcr.Clear(clrNone);
    x.SetFilter(f);
    Check(x.GetFilter()=f);
    x.SetFilter(nil);
    CheckNull(x.GetFilter());
    x.SetFilter(f);
    Check(x.GetFilter()=f);
    x.Reset();
    CheckEquals(False,x.MoveNext());

    fcr.AddCondition('IntProp',nfoGreater,-1,fcOr);
    x.Reset();
    CheckEquals(True,x.MoveNext());

    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;

  finally
    ls.Free();
    fcr.Free();
  end;
end;

{ TTClass_A_ArrayRemotable }

class function TTClass_A_ArrayRemotable.GetItemClass() : TBaseRemotableClass;
begin
  Result := TClass_A;
end;

{ TTClass_B_ArrayRemotable }

class function TTClass_B_ArrayRemotable.GetItemClass() : TBaseRemotableClass;
begin
  Result := TClass_B;
end;

{ TUtilsProcs_Test }

procedure TUtilsProcs_Test.test_Find_array();
const O_COUNT : PtrInt = 10;
var
  ls : TTClass_A_ArrayRemotable;
  i : PtrInt;
begin
  ls := TTClass_A_ArrayRemotable.Create();
  try
    CheckNull(Find(ls,''));
    CheckNull(Find(ls,'IntProp = 12'));
    
    ls.SetLength(1);
      CheckSame(ls[0], Find(ls,''));
      CheckSame(ls[0], Find(ls,'IntProp = 0'));
      CheckNull(Find(ls,'IntProp = 12'));

    ls.SetLength(O_COUNT);
      for i := 0 to ( O_COUNT - 1 ) do
        TClass_A(ls[i]).FIntProp := i;
      CheckSame(ls[0], Find(ls,''));
      CheckSame(ls[0], Find(ls,'IntProp = 0'));
      CheckNull(Find(ls,Format('IntProp = %d',[2*O_COUNT])));
      for i := 0 to ( O_COUNT - 1 ) do
        CheckSame(ls[i],Find(ls,Format('IntProp = %d',[i])));
  finally
    ls.Free();
  end;
end;

procedure TUtilsProcs_Test.test_Find_collection();
const O_COUNT : PtrInt = 10;
var
  ls : TTClass_A_CollectionRemotable;
  i : PtrInt;
begin
  ls := TTClass_A_CollectionRemotable.Create();
  try
    CheckNull(Find(ls,''));
    CheckNull(Find(ls,'IntProp = 12'));

    ls.Add();
      CheckSame(ls[0], Find(ls,''));
      CheckSame(ls[0], Find(ls,'IntProp = 0'));
      CheckNull(Find(ls,'IntProp = 12'));

    ls.Clear();
      for i := 0 to ( O_COUNT - 1 ) do
        TClass_A(ls.Add()).FIntProp := i;
      CheckSame(ls[0], Find(ls,''));
      CheckSame(ls[0], Find(ls,'IntProp = 0'));
      CheckNull(Find(ls,Format('IntProp = %d',[2*O_COUNT])));
      for i := 0 to ( O_COUNT - 1 ) do
        CheckSame(ls[i],Find(ls,Format('IntProp = %d',[i])));
  finally
    ls.Free();
  end;
end;

procedure TUtilsProcs_Test.test_Filter_array();
const O_COUNT : PtrInt = 10;
var
  ls : TTClass_A_ArrayRemotable;
  i : PtrInt;
  crs : IObjectCursor;
begin
  CheckNull(Filter(TTClass_A_ArrayRemotable(nil),''), 'filter(nil) = nil');
  ls := TTClass_A_ArrayRemotable.Create();
  try
    crs := Filter(ls,'');
    Check( ( crs <> nil ) );
    crs.Reset();
    Check(not crs.MoveNext());

    ls.SetLength(O_COUNT);
      for i := 0 to ( O_COUNT - 1 ) do
        TClass_A(ls[i]).FIntProp := i;
      crs := Filter(ls,'');
      Check( ( crs <> nil ) );
      crs.Reset();
      for i := 0 to ( O_COUNT - 1 ) do begin
        Check(crs.MoveNext());
        CheckSame(ls[i], crs.GetCurrent());
      end;
      Check(not crs.MoveNext());
      
      for i := 0 to ( O_COUNT - 1 ) do begin
        crs := Filter(ls,Format('IntProp = %d',[i]));
        Check( ( crs <> nil ) );
        crs.Reset();
        Check(crs.MoveNext());
        CheckSame(ls[i], crs.GetCurrent());
        Check(not crs.MoveNext());
      end;
  finally
    ls.Free();
  end;
end;

procedure TUtilsProcs_Test.test_Filter_collection();
const O_COUNT : PtrInt = 10;
var
  ls : TTClass_A_CollectionRemotable;
  i : PtrInt;
  crs : IObjectCursor;
begin
  CheckNull(Filter(TTClass_A_CollectionRemotable(nil),''), 'filter(nil) = nil');
  ls := TTClass_A_CollectionRemotable.Create();
  try
    crs := Filter(ls,'');
    Check( ( crs <> nil ) );
    crs.Reset();
    Check(not crs.MoveNext());

    ls.Clear();
      for i := 0 to ( O_COUNT - 1 ) do
        TClass_A(ls.Add()).FIntProp := i;
      crs := Filter(ls,'');
      Check( ( crs <> nil ) );
      crs.Reset();
      for i := 0 to ( O_COUNT - 1 ) do begin
        Check(crs.MoveNext());
        CheckSame(ls[i], crs.GetCurrent());
      end;
      Check(not crs.MoveNext());

      for i := 0 to ( O_COUNT - 1 ) do begin
        crs := Filter(ls,Format('IntProp = %d',[i]));
        Check( ( crs <> nil ) );
        crs.Reset();
        Check(crs.MoveNext());
        CheckSame(ls[i], crs.GetCurrent());
        Check(not crs.MoveNext());
      end;
  finally
    ls.Free();
  end;
end;

{ TTClass_A_CollectionRemotable }

class function TTClass_A_CollectionRemotable.GetItemClass() : TBaseRemotableClass;
begin
  Result := TClass_A;
end;

{ TObjectCollectionRemotableCursor_Test }

procedure TObjectCollectionRemotableCursor_Test.All();
const O_COUNT = 100;
var
  x : IObjectCursor;
  ls : TObjectCollectionRemotable;
  i : PtrInt;
begin
  ls := TTClass_A_CollectionRemotable.Create();
  try
    x := TObjectCollectionRemotableCursor.Create(ls);
    x.Reset();
    CheckEquals(False,x.MoveNext());
    x.Reset();
    CheckEquals(False,x.MoveNext());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;

    ls.Add();
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());
    try
      x.GetCurrent();
      Check(False);
    except
      on e : ECursorException do begin
        // GOOD
      end;
    end;
    x.Reset();
    CheckEquals(True,x.MoveNext());
    CheckSame(ls[0],x.GetCurrent());
    CheckEquals(False,x.MoveNext());

    ls.Clear();
    for i := 0 to Pred(O_COUNT) do
      TClass_A(ls.Add()).FIntProp := i;
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
    CheckEquals(False,x.MoveNext());
    x.Reset();
    for i := 0 to Pred(O_COUNT) do begin
      CheckEquals(True,x.MoveNext());
      CheckSame(ls[i],x.GetCurrent());
    end;
  finally
    ls.Free();
  end;
end;

initialization
  RegisterTest('Cursors',TBaseObjectArrayRemotableCursor_Test.Suite);
  RegisterTest('Cursors',TBaseObjectArrayRemotableFilterableCursor_Test.Suite);
  RegisterTest('Cursors',TObjectCollectionRemotableCursor_Test.Suite);
  RegisterTest('Cursors',TUtilsProcs_Test.Suite);

end.
