unit cursor_intf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  ECursorException = class(Exception)
  end;

  ICursor = interface
    ['{2B7756B1-E239-4B6F-A7A3-4B57B98FAD4F}']
    procedure Reset();
    function MoveNext() : Boolean;
    function Clone():ICursor;
  end;
  
  IObjectFilter = interface
    ['{3DFB1A26-ED2D-428A-9F62-2091A076D97B}']
    function Evaluate(const AObject : TObject) : Boolean;
  end;

  IObjectCursor = interface(ICursor)
    ['{13E9C22D-0508-4D7A-A969-96E2291B4FE8}']
    function GetCurrent() : TObject;
  end;

  IFilterableObjectCursor = interface(IObjectCursor)
    ['{F11B588A-E8CF-45D3-98D2-B49755FFC22D}']
    function GetFilter() : IObjectFilter;
    function SetFilter(const AFilter : IObjectFilter) : IObjectFilter;
  end;

  function CreateCursorOn(
    AInputCursor : IObjectCursor;
    AFilter      : IObjectFilter
  ) : IFilterableObjectCursor ;


  (*
 ['{4E3C49EE-5EA6-47CD-8862-3AA4F96BD86E}']
 ['{65D250B6-90AC-40DC-A6EE-4750188D1D94}']
 ['{8B4AE228-C231-45E5-B8A4-2864481B9263}']
 ['{658709D2-2D25-44DB-83CF-DC430D55A21F}']
 ['{B2CFB744-43CF-4787-8256-A0F34E26A729}']
 ['{D3A4A37A-B63A-42AD-8E44-4AD4C28E3C34}']
 ['{DB7A8303-0621-41A0-A948-A7BD71CA99F8}']
 ['{3BB114EB-73CF-4555-ABC7-ABA4A643DBDA}']
 ['{C64B6235-54BE-4DA9-A5E8-D67B579FA14F}']

  *)
  
implementation

type

  { TSimpleObjectFilterableCursor }

  TSimpleObjectFilterableCursor = class(
    TInterfacedObject,
    ICursor,IObjectCursor,IFilterableObjectCursor
  )
  private
    FBaseCursor : IObjectCursor;
    FFilter : IObjectFilter;
  protected
    procedure Reset();
    function MoveNext() : Boolean;
    function Clone():ICursor;
    function GetCurrent() : TObject;
    function GetFilter() : IObjectFilter;
    function SetFilter(const AFilter : IObjectFilter) : IObjectFilter;
  public
    constructor Create(
      AInputCursor : IObjectCursor;
      AFilter      : IObjectFilter
    );
  end;

function CreateCursorOn(
  AInputCursor : IObjectCursor;
  AFilter      : IObjectFilter
) : IFilterableObjectCursor ;
begin
  Result := TSimpleObjectFilterableCursor.Create(AInputCursor,AFilter);
end;


{ TSimpleObjectFilterableCursor }

procedure TSimpleObjectFilterableCursor.Reset();
begin
  FBaseCursor.Reset();
end;

function TSimpleObjectFilterableCursor.MoveNext(): Boolean;
begin
  if ( FFilter = nil ) then begin
    Result := FBaseCursor.MoveNext();
  end else begin
    while FBaseCursor.MoveNext() do begin
      if FFilter.Evaluate(FBaseCursor.GetCurrent()) then begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;
end;

function TSimpleObjectFilterableCursor.Clone(): ICursor;
var
  baseClone : ICursor;
begin
  Result := nil;
  baseClone := FBaseCursor.Clone();
  if ( baseClone <> nil ) then
    Result := TSimpleObjectFilterableCursor.Create(baseClone as IObjectCursor,FFilter);
end;

function TSimpleObjectFilterableCursor.GetCurrent(): TObject;
begin
  Result := FBaseCursor.GetCurrent();
end;

function TSimpleObjectFilterableCursor.GetFilter(): IObjectFilter;
begin
  Result := FFilter;
end;

function TSimpleObjectFilterableCursor.SetFilter(const AFilter: IObjectFilter): IObjectFilter;
begin
  FFilter := AFilter;
  Result := FFilter;
end;

constructor TSimpleObjectFilterableCursor.Create(
  AInputCursor : IObjectCursor;
  AFilter      : IObjectFilter
);
begin
  Assert(Assigned(AInputCursor));
  inherited Create();
  FBaseCursor := AInputCursor;
  FFilter := AFilter;
end;


end.
