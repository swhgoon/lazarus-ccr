{
    This file is part of the Web Service Toolkit
    Copyright (c) 2010 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}

unit filter_intf;

interface
uses
  SysUtils,
  wst_types, base_service_intf;

type

  EFilterException = class(Exception) end;

  IDataFilter = interface
    ['{9D9886A4-37B6-4D62-BD3B-603A8EF00A13}']
    function GetPropertyManager() : IPropertyManager;
    function GetName() : string;
    function ExecuteInput(const AData; const ASize : Integer) : TByteDynArray;
    function ExecuteOutput(const AData; const ASize : Integer) : TByteDynArray;
  end;

  IDataFilterRegistry = interface
    ['{06489785-4447-4844-965B-9A50A417B20D}']
    function Find(
      const AName : string;
      out   ARes  : IDataFilter
    ):Boolean;
    procedure Register(
      const AName    : string;
            AFactory : IItemFactory
    );
  end;

  function GetDataFilterRegistry():IDataFilterRegistry;

implementation

type
  { TDataFilterRegistry }
  TDataFilterRegistry = class(TBaseFactoryRegistry,IInterface,IDataFilterRegistry)
  protected
    function Find(
      const AName : string;
      out   ARes  : IDataFilter
    ):Boolean;
  end;


var
  DataFilterRegistryInst : IDataFilterRegistry = nil;
function GetDataFilterRegistry():IDataFilterRegistry;
begin
  if not Assigned(DataFilterRegistryInst) then
    DataFilterRegistryInst := TDataFilterRegistry.Create() as IDataFilterRegistry;// Lock!!!
  Result := DataFilterRegistryInst;
end;

{ TDataFilterRegistry }

function TDataFilterRegistry.Find(
  const AName : string;
  out   ARes  : IDataFilter
): Boolean;
var
  fct : IItemFactory;
begin
  fct := FindFactory(AName);
  if Assigned(fct) then begin
    ARes := fct.CreateInstance() as IDataFilter;
    Result := True;
  end else begin
    Result := False;
  end;
end;

end.
