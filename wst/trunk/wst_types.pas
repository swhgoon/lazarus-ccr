{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit wst_types;

interface

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

  { TDataObject }

  TDataObject = class
  private
    FData : Pointer;
  public
    constructor Create(const AData : Pointer);
    property Data : Pointer read FData write FData;
  end;
  
implementation

{ TDataObject }

constructor TDataObject.Create(const AData : Pointer);
begin
  FData := AData;
end;

end.
