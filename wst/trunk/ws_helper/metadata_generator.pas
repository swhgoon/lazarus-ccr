{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit metadata_generator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  parserdefs, binary_streamer;

const
  sWST_META = 'WST_METADATA_0.2.2.0';

type

  { TMetadataGenerator }

  TMetadataGenerator = class
  private
    FStream : IDataStore;
    FSymbolTable: TSymbolTable;

    procedure GenerateHeader();
    procedure GenerateIntfMetadata(AIntf : TInterfaceDefinition);
  public
    constructor Create(
      ASymTable   : TSymbolTable;
      ADstStream  : IDataStore
    );
    procedure Execute();
  end;


implementation

{ TMetadataGenerator }

procedure TMetadataGenerator.GenerateHeader();
var
  c, i, k : LongInt;
begin
  FStream.WriteStr(sWST_META);
  FStream.WriteStr(FSymbolTable.Name);
  k := 0;
  c := FSymbolTable.Count;
  for i := 0 to pred(c) do begin
    if FSymbolTable.Item[i] is TInterfaceDefinition then
      inc(k);
  end;
  FStream.WriteInt8U(k);
end;

procedure TMetadataGenerator.GenerateIntfMetadata(AIntf: TInterfaceDefinition);

  procedure WriteMethod(AMeth:TMethodDefinition);

    procedure WriteParam(APrm : TParameterDefinition);
    begin
      FStream.WriteStr(APrm.ExternalName);
      FStream.WriteStr(APrm.DataType.ExternalName);
      FStream.WriteEnum(Ord(APrm.Modifier));
    end;

  var
    j, k : LongInt;
  begin
    k := AMeth.ParameterCount;
    FStream.WriteStr(AMeth.Name);
    FStream.WriteInt8U(k);
    for j := 0 to pred(k) do
      WriteParam(AMeth.Parameter[j]);
  end;
  
var
  i, c : LongInt;
begin
  FStream.WriteStr(AIntf.Name);
  c := AIntf.MethodCount;
  FStream.WriteInt8U(c);
  for i := 0 to pred(c) do
    WriteMethod(AIntf.Method[i]);
end;

constructor TMetadataGenerator.Create(ASymTable: TSymbolTable;ADstStream: IDataStore);
begin
  Assert(Assigned(ASymTable));
  Assert(Assigned(ADstStream));
  FSymbolTable := ASymTable;
  FStream := ADstStream;
end;

procedure TMetadataGenerator.Execute();
Var
  i,c : Integer;
  intf : TInterfaceDefinition;
begin
  GenerateHeader();
  c := Pred(FSymbolTable.Count);
  for i := 0 to c do begin
    if FSymbolTable.Item[i] is TInterfaceDefinition then begin
      intf := FSymbolTable.Item[i] as TInterfaceDefinition;
      GenerateIntfMetadata(intf);
    end;
  end;
end;

end.

