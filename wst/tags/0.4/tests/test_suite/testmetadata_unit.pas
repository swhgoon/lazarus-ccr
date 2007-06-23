{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit testmetadata_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite,
  fpcunit, testutils, testregistry,
  metadata_generator, binary_streamer, metadata_repository, parserdefs,
  metadata_wsdl;
  
type

  { TTestMetadata }

  TTestMetadata= class(TTestCase)
  protected
    function CreateSymbolTable():TSymbolTable;
  published
    procedure test_Metadata();
  end;


implementation

{ TTestMetadata }

function TTestMetadata.CreateSymbolTable(): TSymbolTable;
Var
  inft : TInterfaceDefinition;
begin
  Result := TSymbolTable.Create('test_unit_name');
  Result.Add(TTypeDefinition.Create('integer'));
  Result.Add(TTypeDefinition.Create('string'));
  Result.Add(TTypeDefinition.Create('double'));

  inft := TInterfaceDefinition.Create('service_1');
  Result.Add(inft);
  inft.AddMethod('void_operation_proc',mtProcedure);
  inft.AddMethod('void_operation_func',mtProcedure).AddParameter('result',pmOut,Result.ByName('integer') as TTypeDefinition);

  inft := TInterfaceDefinition.Create('service_2');
  Result.Add(inft);
  with inft.AddMethod('dis_proc',mtProcedure) do begin
    AddParameter('d',pmNone,Result.ByName('double') as TTypeDefinition);
    AddParameter('i',pmConst,Result.ByName('integer') as TTypeDefinition);
    AddParameter('s',pmOut,Result.ByName('string') as TTypeDefinition);
  end;
  with inft.AddMethod('sid_func',mtFunction) do begin
    AddParameter('s',pmConst,Result.ByName('string') as TTypeDefinition);
    AddParameter('i',pmVar,Result.ByName('integer') as TTypeDefinition);
    AddParameter('d',pmOut,Result.ByName('double') as TTypeDefinition);
  end;
end;

procedure PrintWSDL(ARep : PServiceRepository);
var
  locDoc : TXMLDocument;
  strm : TMemoryStream;
  s : string;
begin
  strm := nil;;
  locDoc := TXMLDocument.Create();
  try
    GenerateWSDL(ARep,locDoc);
    strm := TMemoryStream.Create();
    WriteXMLFile(locDoc,strm);
    SetLength(s,strm.Size);
    Move(strm.Memory^,s[1],strm.Size);
    WriteLn('*******************************************************');
    WriteLn(s);
    WriteLn('*******************************************************');
  finally
    locDoc.Free();
    strm.Free();
  end;
end;

procedure TTestMetadata.test_Metadata();
var
  st : TSymbolTable;
  mg : TMetadataGenerator;
  wtr : IDataStore;
  strm : TMemoryStream;

  rp : PServiceRepository;
  ps : PService;
  po : PServiceOperation;
  pop : POperationParam;
begin
  strm := nil;
  mg := nil;
  rp := nil;
  st := CreateSymbolTable();
  try
    strm := TMemoryStream.Create();
    wtr := CreateBinaryWriter(strm);
    mg := TMetadataGenerator.Create(st,wtr);
    mg.Execute();
    wtr := nil;
    strm.Position := 0;
    
    AssertTrue(strm.Size>10);
    AssertEquals('symbol count',2,LoadRepositoryData(strm,rp));
    AssertEquals('unit name','test_unit_name',rp^.Name);
    AssertEquals('services count',2,rp^.ServicesCount);
    AssertNotNull('services pointer',rp^.Services);
    
    ps := rp^.Services;
    AssertEquals('service name','service_1',ps^.Name);
    AssertEquals('operations count',2,ps^.OperationsCount);
    AssertNotNull('operations pointer',ps^.Operations);
      po := ps^.Operations;
      AssertEquals('operation name','void_operation_proc',po^.Name);
      AssertEquals('params count',0,po^.ParamsCount);
      AssertNull('params pointer',po^.Params);
      Inc(po);
      AssertEquals('operation name','void_operation_func',po^.Name);
      AssertEquals('params count',1,po^.ParamsCount);
      AssertNotNull('params pointer',po^.Params);
        pop := po^.Params;
        AssertEquals('param name','result',pop^.Name);
        AssertEquals('param type name','integer',pop^.TypeName);
        AssertEquals('param modifier',ord(pmOut),ord(pop^.Modifier));
        
     rp^.NameSpace := 'http://test_name_space/';
     //PrintWSDL(rp);
  finally
    mg.Free();
    st.Free();
    strm.Free();
    ClearRepositoryData(rp);
  end;
end;

initialization
  RegisterTest(TTestMetadata);
  
end.
