{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit metadata_repository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


const
  sWST_SIGNATURE = 'WST_METADATA_0.2.2.0';
  sWST_META      = 'wst_meta';

type

  EMetadataException = class(Exception)
  end;

  PPropertyData = ^TPropertyData;
  TPropertyData = record
    Name : string;
    Data : string;
    Next : PPropertyData;
  end;

  TOperationParamFlag = ( opfNone, opfIn, opfVar, opfOut );
  
  POperationParam = ^TOperationParam;
  TOperationParam = record
    Name     : ShortString;
    TypeName : ShortString;
    Modifier : TOperationParamFlag;
  end;
  
  PServiceOperation = ^TServiceOperation;
  TServiceOperation = record
    Name         : ShortString;
    ParamsCount  : Byte;
    Params       : POperationParam;
    Properties   : PPropertyData;
  end;

  PService = ^TService;
  TService = record
    Name             : ShortString;
    OperationsCount  : Byte;
    Operations       : PServiceOperation;
  end;

  PServiceRepository = ^TServiceRepository;
  TServiceRepository = record
    NameSpace        : ShortString;
    Name             : ShortString;
    RootAddress      : ShortString;
    ServicesCount    : Byte;
    Services         : PService;
  end;

  IModuleMetadataMngr = interface
    ['{B10ACF6A-A599-45A3-B083-BEEFB810C889}']
    function IndexOfName(const ARepName : shortstring):Integer;
    function GetCount():Integer;
    function GetRepositoryName(const AIndex : Integer):shortstring;
    procedure SetRepositoryNameSpace(const ARepName,ANameSpace : shortstring);
    function LoadRepositoryName(
      const ARepName,ARootAddress  : shortstring;
      out   ARepository  : PServiceRepository
    ):Integer;
    procedure ClearRepository(var ARepository : PServiceRepository);
    procedure SetOperationCustomData(
      const ARepName       : shortstring;
      const AServiceName   : shortstring;
      const AOperationName : shortstring;
      const ADataName,
            AData          : string
    );
    //---------------------------------
    function GetServiceMetadata(const ARepName,AServiceName : shortstring) : PService;
    procedure ClearServiceMetadata(var AService : PService);
  end;

  function GetModuleMetadataMngr():IModuleMetadataMngr;

  function LoadRepositoryData(
    const AStream   : TStream;
    out ARepository : PServiceRepository
  ):LongInt;
  procedure ClearRepositoryData(var ARepository : PServiceRepository);
  
implementation
uses LResources, binary_streamer;

procedure ClearProperties(var AProps : PPropertyData);
var
  c : Integer;
  q, p : PPropertyData;
begin
  if not Assigned(AProps) then
    Exit;
  c := SizeOf(PPropertyData^);
  p := AProps;
  while Assigned(p) do begin
    q := p;
    p := p^.Next;
    q^.Name := '';
    q^.Data := '';
    Freemem(q,c);
  end;
  AProps := nil;
end;

function CloneProperties(const AProps : PPropertyData) : PPropertyData;
var
  c : Integer;
  p,q, q0 : PPropertyData;
begin
  Result := nil;
  if not Assigned(AProps) then
    Exit;
  c := SizeOf(PPropertyData^);
  q0 := GetMem(c);
  q := q0;
  p := AProps;
  while Assigned(p) do begin
    q^.Next := GetMem(c);
    FillChar(q^.Next^,c,#0);
    q := q^.Next;
    q^.Name := p^.Name;
    q^.Data := p^.Data;
    p := p^.Next;
  end;
  Result := q0^.Next;
  Freemem(q0,c);
end;

function Find(const AProps : PPropertyData; const APropName : string) : PPropertyData;
begin
  if Assigned(AProps) then begin
    Result := AProps;
    while Assigned(Result) do begin
      if AnsiSameText(APropName,Result^.Name) then
        Exit;
      Result := Result^.Next;
    end;
  end;
  Result := nil;
end;

function Add(
  var   AProps    : PPropertyData;
  const APropName,
        APropData : string
) : PPropertyData;
begin
  if not Assigned(AProps) then begin
    AProps := GetMem(SizeOf(PPropertyData^));
    FillChar(AProps^,SizeOf(PPropertyData^),#0);
    AProps^.Next := nil;
    Result := AProps;
  end else begin
    Result := Find(AProps,APropName);
    if not Assigned(Result) then begin
      AProps^.Next := GetMem(SizeOf(PPropertyData^));
      FillChar(AProps^.Next^,SizeOf(PPropertyData^),#0);
      Result := AProps^.Next;
      Result^.Next := nil;
    end;
  end;
  Result^.Name := APropName;
  Result^.Data := APropData;
end;

procedure ClearService(AService : PService; const AFreeService : Boolean);

  procedure ClearOperation(AOperation : PServiceOperation);
  var
    cc : LongInt;
  begin
    cc := AOperation^.ParamsCount;
    if ( cc > 0 ) then begin
      Freemem(AOperation^.Params, cc * SizeOf(POperationParam^) );
    end;
    ClearProperties(AOperation^.Properties);
  end;

var
  j, k : LongInt;
  po : PServiceOperation;
begin
  if not Assigned(AService) then
    Exit;
  k := AService^.OperationsCount;
  if ( k > 0 ) then begin
    po := AService^.Operations;
    for j := 0 to Pred(k) do begin
      ClearOperation(@(po[j]));
    end;
    Freemem(AService^.Operations, k * SizeOf(PServiceOperation^) );
    AService^.Operations := nil;
  end;
  if AFreeService then
    Freemem(AService,SizeOf(PService^));
end;

procedure ClearRepositoryData(var ARepository : PServiceRepository);
var
  i, c : LongInt;
  ps : PService;
begin
  if Assigned(ARepository) then begin
    c := ARepository^.ServicesCount;
    if ( c > 0 ) then begin
      ps := ARepository^.Services;
      for i := 0 to Pred(c) do begin
        ClearService(@(ps[i]),false);
      end;
      Freemem(ARepository^.Services, c * SizeOf(PService^) );
    end;
    Freemem(ARepository,SizeOf(PServiceRepository^));
    ARepository := nil;
  end;
end;

function LoadRepositoryData(
  const AStream   : TStream;
  out ARepository : PServiceRepository
):LongInt;
var
  rdr : IDataStoreReader;

  procedure LoadService(AService : PService);

    procedure LoadOperation(AOperation : PServiceOperation);

      procedure LoadParam(APrm : POperationParam);
      begin
        APrm^.Name := rdr.ReadStr();
        APrm^.TypeName := rdr.ReadStr();
        APrm^.Modifier := TOperationParamFlag(rdr.ReadEnum());
      end;

    var
      ii, cc : LongInt;
      pp : POperationParam;
    begin
      AOperation^.Name := rdr.ReadStr();
      AOperation^.Properties := nil;
      cc := rdr.ReadInt8U();
      if ( cc > 0 ) then begin
        AOperation^.Params := GetMem( cc * SizeOf(POperationParam^) );
        FillChar(AOperation^.Params^, cc * SizeOf(POperationParam^), #0);
        AOperation^.ParamsCount := cc;
        pp := AOperation^.Params;
        for ii := 0 to Pred(cc) do begin
          LoadParam(@(pp[ii]));
        end;
      end;
    end;

  var
    j, k : LongInt;
    po : PServiceOperation;
  begin
    AService^.Name := rdr.ReadStr();
    k := rdr.ReadInt8U();
    if ( k > 0 ) then begin
      AService^.Operations := GetMem( k * SizeOf(PServiceOperation^) );
      AService^.OperationsCount := k;
      FillChar(AService^.Operations^,k * SizeOf(PServiceOperation^), #0);
      po := AService^.Operations;
      for j := 0 to Pred(k) do begin
        LoadOperation(@(po[j]));
      end;
    end;
  end;

var
  buf : string;
  i, c : LongInt;
  ps : PService;
begin
  ARepository := nil;
  Result := 0;
  rdr := CreateBinaryReader(AStream);
  buf := rdr.ReadStr();
  if ( sWST_SIGNATURE <> buf ) then
    raise EMetadataException.CreateFmt('Invalid Metadata signature : "%s"',[buf]);
  c := SizeOf(PServiceRepository^);
  ARepository := GetMem(c);
  try
    FillChar(ARepository^,c,#0);
    ARepository^.Name := rdr.ReadStr();
    c := rdr.ReadInt8U();
    if ( c > 0 ) then begin
      ARepository^.Services := GetMem( c * SizeOf(PService^) );
      ARepository^.ServicesCount := c;
      FillChar(ARepository^.Services^,c * SizeOf(PService^),#0);
      ps := ARepository^.Services;
      for i := 0 to Pred(c) do begin
        LoadService(@(ps[i]));
      end;
    end;
    Result := c;
  except
    ClearRepositoryData(ARepository);
    raise;
  end;
end;


procedure CopyService(ASrcService,ADestService : PService);

  procedure CopyOperation(ASrcOperation, ADstOperation : PServiceOperation);

    procedure CopyParam(ASrcPrm, ADstPrm : POperationParam);
    begin
      ADstPrm^ := ASrcPrm^;
    end;

  var
    ii, cc : LongInt;
    pp : POperationParam;
  begin
    ADstOperation^.Name := ASrcOperation^.Name;
    ADstOperation^.Properties := CloneProperties(ASrcOperation^.Properties);
    cc := ASrcOperation^.ParamsCount;
    if ( cc > 0 ) then begin
      ADstOperation^.Params := GetMem( cc * SizeOf(POperationParam^) );
      FillChar(ADstOperation^.Params^, cc * SizeOf(POperationParam^), #0);
      ADstOperation^.ParamsCount := cc;
      pp := ADstOperation^.Params;
      for ii := 0 to Pred(cc) do begin
        CopyParam(@(ASrcOperation^.Params[ii]),@(pp[ii]));
      end;
    end;
  end;

var
  j, k : LongInt;
  po : PServiceOperation;
begin
  ADestService^.Name := ASrcService^.Name;
  k := ASrcService^.OperationsCount;
  if ( k > 0 ) then begin
    ADestService^.Operations := GetMem( k * SizeOf(PServiceOperation^) );
    ADestService^.OperationsCount := k;
    FillChar(ADestService^.Operations^,k * SizeOf(PServiceOperation^), #0);
    po := ADestService^.Operations;
    for j := 0 to Pred(k) do begin
      CopyOperation(@(ASrcService^.Operations[j]),@(po[j]));
    end;
  end;
end;

function CloneService(const ASrcService : PService) : PService;
var
  c : Integer;
begin
  c := SizeOf(PService^);
  Result := GetMem(c);
  FillChar(Result^,c,#0);
  CopyService(ASrcService,Result);
end;

procedure CloneRepository(
  const ASource : PServiceRepository;
  out   ADest   : PServiceRepository
);
var
  buf : string;
  i, c : LongInt;
  ps : PService;
begin
  ADest := nil;
  if not Assigned(ASource) then
    Exit;
  c := SizeOf(PServiceRepository^);
  ADest := GetMem(c);
  try
    FillChar(ADest^,c,#0);
    ADest^.Name := ASource^.Name;
    ADest^.NameSpace := ASource^.NameSpace;
    ADest^.RootAddress := ASource^.RootAddress;
    c := ASource^.ServicesCount;
    if ( c > 0 ) then begin
      ADest^.Services := GetMem( c * SizeOf(PService^) );
      ADest^.ServicesCount := c;
      FillChar(ADest^.Services^,c * SizeOf(PService^),#0);
      ps := ADest^.Services;
      for i := 0 to Pred(c) do begin
        CopyService(@(ASource^.Services[i]),@(ps[i]));
      end;
    end;
  except
    ClearRepositoryData(ADest);
    raise;
  end;
end;

type

  { TModuleMetadataMngr }

  TModuleMetadataMngr = class(TInterfacedObject,IInterface,IModuleMetadataMngr)
  private
    FList : TStringList;
    FRepositories : array of PServiceRepository;
  private
    procedure LoadRegisteredNames();
    procedure ClearList();
    function FindInnerListIndex(const ARepName : shortstring):Integer;
    function InternalLoadRepository(const ARepName : shortstring):Integer;
  protected
    function IndexOfName(const ARepName : shortstring):Integer;
    procedure RegisterRepository(const ARepName : shortstring);
    function GetCount():Integer;
    function GetRepositoryName(const AIndex : Integer):shortstring;
    procedure SetRepositoryNameSpace(const ARepName,ANameSpace : shortstring);
    function LoadRepositoryName(
      const ARepName,ARootAddress  : shortstring;
      out   ARepository  : PServiceRepository
    ):Integer;
    procedure ClearRepository(var ARepository : PServiceRepository);
    procedure SetOperationCustomData(
      const ARepName       : shortstring;
      const AServiceName   : shortstring;
      const AOperationName : shortstring;
      const ADataName,
            AData          : string
    );
    function GetServiceMetadata(const ARepName,AServiceName : shortstring) : PService;
    procedure ClearServiceMetadata(var AService : PService);
  public
    constructor Create();
    destructor Destroy();override;
  end;

var
  ModuleMetadataMngrInst : IModuleMetadataMngr = nil;
  
function GetModuleMetadataMngr():IModuleMetadataMngr;
begin
  if not Assigned(ModuleMetadataMngrInst) then
    ModuleMetadataMngrInst := TModuleMetadataMngr.Create() as IModuleMetadataMngr;
  Result := ModuleMetadataMngrInst;
end;
  
{ TModuleMetadataMngr }

procedure TModuleMetadataMngr.LoadRegisteredNames();
var
  i, c : Integer;
  itm : TLResource;
begin
  c := LazarusResources.Count;
  for i := 0 to Pred(c) do begin
    itm := LazarusResources.Items[i];
    if AnsiSameText(sWST_META,itm.ValueType) then
      RegisterRepository(itm.Name);
  end;
end;

procedure TModuleMetadataMngr.ClearList();
var
  i : Integer;
begin
  for i := 0 to Length(FRepositories) - 1 do begin
    ClearRepository(FRepositories[i]);
  end;
  SetLength(FRepositories,0);
end;

function TModuleMetadataMngr.FindInnerListIndex(const ARepName: shortstring): Integer;
begin
  for Result := 0 to Pred(Length(FRepositories)) do begin
    if AnsiSameText(ARepName,FRepositories[Result]^.Name) then
      Exit;
  end;
  Result := -1;
end;

function TModuleMetadataMngr.InternalLoadRepository(const ARepName: shortstring): Integer;
var
  tmpStrm : TMemoryStream;
  strBuffer : string;
  i : Integer;
  rs : TLResource;
  tmpRes : PServiceRepository;
begin
  rs := LazarusResources.Find(ARepName);
  if not Assigned(rs) then
    raise EMetadataException.CreateFmt('Repository not registered : "%s"',[ARepName]);
  Result := FindInnerListIndex(ARepName);
  if ( Result < 0 ) then begin
    tmpStrm := TMemoryStream.Create();
    try
      strBuffer := LazarusResources.Find(ARepName).Value;
      i := Length(strBuffer);
      tmpStrm.Write(strBuffer[1],i);
      tmpStrm.Position := 0;
      LoadRepositoryData(tmpStrm,tmpRes);
      if Assigned(tmpRes) then begin
        Result := Length(FRepositories);
        SetLength(FRepositories, ( Result + 1 ) );
        FRepositories[Result] := tmpRes;
        i := Length(tmpRes^.RootAddress);
        if ( i = 0 ) or ( tmpRes^.RootAddress[i] <> '/' ) then
          tmpRes^.RootAddress := tmpRes^.RootAddress + '/';
        tmpRes^.RootAddress := tmpRes^.RootAddress + 'services/';
        tmpRes^.NameSpace := FList.Values[tmpRes^.Name];
        if ( Length(tmpRes^.NameSpace) = 0 ) then
          tmpRes^.NameSpace := 'urn:' + tmpRes^.Name;
      end;
    finally
      tmpStrm.Free();
    end;
  end;
end;

function TModuleMetadataMngr.IndexOfName(const ARepName: shortstring): Integer;
begin
  Result := FList.IndexOfName(ARepName);
end;

procedure TModuleMetadataMngr.RegisterRepository(const ARepName: shortstring);
begin
  if ( FList.IndexOfName(ARepName) = -1 ) then begin
    FList.Values[ARepName] := 'urn:' + ARepName;
  end;
end;

function TModuleMetadataMngr.GetCount(): Integer;
begin
  Result := FList.Count;
end;

function TModuleMetadataMngr.GetRepositoryName(const AIndex: Integer): shortstring;
begin
  Result := FList.Names[AIndex];
end;

procedure TModuleMetadataMngr.SetRepositoryNameSpace(const ARepName,ANameSpace: shortstring);
var
  i : Integer;
begin
  FList.Values[ARepName] := ANameSpace;
  i := FindInnerListIndex(ARepName);
  if ( i >= 0 ) then
    FRepositories[i]^.NameSpace := ANameSpace;
end;

function TModuleMetadataMngr.LoadRepositoryName(
  const ARepName,ARootAddress  : shortstring;
  out   ARepository  : PServiceRepository
): Integer;
var
  strBuffer : string;
  i : Integer;
begin
  Result := 0;
  ARepository := nil;
  i := FindInnerListIndex(ARepName);
  if ( i < 0 ) then begin
    i := InternalLoadRepository(ARepName);
  end;
  if ( Length(ARootAddress) > 0 ) and ( AnsiPos(ARootAddress,FRepositories[i]^.RootAddress) <> 1 ) then begin
    strBuffer := ARootAddress;
    if ( strBuffer[Length(strBuffer)] = '/' ) then
      Delete(strBuffer,Length(strBuffer),1);
    FRepositories[i]^.RootAddress := strBuffer + FRepositories[i]^.RootAddress;
  end;
  if ( i >= 0 ) then begin
    CloneRepository(FRepositories[i],ARepository);
    Exit;
  end;
end;

procedure TModuleMetadataMngr.ClearRepository(var ARepository: PServiceRepository);
begin
  ClearRepositoryData(ARepository);
end;

function FindService(
  const ARep         : PServiceRepository;
  const AServiceName : shortstring
) : PService;
var
  i : Integer;
begin
  for i := 0 to Pred(ARep^.ServicesCount) do begin
    if AnsiSameText(AServiceName,ARep^.Services[i].Name) then begin
      Result := @(ARep^.Services[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function FindOperation(
  const AServ          : PService;
  const AOperationName : shortstring
) : PServiceOperation;
var
  i : Integer;
begin
  for i := 0 to Pred(AServ^.OperationsCount) do begin
    if AnsiSameText(AOperationName,AServ^.Operations[i].Name) then begin
      Result := @(AServ^.Operations[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TModuleMetadataMngr.SetOperationCustomData(
  const ARepName        : shortstring;
  const AServiceName    : shortstring;
  const AOperationName  : shortstring;
  const ADataName,
        AData           : string
);
var
  i : Integer;
  rp : PServiceRepository;
  sp : PService;
  sop : PServiceOperation;
begin
  i := FindInnerListIndex(ARepName);
  if ( i < 0 ) then
    i := InternalLoadRepository(ARepName);
  rp := FRepositories[i];
  sp := FindService(rp,AServiceName);
  if not Assigned(sp) then
    raise EMetadataException.CreateFmt('Service non found : "%s"',[AServiceName]);
  sop := FindOperation(sp,AOperationName);
  if not Assigned(sop) then
    raise EMetadataException.CreateFmt('Operation non found : "%s"',[AOperationName]);
  Add(sop^.Properties,ADataName,AData);
end;

function TModuleMetadataMngr.GetServiceMetadata(const ARepName,AServiceName: shortstring): PService;
var
  i : Integer;
  rp : PServiceRepository;
begin
  Result := nil;
  i := FindInnerListIndex(ARepName);
  if ( i < 0 ) then
    i := InternalLoadRepository(ARepName);
  rp := FRepositories[i];
  for i := 0 to Pred(rp^.ServicesCount) do begin
    if AnsiSameText(AServiceName,rp^.Services[i].Name) then begin
      Result := CloneService(@(rp^.Services[i]));
      Exit;
    end;
  end;
end;

procedure TModuleMetadataMngr.ClearServiceMetadata(var AService: PService);
begin
  ClearService(AService,True);
  AService := nil;
end;

constructor TModuleMetadataMngr.Create();
begin
  inherited Create();
  FRepositories := nil;
  FList := TStringList.Create();
  LoadRegisteredNames();
end;

destructor TModuleMetadataMngr.Destroy();
begin
  ClearList();
  FreeAndNil(FList);
  inherited Destroy();
end;


initialization

finalization
  ModuleMetadataMngrInst := nil;
  
End.
