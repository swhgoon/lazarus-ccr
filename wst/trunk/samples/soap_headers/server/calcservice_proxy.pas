{
This unit has been produced by ws_helper.
  Input unit name : "calcservice".
  This unit name  : "calcservice_proxy".
  Date            : "17/08/2008 20:55:09".
}

Unit calcservice_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, calcservice;

Type


  TCalcService_Proxy=class(TBaseProxy,ICalcService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function Add(
      const  A : integer; 
      const  B : integer
    ):integer;
    function Substract(
      const  A : integer; 
      const  B : integer
    ):integer;
  End;

  Function wst_CreateInstance_ICalcService(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'):ICalcService;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_ICalcService(const AFormat : string; const ATransport : string):ICalcService;
Begin
  Result := TCalcService_Proxy.Create('ICalcService',AFormat+GetServiceDefaultFormatProperties(TypeInfo(ICalcService)),ATransport + 'address=' + GetServiceDefaultAddress(TypeInfo(ICalcService)));
End;

{ TCalcService_Proxy implementation }

class function TCalcService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(ICalcService);
end;

function TCalcService_Proxy.Add(
  const  A : integer; 
  const  B : integer
):integer;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Add', GetTarget(),(Self as ICallContext));
      locSerializer.Put('A', TypeInfo(integer), A);
      locSerializer.Put('B', TypeInfo(integer), B);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      strPrmName := 'result';
      locSerializer.Get(TypeInfo(integer), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TCalcService_Proxy.Substract(
  const  A : integer; 
  const  B : integer
):integer;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Substract', GetTarget(),(Self as ICallContext));
      locSerializer.Put('A', TypeInfo(integer), A);
      locSerializer.Put('B', TypeInfo(integer), B);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      strPrmName := 'result';
      locSerializer.Get(TypeInfo(integer), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i calcservice.wst}

  {$IF DECLARED(Register_calcservice_ServiceMetadata)}
  Register_calcservice_ServiceMetadata();
  {$IFEND}
End.
