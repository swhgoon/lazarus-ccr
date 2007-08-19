{
This unit has been produced by ws_helper.
  Input unit name : "record_sample".
  This unit name  : "record_sample_proxy".
  Date            : "17/08/2007 19:37:26".
}

Unit record_sample_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, record_sample;

Type


  TRecordService_Proxy=class(TBaseProxy,RecordService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function Add(
      const  AValue : RecordA
    ):RecordB;
    function AddRec(
      const  AA : RecordA; 
      const  AB : RecordB; 
      const  AC : RecordC
    ):RecordC;
  End;

  Function wst_CreateInstance_RecordService(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'):RecordService;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_RecordService(const AFormat : string; const ATransport : string):RecordService;
Begin
  Result := TRecordService_Proxy.Create('RecordService',AFormat+GetServiceDefaultFormatProperties(TypeInfo(RecordService)),ATransport + 'address=' + GetServiceDefaultAddress(TypeInfo(RecordService)));
End;

{ TRecordService_Proxy implementation }

class function TRecordService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(RecordService);
end;

function TRecordService_Proxy.Add(
  const  AValue : RecordA
):RecordB;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Add', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AValue', TypeInfo(RecordA), AValue);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      strPrmName := 'Result';
      locSerializer.Get(TypeInfo(RecordB), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TRecordService_Proxy.AddRec(
  const  AA : RecordA; 
  const  AB : RecordB; 
  const  AC : RecordC
):RecordC;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('AddRec', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AA', TypeInfo(RecordA), AA);
      locSerializer.Put('AB', TypeInfo(RecordB), AB);
      locSerializer.Put('AC', TypeInfo(RecordC), AC);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      strPrmName := 'Result';
      locSerializer.Get(TypeInfo(RecordC), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i record_sample.wst}

  {$IF DECLARED(Register_record_sample_ServiceMetadata)}
  Register_record_sample_ServiceMetadata();
  {$IFEND}
End.
