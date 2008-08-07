{
This unit has been produced by ws_helper.
  Input unit name : "base64sample".
  This unit name  : "base64sample_proxy".
  Date            : "07/08/2008 13:25:25".
}

Unit base64sample_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, base64sample;

Type


  TSampleService_Proxy=class(TBaseProxy,SampleService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function DuplicateContent(
      const  AInitialContent : TBase64StringRemotable; 
      const  ARepeatCount : integer
    ):TBase64StringRemotable;
  End;

  Function wst_CreateInstance_SampleService(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'):SampleService;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_SampleService(const AFormat : string; const ATransport : string):SampleService;
Begin
  Result := TSampleService_Proxy.Create('SampleService',AFormat+GetServiceDefaultFormatProperties(TypeInfo(SampleService)),ATransport + 'address=' + GetServiceDefaultAddress(TypeInfo(SampleService)));
End;

{ TSampleService_Proxy implementation }

class function TSampleService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(SampleService);
end;

function TSampleService_Proxy.DuplicateContent(
  const  AInitialContent : TBase64StringRemotable; 
  const  ARepeatCount : integer
):TBase64StringRemotable;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('DuplicateContent', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AInitialContent', TypeInfo(TBase64StringRemotable), AInitialContent);
      locSerializer.Put('ARepeatCount', TypeInfo(integer), ARepeatCount);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'result';
      locSerializer.Get(TypeInfo(TBase64StringRemotable), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i base64sample.wst}

  {$IF DECLARED(Register_base64sample_ServiceMetadata)}
  Register_base64sample_ServiceMetadata();
  {$IFEND}
End.
