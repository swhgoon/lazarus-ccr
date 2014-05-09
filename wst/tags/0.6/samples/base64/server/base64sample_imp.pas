{
This unit has been produced by ws_helper.
  Input unit name : "base64sample".
  This unit name  : "base64sample_imp".
  Date            : "07/08/2008 13:17:40".
}
Unit base64sample_imp;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils, base64sample;

Type


  TSampleService_ServiceImp=class(TBaseServiceImplementation,SampleService)
  Protected
    function DuplicateContent(
      const  AInitialContent : TBase64StringRemotable; 
      const  ARepeatCount : integer
    ):TBase64StringRemotable;
  End;


  procedure RegisterSampleServiceImplementationFactory();

Implementation
uses config_objects;

{ TSampleService_ServiceImp implementation }
function TSampleService_ServiceImp.DuplicateContent(
  const  AInitialContent : TBase64StringRemotable; 
  const  ARepeatCount : integer
):TBase64StringRemotable;
var
  i : PtrInt;
Begin
  if ( ARepeatCount < 0 ) then
    raise Exception.CreateFmt('Invalid "ARepeatCount" value : %d',[ARepeatCount]);
  Result := TBase64StringRemotable.Create();
  if ( ARepeatCount > 0 ) then begin
    Result.BinaryData := AInitialContent.BinaryData;
    for i := 2 to ARepeatCount do
      Result.BinaryData := Result.BinaryData + AInitialContent.BinaryData;
  end;
End;



procedure RegisterSampleServiceImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('SampleService',TImplementationFactory.Create(TSampleService_ServiceImp,wst_GetServiceConfigText('SampleService')) as IServiceImplementationFactory);
End;

End.
