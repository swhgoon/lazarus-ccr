{
This unit has been produced by ws_helper.
  Input unit name : "echo_service".
  This unit name  : "echo_service".
  Date            : "06/04/2009 17:57:59".
}
unit echo_service;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$IFNDEF FPC}
  {$DEFINE WST_RECORD_RTTI}
{$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'uri:echo-service';
  sUNIT_NAME = 'echo_service';

type


  IEchoService = interface(IInvokable)
    ['{FCD0F68F-3023-46C6-AD09-1DDA4A2989EB}']
    function EchoWideString(
      const  AValue : WideString
    ):WideString;
  end;

  procedure Register_echo_service_ServiceMetadata();

Implementation
uses metadata_repository, record_rtti, wst_types;


procedure Register_echo_service_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'IEchoService',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IEchoService',
    'EchoWideString',
    '_E_N_',
    'EchoWideString'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IEchoService',
    'EchoWideString',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IEchoService',
    'EchoWideString',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
end;


initialization



End.
