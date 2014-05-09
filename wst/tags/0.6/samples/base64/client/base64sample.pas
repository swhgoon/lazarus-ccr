{
This unit has been produced by ws_helper.
  Input unit name : "base64sample".
  This unit name  : "base64sample".
  Date            : "07/08/2008 13:25:25".
}
unit base64sample;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$IFNDEF FPC}
  {$DEFINE WST_RECORD_RTTI}
{$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'urn:base64sample';
  sUNIT_NAME = 'base64sample';

type


  SampleService = interface(IInvokable)
    ['{6ACC9331-DD5B-48AA-92ED-F384D144EB1E}']
    function DuplicateContent(
      const  AInitialContent : TBase64StringRemotable; 
      const  ARepeatCount : integer
    ):TBase64StringRemotable;
  end;

  procedure Register_base64sample_ServiceMetadata();

Implementation
uses metadata_repository, record_rtti, wst_types;


procedure Register_base64sample_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'SampleService',
    'TRANSPORT_Address',
    'http://127.0.0.1:8000/services/SampleService'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'SampleService',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'SampleService',
    'DuplicateContent',
    '_E_N_',
    'DuplicateContent'
  );
end;


initialization



End.
