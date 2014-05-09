{
This unit has been produced by ws_helper.
  Input unit name : "calcservice".
  This unit name  : "calcservice".
  Date            : "27/08/2008 17:12:53".
}
unit calcservice;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$IFNDEF FPC}
  {$DEFINE WST_RECORD_RTTI}
{$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'urn:calc-service';
  sUNIT_NAME = 'calcservice';

type

  TLoginHeader = class;
  TSessionHeader = class;

{ TLoginHeader
The TLoginHeader class is used by the client for its first invocation of the service.
At the first response the server sends a TSessionHeader instance that contains a
session token. The others time the client invokes the server the client have to
send the session token using the TSessionHeader class.

}
  TLoginHeader = class(THeaderBlock)
  private
    FUserName : string;
    FPassword : string;
  published
    property UserName : string read FUserName write FUserName;
    property Password : string read FPassword write FPassword;
  end;

{ TSessionHeader
This header class is used by the client to send its session token to the server.

}
  TSessionHeader = class(THeaderBlock)
  private
    FSessionToken : string;
  published
    property SessionToken : string read FSessionToken write FSessionToken;
  end;

  ICalcService = interface(IInvokable)
    ['{090EADB7-6B25-4F35-9419-2AF113D44BF8}']
    function Add(
      const  A : integer; 
      const  B : integer
    ):integer;
    function Substract(
      const  A : integer; 
      const  B : integer
    ):integer;
  end;

  procedure Register_calcservice_ServiceMetadata();

Implementation
uses metadata_repository, record_rtti, wst_types;


procedure Register_calcservice_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'ICalcService',
    'TRANSPORT_Address',
    'http://127.0.0.1:8000/services/ICalcService'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'ICalcService',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'ICalcService',
    'Add',
    '_E_N_',
    'Add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'ICalcService',
    'Add',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'ICalcService',
    'Add',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'ICalcService',
    'Substract',
    '_E_N_',
    'Substract'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'ICalcService',
    'Substract',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'ICalcService',
    'Substract',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
end;


initialization
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TLoginHeader),'TLoginHeader');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TSessionHeader),'TSessionHeader');



End.
