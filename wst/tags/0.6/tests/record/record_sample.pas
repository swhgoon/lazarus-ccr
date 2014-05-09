{
This unit has been produced by ws_helper.
  Input unit name : "record_sample".
  This unit name  : "record_sample".
  Date            : "26/08/2007 01:12:11".
}
unit record_sample;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$IFNDEF FPC}
  {$DEFINE WST_RECORD_RTTI}
{$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'urn:record_sample';
  sUNIT_NAME = 'record_sample';

type

  TRecordClass = class;

  TRecordClass = class(TBaseComplexRemotable)
  private
    FfieldA : integer;
    FfieldB : Single;
  published
    property fieldA : integer read FfieldA write FfieldA;
    property fieldB : Single read FfieldB write FfieldB;
  end;

  RecordService = interface(IInvokable)
    ['{E42B7653-4B50-4956-88B4-FBCEC57B667A}']
    function Add(
      const  AValue : TRecordClass
    ):Int64;
  end;

  procedure Register_record_sample_ServiceMetadata();

Implementation
uses metadata_repository, record_rtti, wst_types;


procedure Register_record_sample_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'RecordService',
    'TRANSPORT_Address',
    'http://127.0.0.1:20000'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'RecordService',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'RecordService',
    'Add',
    '_E_N_',
    'Add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'RecordService',
    'Add',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'RecordService',
    'Add',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
end;


initialization
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TRecordClass),'TRecordClass');



End.
