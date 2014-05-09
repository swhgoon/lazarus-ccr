{
This unit has been produced by ws_helper.
  Input unit name : "record_sample".
  This unit name  : "record_sample_imp".
  Date            : "17/08/2007 19:37:26".
}
Unit record_sample_imp;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils, record_sample;

Type


  TRecordService_ServiceImp=class(TBaseServiceImplementation,RecordService)
  Protected
    function Add(
      const  AValue : RecordA
    ):RecordB;
    function AddRec(
      const  AA : RecordA; 
      const  AB : RecordB; 
      const  AC : RecordC
    ):RecordC;
  End;


  procedure RegisterRecordServiceImplementationFactory();

Implementation
uses config_objects;

{ TRecordService_ServiceImp implementation }
function TRecordService_ServiceImp.Add(
  const  AValue : RecordA
):RecordB;
Begin
  Result.singleField := AValue.fieldA + AValue.fieldB;
  Result.intField := Trunc(AValue.fieldA + AValue.fieldB);
  Result.comment := 'Computed in Add().';
  Result.RecordField := AValue;
End;

function TRecordService_ServiceImp.AddRec(
  const  AA : RecordA; 
  const  AB : RecordB; 
  const  AC : RecordC
):RecordC;
Begin
  Result.RecordField.intField := 1234;
  Result.RecordField.RecordField.fieldA := 0;
  Result.RecordField.RecordField.fieldB := 0;
  Result.intField := Trunc(AA.fieldA + AA.fieldB);
  Result.RecordField.singleField := AB.singleField + AB.intField;
  Result.RecordField.comment := 'Computed in AddRec().';
End;



procedure RegisterRecordServiceImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('RecordService',TImplementationFactory.Create(TRecordService_ServiceImp,wst_GetServiceConfigText('RecordService')) as IServiceImplementationFactory);
End;

End.
