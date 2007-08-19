{
This unit has been produced by ws_helper.
  Input unit name : "record_sample".
  This unit name  : "record_sample".
  Date            : "17/08/2007 19:37:26".
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
  sNAME_SPACE = 'record_sample';
  sUNIT_NAME = 'record_sample';

type


  RecordA = record
    fieldB : Single;
    fieldA : Integer;
    comment : String;
  end;

  RecordB = record
    singleField : Single;
    intField : Integer;
    comment : String;
    RecordField : RecordA;
  end;

  RecordC = record
    intField : Integer;
    RecordField : RecordB;
  end;

  RecordService = interface(IInvokable)
    ['{E42B7653-4B50-4956-88B4-FBCEC57B667A}']
    function Add(
      const  AValue : RecordA
    ):RecordB;
    function AddRec(
      const  AA : RecordA; 
      const  AB : RecordB; 
      const  AC : RecordC
    ):RecordC;
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
end;



{$IFDEF WST_RECORD_RTTI}
function __RecordA_TYPEINFO_FUNC__() : PTypeInfo;
var
  p : ^RecordA;
  r : RecordA;
begin
  p := @r;
  Result := MakeRawTypeInfo(
    'RecordA',
    SizeOf(RecordA),
    [ PtrUInt(@(p^.fieldB)) - PtrUInt(p), PtrUInt(@(p^.fieldA)) - PtrUInt(p), PtrUInt(@(p^.comment)) - PtrUInt(p) ],
    [ TypeInfo(Single), TypeInfo(Integer), TypeInfo(String) ]
  );
end;
{$ENDIF WST_RECORD_RTTI}

{$IFDEF WST_RECORD_RTTI}
function __RecordB_TYPEINFO_FUNC__() : PTypeInfo;
var
  p : ^RecordB;
  r : RecordB;
begin
  p := @r;
  Result := MakeRawTypeInfo(
    'RecordB',
    SizeOf(RecordB),
    [ PtrUInt(@(p^.singleField)) - PtrUInt(p), PtrUInt(@(p^.intField)) - PtrUInt(p), PtrUInt(@(p^.comment)) - PtrUInt(p), PtrUInt(@(p^.RecordField)) - PtrUInt(p) ],
    [ TypeInfo(Single), TypeInfo(Integer), TypeInfo(String), TypeInfo(RecordA) ]
  );
end;
{$ENDIF WST_RECORD_RTTI}

{$IFDEF WST_RECORD_RTTI}
function __RecordC_TYPEINFO_FUNC__() : PTypeInfo;
var
  p : ^RecordC;
  r : RecordC;
begin
  p := @r;
  Result := MakeRawTypeInfo(
    'RecordC',
    SizeOf(RecordC),
    [ PtrUInt(@(p^.intField)) - PtrUInt(p), PtrUInt(@(p^.RecordField)) - PtrUInt(p) ],
    [ TypeInfo(Integer), TypeInfo(RecordB) ]
  );
end;
{$ENDIF WST_RECORD_RTTI}
initialization


  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(RecordA),'RecordA').RegisterExternalPropertyName('__FIELDS__','fieldB;fieldA;comment');
{$IFNDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordA)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(TypeInfo(RecordA)),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordA)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
{$IFDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordA)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(__RecordA_TYPEINFO_FUNC__()),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordA)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}

  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(RecordB),'RecordB').RegisterExternalPropertyName('__FIELDS__','singleField;intField;comment;RecordField');
{$IFNDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordB)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(TypeInfo(RecordB)),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordB)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
{$IFDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordB)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(__RecordB_TYPEINFO_FUNC__()),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordB)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}

  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(RecordC),'RecordC').RegisterExternalPropertyName('__FIELDS__','intField;RecordField');
{$IFNDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordC)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(TypeInfo(RecordC)),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordC)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
{$IFDEF WST_RECORD_RTTI}
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordC)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(__RecordC_TYPEINFO_FUNC__()),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordC)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}


End.
