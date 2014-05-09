program record_client;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, {$IFDEF WINDOWS}ActiveX,{$ENDIF}
  soap_formatter,
  synapse_http_protocol,
  //indy_http_protocol,
  metadata_repository,
  record_sample, record_sample_proxy;

function ReadEntryStr(const APromp : string):string ;
begin
  Result := '';
  Write(APromp);
  while True do begin
    ReadLn(Result);
    Result := Trim(Result);
    if ( Length(Result) > 0 ) then
      Break;
  end;
end;

function ReadEntryInt(const APromp : string):Integer ;
var
  locBuffer : string;
begin
  Write(APromp);
  while True do begin
    ReadLn(locBuffer);
    locBuffer := Trim(locBuffer);
    if TryStrToInt(locBuffer,Result) then
      Break;
  end;
end;

function ReadEntryFloat(const APromp : string) : Single ;
var
  locBuffer : string;
begin
  Write(APromp);
  while True do begin
    ReadLn(locBuffer);
    locBuffer := Trim(locBuffer);
    if TryStrToFloat(locBuffer,Result) then
      Break;
  end;
end;

var
  locService : RecordService;
  A : RecordA;
  B : RecordB;
  C : RecordC;
begin
{$IFDEF WINDOWS}
  CoInitialize(nil);
  try
{$ENDIF}
  SYNAPSE_RegisterHTTP_Transport();
  //INDY_RegisterHTTP_Transport();
  WriteLn('Web Services Toolkit Record sample');
  WriteLn('This sample demonstrates the Object Pascal "Record" support by WST');
  WriteLn();
  locService := TRecordService_Proxy.Create(
                  'RecordService','soap:Style=RPC;EncodingStyle=Literal','http:address=http://127.0.0.1:20000/services/RecordService');
  while True do begin
    A.fieldA := 0;
    A.fieldB := 0;
    C.intField := 1;
    C.RecordField.RecordField.fieldA := 21;
    C.RecordField.RecordField.fieldB := 22;
    C.RecordField.RecordField.comment := 'Comment 23';
    C.RecordField.intField := 3;
    C.RecordField.RecordField.comment := '31 comment';
    C.RecordField.comment := 'xx comment ddf';
    A.fieldA := ReadEntryInt('Enter the Integer field : ');
    A.fieldB := ReadEntryFloat('Enter the Single field : ');
    B.intField := 2 * A.fieldA;
    B := locService.Add(A);
    WriteLn;
    WriteLn('Response ( B ) : ');
    WriteLn('  intField    : ',B.intField);
    WriteLn('  singleField : ',B.singleField);
    WriteLn('  comment     : ',B.comment);
    WriteLn();
    WriteLn;
    C := locService.AddRec(A,B,C);
    WriteLn;
    WriteLn('Response ( C ) : ');
    WriteLn('  intField                : ',C.intField);
    WriteLn('  RecordField.intField    : ',C.RecordField.intField);
    WriteLn('  RecordField.singleField : ',C.RecordField.singleField);
    WriteLn('  RecordField.singleField : ',C.RecordField.comment);
    WriteLn('  RecordField.RecordField.fieldA : ',C.RecordField.RecordField.fieldA);
    WriteLn('  RecordField.RecordField.fieldB : ',C.RecordField.RecordField.fieldB);
    WriteLn('  RecordField.RecordField.comment : ',C.RecordField.RecordField.comment);
    WriteLn();

    if ( UpperCase(ReadEntryStr('Continue ( Y/N ) :'))[1] <> 'Y' ) then
      Break;
  end;
{$IFDEF WINDOWS}
  finally
    CoUninitialize();
  end;
{$ENDIF}
end.

