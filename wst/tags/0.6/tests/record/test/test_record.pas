program test_record;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  ,TypInfo, record_rtti;

type

  TSampleRecord = record
    fieldA : Integer;
    fieldB : Single;
  end;
  
procedure PrintRecType(ARecTyp : PRecordTypeData);
var
  i : Integer;
  f : TRecordFieldInfo;
begin
  Assert(Assigned(ARecTyp));
  WriteLn('');
  WriteLn('Type name = ', ARecTyp^.Name);
  WriteLn('  RecordSize = ', ARecTyp^.RecordSize);
  WriteLn('  FieldCount = ', ARecTyp^.FieldCount);
  for i := 1 to ARecTyp^.FieldCount do begin
    f := ARecTyp^.Fields[i-1];
    WriteLn('    Field[',i,']');
    WriteLn('      Name   = ',f.Name);
    WriteLn('      Offset = ',f.Offset);
    WriteLn('      TypeInfo = ',PtrUInt(f.TypeInfo));
    if ( f.TypeInfo <> nil ) then begin
      WriteLn('        TypeInfo^.Name = ',f.TypeInfo^^.Name);
    end;
  end;
  WriteLn('');
end;

var
  recTyp : PRecordTypeData;
begin
  recTyp := MakeRecordTypeInfo(TypeInfo(TSampleRecord));
  PrintRecType(recTyp);
  FreeRecordTypeInfo(recTyp);
  ReadLn;
end.

