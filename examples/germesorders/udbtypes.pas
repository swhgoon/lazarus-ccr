unit uDbTypes;

{$mode objfpc}{$H+}

interface
uses Db;

type
  TDbKeyType = integer;
  
const
  ftDbKey:TFieldType = ftInteger;

procedure DbFieldAssignAsDbKey(D:TDataSet; const FieldName:string; F:TField);overload;
procedure DbFieldAssignAsDbKey(D:TDataSet; const FieldName:string; const Val:string);overload;
function DBFieldAsDBKey(D:TDataSet; const FieldName:string):TDbKeyType;
function StrToDBKey(const S:String):TDbKeyType;

implementation
uses sysutils;

function StrToDBKey(const S:String):TDbKeyType;
begin
  Result:=StrToInt(S);
end;

procedure DbFieldAssignAsDbKey(D:TDataSet; const FieldName:string; const Val:string);overload;
begin
  D.FieldByName(FieldName).AsInteger:=StrToInt(Val);
end;

procedure DbFieldAssignAsDbKey(D:TDataSet; const FieldName:string; F:TField);
begin
  D.FieldByName(FieldName).AsInteger:=F.AsInteger;
end;

function DBFieldAsDBKey(D:TDataSet; const FieldName:string):TDbKeyType;
begin
  Result:=D.FieldByName(FieldName).AsInteger;
end;

end.

