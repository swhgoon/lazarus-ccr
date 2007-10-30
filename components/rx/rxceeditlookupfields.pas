unit rxceEditLookupFields;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits;

type

  { TLookupFieldProperty }

  TLookupFieldProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure FillValues(const Values: TStrings); virtual;
  end;

  { TLookupDisplayProperty }

  TLookupDisplayProperty = class(TLookupFieldProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
  
procedure RegisterCEEditLookupFields;
implementation
uses
  //
  db, duallist, Forms, rxstrutils, TypInfo,
  //unit for edits
  rxlookup;

procedure RegisterCEEditLookupFields;
begin
  RegisterPropertyEditor(TypeInfo(string), TRxDBLookupCombo, 'LookupField', TLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxDBLookupCombo, 'LookupDisplay', TLookupDisplayProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxLookupEdit, 'LookupField', TLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxLookupEdit, 'LookupDisplay', TLookupDisplayProperty);
end;

{ TLookupFieldProperty }

function TLookupFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList, paMultiSelect];
end;

procedure TLookupFieldProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    FillValues(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TLookupFieldProperty.FillValues(const Values: TStrings);
var
  DataSource: TDataSource;
begin
  DataSource := GetObjectProp(GetComponent(0), 'LookupSource') as TDataSource;
//  DataSource := TRxDBLookupCombo(GetComponent(0)).LookupSource;
  if (DataSource is TDataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;

{ TLookupDisplayProperty }

function TLookupDisplayProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes + [paDialog]
end;

procedure TLookupDisplayProperty.Edit;
var
  DualListDialog1: TDualListDialog;
  Cmp1:TRxDBLookupCombo;
  Cmp2:TRxLookupEdit;

procedure DoInitFill;
var
  i,j:integer;
  LookupDisplay:string;
begin
  if Assigned(Cmp1) then
    LookupDisplay:=Cmp1.LookupDisplay
  else
    LookupDisplay:=Cmp2.LookupDisplay;
  if LookupDisplay<>'' then
  begin
    StrToStrings(LookupDisplay, DualListDialog1.List2, ';');
    for i:=DualListDialog1.List1.Count-1 downto 0 do
    begin
      j:=DualListDialog1.List2.IndexOf(DualListDialog1.List1[i]);
      if j>=0 then
        DualListDialog1.List1.Delete(i);
    end;
  end;
end;

function DoFillDone:string;
var
  i:integer;
begin
  for i:=0 to DualListDialog1.List2.Count-1 do
    Result:=Result + DualListDialog1.List2[i]+';';
  if Result<>'' then
    Result:=Copy(Result, 1, Length(Result)-1);
end;

procedure DoSetCaptions;
begin
  DualListDialog1.Label1Caption:='All fields';
  DualListDialog1.Label2Caption:='Fields is LookupDisplay';
  DualListDialog1.Title:='Fill fields in LookupDisplay property';
end;

begin
  Cmp1:=nil;
  Cmp2:=nil;

  if GetComponent(0) is TRxDBLookupCombo then
    Cmp1:=TRxDBLookupCombo(GetComponent(0))
  else
    Cmp2:=TRxLookupEdit(GetComponent(0));
  
  DualListDialog1:=TDualListDialog.Create(Application);
  try
    DoSetCaptions;
    FillValues(DualListDialog1.List1);
    DoInitFill;
    if DualListDialog1.Execute then
    begin
      if Assigned(Cmp1) then
        Cmp1.LookupDisplay:=DoFillDone
      else
        Cmp2.LookupDisplay:=DoFillDone;
    end;
  finally
    FreeAndNil(DualListDialog1);
  end;
end;

end.

