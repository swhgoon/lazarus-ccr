unit exsortzeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, RxDBGrid, ZConnection, ZDataset, ZAbstractDataset, ZAbstractRODataset;

type

  { TZeosDataSetSortEngine }

  TZeosDataSetSortEngine = class(TRxDBGridSortEngine)
  public
    procedure Sort(Field:TField; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);override;
    procedure SortList(ListField: string; ADataSet: TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions); override;
  end;

implementation

procedure TZeosDataSetSortEngine.Sort(Field:TField; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);
begin
  if not Assigned(ADataSet) then exit;
  if ADataSet is TZAbstractDataset then
  begin
    TZAbstractDataset(ADataSet).SortedFields:=Field.FieldName;
    if Asc then
      TZAbstractDataset(ADataSet).SortType:=stAscending
    else
      TZAbstractDataset(ADataSet).SortType:=stDescending;
  end;
end;

procedure TZeosDataSetSortEngine.SortList(ListField: string;
  ADataSet: TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions);
var
  S:string;
  i, C:integer;
begin
  if not Assigned(ADataSet) then exit;

  S:='';
  C:=Pos(';', ListField);
  i:=0;
  while C>0 do
  begin
    if S<>'' then S:=S+';';
    S:=S + Copy(ListField, 1, C-1);
    Delete(ListField, 1, C);

    if (i<=High(Asc)) and (not Asc[i]) then
      S:=S + ' DESC';
    C:=Pos(';', ListField);
    inc(i);
  end;

  if ListField<>'' then
  begin
    if S<>'' then S:=S+';';
    S:=S + ListField;
    if (i<=High(Asc)) and (not Asc[i]) then
      S:=S + ' DESC';
  end;

  (ADataSet as TZAbstractRODataset).SortedFields:=S;
end;


initialization
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, 'TZReadOnlyQuery');
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, 'TZQuery');
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, 'TZTable');
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, 'TZMacroQuery');
end.

