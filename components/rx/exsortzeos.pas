unit exsortzeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, RxDBGrid, ZConnection, ZDataset, ZAbstractRODataset;

type

  { TZeosDataSetSortEngine }

  TZeosDataSetSortEngine = class(TRxDBGridSortEngine)
  public
    procedure Sort(Field:TField; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);override;
    procedure SortList(ListField:string; ADataSet:TDataSet; Asc:boolean);override;
  end;

implementation

procedure TZeosDataSetSortEngine.Sort(Field:TField; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);
begin
  if Assigned(ADataSet) then
    if ADataSet.ClassName='TZReadOnlyQuery' then
      begin
        (ADataSet as TZReadOnlyQuery).SortedFields:=Field.FieldName;
        if Asc then
          (ADataSet as TZReadOnlyQuery).SortType:=stAscending
        else
          (ADataSet as TZReadOnlyQuery).SortType:=stDescending;
      end
    else
      if ADataSet.ClassName='TZQuery' then
        begin
          (ADataSet as TZQuery).SortedFields:=Field.FieldName;
          if Asc then
            (ADataSet as TZQuery).SortType:=stAscending
          else
            (ADataSet as TZQuery).SortType:=stDescending;
        end
      else
        if ADataSet.ClassName='TZTable' then
          begin
            (ADataSet as TZTable).SortedFields:=Field.FieldName;
            if Asc then
              (ADataSet as TZTable).SortType:=stAscending
            else
              (ADataSet as TZTable).SortType:=stDescending;
          end;
end;

procedure TZeosDataSetSortEngine.SortList(ListField:string; ADataSet:TDataSet; Asc:boolean);
begin
  if Assigned(ADataSet) then
    if ADataSet.ClassName='TZReadOnlyQuery' then
      begin
        (ADataSet as TZReadOnlyQuery).SortedFields:=ListField;
        if Asc then
          (ADataSet as TZReadOnlyQuery).SortType:=stAscending
        else
          (ADataSet as TZReadOnlyQuery).SortType:=stDescending;
      end
    else
      if ADataSet.ClassName='TZQuery' then
        begin
          (ADataSet as TZQuery).SortedFields:=ListField;
          if Asc then
            (ADataSet as TZQuery).SortType:=stAscending
          else
            (ADataSet as TZQuery).SortType:=stDescending;
        end
      else
        if ADataSet.ClassName='TZTable' then
          begin
            (ADataSet as TZTable).SortedFields:=ListField;
            if Asc then
              (ADataSet as TZTable).SortType:=stAscending
            else
              (ADataSet as TZTable).SortType:=stDescending;
          end;
end;


initialization
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, TZReadOnlyQuery);
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, TZQuery);
  RegisterRxDBGridSortEngine(TZeosDataSetSortEngine, TZTable);
end.

