unit exsortzeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  {$IFDEF FPC}
  RxDBGrid
  {$ELSE}
  exDBGrid
  {$ENDIF}
  ,ZConnection, ZDataset, ZAbstractRODataset;

type
  TFBDataSetSortEngine = class(TExDBGridSortEngine)
  public
    procedure Sort(Field:TField; ADataSet:TDataSet; Asc:boolean);override;
  end;

implementation
//uses FBCustomDataSet;

procedure TFBDataSetSortEngine.Sort(Field:TField; ADataSet:TDataSet; Asc:boolean);
begin
  if Assigned(ADataSet) then begin
    (ADataSet as TZQuery).SortedFields:=Field.FieldName;
    if Asc then (ADataSet as TZQuery).SortType:=stAscending
    else (ADataSet as TZQuery).SortType:=stDescending;
  end
end;

initialization
  RegisterExDBGridSortEngine(TFBDataSetSortEngine, TZQuery);
end.

