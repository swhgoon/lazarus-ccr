unit rxsortmemds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  {$IFDEF FPC}
  RxDBGrid
  {$ELSE}
  exDBGrid
  {$ENDIF}
  ;

type
  TRxMemoryDataSortEngine = class(TExDBGridSortEngine)
  public
    procedure Sort(Field:TField; ADataSet:TDataSet; Asc:boolean);override;
  end;

implementation
uses rxmemds;

procedure TRxMemoryDataSortEngine.Sort(Field:TField; ADataSet:TDataSet; Asc:boolean);
begin
  if Assigned(ADataSet) then
    (ADataSet as TRxMemoryData).SortOnFields(Field.FieldName, Asc);
end;

initialization
  RegisterExDBGridSortEngine(TRxMemoryDataSortEngine, TRxMemoryData);
end.

