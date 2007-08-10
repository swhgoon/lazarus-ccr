unit exsortfb;

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
  TFBDataSetSortEngine = class(TExDBGridSortEngine)
  public
    procedure Sort(Field:TField; ADataSet:TDataSet; Asc:boolean);override;
  end;

implementation
uses FBCustomDataSet;

procedure TFBDataSetSortEngine.Sort(Field:TField; ADataSet:TDataSet; Asc:boolean);
begin
  if Assigned(ADataSet) then
    (ADataSet as TFBDataSet).SortOnField(Field.FieldName, Asc);
end;

initialization
  RegisterExDBGridSortEngine(TFBDataSetSortEngine, TFBDataSet);
end.

