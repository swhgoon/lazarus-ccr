unit exsortzeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, RxDBGrid, ZConnection, ZDataset, ZAbstractDataset, ZAbstractRODataset;

type

  { TZeosDataSetSortEngine }

  TZeosDataSetSortEngine = class(TRxDBGridSortEngine)
  protected
    procedure UpdateFooterRows(ADataSet:TDataSet; AGrid:TRxDBGrid);override;
    function EnabledFooterRowsCalc:boolean;override;
  public
    procedure Sort(FieldName: string; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);override;
    procedure SortList(ListField: string; ADataSet: TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions); override;
  end;

implementation
uses ZDbcIntfs, ZVariant;

type
  THackZeosDS = class(TZAbstractRODataset);
  THackRxColumnFooter = class(TRxColumnFooter);

  THackDataLink = class(TDataLink);
  THackDataSet = class(TDataSet);
  THackRxDBGrid = class(TRxDBGrid);

procedure TZeosDataSetSortEngine.UpdateFooterRows(ADataSet: TDataSet;
  AGrid: TRxDBGrid);
var
  RS:IZResultSet;
  CurRow, i:integer;
  Col:TRxColumn;

  DHL:THackDataLink;
  DHS:THackDataSet;
  SaveState:TDataSetState;
  SavePos:integer;
  SaveActiveRecord:integer;

  SaveAfterScroll:TDataSetNotifyEvent;
  SaveBeforeScroll:TDataSetNotifyEvent;
begin
  if not Assigned(ADataSet) then exit;
  if not Assigned(AGrid) then
  begin
    SavePos:=SavePos;
    exit;
  end;
  DHL:=THackDataLink(THackRxDBGrid(AGrid).Datalink);
  DHS:=THackDataSet(ADataSet);
  SaveState:=DHS.SetTempState(dsBrowse);

  SaveAfterScroll:=ADataSet.AfterScroll;
  SaveBeforeScroll:=ADataSet.BeforeScroll;
  ADataSet.AfterScroll:=nil;
  ADataSet.BeforeScroll:=nil;

  SaveActiveRecord:=DHL.ActiveRecord;
  DHL.ActiveRecord:=0;
  SavePos:=ADataSet.RecNo;


  ADataSet.First;
  while not ADataSet.EOF do
  begin

    for i:=0 to AGrid.Columns.Count-1 do
    begin
      Col:=TRxColumn(AGrid.Columns[i]);
      if THackRxColumnFooter(Col.Footer).ValueType in [fvtSum, fvtAvg, fvtMax, fvtMin] then
        THackRxColumnFooter(Col.Footer).UpdateTestValueFromVar( ADataSet.FieldByName(Col.Footer.FieldName).Value);
    end;

    ADataSet.Next;
  end;

  DHS.RecNo := DHL.RecordCount + SavePos + 1;

  while not ADataSet.BOF do
  begin
    if SavePos = ADataSet.RecNo then
      break;
    ADataSet.Prior;
  end;

  DHL.ActiveRecord:=SaveActiveRecord;
  DHS.RestoreState(SaveState);

  ADataSet.AfterScroll  := SaveAfterScroll;
  ADataSet.BeforeScroll := SaveBeforeScroll;
{  RS:=THackZeosDS(ADataSet).ResultSet;
  CurRow:=RS.GetRow;
  RS.First;
//  while not RS.IsLast do
  while not RS.IsAfterLast do
  begin
    for i:=0 to AGrid.Columns.Count-1 do
    begin
      Col:=TRxColumn(AGrid.Columns[i]);
      if THackRxColumnFooter(Col.Footer).ValueType in [fvtSum, fvtAvg, fvtMax, fvtMin] then
        THackRxColumnFooter(Col.Footer).UpdateTestValueFromVar(EncodeVariant(RS.GetValueByName(Col.FieldName)));
    end;
    RS.Next;
  end;
  RS.MoveAbsolute(CurRow);}
end;

function TZeosDataSetSortEngine.EnabledFooterRowsCalc: boolean;
begin
  Result:=true;
end;

procedure TZeosDataSetSortEngine.Sort(FieldName: string; ADataSet: TDataSet;
  Asc: boolean; SortOptions: TRxSortEngineOptions);
begin
  if not Assigned(ADataSet) then exit;
  if ADataSet is TZAbstractDataset then
  begin
    TZAbstractDataset(ADataSet).SortedFields:=FieldName;
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

