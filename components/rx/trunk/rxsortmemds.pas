{ This module from FPC port of RX components library

  Copyright (C) 2005-2010 Lagunov Aleksey alexs@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit rxsortmemds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, RxDBGrid;

type

  { TRxMemoryDataSortEngine }

  TRxMemoryDataSortEngine = class(TRxDBGridSortEngine)
  protected
    procedure UpdateFooterRows(ADataSet:TDataSet; AGrid:TRxDBGrid);override;
    function EnabledFooterRowsCalc:boolean;override;
  public
    procedure Sort(FieldName: string; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);override;
    procedure SortList(ListField:string; ADataSet:TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions);override;
  end;

implementation
uses rxmemds;

type
  THackRxMemoryData = class(TRxMemoryData);
  THackRxColumnFooter = class(TRxColumnFooter);
  THackDataLink = class(TDataLink);
  THackDataSet = class(TDataSet);
  THackRxDBGrid = class(TRxDBGrid);

procedure TRxMemoryDataSortEngine.UpdateFooterRows(ADataSet: TDataSet;
  AGrid: TRxDBGrid);
var
  i:integer;
  Col:TRxColumn;

  DHL:THackDataLink;
  DHS:THackDataSet;
  SaveState:TDataSetState;
  SavePos:integer;
  SaveActiveRecord:integer;
begin
  if not Assigned(ADataSet) then exit;

  DHL:=THackDataLink(THackRxDBGrid(AGrid).Datalink);
  DHS:=THackDataSet(ADataSet);
  SaveState:=DHS.SetTempState(dsBrowse);

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
end;

function TRxMemoryDataSortEngine.EnabledFooterRowsCalc: boolean;
begin
  Result:=true;
end;

procedure TRxMemoryDataSortEngine.Sort(FieldName: string; ADataSet: TDataSet;
  Asc: boolean; SortOptions: TRxSortEngineOptions);
begin
  if Assigned(ADataSet) then
    (ADataSet as TRxMemoryData).SortOnFields(FieldName, seoCaseInsensitiveSort in SortOptions, not Asc);
end;

procedure TRxMemoryDataSortEngine.SortList(ListField: string;
  ADataSet: TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions);
begin
  if Assigned(ADataSet) then
//    (ADataSet as TRxMemoryData).SortOnFieldsEx(ListField, seoCaseInsensitiveSort in SortOptions, Asc);
    (ADataSet as TRxMemoryData).SortOnFields(ListField, seoCaseInsensitiveSort in SortOptions, Asc[0]);
end;

initialization
  RegisterRxDBGridSortEngine(TRxMemoryDataSortEngine, 'TRxMemoryData');
end.

