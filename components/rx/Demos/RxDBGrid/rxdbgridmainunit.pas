unit RxDBGridMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, rxmemds,
  DB, rxdbgrid, ExtCtrls, Buttons, Menus, ActnList, StdCtrls, DBGrids;

type

  { TRxDBGridMainForm }

  TRxDBGridMainForm = class(TForm)
    actCalcTotal: TAction;
    actOptimizeWidthCol1: TAction;
    actOptimizeColumnsWidthAll: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Datasource1: TDatasource;
    Datasource2: TDatasource;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    RxDBGrid1: TRxDBGrid;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1Date_Present1: TDateField;
    RxMemoryData1Developer1: TStringField;
    RxMemoryData1DEVELOPER_ID1: TLongintField;
    RxMemoryData1ID1: TLongintField;
    RxMemoryData1NAME1: TStringField;
    RxMemoryData1PRICE1: TFloatField;
    RxMemoryData2: TRxMemoryData;
    RxMemoryData2DEVELOPER_ID1: TLongintField;
    RxMemoryData2DEVELOPER_NAME1: TStringField;
    procedure actCalcTotalExecute(Sender: TObject);
    procedure actOptimizeColumnsWidthAllExecute(Sender: TObject);
    procedure actOptimizeWidthCol1Execute(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RxDBGrid1Filtred(Sender: TObject);
    procedure RxMemoryData1FilterRecordEx(DataSet: TDataSet; var Accept: Boolean
      );
  private
    procedure DoFillFilters;
  public
    { public declarations }
  end; 

var
  RxDBGridMainForm: TRxDBGridMainForm;

implementation

{ TRxDBGridMainForm }

procedure TRxDBGridMainForm.FormCreate(Sender: TObject);
begin
  RxMemoryData2.Open;
  RxMemoryData2.AppendRecord([1, 'Open source']);
  RxMemoryData2.AppendRecord([2, 'Borland']);
  RxMemoryData2.AppendRecord([3, 'Microsoft']);

  RxMemoryData1.Open;
  RxMemoryData1.AppendRecord([1, 'Lazarus 0.9.23', 0, 'Open source', EncodeDate(2006, 1, 1), 1]);
  RxMemoryData1.AppendRecord([2, 'Delphi 7.0 Prof', 990, 'Borland', EncodeDate(2002, 1, 1), 2]);
  RxMemoryData1.AppendRecord([3, 'Open Office 2.2.0', 0, 'Open source', EncodeDate(2006, 10, 1), 1]);
  RxMemoryData1.AppendRecord([4, 'Microsoft Office', 150, 'Microsoft', EncodeDate(1997, 8, 12), 3]);
  RxMemoryData1.AppendRecord([5, 'Microsoft Windows 95', 50, 'Microsoft', EncodeDate(1995, 8, 12), 3]);
  RxMemoryData1.AppendRecord([6, 'Microsoft Windows 98', 90, 'Microsoft', EncodeDate(1997, 12, 12), 3]);
  RxMemoryData1.AppendRecord([7, 'Microsoft Windows ME', 90, 'Microsoft', EncodeDate(1999, 10, 25), 3]);
  RxMemoryData1.AppendRecord([8, 'Microsoft Windows NT 4.0', 250, 'Microsoft', EncodeDate(1996, 2, 3), 3]);
  RxMemoryData1.AppendRecord([9, 'Microsoft Windows 2000', 150, 'Microsoft', EncodeDate(1999, 11, 13), 3]);
  RxMemoryData1.AppendRecord([10, 'Microsoft Windows XP', 130, 'Microsoft', EncodeDate(2003, 10, 1), 3]);
  RxMemoryData1.AppendRecord([11, 'Microsoft Windows Vista', 180, 'Microsoft', EncodeDate(2007, 2, 1), 3]);
  DoFillFilters;
  RxMemoryData1.First;
  RxDBGrid1.CalcStatTotals; //fix error in GotoBookmark
end;

procedure TRxDBGridMainForm.RxDBGrid1Filtred(Sender: TObject);
begin
  RxMemoryData1.First;
end;

procedure TRxDBGridMainForm.RxMemoryData1FilterRecordEx(DataSet: TDataSet;
  var Accept: Boolean);
var
  i:integer;
begin
  Accept:=true;
  for i:=0 to RxDBGrid1.Columns.Count-1 do
  begin
    with TRxColumn(RxDBGrid1.Columns[i]) do
      if (Filter.Value<>'') and (Filter.Value<>Field.AsString) then
      begin
        Accept:=false;
        exit;
      end;
  end;
end;

procedure TRxDBGridMainForm.DoFillFilters;
var
  C:TRxColumn;
  i:integer;
begin
  for i:=0 to RxDBGrid1.Columns.Count-1 do
  begin
    C:=TRxColumn(RxDBGrid1.Columns[i]);
    C.Filter.EmptyValue:='None...';
    C.Filter.ValueList.Add(C.Filter.EmptyValue);
  end;

  RxMemoryData1.First;
  while not RxMemoryData1.EOF do
  begin
    for i:=0 to RxDBGrid1.Columns.Count-1 do
    begin
      C:=TRxColumn(RxDBGrid1.Columns[i]);
      if C.Filter.ValueList.IndexOf(C.Field.AsString)<0 then
        C.Filter.ValueList.Add(C.Field.AsString);
    end;
    RxMemoryData1.Next;
  end;
end;

procedure TRxDBGridMainForm.actCalcTotalExecute(Sender: TObject);
begin
  RxDBGrid1.CalcStatTotals; //fix error in GotoBookmark
end;

procedure TRxDBGridMainForm.actOptimizeColumnsWidthAllExecute(Sender: TObject);
begin
  RxDBGrid1.OptimizeColumnsWidthAll;
end;

procedure TRxDBGridMainForm.actOptimizeWidthCol1Execute(Sender: TObject);
begin
  TRxColumn(RxDBGrid1.SelectedColumn).OptimizeWidth;
end;

procedure TRxDBGridMainForm.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
    RxDBGrid1.OptionsRx:=RxDBGrid1.OptionsRx + [rdgFilter]
  else
    RxDBGrid1.OptionsRx:=RxDBGrid1.OptionsRx - [rdgFilter];
  RxMemoryData1.Filtered:=CheckBox1.Checked;
end;


initialization
  {$I rxdbgridmainunit.lrs}

end.

