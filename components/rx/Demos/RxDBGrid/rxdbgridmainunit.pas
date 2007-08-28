unit RxDBGridMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, rxmemds,
  DB, rxdbgrid, ExtCtrls, Buttons, Menus, ActnList, StdCtrls;

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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    RxDBGrid1: TRxDBGrid;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1Developer1: TStringField;
    RxMemoryData1ID1: TLongintField;
    RxMemoryData1NAME1: TStringField;
    RxMemoryData1PRICE1: TFloatField;
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
  RxMemoryData1.Open;
  RxMemoryData1.AppendRecord([1, 'Lazarus 0.9.23', 0, 'Open source']);
  RxMemoryData1.AppendRecord([2, 'Delphi 7.0 Prof', 990, 'Borland']);
  RxMemoryData1.AppendRecord([3, 'Open Office 2.2.0', 0, 'Open source']);
  RxMemoryData1.AppendRecord([4, 'Microsoft Office', 150, 'Microsoft']);
  RxMemoryData1.AppendRecord([5, 'Microsoft Windows 95', 50, 'Microsoft']);
  RxMemoryData1.AppendRecord([6, 'Microsoft Windows 98', 90, 'Microsoft']);
  RxMemoryData1.AppendRecord([7, 'Microsoft Windows ME', 90, 'Microsoft']);
  RxMemoryData1.AppendRecord([8, 'Microsoft Windows NT 4.0', 250, 'Microsoft']);
  RxMemoryData1.AppendRecord([9, 'Microsoft Windows 2000', 150, 'Microsoft']);
  RxMemoryData1.AppendRecord([10, 'Microsoft Windows XP', 130, 'Microsoft']);
  RxMemoryData1.AppendRecord([11, 'Microsoft Windows Vista', 180, 'Microsoft']);
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

