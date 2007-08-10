unit RxDBGridMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, rxmemds,
  DB, rxdbgrid, ExtCtrls, Buttons, Menus, ActnList;

type

  { TRxDBGridMainForm }

  TRxDBGridMainForm = class(TForm)
    actCalcTotal: TAction;
    actOptimizeWidthCol1: TAction;
    actOptimizeColumnsWidthAll: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Datasource1: TDatasource;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    RxDBGrid1: TRxDBGrid;
    RxMemoryData1: TRxMemoryData;
    procedure actCalcTotalExecute(Sender: TObject);
    procedure actOptimizeColumnsWidthAllExecute(Sender: TObject);
    procedure actOptimizeWidthCol1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
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
  RxMemoryData1.AppendRecord([1, 'Lazarus 0.9.23', 0]);
  RxMemoryData1.AppendRecord([2, 'Delphi 7.0 Prof', 990]);
  RxMemoryData1.AppendRecord([3, 'Open Office 2.2.0', 0]);
  RxMemoryData1.AppendRecord([4, 'Microsof Office', 150]);
  RxDBGrid1.CalcStatTotals; //fix error in GotoBookmark
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


initialization
  {$I rxdbgridmainunit.lrs}

end.

