unit rxdbgrid_findunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, rxdbgrid, DB;

type

  { TrxDBGridFindForm }

  TrxDBGridFindForm = class(TForm)
    BtnFind: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    procedure BtnFindClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FGrid:TRxDBGrid;
    FDataSet:TDataSet;
    procedure SetGrid(AGrid:TRxDBGrid);
  public
    { public declarations }
  end; 

procedure ShowRxDBGridFindForm(Grid:TRxDBGrid);

implementation
uses dbutils, DBGrids;

procedure ShowRxDBGridFindForm(Grid: TRxDBGrid);
var
  rxDBGridFindForm: TrxDBGridFindForm;
begin
  rxDBGridFindForm:=TrxDBGridFindForm.Create(Application);
  rxDBGridFindForm.SetGrid(Grid);
  rxDBGridFindForm.ShowModal;
  rxDBGridFindForm.Free;
end;

{ TrxDBGridFindForm }

procedure TrxDBGridFindForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TrxDBGridFindForm.FormActivate(Sender: TObject);
begin
{  BtnFind.Height:=Canvas.TextHeight('W') + 6;
  Button2.Height:=BtnFind.Height;}
  ComboBox1.Height:=Edit1.Height;
end;

procedure TrxDBGridFindForm.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure TrxDBGridFindForm.BtnFindClick(Sender: TObject);
var
  FieldName:string;
  LOptions: TLocateOptions;
begin
  FieldName:=FGrid.Columns[ComboBox1.ItemIndex].FieldName;
  LOptions:=[];
  if not CheckBox1.Checked then
    LOptions:=LOptions+[loCaseInsensitive];

  if CheckBox2.Checked then
    LOptions:=LOptions+[loPartialKey];
  DataSetLocateThrough(FDataSet, FieldName, Edit1.Text, LOptions);
end;

type
  THckGrid = class(TCustomDBGrid)
  end;

procedure TrxDBGridFindForm.SetGrid(AGrid: TRxDBGrid);
var
  i:integer;
begin
  if AGrid=FGrid then exit;
  FGrid:=AGrid;
  ComboBox1.Items.Clear;
  if Assigned(AGrid) then
  begin
    for i:=0 to AGrid.Columns.Count-1 do
      ComboBox1.Items.Add(AGrid.Columns[i].Title.Caption);
    ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(AGrid.SelectedColumn.Title.Caption);
  end;

  FDataSet:=nil;
  if Assigned(FGrid) and Assigned(THckGrid(FGrid).DataSource) then
    FDataSet:=THckGrid(FGrid).DataSource.DataSet;
  BtnFind.Enabled:=Assigned(FDataSet) and FDataSet.Active
end;

initialization
  {$I rxdbgrid_findunit.lrs}

end.

