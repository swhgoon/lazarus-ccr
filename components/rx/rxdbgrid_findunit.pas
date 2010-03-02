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
    procedure FormCreate(Sender: TObject);
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
uses dbutils, DBGrids, rxdconst;

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

procedure TrxDBGridFindForm.FormCreate(Sender: TObject);
begin
  Caption:=sRxDbGridFindCaption;
  Label1.Caption:=sRxDbGridFindText;
  Label2.Caption:=sRxDbGridFindOnField;
  CheckBox1.Caption:=sRxDbGridFindCaseSens;
  CheckBox2.Caption:=sRxDbGridFindPartial;
  RadioGroup1.Caption:=sRxDbGridFindDirecion;
  RadioGroup1.Items.Clear;
  RadioGroup1.Items.Add(sRxDbGridFindRangeAll);
  RadioGroup1.Items.Add(sRxDbGridFindRangeForw);
  RadioGroup1.Items.Add(sRxDbGridFindRangeBack);
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
  if Edit1.Text<>'' then
  begin
    try
      FieldName:=FGrid.Columns[ComboBox1.ItemIndex].FieldName;
      LOptions:=[];
      if not CheckBox1.Checked then
        LOptions:=LOptions+[loCaseInsensitive];

      if CheckBox2.Checked then
        LOptions:=LOptions+[loPartialKey];
      DataSetLocateThrough(FDataSet, FieldName, Edit1.Text, LOptions);
    finally
    end;
  end;
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

