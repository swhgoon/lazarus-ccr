unit rxdbgrid_columsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CheckLst,
  StdCtrls, Buttons, ButtonPanel, rxdbgrid;

type

  { TrxDBGridColumsForm }

  TrxDBGridColumsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    FGrid:TRxDBGrid;
    procedure SetGrid(AGrid:TRxDBGrid);
    procedure SetGridColumnsParams;
  public
    { public declarations }
  end; 


procedure ShowRxDBGridColumsForm(Grid:TRxDBGrid);
implementation
uses rxdconst;

procedure ShowRxDBGridColumsForm(Grid: TRxDBGrid);
var
  rxDBGridColumsForm: TrxDBGridColumsForm;
begin
  rxDBGridColumsForm:=TrxDBGridColumsForm.Create(Application);
  rxDBGridColumsForm.SetGrid(Grid);
  if rxDBGridColumsForm.ShowModal = mrOk then
  begin
    if Assigned(Grid) then
      rxDBGridColumsForm.SetGridColumnsParams;
  end;
  rxDBGridColumsForm.Free;
end;

{ TrxDBGridColumsForm }

procedure TrxDBGridColumsForm.FormCreate(Sender: TObject);
begin
  SpeedButton1.AnchorSideLeft.Control:=ButtonPanel1.HelpButton;
  SpeedButton1.AnchorSideTop.Control:=ButtonPanel1.HelpButton;
  SpeedButton1.AnchorSideBottom.Control:=ButtonPanel1.HelpButton;

  Caption:=sRxDbGridSelColCaption;
  SpeedButton1.Hint:=sRxDbGridSelColHint1;
  SpeedButton2.Hint:=sRxDbGridSelColHint2;
end;

procedure TrxDBGridColumsForm.SpeedButton1Click(Sender: TObject);
var
  S:string;
  i:integer;
begin
  if CheckListBox1.Items.Count > 1 then
  begin
    if CheckListBox1.ItemIndex>0 then
    begin
      i:=CheckListBox1.ItemIndex-1;
      S:=CheckListBox1.Items[CheckListBox1.ItemIndex];
      CheckListBox1.Items[CheckListBox1.ItemIndex]:=CheckListBox1.Items[i];
      CheckListBox1.Items[i]:=S;
      CheckListBox1.ItemIndex:=i;
    end;
  end;
end;

procedure TrxDBGridColumsForm.SpeedButton2Click(Sender: TObject);
var
  S:string;
  i:integer;
begin
  if CheckListBox1.Items.Count > 1 then
  begin
    if CheckListBox1.ItemIndex<CheckListBox1.Items.Count-1 then
    begin
      i:=CheckListBox1.ItemIndex+1;
      S:=CheckListBox1.Items[CheckListBox1.ItemIndex];
      CheckListBox1.Items[CheckListBox1.ItemIndex]:=CheckListBox1.Items[i];
      CheckListBox1.Items[i]:=S;
      CheckListBox1.ItemIndex:=i;
    end;
  end;
end;

procedure TrxDBGridColumsForm.SetGrid(AGrid: TRxDBGrid);
var
  i:integer;
begin
  if AGrid=FGrid then exit;
  FGrid:=AGrid;
  CheckListBox1.Items.Clear;
  if Assigned(AGrid) then
  begin
    for i:=0 to AGrid.Columns.Count-1 do
    begin
      CheckListBox1.Items.Add(AGrid.Columns[i].Title.Caption);
      CheckListBox1.Checked[i]:=AGrid.Columns[i].Visible;
    end;
  end;
end;

procedure TrxDBGridColumsForm.SetGridColumnsParams;
var
  i:integer;
  Col:TRxColumn;
begin
  for i:=0 to CheckListBox1.Items.Count-1 do
  begin
    Col:=FGrid.ColumnByCaption(CheckListBox1.Items[i]);
    if Assigned(Col) then
    begin
      Col.Visible:=CheckListBox1.Checked[i];
      Col.Index:=i;
    end
  end;
end;

initialization
  {$I rxdbgrid_columsunit.lrs}

end.

