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
  private
    FGrid:TRxDBGrid;
    procedure SetGrid(AGrid:TRxDBGrid);
    procedure SetGridColumnsVisible;
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
    rxDBGridColumsForm.SetGridColumnsVisible;
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

procedure TrxDBGridColumsForm.SetGridColumnsVisible;
var
  i:integer;
begin
  if Assigned(FGrid) then
  begin
    for i:=0 to CheckListBox1.Items.Count-1 do
      FGrid.Columns[i].Visible:=CheckListBox1.Checked[i];
  end;
end;

initialization
  {$I rxdbgrid_columsunit.lrs}

end.

