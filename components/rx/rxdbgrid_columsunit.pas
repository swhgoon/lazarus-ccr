{ rxdbgrid_columsunit unit

  Copyright (C) 2005-2010 Lagunov Aleksey alexs@hotbox.ru and Lazarus team
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

{$R *.lfm}

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
  if (CheckListBox1.Items.Count > 1) and (CheckListBox1.ItemIndex>-1) then
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
  if (CheckListBox1.Items.Count > 1) and (CheckListBox1.ItemIndex>-1) then
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

end.

