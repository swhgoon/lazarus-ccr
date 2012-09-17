{ RXDBGrid unit

  Copyright (C) 2005-2010 Lagunov Aleksey alexs@hotbox.ru
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

unit rxsortby;

{$I rx.inc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ButtonPanel, rxdbgrid, db;

type

  { TrxSortByForm }

  TrxSortByForm = class(TForm)
    AddBtn: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    DownBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    RemoveBtn: TBitBtn;
    UpBtn: TBitBtn;
    procedure AddBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
  private

  public
    { public declarations }
    function Execute(ADBGrid:TRxDBGrid; SortFieldNames:TStringList; var Asc:boolean):Boolean;
  end;

var
  rxSortByForm: TrxSortByForm;

implementation
uses rxdconst, DBGrids;

{$R *.lfm}

{ TrxSortByForm }

procedure TrxSortByForm.DownBtnClick(Sender: TObject);
var
  TmpField:String;
  C1:TObject;
  Poz: Integer;
begin
  if ListBox1.ItemIndex < ListBox1.Items.Count-1 Then
  begin
    Poz:=ListBox1.ItemIndex;

    TmpField:=ListBox1.Items[Poz+1];
    C1:=ListBox1.Items.Objects[Poz+1];

    ListBox1.Items[Poz+1]:=ListBox1.Items[Poz];
    ListBox1.Items.Objects[Poz+1]:=ListBox1.Items.Objects[Poz];

    ListBox1.Items[Poz]:=TmpField;
    ListBox1.Items.Objects[Poz]:=C1;
    ListBox1.ItemIndex:=Poz+1;
  end;
end;

procedure TrxSortByForm.FormCreate(Sender: TObject);
begin
  ComboBox1.Clear;
  ComboBox1.Items.Add(sRxAscendign);
  ComboBox1.Items.Add(sRxDescending);
  Caption:=sRxSortByFormCaption;
  Label2.Caption:=sRxSortByFormAllFields;
  Label1.Caption:=sRxSortByFormSortFields;
  Label4.Caption:=sRxSortByFormSortOrder;
  AddBtn.Caption:=sRxSortByFormAddField;
  RemoveBtn.Caption:=sRxSortByFormRemoveField;
  UpBtn.Caption:=sRxSortByFormMoveUpField;
  DownBtn.Caption:=sRxSortByFormMoveDnField;
  CheckBox1.Caption:=sRxSortByFormCaseInsens;
end;

procedure TrxSortByForm.ListBox1DblClick(Sender: TObject);
begin
  RemoveBtn.Click;
end;


procedure TrxSortByForm.ListBox2DblClick(Sender: TObject);
begin
  AddBtn.Click;
end;


procedure TrxSortByForm.AddBtnClick(Sender: TObject);
begin
  if ListBox2.ItemIndex <> -1 Then
  begin
    ListBox1.Items.Objects[ListBox1.Items.Add(ListBox2.Items[ListBox2.ItemIndex])]:=ListBox2.Items.Objects[ListBox2.ItemIndex];
    ListBox2.Items.Delete(ListBox2.ItemIndex);
    ListBox1.ItemIndex:=ListBox1.Items.Count-1;
  end;
end;

procedure TrxSortByForm.RemoveBtnClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 Then
  begin
    ListBox2.Items.Objects[ListBox2.Items.Add(ListBox1.Items[ListBox1.ItemIndex])]:=ListBox1.Items.Objects[ListBox1.ItemIndex];
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
end;

procedure TrxSortByForm.UpBtnClick(Sender: TObject);
var
  TmpField:String;
  Poz     : Integer;
  C1:TObject;
begin
  if ListBox1.ItemIndex > 0 Then
  begin
    Poz:=ListBox1.ItemIndex;
    TmpField:=ListBox1.Items[Poz-1];
    C1:=ListBox1.Items.Objects[Poz-1];

    ListBox1.Items[Poz-1]:=ListBox1.Items[Poz];
    ListBox1.Items.Objects[Poz-1]:=ListBox1.Items.Objects[Poz];

    ListBox1.Items[Poz]:=TmpField;
    ListBox1.Items.Objects[Poz]:=C1;

    ListBox1.ItemIndex:=Poz-1;
  end;
end;


function TrxSortByForm.Execute(ADBGrid: TRxDBGrid;
  SortFieldNames: TStringList; var Asc: boolean): Boolean;
var
  i, j : Integer;
  S             : String;
  C:TColumn;
begin
  Result:=False;
  if not (Assigned(ADBGrid.DataSource) and Assigned(ADBGrid.DataSource.DataSet) and ADBGrid.DataSource.DataSet.Active) then  exit;
  ListBox1.Clear;
  ListBox2.Clear;

  if not Asc then
    ComboBox1.ItemIndex:=1
  else
    ComboBox1.ItemIndex:=0;

  for i:=0 to ADBGrid.Columns.Count-1 do
  begin
    C:=ADBGrid.Columns[i];
    if SortFieldNames.IndexOf(C.Field.FieldName) > -1 then
      ListBox1.Items.Objects[ListBox1.Items.Add(C.Title.Caption)]:=C
    else
      ListBox2.Items.Objects[ListBox2.Items.Add(C.Title.Caption)]:=C;
  end;

  if ShowModal = mrOK Then
  begin
    Asc:= ComboBox1.ItemIndex = 0;

    SortFieldNames.Clear;
    for i:=0 to ListBox1.Items.Count-1 do
    begin
      C:=ListBox1.Items.Objects[i] as TColumn;
      SortFieldNames.Add(C.FieldName);
    end;

    Result:=True;
  end;
end;

end.


