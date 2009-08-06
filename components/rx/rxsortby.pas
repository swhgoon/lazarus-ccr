unit rxsortby;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ButtonPanel, db;

type

  { TrxSortByForm }

  TrxSortByForm = class(TForm)
    AddBtn: TBitBtn;
    ButtonPanel1: TButtonPanel;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
  private
    OrderListTemp: TStringList;
    OrderAsc:boolean;
  public
    { public declarations }
    function Execute(adoTable : TDataSet; SortFieldNames:TStringList; var Asc:boolean):Boolean;
  end;

var
  rxSortByForm: TrxSortByForm;

implementation
uses rxdconst;

{ TrxSortByForm }

procedure TrxSortByForm.DownBtnClick(Sender: TObject);
var
  TmpField:String;
  Poz     : Integer;
begin
  if ListBox1.ItemIndex < ListBox1.Items.Count-1 Then
  begin
    Poz:=ListBox1.ItemIndex;
    TmpField:=ListBox1.Items[Poz+1];
    ListBox1.Items[Poz+1]:=ListBox1.Items[Poz];
    ListBox1.Items[Poz]:=TmpField;
    ListBox1.ItemIndex:=Poz+1;
  end;
end;

procedure TrxSortByForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
var
  X:Integer;
begin
  if ModalResult = mrOk then
  begin
    OrderAsc:=(ComboBox1.ItemIndex=0);
    OrderListTemp.Clear;
    for X:=0 To ListBox1.Items.Count-1 do
      OrderListTemp.Add(ListBox1.Items[X]);
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
    ListBox1.Items.Add(ListBox2.Items.Strings[ListBox2.ItemIndex]);
    ListBox2.Items.Delete(ListBox2.ItemIndex);
    ListBox1.ItemIndex:=ListBox1.Items.Count-1;
  end;
end;

procedure TrxSortByForm.RemoveBtnClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 Then
  begin
    ListBox2.Items.Add(ListBox1.Items.Strings[ListBox1.ItemIndex]);
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
end;

procedure TrxSortByForm.UpBtnClick(Sender: TObject);
var
  TmpField:String;
  Poz     : Integer;
begin
  if ListBox1.ItemIndex > 0 Then
  begin
    Poz:=ListBox1.ItemIndex;
    TmpField:=ListBox1.Items[Poz-1];
    ListBox1.Items[Poz-1]:=ListBox1.Items[Poz];
    ListBox1.Items[Poz]:=TmpField;
    ListBox1.ItemIndex:=Poz-1;
  end;
end;


function TrxSortByForm.Execute(adoTable : TDataSet; SortFieldNames: TStringList; var Asc:boolean): Boolean;
var
  X,P           : Integer;
  S             : String;
  SortFieldNamesTmp : TStringList;
begin
  Result:=False;
  if not Asc then
    ComboBox1.ItemIndex:=1
  else
    ComboBox1.ItemIndex:=0;
  SortFieldNamesTmp:=TStringList.Create;
  for X:=0 to adoTable.FieldDefs.Count-1 do
//  If (NOT adoTable.FieldDefs[X].FieldClass.IsBlob) Then
    SortFieldNamesTmp.Add(adoTable.FieldDefs.Items[X].Name);
  if SortFieldNames.Count > 0 Then
  begin
    ListBox1.Clear;
    for X:=0 To SortFieldNames.Count-1 Do
    begin
      S:=SortFieldNames.Strings[X];
      ListBox1.Items.Add(S);
      P:=SortFieldNamesTmp.IndexOF(SortFieldNames.Strings[X]);
      if P > -1 then
        SortFieldNamesTmp.Delete(P);
    end;
  end;
  if SortFieldNamesTmp.Count > 0 then
  begin
    ListBox2.Clear;
    for X:=0 To SortFieldNamesTmp.Count-1 do
      ListBox2.Items.Add(SortFieldNamesTmp.Strings[X]);
  end;
  SortFieldNamesTmp.Free;

  OrderListTemp:=SortFieldNames;
  OrderAsc:=Asc;
  if ShowModal = mrOK Then
  begin
    Asc:=OrderAsc;
    Result:=True;
  end;
end;

initialization
  {$I rxsortby.lrs}

end.

