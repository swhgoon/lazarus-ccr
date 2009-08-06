unit rxfilterby;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, db;

type

  { TrxFilterByForm }

  TrxFilterByForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    ComboBox10: TComboBox;
    ComboBox11: TComboBox;
    ComboBox12: TComboBox;
    ComboBox13: TComboBox;
    ComboBox14: TComboBox;
    ComboBox15: TComboBox;
    ComboBox16: TComboBox;
    ComboBox17: TComboBox;
    ComboBox18: TComboBox;
    ComboBox19: TComboBox;
    ComboBox2: TComboBox;
    ComboBox20: TComboBox;
    ComboBox21: TComboBox;
    ComboBox22: TComboBox;
    ComboBox23: TComboBox;
    ComboBox24: TComboBox;
    ComboBox25: TComboBox;
    ComboBox26: TComboBox;
    ComboBox27: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Combo_1 : Array[1..9] of TComboBox;
    Combo_2 : Array[1..9] of TComboBox;
    Edit_1  : Array[1..9] of TEdit;
    Combo_3 : Array[1..9] of TComboBox;
    Table   : TDataSet;
    procedure ClearALL(adoTable : TDataSet);
    function  FindCombo(CB:TComboBox):Integer;
    function  FindEdit(ED:TEdit):Integer;
  public
    function Execute(adoTable : TDataSet; Var FilterStr : String; Var LastFilter : TstringList):Boolean;
  end;

var
  rxFilterByForm: TrxFilterByForm;

implementation
uses rxdconst;

{ TrxFilterByForm }

procedure TrxFilterByForm.Button2Click(Sender: TObject);
begin
 ModalResult := mrCancel;
end;

procedure TrxFilterByForm.Button3Click(Sender: TObject);
begin
 ClearALL(Table);
end;

procedure TrxFilterByForm.ComboBoxChange(Sender: TObject);
Var
 CBN : Integer;
 CB  : TComboBox;
begin
 CB  := (Sender AS TComboBox);
 CBN := FindCombo(CB);
 if CBN=0 Then Exit;
 if (CB.Text=' IS NULL ') Or (CB.Text=' IS NOT NULL ') Then
    Begin
     Edit_1[CBN].Text    := '';
     Edit_1[CBN].Enabled := False;
     Edit_1[CBN].Color   := clInactiveCaption;
    End
 Else
    Begin
     Edit_1[CBN].Enabled := True;
     Edit_1[CBN].Color   := clWindow;
    End;
end;

procedure TrxFilterByForm.EditChange(Sender: TObject);
Var
 EDN : Integer;
 ED  : TEdit;
begin
 ED  := (Sender AS TEdit);
 EDN := FindEdit(ED);
 if EDN=0 Then Exit;
 if ED.Text='' Then Combo_1[EDN].ItemIndex:=-1;
end;

procedure TrxFilterByForm.FormCreate(Sender: TObject);
begin
  Label1.Caption:=sRxFilterFormSelectExp;
  Label2.Caption:=sRxFilterFormSelectExp;
  Label3.Caption:=sRxFilterFormOperaion;
  Label4.Caption:=sRxFilterFormCondition;
  Label5.Caption:=sRxFilterFormOperand;
  Label6.Caption:=sRxFilterFormEnd;
  Button3.Caption:=sRxFilterFormClear;
  Button2.Caption:=sRxFilterFormCancel;
  Button1.Caption:=sRxFilterFormApply;
end;

procedure TrxFilterByForm.Button1Click(Sender: TObject);
begin
 ModalResult := mrOK;
end;

procedure TrxFilterByForm.ClearALL(adoTable : TDataSet);
var
  X : Integer;
begin
 //*****************************************************************************
 Combo_1[1].Items.Clear;
 Combo_1[1].Items.Add('');
 For X := 0 To adoTable.FieldCount-1 do
     Begin
      if (adoTable.Fields[X].FieldKind=fkData) And (adoTable.Fields[X].Visible) Then
         Combo_1[1].Items.Add(adoTable.Fields[X].FieldName);
     End;
  Combo_1[1].ItemIndex := 0;
  For X := 2 To 9 do
     Begin
       Combo_1[X].Items.Assign(Combo_1[1].Items);
       Combo_1[X].ItemIndex := 0;
     End;

  Combo_2[1].Items.Clear;
  Combo_2[1].Items.Add(' = ');
  Combo_2[1].Items.Add(' > ');
  Combo_2[1].Items.Add(' < ');
  Combo_2[1].Items.Add(' >= ');
  Combo_2[1].Items.Add(' <= ');
  Combo_2[1].Items.Add(' <> ');
  Combo_2[1].Items.Add(' LIKE ');
  Combo_2[1].Items.Add(' IS NULL ');
  Combo_2[1].Items.Add(' IS NOT NULL ');
  Combo_2[1].ItemIndex := 0;
  for X := 2 To 9 do
  begin
    Combo_2[X].Items.Assign(Combo_2[1].Items);
    Combo_2[X].ItemIndex := 0;
  end;
  for X := 1 To 9 do
  begin
    Combo_3[X].ItemIndex := 0;
  end;
  for X := 1 To 9 do Edit_1[X].Text := '';
 //*****************************************************************************
end;

function TrxFilterByForm.Execute(adoTable : TDataSet; Var FilterStr : String; Var LastFilter : TstringList):Boolean;
Var
 X  : Integer;
 P  : Integer;
 S  : String;
 SD : String;
 FF : TField;
Begin
 Result := False;
 //*****************************************************************************
 Combo_1[1]:= ComboBox1;
 Combo_1[2]:= ComboBox4;
 Combo_1[3]:= ComboBox7;
 Combo_1[4]:= ComboBox10;
 Combo_1[5]:= ComboBox13;
 Combo_1[6]:= ComboBox16;
 Combo_1[7]:= ComboBox19;
 Combo_1[8]:= ComboBox22;
 Combo_1[9]:= ComboBox25;

 Combo_2[1]:= ComboBox2;
 Combo_2[2]:= ComboBox5;
 Combo_2[3]:= ComboBox8;
 Combo_2[4]:= ComboBox11;
 Combo_2[5]:= ComboBox14;
 Combo_2[6]:= ComboBox17;
 Combo_2[7]:= ComboBox20;
 Combo_2[8]:= ComboBox23;
 Combo_2[9]:= ComboBox26;

 Combo_3[1]:= ComboBox3;
 Combo_3[2]:= ComboBox6;
 Combo_3[3]:= ComboBox9;
 Combo_3[4]:= ComboBox12;
 Combo_3[5]:= ComboBox15;
 Combo_3[6]:= ComboBox18;
 Combo_3[7]:= ComboBox21;
 Combo_3[8]:= ComboBox24;
 Combo_3[9]:= ComboBox27;
 Combo_3[9].Visible := False;

 Edit_1[1] := Edit1;
 Edit_1[2] := Edit2;
 Edit_1[3] := Edit3;
 Edit_1[4] := Edit4;
 Edit_1[5] := Edit5;
 Edit_1[6] := Edit6;
 Edit_1[7] := Edit7;
 Edit_1[8] := Edit8;
 Edit_1[9] := Edit9;

 //*****************************************************************************
 Table := adoTable;
 ClearALL(Table);
 if LastFilter.Count > 0 Then
    Begin
     For X := 0 To LastFilter.Count-1 do
         Begin
           S := LastFilter.Strings[X];
           P := Pos('|||',S);
           if P > 0 Then
              Begin
                Combo_1[X+1].ItemIndex := Combo_1[X+1].Items.IndexOf(System.Copy(S,1,P-1));
                System.Delete(S,1,P+2);
              End;
           P := Pos('|||',S);
           if P > 0 Then
              Begin
                SD:=System.Copy(S,1,P-1);
                Combo_2[X+1].ItemIndex :=  Combo_2[X+1].Items.IndexOf(System.Copy(S,1,P-1));
                System.Delete(S,1,P+2);
                if (SD=' IS NULL ') or (SD=' IS NOT NULL ') Then
                   Begin
                     Edit_1[X+1].Text    := '';
                     Edit_1[X+1].Enabled := False;
                     Edit_1[X+1].Color   := clInactiveCaption;
                   End;
              End;
           P := Pos('|||',S);
           if P > 0 Then
              Begin
                Edit_1[X+1].Text := System.Copy(S,1,P-1);
                System.Delete(S,1,P+2);
              End;
           Combo_3[X+1].ItemIndex := Combo_3[X+1].Items.IndexOf(S);
           if Combo_3[X+1].ItemIndex = -1 Then Combo_3[X+1].ItemIndex := 0;
         End;
    End;

 ShowModal;
 if ModalResult=mrOK Then
    Begin
      Result    := True;
      FilterStr := '';
      LastFilter.Clear;
      For X := 1 to 9 Do
          Begin
           if  (Combo_1[X].Text <> '')
           And (Combo_2[X].Text <> '') Then
             Begin
              if (Edit_1[X].Enabled=False) or (Edit_1[X].Text <> '') Then
                 Begin
                   if X>1 Then
                      FilterStr := FilterStr+Combo_3[X-1].Text+' ';
                   FF  := Table.FindField(Combo_1[X].Text);
                   Case FF.DataType of
                        ftDateTime   ,
                        ftDate       : FilterStr := FilterStr+'('+Combo_1[X].Text+Combo_2[X].Text+Char(39)+Copy(Edit_1[X].Text,7,4)+Copy(Edit_1[X].Text,3,4)+Copy(Edit_1[X].Text,1,2)+Copy(Edit_1[X].Text,11,9)+Char(39)+') ';
                        ftUnknown    : FilterStr := FilterStr+'('+Combo_1[X].Text+Combo_2[X].Text+Edit_1[X].Text+') ';
                        ftTime,
                        ftString,
                        ftMemo       : FilterStr := FilterStr+'('+Combo_1[X].Text+Combo_2[X].Text+Char(39)+Edit_1[X].Text+Char(39)+') ';
                   else
                        FilterStr := FilterStr+'('+Combo_1[X].Text+Combo_2[X].Text+Edit_1[X].Text+') ';
                   End;
                   LastFilter.Add(Combo_1[X].Text+'|||'+Combo_2[X].Text+'|||'+Edit_1[X].Text+'|||'+Combo_3[X].Text);
                 End;
             End;
          End;
    End;
End;

Function  TrxFilterByForm.FindCombo(CB:TComboBox):Integer;
Var
 X : Integer;
Begin
 Result :=0;
 For X := 1 to 9 do
     Begin
      if Combo_2[X]=CB Then
         Begin
           Result := X;
           Exit;
         End;
     End;
End;

Function  TrxFilterByForm.FindEdit(ED:TEdit):Integer;
Var
 X : Integer;
Begin
 Result :=0;
 For X := 1 to 9 do
     Begin
      if Edit_1[X]=ED Then
         Begin
           Result := X;
           Exit;
         End;
     End;
End;



initialization
  {$I rxfilterby.lrs}

end.

