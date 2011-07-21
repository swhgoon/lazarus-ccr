
Unit addradio;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
Buttons, EditBtn, streamcol, mainform;

Type 

  { TaddRadioForm }

  TaddRadioForm = Class(TForm)
    BitBtn1: TBitBtn;
    AdvancedBtn: TButton;
    PlaylistURLEdit: TEdit;
    StreamUrlEdit: TEdit;
    StationNameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    StationName: TLabel;
    Procedure AdvancedBtnClick(Sender: TObject);
    Procedure BitBtn1Click(Sender: TObject);
    Procedure PlaylistURLEditClick(Sender: TObject);
    Procedure StreamUrlEditChange(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Private 
    { private declarations }
    FAdvanced: boolean;
    Public 
    { public declarations }
  End;

Var 
  addRadioForm: TaddRadioForm;

  Implementation

{ TaddRadioForm }

Procedure TaddRadioForm.StreamUrlEditChange(Sender: TObject);
Begin

End;

Procedure TaddRadioForm.FormCreate(Sender: TObject);
Begin
  Height := 115;
  FAdvanced := false;
  AdvancedBtnClick(Nil);
End;

Procedure TaddRadioForm.AdvancedBtnClick(Sender: TObject);
Begin
  If FAdvanced=false Then
    Begin
      FAdvanced := true;
      AdvancedBtn.Caption := 'Reduced <<';
      Height := 220;
      StationName.Visible := true;
      Label2.Visible := true;
      StreamUrlEdit.Visible := true;
      StationNameEdit.Visible := true;
      PlaylistURLEdit.Enabled := false;
    End
  Else
    Begin

{  FAdvanced:=false;
  AdvancedBtn.Caption:='Advanced >>';
  Height:=115;
  StationName.Visible:=false;
  Label2.Visible:=false;
  StreamUrlEdit.Visible:=false;
  StationNameEdit.Visible:=false;
  PlaylistURLEdit.Enabled:=true;}
    End;
End;

Procedure TaddRadioForm.BitBtn1Click(Sender: TObject);

Var i: integer;
Begin
  If FAdvanced Then
    Begin
      i := StreamCollection.add(StreamUrlEdit.Text, StationNameEdit.Text);
      writeln(StreamUrlEdit.Text);
    End
  Else
    Begin

    End;
  Main.update_artist_view;
  close;
End;

Procedure TaddRadioForm.PlaylistURLEditClick(Sender: TObject);
Begin
  If PlaylistURLEdit.Enabled Then PlaylistURLEdit.Text := '';
End;

initialization
  {$I addradio.lrs}

End.
