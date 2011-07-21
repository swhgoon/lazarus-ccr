
Unit BigCoverImg;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

Type 

  { TBigCoverImg }

  TBigCoverImg = Class(TForm)
    Image1: TImage;
    BackImg: TImage;
    Procedure BackImgClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure Image1Click(Sender: TObject);
    Private 
    { private declarations }
    Public 
    { public declarations }
  End;

Var 
  BigCoverImgForm: TBigCoverImg;

  Implementation

  Uses mainform;
{ TBigCoverImg }

Procedure TBigCoverImg.Image1Click(Sender: TObject);
Begin
  close;
End;

Procedure TBigCoverImg.BackImgClick(Sender: TObject);
Begin
  close;
End;

Procedure TBigCoverImg.FormClose(Sender: TObject; Var CloseAction: TCloseAction
);
Begin
  main.Enabled := true;
  Image1.Free;
End;

initialization
  {$I bigcoverimg.lrs}

End.
