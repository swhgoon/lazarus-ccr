unit gameconfigform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLoad: TButton;
    btnWordlist: TButton;
    comboGameType: TComboBox;
    comboSound: TComboBox;
    comboMusic: TComboBox;
    comboLevel: TComboBox;
    lblGameType: TLabel;
    listWordlist: TLabel;
    lblSettings: TLabel;
    lblSound: TLabel;
    lblMusic: TLabel;
    lblLevel: TLabel;
    lblCredits: TLabel;
    ltbWordlist: TListBox;
    memoGameType: TMemo;
    memoCredits: TMemo;
    procedure comboGameTypeChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.comboGameTypeChange(Sender: TObject);
begin

  Case comboGameType.itemIndex of
  0: begin
    memoGameType.Clear;
    memoGameType.Lines.Add('Description: <Descrição do TappyWords>');
    memoGameType.Lines.Add('');
    memoGameType.Lines.Add('Hint: <Alguma dica para TappyWords>');
    end;

  1: begin
    memoGameType.Clear;
    memoGameType.Lines.Add('Description: <Descrição do TappyMath>');
    memoGameType.Lines.Add('');
    memoGameType.Lines.Add('Hint: <Alguma dica para TappyMath>');
    end;

  end;

end;

end.

