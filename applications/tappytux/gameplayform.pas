unit gameplayform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  // TappyTux
  {GameData,} tappydrawer, tappymodules;

type

  { TformTappyTuxGame }

  TformTappyTuxGame = class(TForm)
    btnExit: TButton;
    Answer: TEdit;
    Question2: TEdit;
    Question4: TEdit;
    Question3: TEdit;
    Question5: TEdit;
    Level: TEdit;
    Score: TEdit;
    Lives: TEdit;
    Question1: TEdit;
    LabelLevels: TLabel;
    LabelScore: TLabel;
    LabelLives: TLabel;
    Test: TMemo;
    procedure btnExitClick(Sender: TObject);
    procedure AnswerKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
    backgroundImage: TJpegImage;
  end;

var
  formTappyTuxGame: TformTappyTuxGame;

implementation

uses gameconfigform, mod_tappywords;

{$R *.lfm}

{ TformTappyTuxGame }

procedure TformTappyTuxGame.btnExitClick(Sender: TObject);
begin
  Close;
  formConfig.Show;

  GetCurrentModule().EndGame();
end;

procedure TformTappyTuxGame.AnswerKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    formTappyTuxGame.Test.Lines.Add(Answer.Text);

    if (Answer.Text = Question1.Text) then Question1.Top := 50;
    if (Answer.Text = Question2.Text) then Question2.Top := 50;
    if (Answer.Text = Question3.Text) then Question3.Top := 50;
    if (Answer.Text = Question4.Text) then Question4.Top := 50;
    if (Answer.Text = Question5.Text) then Question5.Top := 50;

    formTappyTuxGame.Answer.Clear;
    //TappyTuxGame.ThrowHammer();

  end;
end;

procedure TformTappyTuxGame.FormCreate(Sender: TObject);
begin
  // Creation of internal components
  vTappyTuxDrawer := TTappyTuxDrawer.Create(Self);
  vTappyTuxDrawer.Parent := Self;
  vTappyTuxDrawer.Top := 0;
  vTappyTuxDrawer.Left := 100;
  vTappyTuxDrawer.Height := Height-vTappyTuxDrawer.Top;
  vTappyTuxDrawer.Width := Width-vTappyTuxDrawer.Left;
  vTappyTuxDrawer.SendToBack();
end;


{procedure TformTappyTuxGame.OnClose;
begin
  formConfig
end;}

end.

