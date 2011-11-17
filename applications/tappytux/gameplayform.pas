unit gameplayform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // LCL
  ExtCtrls,
  // TappyTux
  tappydrawer, tappymodules;

type

  { TformTappyTuxGame }

  TformTappyTuxGame = class(TForm)
    btnExit: TButton;
    Answer: TEdit;
    Level: TEdit;
    Score: TEdit;
    Lives: TEdit;
    LabelLevels: TLabel;
    LabelScore: TLabel;
    LabelLives: TLabel;
    procedure btnExitClick(Sender: TObject);
    procedure AnswerKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NoClick(Sender: TObject);
    procedure TestChange(Sender: TObject);
    procedure YesClick(Sender: TObject);

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
  GetCurrentModule().EndGame();
  Close;
  formConfig.Show;

end;

procedure TformTappyTuxGame.AnswerKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    GetCurrentModule().Answered(Answer.Text);
    formTappyTuxGame.Answer.Clear;
  end;
end;

procedure TformTappyTuxGame.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  GetCurrentModule().EndGame();
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

procedure TformTappyTuxGame.FormShow(Sender: TObject);
begin

end;

procedure TformTappyTuxGame.NoClick(Sender: TObject);
begin

end;

procedure TformTappyTuxGame.TestChange(Sender: TObject);
begin

end;

procedure TformTappyTuxGame.YesClick(Sender: TObject);
begin

end;


end.

