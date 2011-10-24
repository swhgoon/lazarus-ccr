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
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    procedure btnExitClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
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

uses gameconfigform;

{$R *.lfm}

{ TformTappyTuxGame }

procedure TformTappyTuxGame.btnExitClick(Sender: TObject);
begin
  Close;
  formConfig.Show;

  GetCurrentModule().EndGame();
end;

procedure TformTappyTuxGame.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    formTappyTuxGame.Memo1.Lines.Add(Edit1.Text);
    formTappyTuxGame.Edit1.Clear;
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

