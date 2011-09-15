unit gameconfigform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  tappymodules, gameplayform;

type

  { TformConfig }

  TformConfig = class(TForm)
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
    procedure btnLoadClick(Sender: TObject);
    procedure comboGameTypeChange(Sender: TObject);
    procedure comboSoundChange(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure TranslateUI();
  end;

var
  formConfig: TformConfig;

implementation


{$R *.lfm}

{ TformConfig }

procedure TformConfig.comboGameTypeChange(Sender: TObject);
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

procedure TformConfig.btnLoadClick(Sender: TObject);
begin
  SetCurrentModule(comboGameType.ItemIndex);

  formTappyTuxGame.Show;
  Hide;
end;

procedure TformConfig.comboSoundChange(Sender: TObject);
begin

  {
  Case comboSound.itemIndex of
  0: begin
    TTappyGameData.SndFX = true;
    end;

  1: begin
    TTappyGameData.SndFX = false;
    end;


  end; }


end;

procedure TformConfig.FormClick(Sender: TObject);
begin

end;

procedure TformConfig.FormCreate(Sender: TObject);
begin
  TranslateUI();
end;

procedure TformConfig.TranslateUI;
var
  i: Integer;
  lModule: TTappyModule;
begin
  comboGameType.Items.Clear;
  for i := 0 to GetModuleCount() - 1 do
  begin
    lModule := GetModule(i);
    comboGameType.Items.Add(lModule.ShortDescription);
  end;
  comboGameType.ItemIndex := 0;
end;

end.

