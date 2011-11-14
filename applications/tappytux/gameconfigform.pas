unit gameconfigform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // LCL
  ExtCtrls,
  // TappyTux
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
  memoGameType.Clear;
  memoGameType.Lines.Add(GetModule(comboGameType.itemIndex).LongDescription);

end;

procedure TformConfig.btnLoadClick(Sender: TObject);
begin
  SetCurrentModule(comboGameType.ItemIndex);
  formTappyTuxGame.Show;
  GetCurrentModule().StartNewGame(comboSound.ItemIndex, comboMusic.ItemIndex,
                                  comboLevel.ItemIndex, ltbWordlist.ItemIndex);

  Hide;
end;

procedure TformConfig.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  TranslateUI();

  // Initialize modules
  for i := 0 to GetModuleCount() -1 do
    GetModule(i).InitModule();
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

