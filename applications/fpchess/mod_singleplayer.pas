unit mod_singleplayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, Forms, Controls,
  chessmodules;

type

  { TSinglePlayerChessModule }

  TSinglePlayerChessModule = class(TChessModule)
  private
    textSecondPlayerName: TStaticText;
    editSecondPlayerName: TEdit;
  public
    SecondPlayerName: string;
    constructor Create();
    procedure CreateUserInterface(); override;
    procedure ShowUserInterface(AParent: TWinControl); override;
    procedure HideUserInterface(); override;
    procedure FreeUserInterface(); override;
    procedure PrepareForGame(); override;
    function IsMovingAllowedNow(): Boolean; override;
    function GetSecondPlayerName(): string; override;
    procedure HandleOnMove(AFrom, ATo: TPoint); override;
  end;

implementation

{ TSinglePlayerChessModule }

constructor TSinglePlayerChessModule.Create;
begin
  inherited Create;

  Description := 'Play against a friend in the same computer';
  Kind := cmkSinglePlayer;
end;

procedure TSinglePlayerChessModule.CreateUserInterface;
begin
  textSecondPlayerName := TStaticText.Create(nil);
  textSecondPlayerName.SetBounds(20, 20, 180, 50);
  textSecondPlayerName.Caption := 'Name of the second player';

  editSecondPlayerName := TEdit.Create(nil);
  editSecondPlayerName.SetBounds(200, 20, 150, 50);
  editSecondPlayerName.Text := 'Second player';
end;

procedure TSinglePlayerChessModule.ShowUserInterface(AParent: TWinControl);
begin
  textSecondPlayerName.Parent := AParent;
  editSecondPlayerName.Parent := AParent;
end;

procedure TSinglePlayerChessModule.HideUserInterface();
begin
  textSecondPlayerName.Parent := nil;
  editSecondPlayerName.Parent := nil;
end;

procedure TSinglePlayerChessModule.FreeUserInterface();
begin
  textSecondPlayerName.Free;
  editSecondPlayerName.Free;
end;

procedure TSinglePlayerChessModule.PrepareForGame;
begin
  SecondPlayerName := editSecondPlayerName.Text;
end;

function TSinglePlayerChessModule.IsMovingAllowedNow: Boolean;
begin
  Result := True;
end;

function TSinglePlayerChessModule.GetSecondPlayerName: string;
begin
  Result := SecondPlayerName;
end;

procedure TSinglePlayerChessModule.HandleOnMove(AFrom, ATo: TPoint);
begin

end;

initialization
  RegisterChessModule(TSinglePlayerChessModule.Create);
end.

