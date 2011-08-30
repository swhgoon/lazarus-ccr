unit mod_samecomputer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, Forms, Controls,
  chessmodules;

type

  { TSameComputerChessModule }

  TSameComputerChessModule = class(TChessModule)
  private
    textSecondPlayerName: TStaticText;
    editSecondPlayerName: TEdit;
  public
    SecondPlayerName: string;
    constructor Create(); override;
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

{ TSameComputerChessModule }

constructor TSameComputerChessModule.Create;
begin
  inherited Create;

  Name := 'mod_samecomputer.pas';
  SelectionDescription := 'Play against a friend in the same computer';
  PlayingDescription := 'Playing against a friend in the same computer';
  Kind := cmkSameComputer;
end;

procedure TSameComputerChessModule.CreateUserInterface;
begin
  textSecondPlayerName := TStaticText.Create(nil);
  textSecondPlayerName.SetBounds(20, 20, 180, 50);
  textSecondPlayerName.Caption := 'Name of the second player';

  editSecondPlayerName := TEdit.Create(nil);
  editSecondPlayerName.SetBounds(200, 20, 150, 50);
  editSecondPlayerName.Text := 'Second player';
end;

procedure TSameComputerChessModule.ShowUserInterface(AParent: TWinControl);
begin
  textSecondPlayerName.Parent := AParent;
  editSecondPlayerName.Parent := AParent;
end;

procedure TSameComputerChessModule.HideUserInterface();
begin
  textSecondPlayerName.Parent := nil;
  editSecondPlayerName.Parent := nil;
end;

procedure TSameComputerChessModule.FreeUserInterface();
begin
  textSecondPlayerName.Free;
  editSecondPlayerName.Free;
end;

procedure TSameComputerChessModule.PrepareForGame;
begin
  SecondPlayerName := editSecondPlayerName.Text;
end;

function TSameComputerChessModule.IsMovingAllowedNow: Boolean;
begin
  Result := True;
end;

function TSameComputerChessModule.GetSecondPlayerName: string;
begin
  Result := SecondPlayerName;
end;

procedure TSameComputerChessModule.HandleOnMove(AFrom, ATo: TPoint);
begin

end;

initialization
  RegisterChessModule(TSameComputerChessModule.Create);
end.

