{
  For playing through the internet via FICS - Free Internet Chess Server

  Based on this article:
  http://blog.mekk.waw.pl/archives/7-How-to-write-a-FICS-bot-part-I.html

  FICS website:
  http://www.freechess.org/
}
unit mod_fics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, Forms, Controls,
  chessmodules, chessgame;

type

  { TFICSChessModule }

  TFICSChessModule = class(TChessModule)
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

{ TFICSChessModule }

constructor TFICSChessModule.Create;
begin
  inherited Create;

  Description := 'Play online via the Free Internet Chess Server';
  Kind := cmkSinglePlayer;
end;

procedure TFICSChessModule.CreateUserInterface;
begin
{  textSecondPlayerName := TStaticText.Create(nil);
  textSecondPlayerName.SetBounds(20, 20, 180, 50);
  textSecondPlayerName.Caption := 'Name of the second player';

  editSecondPlayerName := TEdit.Create(nil);
  editSecondPlayerName.SetBounds(200, 20, 150, 50);
  editSecondPlayerName.Text := 'Second player';}
end;

procedure TFICSChessModule.ShowUserInterface(AParent: TWinControl);
begin
{  textSecondPlayerName.Parent := AParent;
  editSecondPlayerName.Parent := AParent;}
end;

procedure TFICSChessModule.HideUserInterface();
begin
{  textSecondPlayerName.Parent := nil;
  editSecondPlayerName.Parent := nil;}
end;

procedure TFICSChessModule.FreeUserInterface;
begin
{  textSecondPlayerName.Free;
  editSecondPlayerName.Free;}
end;

procedure TFICSChessModule.PrepareForGame;
begin
//  SecondPlayerName := editSecondPlayerName.Text;
  ChessModuleDebugLn('[TFICSChessModule.PrepareForGame]');
end;

function TFICSChessModule.IsMovingAllowedNow: Boolean;
begin
  Result := not (vChessGame.IsWhitePlayerTurn xor vChessGame.FirstPlayerIsWhite);
end;

function TFICSChessModule.GetSecondPlayerName: string;
begin
//  Result := SecondPlayerName;
end;

// If a move came, it is because the local player did a move
// so send this move and start listening for a move
procedure TFICSChessModule.HandleOnMove(AFrom, ATo: TPoint);
begin

end;

initialization
  RegisterChessModule(TFICSChessModule.Create);
end.

