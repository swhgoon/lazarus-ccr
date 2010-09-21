unit chessgame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

const
  colA = 1;
  colB = 2;
  colC = 3;
  colD = 4;
  colE = 5;
  colF = 6;
  colG = 7;
  colH = 8;

  INT_CHESSTILE_SIZE = 20;
  INT_CHESSBOARD_SIZE = 200;

type

  TChessTile = (ctEmpty,
    ctWPawn, ctWKnight, ctWBishop, ctWRook, ctWQueen, ctWKing,
    ctBPawn, ctBKnight, ctBBishop, ctBRook, ctBQueen, ctBKing
    );

  {@@
    The index [1][1] refers to the left-bottom corner of the table,
    also known as A1.
    The first index is the column, to follow the same standard used to
    say coordinates, for example: C7 = [3][7]
  }
  TChessBoard = array[1..8] of array[1..8] of TChessTile;

  { TChessGame }

  TChessGame = class
  public
    Board: TChessBoard;
    procedure StartNewGame(APlayAsWhite: Boolean); overload;
    procedure StartNewGame(APlayAsWhite: Integer); overload;
  end;

var
  vChessGame: TChessGame;

implementation

{ TChessGame }

procedure TChessGame.StartNewGame(APlayAsWhite: Boolean);
var
  lWPawnRow, lWMainRow, lBPawnRow, lBMainRow: Byte;
  i: Integer;
  j: Integer;
begin
  //
  if APlayAsWhite then
  begin
    lWPawnRow := 2;
    lWMainRow := 1;
    lBPawnRow := 7;
    lBMainRow := 8;
  end
  else
  begin
    lWPawnRow := 7;
    lWMainRow := 8;
    lBPawnRow := 2;
    lBMainRow := 1;
  end;

  // First, clear the board
  for i := 1 to 8 do
   for j := 1 to 8 do
    Board[i][j] := ctEmpty;

  // White pawns
  for i := 1 to 8 do
   Board[i][lWPawnRow] := ctWPawn;

  // White main row
  Board[1][lWMainRow] := ctWRook;
  Board[2][lWMainRow] := ctWKnight;
  Board[3][lWMainRow] := ctWBishop;
  Board[4][lWMainRow] := ctWQueen;
  Board[5][lWMainRow] := ctWKing;
  Board[6][lWMainRow] := ctWBishop;
  Board[7][lWMainRow] := ctWKnight;
  Board[8][lWMainRow] := ctWRook;

  // White pawns
  for i := 1 to 8 do
   Board[i][lBPawnRow] := ctBPawn;

  // Black main row
  Board[1][lBMainRow] := ctBRook;
  Board[2][lBMainRow] := ctBKnight;
  Board[3][lBMainRow] := ctBBishop;
  Board[4][lBMainRow] := ctBQueen;
  Board[5][lBMainRow] := ctBKing;
  Board[6][lBMainRow] := ctBBishop;
  Board[7][lBMainRow] := ctBKnight;
  Board[8][lBMainRow] := ctBRook;
end;

procedure TChessGame.StartNewGame(APlayAsWhite: Integer);
begin
  StartNewGame(APlayAsWhite = 0);
end;

initialization

vChessGame := TChessGame.Create;

finalization

vChessGame.Free;

end.

