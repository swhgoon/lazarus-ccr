unit chessgame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpimage, dateutils;

const
  colA = 1;
  colB = 2;
  colC = 3;
  colD = 4;
  colE = 5;
  colF = 6;
  colG = 7;
  colH = 8;

  INT_CHESSTILE_SIZE = 40;
  INT_CHESSBOARD_SIZE = 40 * 8;

  FPCOLOR_TRANSPARENT_TILE: TFPColor = (Red: $0000; Green: $8100; Blue: $8100; Alpha: alphaOpaque); //+/-colTeal

type

  TChessTile = (ctEmpty,
    ctWPawn, ctWKnight, ctWBishop, ctWRook, ctWQueen, ctWKing,
    ctBPawn, ctBKnight, ctBBishop, ctBRook, ctBQueen, ctBKing
    );

const
  WhitePieces = [ctWPawn, ctWKnight, ctWBishop, ctWRook, ctWQueen, ctWKing];
  BlackPieces = [ctBPawn, ctBKnight, ctBBishop, ctBRook, ctBQueen, ctBKing];

type
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
    CurrentPlayerIsWhite: Boolean;
    Dragging: Boolean;
    DragStart, MouseMovePos: TPoint;
    UseTimer: Boolean;
    WhitePlayerTime: Integer; // milisseconds
    BlackPlayerTime: Integer; // milisseconds
    MoveStartTime: TDateTime;
    constructor Create;
    procedure StartNewGame(APlayAsWhite: Boolean; AUseTimer: Boolean; APlayerTime: Integer); overload;
    procedure StartNewGame(APlayAsWhite: Integer; AUseTimer: Boolean; APlayerTime: Integer); overload;
    function ClientToBoardCoords(AClientCoords: TPoint): TPoint;
    function CheckStartMove(AFrom: TPoint): Boolean;
    function MovePiece(AFrom, ATo: TPoint): Boolean;
    procedure UpdateTimes();
  end;

var
  vChessGame: TChessGame;

implementation

{ TChessGame }

constructor TChessGame.Create;
begin
  inherited Create;


end;

procedure TChessGame.StartNewGame(APlayAsWhite: Boolean; AUseTimer: Boolean; APlayerTime: Integer);
var
  lWPawnRow, lWMainRow, lBPawnRow, lBMainRow: Byte;
  i: Integer;
  j: Integer;
begin
  UseTimer := AUseTimer;
  CurrentPlayerIsWhite := True;
  WhitePlayerTime := APlayerTime * 60 * 1000; // minutes to milisseconds
  BlackPlayerTime := APlayerTime * 60 * 1000; // minutes to milisseconds
  MoveStartTime := Now;

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

procedure TChessGame.StartNewGame(APlayAsWhite: Integer; AUseTimer: Boolean; APlayerTime: Integer);
begin
  StartNewGame(APlayAsWhite = 0, AUseTimer, APlayerTime);
end;

{
  Returns: If the move is valid and was executed
}
function TChessGame.MovePiece(AFrom, ATo: TPoint): Boolean;
begin
  Result := False;

  if not CheckStartMove(AFrom) then Exit;

  // Parameter checking
  if (AFrom.X < 1) or (AFrom.X > 8) or (ATo.X < 1) or (ATo.X > 8) then Exit;
  if (AFrom.Y < 1) or (AFrom.Y > 8) or (ATo.Y < 1) or (ATo.Y > 8) then Exit;

  // col, row
  Board[ATo.X][ATo.Y] := Board[AFrom.X][AFrom.Y];
  Board[AFrom.X][AFrom.Y] := ctEmpty;

  UpdateTimes();
  CurrentPlayerIsWhite := not CurrentPlayerIsWhite;

  Result := True;
end;

procedure TChessGame.UpdateTimes();
var
  lNow: TDateTime;
  lTimeDelta: Integer;
begin
  lNow := Now;

  lTimeDelta := MilliSecondsBetween(lNow, MoveStartTime);
  MoveStartTime := lNow;

  if CurrentPlayerIsWhite then WhitePlayerTime := WhitePlayerTime - lTimeDelta
  else BlackPlayerTime := BlackPlayerTime - lTimeDelta;
end;

function TChessGame.ClientToBoardCoords(AClientCoords: TPoint): TPoint;
begin
  Result.X := 1 + AClientCoords.X div INT_CHESSTILE_SIZE;
  Result.Y := 1 + (INT_CHESSBOARD_SIZE - AClientCoords.Y) div INT_CHESSTILE_SIZE;
end;

{@@
  AFrom - The start move position in board coordinates
}
function TChessGame.CheckStartMove(AFrom: TPoint): Boolean;
begin
  if CurrentPlayerIsWhite then
    Result := Board[AFrom.X][AFrom.Y] in WhitePieces
  else
    Result := Board[AFrom.X][AFrom.Y] in BlackPieces;
end;

initialization

vChessGame := TChessGame.Create;

finalization

vChessGame.Free;

end.

