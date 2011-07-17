unit chessgame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpimage, dateutils,
  Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, Spin;

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

  TPacketKind = (pkConnect, pkStartGameClientAsWhite, pkStartGameClientAsBlack, pkMove);
  BitBoard = array[1..8] of array [1..8] of boolean;// Map of attacked squares

  { TPacket }

  TPacket = class
  public
    // Packet Data
    ID: Cardinal;
    Kind: TPacketKind;
    MoveStartX, MoveStartY, MoveEndX, MoveEndY: Byte;
    Next: TPacket; // To build a linked list
  end;

  TChessTile = (ctEmpty,
    ctWPawn, ctWKnight, ctWBishop, ctWRook, ctWQueen, ctWKing,
    ctBPawn, ctBKnight, ctBBishop, ctBRook, ctBQueen, ctBKing
    );

const
  WhitePieces = [ctWPawn, ctWKnight, ctWBishop, ctWRook, ctWQueen, ctWKing];
  BlackPieces = [ctBPawn, ctBKnight, ctBBishop, ctBRook, ctBQueen, ctBKing];
  WhitePiecesOrEmpty = [ctEmpty, ctWPawn, ctWKnight, ctWBishop, ctWRook, ctWQueen, ctWKing];
  BlackPiecesOrEmpty = [ctEmpty, ctBPawn, ctBKnight, ctBBishop, ctBRook, ctBQueen, ctBKing];

type
  {@@
    The index [1][1] refers to the left-bottom corner of the table,
    also known as A1.
    The first index is the column, to follow the same standard used to
    say coordinates, for example: C7 = [3][7]
  }
  TChessBoard = array[1..8] of array[1..8] of TChessTile;

  TChessMove = record
    From, To_: TPoint;
    PieceMoved, PieceEaten: TChessTile;
  end;

  { TChessGame }

  TChessGame = class
  private
  public
    Board: TChessBoard;
    msg : String;
    CurrentPlayerIsWhite: Boolean;
    Dragging: Boolean;
    DragStart, MouseMovePos: TPoint;
    UseTimer: Boolean;
    WhitePlayerTime: Integer; // milisseconds
    BlackPlayerTime: Integer; // milisseconds
    MoveStartTime: TDateTime;
    // Last move (might in the future store all history)
    PreviousMove: TChessMove;
    // Data for Enpassant
    EnpassantSquare: TPoint; // Negative coords indicate that it is not allowed
    // Data for the Roque
    IsWhiteLeftRoquePossible, IsWhiteRightRoquePossible: Boolean;
    IsBlackLeftRoquePossible, IsBlackRightRoquePossible: Boolean;
    Castle:boolean;//If the move will be a castle.
    CastleCord: TPoint;
    //
    constructor Create;
    procedure StartNewGame(APlayAsWhite: Boolean; AUseTimer: Boolean; APlayerTime: Integer); overload;
    procedure StartNewGame(APlayAsWhite: Integer; AUseTimer: Boolean; APlayerTime: Integer); overload;
    function ClientToBoardCoords(AClientCoords: TPoint): TPoint;
    function CheckStartMove(AFrom: TPoint): Boolean;
    function CheckEndMove(ATo: TPoint): Boolean;
    function WillKingBeInCheck(AFrom, ATo, AEnpassantToClear: TPoint): Boolean;
    function IsKingInCheck(AKingPos: TPoint): Boolean;
    function FindKing(): TPoint;
    function MovePiece(AFrom, ATo: TPoint): Boolean;
    procedure DoMovePiece(AFrom, ATo, AEnpassantToClear: TPoint);
    function ValidateRookMove(AFrom, ATo: TPoint) : boolean;
    procedure ResetCastleVar(AFrom : TPoint);
    function ValidateKnightMove(AFrom, ATo: TPoint) : boolean;
    function ValidateBishopMove(AFrom, ATo: TPoint) : boolean;
    function ValidateQueenMove(AFrom, ATo: TPoint) : boolean;
    function ValidateKingMove(AFrom, ATo: TPoint) : boolean;
    function CheckPassageSquares(side: boolean; AFrom, ATo : TPoint) : boolean;
    procedure DoCastle();
    function ValidatePawnMove(AFrom, ATo: TPoint;
      var AEnpassantSquare, AEnpassantSquareToClear: TPoint) : boolean;
    function IsSquareOccupied(ASquare: TPoint): Boolean;
    procedure UpdateTimes();
  end;

var
  vChessGame: TChessGame;

operator = (A, B: TPoint): Boolean;

implementation

operator=(A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

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

  EnpassantSquare := Point(-1, -1); // Negative coords indicate that it is not allowed
  IsWhiteLeftRoquePossible := True;
  IsWhiteRightRoquePossible := True;
  IsBlackLeftRoquePossible := True;
  IsBlackRightRoquePossible := True;

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
var
  i : integer;
  LEnpassantSquare, LEnpassantToClear: TPoint;
begin
  LEnpassantSquare := Point(-1, -1);
  LEnpassantToClear := Point(-1, -1);
  Castle:=false;
  Result := False;

  // Verify what is in the start and destination squares
  if not CheckStartMove(AFrom) then Exit;
  if not CheckEndMove(ATo) then Exit;

  // Verify if the movement is in accordace to the rules for this piece
  if Board[AFrom.X][AFrom.Y] in [ctWPawn, ctBPawn] then result := ValidatePawnMove(AFrom,ATo, LEnpassantSquare, LEnpassantToClear)
  else if Board[AFrom.X][AFrom.Y] in [ctWRook, ctBRook] then result := ValidateRookMove(AFrom,ATo)
  else if Board[AFrom.X][AFrom.Y] in [ctWKnight, ctBKnight] then result := ValidateKnightMove(AFrom,ATo)
  else if Board[AFrom.X][AFrom.Y] in [ctWBishop, ctBBishop] then result := ValidateBishopMove(AFrom,ATo)
  else if Board[AFrom.X][AFrom.Y] in [ctWQueen, ctBQueen] then result := ValidateQueenMove(AFrom,ATo)
  else if Board[AFrom.X][AFrom.Y] in [ctWKing, ctBKing] then result := ValidateKingMove(AFrom,ATo);

  if not Result then Exit;

  // Check if the king will be left in check by this move
  if WillKingBeInCheck(AFrom, ATo, LEnpassantToClear) then Exit;

  // If we arrived here, this means that the move will be really executed

  // Store this move as the previously executed one
  PreviousMove.From := AFrom;
  PreviousMove.To_ := ATo;
  PreviousMove.PieceMoved := Board[AFrom.X][AFrom.Y];
  PreviousMove.PieceEaten := Board[ATo.X][ATo.Y];
  EnpassantSquare := LEnpassantSquare;

  // Now we will execute the move
  DoMovePiece(AFrom, ATo, LEnpassantToClear);
  if Castle then DoCastle();

  //
  UpdateTimes();

  // Change player
  CurrentPlayerIsWhite := not CurrentPlayerIsWhite;
end;

{ Really moves the piece without doing any check }
procedure TChessGame.DoMovePiece(AFrom, ATo, AEnpassantToClear: TPoint);
begin
  // col, row
  Board[ATo.X][ATo.Y] := Board[AFrom.X][AFrom.Y];
  Board[AFrom.X][AFrom.Y] := ctEmpty;

  // If Enpassant, clear the remaining pawn
  if AEnpassantToClear.X <> -1 then
    Board[AEnpassantToClear.X][AEnpassantToClear.Y] := ctEmpty;
end;

procedure TChessGame.DoCastle();
begin
  if CastleCord.X=8 then
    Board[6][CastleCord.Y]:=Board[8][CastleCord.Y]
  else
    Board[4][CastleCord.Y]:=Board[1][CastleCord.Y];
  Board[CastleCord.X][CastleCord.Y]:=ctEmpty;
end;

//return true if the move of a Rook is valid.
function TChessGame.ValidateRookMove(AFrom, ATo: TPoint): boolean;
var
  i: Integer;
begin
  Result := False;

  //////////////////////////////////////UP////////////////////////////////////////
  if (AFrom.X = ATo.X) and (AFrom.Y < ATo.Y) then
  begin
    // Check if there are pieces in the middle of the way
    for i := AFrom.Y + 1 to ATo.Y - 1 do
      if Board[AFrom.X][i] <> ctEmpty then Exit;
    ResetCastleVar(AFrom);
    Exit(True);
  end;
///////////////////////////////////DOWN/////////////////////////////////////////
  if (AFrom.X = ATo.X) and (AFrom.Y > ATo.Y) then
  begin
    // Check if there are pieces in the middle of the way
    for i := AFrom.Y - 1 downto ATo.Y + 1 do
      if Board[AFrom.X][i] <> ctEmpty then Exit;
    ResetCastleVar(AFrom);
    Exit(True);
  end;
////////////////////////////////////RIGHT////////////////////////////////////////
  if (AFrom.X < ATo.X) and (AFrom.Y = ATo.Y) then
  begin
    // Check if there are pieces in the middle of the way
    for i := AFrom.X + 1 to ATo.X - 1 do
      if Board[i][AFrom.Y] <> ctEmpty then Exit;
    ResetCastleVar(AFrom);
    Exit(True);
  end;
///////////////////////////////////LEFT/////////////////////////////////////////
  if (AFrom.X > ATo.X) and (AFrom.Y = ATo.Y) then
  begin
    // Check if there are pieces in the middle of the way
    for i := AFrom.X - 1 downto ATo.X + 1 do
      if Board[i][AFrom.Y] <> ctEmpty then Exit;
    ResetCastleVar(AFrom);
    Exit(True);
  end;
end;

//check if castle is still posible.
procedure TChessGame.ResetCastleVar(AFrom : TPoint);
begin
  if ((AFrom.X=1) and (AFrom.Y=1) and (IsWhiteLeftRoquePossible)) then IsWhiteLeftRoquePossible:=false;
  if ((AFrom.X=8) and (AFrom.Y=1) and (IsWhiteRightRoquePossible)) then IsWhiteRightRoquePossible:=false;
  if ((AFrom.X=1) and (AFrom.Y=8) and (IsBlackLeftRoquePossible)) then IsBlackLeftRoquePossible:=false;
  if ((AFrom.X=8) and (AFrom.Y=8) and (IsBlackLeftRoquePossible)) then IsBlackRightRoquePossible:=false;
end;

{
  The knight has 8 possible destinations only:

        [X][ ][X]
     [X][ ][ ][ ][X]
     [ ][ ][K][ ][ ]
     [X][ ][ ][ ][X]
        [X]   [X]
}
function TChessGame.ValidateKnightMove(AFrom, ATo: TPoint): Boolean;
begin
  Result := (AFrom.X = ATo.X + 1) and (AFrom.Y + 2 = ATo.Y); // upper left corner
  Result := Result or ((AFrom.X = ATo.X + 2) and (AFrom.Y + 1 = ATo.Y)); // upper left corner
  Result := Result or ((AFrom.X = ATo.X + 2) and (AFrom.Y - 1 = ATo.Y)); // lower left corner
  Result := Result or ((AFrom.X = ATo.X + 1) and (AFrom.Y - 2 = ATo.Y)); // lower left corner
  Result := Result or ((AFrom.X = ATo.X - 1) and (AFrom.Y - 2 = ATo.Y)); // lower right corner
  Result := Result or ((AFrom.X = ATo.X - 2) and (AFrom.Y - 1 = ATo.Y)); // lower right corner
  Result := Result or ((AFrom.X = ATo.X - 2) and (AFrom.Y + 1 = ATo.Y)); // upper right corner
  Result := Result or ((AFrom.X = ATo.X - 1) and (AFrom.Y + 2 = ATo.Y)); // upper right corner
end;

function TChessGame.ValidateBishopMove(AFrom, ATo: TPoint): Boolean;
var
  i,j : Integer;
begin
  result :=false;
  //Up left
  if (AFrom.X>ATo.X) and (AFrom.Y<ATo.Y) and (AFrom.X-ATo.X=ATo.Y-AFrom.Y)then
  begin
    i := AFrom.X-1;
    j := AFrom.Y+1;
    while (i>=ATo.X+1) and (j<=ATo.Y-1) do
    begin
      if Board[i][j] <> ctEmpty then Exit;
      i := i - 1;
      j := j + 1;
    end;
    exit(True);
  end;
  //Up right
  if (AFrom.X<ATo.X) and (AFrom.Y<ATo.Y) and (ATo.X-AFrom.X=ATo.Y-AFrom.Y) then
  begin
    i := AFrom.X+1;
    j := AFrom.Y+1;
    while (i<=ATo.X-1) and (j<=ATo.Y-1) do
    begin
      if Board[i][j] <> ctEmpty then Exit;
      i := i + 1;
      j := j + 1;
    end;
    exit(True);
  end;
  //Down left
  if (AFrom.X>ATo.X) and (AFrom.Y>ATo.Y) and (AFrom.X-ATo.X=AFrom.Y-ATo.Y) then
  begin
    i := AFrom.X-1;
    j := AFrom.Y-1;
    while (i>=ATo.X+1) and (j>=ATo.Y+1) do
    begin
      if Board[i][j] <> ctEmpty then Exit;
      i := i - 1;
      j := j - 1;
    end;
    exit(True);
  end;
  //Down right
  if (AFrom.X<ATo.X) and (AFrom.Y>ATo.Y) and (ATo.X-AFrom.X=AFrom.Y-ATo.Y)then
  begin
    i := AFrom.X+1;
    j := AFrom.Y-1;
    while (i<=ATo.X-1) and (j>=ATo.Y+1) do
    begin
      if Board[i][j] <> ctEmpty then Exit;
      i := i + 1;
      j := j - 1;
    end;
    exit(True);
  end;
end;

function TChessGame.ValidateQueenMove(AFrom, ATo: TPoint): Boolean;
begin
  Result := ValidateRookMove(AFrom, ATo) or ValidateBishopMove(AFrom, ATo);
end;

function TChessGame.ValidateKingMove(AFrom, ATo: TPoint): Boolean;
var passage : boolean;
begin
  Result := False;

  // Verify the possibility of a Roque
  if CurrentPlayerIsWhite then
  begin
    // Castle to the right
    if IsWhiteRightRoquePossible and (AFrom.X = 5) and (AFrom.Y = 1)
      and (ATo.X = 7) and (ATo.Y = 1) and (board[6][1]=ctEmpty) then
    begin
      if not(CheckPassageSquares(true,AFrom,ATo)) then exit(false);
      Castle:=true;
      CastleCord.X:=8;
      CastleCord.Y:=1;
      result:= True;
    end;
    // Castle to the left
    if IsWhiteLeftRoquePossible and (AFrom.X = 5) and (AFrom.Y = 1)
      and (ATo.X = 3) and (ATo.Y = 1) and (board[2][1]=ctEmpty) and (board[4][1]=ctEmpty) then
    begin
      if not(CheckPassageSquares(false,AFrom,ATo)) then exit(false);
      Castle:=true;
      CastleCord.X:=1;
      CastleCord.Y:=1;
      result:= True;
    end;
  end
  else
  begin
  // Castle to the right
    if IsBlackRightRoquePossible and (AFrom.X = 5) and (AFrom.Y = 8)
      and (ATo.X = 7) and (ATo.Y = 8) and (board[6][8]=ctEmpty) then
    begin
      if not(CheckPassageSquares(true,AFrom,ATo)) then exit(false);
      Castle:=true;
      CastleCord.X:=8;
      CastleCord.Y:=8;
      result:= True;
    end;
    // Castle to the left
    if IsBlackLeftRoquePossible and (AFrom.X = 5) and (AFrom.Y = 8)
      and (ATo.X = 3) and (ATo.Y = 8) and (board[2][8]=ctEmpty) and (board[4][8]=ctEmpty) then
    begin
      if not(CheckPassageSquares(false,AFrom,ATo)) then exit(false);
      Castle:=true;
      CastleCord.X:=1;
      CastleCord.Y:=8;
      result:= True;
    end;
  end;

  // Simple move
  if (AFrom.X > ATo.X + 1) or (AFrom.X + 1 < ATo.X) then Exit;
  if (AFrom.Y > ATo.Y + 1) or (AFrom.Y + 1 < ATo.Y) then Exit;

  Result := True;
end;

//Return false if during the passage the king will be in check
function TChessGame.CheckPassageSquares(side : boolean; AFrom, ATo : TPoint) : boolean; //Left=false;Right=true;
var
  LocalBoard : TChessBoard;
  kingPos : TPoint;
begin
  kingPos := FindKing();
  Result := IsKingInCheck(kingPos);
  if (result) then exit(false);

  LocalBoard:=Board;

  if CurrentPlayerIsWhite then
  begin
    if side then
    begin
      Board[5][1]:=ctEmpty;
      Board[6][1]:=ctWKing;
      kingPos := FindKing();
      Result := IsKingInCheck(kingPos);
      Board:=LocalBoard;
      Exit(not Result);
    end
    else
    begin
      Board[5][1]:=ctEmpty;
      Board[4][1]:=ctWKing;
      kingPos := FindKing();
      Result := IsKingInCheck(kingPos);
      Board:=LocalBoard;
      Exit(not Result);
    end;
  end
  else
  begin
    if side then
    begin
      Board[5][8]:=ctEmpty;
      Board[6][8]:=ctBKing;
      kingPos := FindKing();
      Result := IsKingInCheck(kingPos);
      Board:=LocalBoard;
      Exit(not Result);
    end
    else
    begin
      Board[5][8]:=ctEmpty;
      Board[4][8]:=ctBKing;
      kingPos := FindKing();
      Result := IsKingInCheck(kingPos);
      Board:=LocalBoard;
      Exit(not Result);
    end;
  end;
end;

{
  The white is always in the bottom at the moment,
  which means the smallest x,y values

  If positive coords are feed to AEnpassantSquare, this means that
  enpassant will be allowed in the next move

  If positive coords are feed to AEnpassantSquareToClear, then we
  made an enpassant capture and a square is to be cleared from the
  captured pawn. This isn't done yet because the check verification
  wasn't made yet, so it is not certain that the move will take place.
}
function TChessGame.ValidatePawnMove(AFrom, ATo: TPoint;
  var AEnpassantSquare, AEnpassantSquareToClear: TPoint): Boolean;
begin
  AEnpassantSquare := Point(-1, -1);
  AEnpassantSquareToClear := Point(-1, -1);
  Result := False;

  if CurrentPlayerIsWhite then
  begin
    // Normal move forward
    if (AFrom.X = ATo.X) and (AFrom.Y = ATo.Y - 1) then
    begin
      Result := not IsSquareOccupied(ATo);
    end
    // Initial double move forward
    else if (AFrom.X = ATo.X) and (AFrom.Y = 2) and (AFrom.Y = ATo.Y - 2) then
    begin
      Result := not IsSquareOccupied(ATo);
      AEnpassantSquare := Point(AFrom.X, ATo.Y - 1);
    end
    // Normal capture in the left
    else if (ATo.X = AFrom.X-1) and (ATo.Y = AFrom.Y+1) and (Board[ATo.X][ATo.Y] in BlackPieces) then
    begin
      Result := True;
    end
    // Normal capture in the right
    else if (ATo.X = AFrom.X+1) and (ATo.Y = AFrom.Y+1) and (Board[ATo.X][ATo.Y] in BlackPieces) then
    begin
      Result := True;
    end
    // En Passant Capture in the left
    else if (EnPassantSquare = ATo) and
      (ATo.X = AFrom.X-1) and (ATo.Y = AFrom.Y+1) then
    begin
      Result := True;
      AEnpassantSquareToClear := Point(ATo.X, ATo.Y-1);
    end
    // En Passant Capture in the right
    else if (EnPassantSquare = ATo) and
      (ATo.X = AFrom.X+1) and (ATo.Y = AFrom.Y+1) then
    begin
      Result := True;
      AEnpassantSquareToClear := Point(ATo.X, ATo.Y-1);
    end;
  end
  else
  begin
    // Normal move forward
    if (AFrom.X = ATo.X) and (AFrom.Y = ATo.Y + 1) then
    begin
      Result := not IsSquareOccupied(ATo);
    end
    // Initial double move forward
    else if (AFrom.X = ATo.X) and (AFrom.Y = 7) and (AFrom.Y = ATo.Y + 2) then
    begin
      Result := not IsSquareOccupied(ATo);
      AEnpassantSquare := Point(AFrom.X, ATo.Y + 1);
    end
    // Capture a piece in the left
    else if (ATo.X = AFrom.X-1) and (ATo.Y = AFrom.Y-1) and (Board[ATo.X][ATo.Y] in WhitePieces) then
    begin
      Result := True;
    end
    // Capture a piece in the right
    else if (ATo.X = AFrom.X+1) and (ATo.Y = AFrom.Y-1) and (Board[ATo.X][ATo.Y] in WhitePieces) then
    begin
      Result := True;
    end
    // En Passant Capture in the left
    else if (EnPassantSquare = ATo) and
      (ATo.X = AFrom.X-1) and (ATo.Y = AFrom.Y-1) then
    begin
      Result := True;
      AEnpassantSquareToClear := Point(ATo.X, ATo.Y+1);
    end
    // En Passant Capture in the right
    else if (EnPassantSquare = ATo) and
      (ATo.X = AFrom.X+1) and (ATo.Y = AFrom.Y-1) then
    begin
      Result := True;
      // Don't clear immediately because we haven't yet checked for kind check
      AEnpassantSquareToClear := Point(ATo.X, ATo.Y+1);
    end;
  end;
end;

function TChessGame.IsSquareOccupied(ASquare: TPoint): Boolean;
begin
  Result := Board[ASquare.X][ASquare.Y] <> ctEmpty;
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

// Check if we are moving to either an empty space or to an enemy piece
function TChessGame.CheckEndMove(ATo: TPoint): Boolean;
begin
  if CurrentPlayerIsWhite then
    Result := Board[ATo.X][ATo.Y] in BlackPiecesOrEmpty
  else
    Result := Board[ATo.X][ATo.Y] in WhitePiecesOrEmpty;
end;

{@@
  Check if we are moving one of our own pieces

  AFrom - The start move position in board coordinates
}
function TChessGame.CheckStartMove(AFrom: TPoint): Boolean;
begin
  if CurrentPlayerIsWhite then
    Result := Board[AFrom.X][AFrom.Y] in WhitePieces
  else
    Result := Board[AFrom.X][AFrom.Y] in BlackPieces;
end;

// True  - The King will be in check
// False - The King will not be in check
function TChessGame.WillKingBeInCheck(AFrom, ATo, AEnpassantToClear: TPoint): Boolean;
var
  kingPos: TPoint;
  localBoard: TChessBoard;
begin
  Result := false;

  localBoard := Board;

  DoMovePiece(AFrom, ATo, AEnpassantToClear);

  kingPos := FindKing();

  Result := IsKingInCheck(kingPos);

  Board:=localBoard;
end;

function TChessGame.IsKingInCheck(AKingPos: TPoint): Boolean;
var
  i,j : integer;
  piecePos : TPoint;
begin
  Result := False;

  for i:=1 to 8 do
    for j:=1 to 8 do
    begin
      piecePos := Point(i, j);
      if not (CurrentPlayerIsWhite) then
      begin
        case Board[i][j] of
        ctWRook:   Result:= ValidateRookMove(piecePos,AKingPos);
        ctWKnight: Result:= ValidateKnightMove(piecePos,AKingPos);
        ctWBishop: Result:= ValidateBishopMove(piecePos,AKingPos);
        ctWQueen:  Result:= ValidateQueenMove(piecePos,AKingPos);
        ctWKing:   Result:= ValidateKingMove(piecePos,AKingPos);
        end;
      end
      else
      begin
        case Board[i][j] of
        ctBRook:   Result:= ValidateRookMove(piecePos,AKingPos);
        ctBKnight: Result:= ValidateKnightMove(piecePos,AKingPos);
        ctBBishop: Result:= ValidateBishopMove(piecePos,AKingPos);
        ctBQueen:  Result:= ValidateQueenMove(piecePos,AKingPos);
        ctBKing:   Result:= ValidateKingMove(piecePos,AKingPos);
        end;
      end;
      if (result) then exit();
    end;
end;

{ Negative coords indicate that the king is not in the game }
function TChessGame.FindKing(): TPoint;
var
  i,j : integer;
begin
  Result := Point(-1, -1);

  for i:=1 to 8 do
    for j:=1 to 8 do
    if (CurrentPlayerIsWhite) and (Board[i][j]=ctWKing) then
    begin
      Result := Point(i, j);
      Exit;
    end
    else if (not CurrentPlayerIsWhite) and (Board[i][j]=ctBKing) then
    begin
      Result := Point(i, j);
      Exit;
    end;
end;

initialization

vChessGame := TChessGame.Create;

finalization

vChessGame.Free;

end.

