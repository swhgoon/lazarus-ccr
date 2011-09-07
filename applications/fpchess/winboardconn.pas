{
Unit with the basics to communicate with engines that use the winboard protocol.
I used some code of the TProcess example also:
https://lazarus-ccr.svn.sourceforge.net/svnroot/lazarus-ccr/examples/process/

}
unit winboardConn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, StdCtrls, chessgame;

type
  moveInCoord = array[1..2] of TPoint;

  TwinboardConn = class
  public
    procedure startEngine(path : String);
    procedure stopEngine;
    procedure tellMove(move : String);
    function engineMove() : moveInCoord;
    function coordToString(AFrom, ATo : TPoint; pieceFrom, pieceTo : TChessTile) : String;
    function stringToCoord(moveStr : String) : moveInCoord;
  private
    procedure readFromPipe;
    function extractMove : string;
    function letterToNumber(num:String) : String;
    function  numberToLetter(n : integer) : String;
    function pieceToLetter(piece : TChessTile) : String;
  end;

var
  engineProcess : TProcess;
  outputText    : String;
  vwinboardConn : TwinboardConn;

implementation

procedure TwinboardConn.startEngine(path : String);
begin
  engineProcess:= TProcess.Create(nil);
  engineProcess.CommandLine := path;

  engineProcess.Options := engineProcess.Options + [poUsePipes];
  engineProcess.Execute;
end;

procedure TwinboardConn.stopEngine;
begin
  if engineProcess.Running then
    engineProcess.Free;
end;

procedure TwinboardConn.tellMove(move : String);
begin
  move := move+#13+#10;
  EngineProcess.Input.Write(move[1], length(move));
end;

//return the engine move.
function TwinboardConn.engineMove : moveInCoord;
var move  : String;
    points: moveInCoord;
begin
  sleep(500);
  repeat
    readFromPipe;
  until length(outputText)>10;
  move := extractMove;
  points := stringToCoord(move);
  result:=points;
end;

function TwinboardConn.coordToString(AFrom, ATo : TPoint; pieceFrom, pieceTo : TChessTile) : String;
var
  move : String;
begin
  //still need to verify castle an en passant

  move:= move + numberToLetter(AFrom.X);
  move:= move + intToStr(AFrom.Y);

  move:= move + numberToLetter(ATo.X);
  move:= move + IntToStr(ATo.Y);
  result:=move;

end;

function TwinboardConn.pieceToLetter(piece : TChessTile) : String;
begin
  case piece of
    ctWRook : result:= 'R';
    ctBRook : result:= 'R';
    ctWKnight : result:= 'N';
    ctBKnight : result:= 'N';
    ctWBishop : result:= 'B';
    ctBBishop : result:= 'B';
    ctWQueen : result:= 'Q';
    ctBQueen : result:= 'Q';
    ctWKing : result:= 'K';
    ctBKing : result:= 'K';
  end;
end;

function  TwinboardConn.numberToLetter(n : integer) : String;
begin
  case n of
    1 : result := 'a';
    2 : result := 'b';
    3 : result := 'c';
    4 : result := 'd';
    5 : result := 'e';
    6 : result := 'f';
    7 : result := 'g';
    8 : result := 'h';
  end;
end;

function TwinboardConn.stringToCoord(moveStr : String) : moveInCoord;
var move : moveInCoord;
begin
  //need to verify castle here
  //if moveStr[1] in ['R','N','B','Q','K'] then
  //  Delete(moveStr,1,1);
  move[1].X:=strToInt(letterToNumber(moveStr[1]));
  move[1].Y:=strToInt(moveStr[2]);
  move[2].X:=strToInt(letterToNumber(moveStr[3]));
  move[2].Y:=strToInt(moveStr[4]);
  result:=move;
end;
//Transform collum number in letter, ex.: 1=a, 3=c ....
function TwinboardConn.letterToNumber(num:String) : String;
begin
  case num[1] of
    'a' : result:='1';
    'b' : result:='2';
    'c' : result:='3';
    'd' : result:='4';
    'e' : result:='5';
    'f' : result:='6';
    'g' : result:='7';
    'h' : result:='8';
  end;
end;

//read all the output text from the TProcess pipe
procedure TwinboardConn.readFromPipe;
var
  NoMoreOutput: boolean;

  procedure DoStuffForProcess;
  var
    Buffer: string;
    BytesAvailable: DWord;
    BytesRead:LongInt;
  begin
    if engineProcess.Running then
    begin
      BytesAvailable := engineProcess.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := engineProcess.OutPut.Read(Buffer[1], BytesAvailable);
        OutputText := OutputText + copy(Buffer,1, BytesRead);
        BytesAvailable := engineProcess.Output.NumBytesAvailable;
        NoMoreOutput := false;
      end;
    end;
  end;
begin
  if engineProcess.Running then
    repeat
      NoMoreOutput := true;
      DoStuffForProcess;
    until noMoreOutput;
end;

function TwinboardConn.extractMove : string;
var move : String;
    i,p  : integer;
begin
  p := pos('My move is: ', outputText);
  p:=p+12;
  if p>0 then
    while outputText[p]<>#10 do
    begin
      move:= move+outputText[p];
      inc(p);
    end;
  result:=move;
  outputText:='';
end;

end.
