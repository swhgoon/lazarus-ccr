{
Unit with the basics to communicate with engines that use the winboard protocol.
I used some code of the TProcess example also:
https://lazarus-ccr.svn.sourceforge.net/svnroot/lazarus-ccr/examples/process/

}
unit winboardConn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Process, StdCtrls, chessgame;

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
    function letterToNumber(num:String) : Integer;
    function numberToLetter(n : integer) : String;
    procedure detectEngine(path : String);
  end;

var
  engineProcess : TProcess;
  outputText    : String;
  vwinboardConn : TwinboardConn;
  algebraicInput: boolean;
  engineMoveComIndex : integer;
  engineMoveCommand : array[1..4,1..2] of String;

implementation

procedure TwinboardConn.startEngine(path : String);
begin
  engineMoveCommand[1,1]:='gnuchess 5.07';
  engineMoveCommand[1,2]:='My move is: ';
  engineMoveCommand[2,1]:='phalanx';
  engineMoveCommand[2,2]:='my move is ';
  engineMoveCommand[3,1]:='gnuchess 5.08';
  engineMoveCommand[3,2]:='move ';
  engineMoveCommand[4,1]:='gnuchess 6.0.1';
  engineMoveCommand[4,2]:='My move is : ';

  if engineProcess<>nil then
    if engineProcess.Running then
      stopEngine;
  engineProcess:= TProcess.Create(nil);
  engineProcess.CommandLine := path;

  engineProcess.Options := engineProcess.Options + [poUsePipes];
  engineProcess.Execute;

  detectEngine(path);
end;

procedure TwinboardConn.detectEngine(path : String);
var engineOut : String;
    extraCommand : String;
begin
  //crafty only show his moves in abbreviated algebraic notation, it's not in
  //the array 'engineMoveCommand' because this is not implemented yet.
  if pos(path,'crafty')<> 0 then algebraicInput:=true;
  sleep(50); //just to guarantee that the engine has already tell his initial output
  readFromPipe;
  if pos('GNU Chess 5.07',outputText)>0 then
    engineMoveComIndex:=1;
  if pos('Phalanx',outputText)>0 then
    engineMoveComIndex:=2;
  if pos('GNU Chess 5.08',outputText)>0 then
    engineMoveComIndex:=3;
  if pos('GNU Chess 6.0.1',outputText)>0 then
    engineMoveComIndex:=4;
  extraCommand:='xboard'+#13+#10;
  EngineProcess.Input.Write(extraCommand[1], length(extraCommand));
  sleep(100);
  outputText:='';
end;

procedure TwinboardConn.stopEngine;
begin
  if engineProcess<>nil then
    if engineProcess.Running then
      engineProcess.Terminate(0);
  engineProcess.free;
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
    Application.processMessages;
    readFromPipe;
  until pos(engineMoveCommand[engineMoveComIndex][2],outputText)>0;
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
  if moveStr[1] in ['P','R','N','B','Q','K'] then
    Delete(moveStr,1,1);
  move[1].X:=letterToNumber(moveStr[1]);
  move[1].Y:=StrToInt(moveStr[2]);
  if moveStr[3] in ['x','-'] then
    Delete(moveStr,3,1);
  if moveStr[4] in ['P','R','N','B','Q','K'] then
    Delete(moveStr,4,1);
  move[2].X:=letterToNumber(moveStr[3]);
  move[2].Y:=strToInt(moveStr[4]);
  result:=move;
end;
//Transform collum letter to number
function TwinboardConn.letterToNumber(num:String) : Integer;
begin
  case num[1] of
    'a' : result:=1;
    'b' : result:=2;
    'c' : result:=3;
    'd' : result:=4;
    'e' : result:=5;
    'f' : result:=6;
    'g' : result:=7;
    'h' : result:=8;
    else result :=0;
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
  p := pos(engineMoveCommand[engineMoveComIndex][2],outputText);

  if p>=0 then
  begin
    p := p+length(engineMoveCommand[engineMoveComIndex][2]);
    while outputText[p]<>#10 do
    begin
      move:= move+outputText[p];
      inc(p);
    end;
  end;
  result:=move;
  outputText:='';
end;

end.
