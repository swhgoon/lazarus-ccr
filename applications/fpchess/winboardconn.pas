{
Unit with the basics to communicate with engines that use the winboard protocol.
I used some code of the TProcess example also:
https://lazarus-ccr.svn.sourceforge.net/svnroot/lazarus-ccr/examples/process/

}
unit winboardConn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Process, StdCtrls, SorokinRegExpr, chessgame;

type
  moveInCoord = array[1..2] of TPoint;

  TWinboardConn = class
  public
    procedure startEngine(path : String);
    procedure stopEngine(freeProcess : boolean);
    destructor Destroy; override;
    procedure tellMove(move : String);
    function engineMove() : moveInCoord;
    function coordToString(AFrom, ATo : TPoint; pieceFrom, pieceTo : TChessTile) : String;
    function stringToCoord(moveStr : String) : moveInCoord;
  private
    procedure readFromPipe;
    function extractMove : string;
    function letterToNumber(num:String) : Integer;
    function numberToLetter(n : integer) : String;
    //procedure detectEngine(path : String);
  end;

var
  engineProcess : TProcess;
  outputText    : String;
  vwinboardConn : TWinboardConn;
  algebraicInput: boolean;
  engineRegExpression: TRegExpr;

implementation

destructor TWinboardConn.Destroy;
begin
  stopEngine(True);

  Inherited Destroy;
end;

procedure TWinboardConn.startEngine(path : String);
var     extraCommands : String;
begin
  //stop engine if it is running
  stopEngine(False);

  //create TProcess and start the engine in xboard mode
  engineProcess:= TProcess.Create(nil);
  engineRegExpression:= TRegExpr.Create;
  engineProcess.CommandLine := path;

  engineProcess.Options := engineProcess.Options + [poUsePipes];
  engineProcess.Execute;
  extraCommands:='xboard'+#13+#10;
  EngineProcess.Input.Write(extraCommands[1], length(extraCommands));
  extraCommands:='level 60 0.5 3'+#13+#10;
  EngineProcess.Input.Write(extraCommands[1], length(extraCommands));
  outputText:='';

end;

procedure TWinboardConn.stopEngine(freeProcess : boolean);
begin
  if engineProcess<>nil then
    if engineProcess.Running then
    begin
      engineProcess.Input.WriteAnsiString('quit'+#13+#10);
      engineProcess.Terminate(0);
    end;
  if freeProcess then
  begin
    engineProcess.free;
  end;
end;

procedure TWinboardConn.tellMove(move : String);
begin
  move := move+#13+#10;
  EngineProcess.Input.Write(move[1], length(move));
end;

//return the engine move.
function TWinboardConn.engineMove : moveInCoord;
var move  : String;
    points: moveInCoord;
begin
  engineRegExpression.Expression:='(?m)^(My move|my move|move)( is|)(: | : | )';
  readFromPipe;
  if engineRegExpression.Exec(outputText) then
  begin
    move := extractMove;
    points := stringToCoord(move);
    result:=points;
  end;
end;

function TWinboardConn.coordToString(AFrom, ATo : TPoint; pieceFrom, pieceTo : TChessTile) : String;
var
  move : String;
begin

  move:= move + numberToLetter(AFrom.X);
  move:= move + intToStr(AFrom.Y);

  move:= move + numberToLetter(ATo.X);
  move:= move + IntToStr(ATo.Y);
  result:=move;

end;

function  TWinboardConn.numberToLetter(n : integer) : String;
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

function TWinboardConn.stringToCoord(moveStr : String) : moveInCoord;
var move : moveInCoord;
begin
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
function TWinboardConn.letterToNumber(num:String) : Integer;
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

//read all the output text from the TProcess pipe (basically copy/pasted from the
//TProcess example)
procedure TWinboardConn.readFromPipe;
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

function TWinboardConn.extractMove : string;
var
    initialPos    : integer;
begin
  //delete the text from the start of the engine output
  engineRegExpression.Expression:='.*(My move|my move|move)( is|)(: | : | )';
  engineRegExpression.Exec(outputText);
  initialPos := pos(engineRegExpression.Match[0],outputText);

  Delete(outputText,initialPos,Length(engineRegExpression.Match[0]));

  //if there's text after the engine move, delete it too
  engineRegExpression.Expression:=' .+';
  if (engineRegExpression.Exec(outputText)) then
  begin
    initialPos := pos(engineRegExpression.Match[0],outputText);
    Delete(outputText,initialPos,Length(engineRegExpression.Match[0]));
  end;

  result:= outputText;

  outputText:='';
end;

end.
