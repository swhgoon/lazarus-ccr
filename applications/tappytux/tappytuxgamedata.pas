unit GameData;

{$mode objfpc}{$H}

interface

uses
  Classes, Forms, SysUtils, process, SynRegExpr, 
  LCLPRoc, tappywords, util, tappytuxconfig;
  
Type
    TTappyGameData = object
              SndFX : Boolean;
              SndMusic: Boolean;
              ModuleName : String;
              Option : String;
              Level : Integer;
              NextLevel : Integer;
              NextLife : Integer;
              Speed : Integer;
              Score : Integer;
              Lives : Integer;
              SongList : TStringList;
              QuestionList :TStringList;
              Procedure Create;
              Function GetQuestion:String;
              Function CheckAnswer(Question,Answer:String):Integer;
              Procedure ScoreUp(ScorInc:Integer);
              Procedure LevelUp;
              Procedure LoseLife;
              Function NextSong: String;
              BGS : TStringList;
              BG : Integer;
              Function NextBG:String;
    end;
    
Type HammerQue = Object
               Target : Array [1..10] of Integer;
               Function addTarget(newTarget : Integer):Boolean;
               Procedure delTarget;
               Count : Integer;
 end;
 

   TSong = class(TThread)
   protected
     procedure Execute; override;
   public
     Constructor Create(isSuspended : boolean);
   end;
   
    TQuestion = class(TThread)
   private
     S : TStringList;
   protected
     procedure Execute; override;
   public
     Constructor Create(isSuspended : boolean);
   published
     property terminated;
   end;

Var
    ThisGame: TTappyGameData;

    Question : TQuestion;
    Scale : Integer;
    TPTDIR: string;

implementation

constructor TQuestion.Create(isSuspended : boolean);
 begin
   S := TSTringlist.Create;
   FreeOnTerminate := True;
   inherited Create(isSuspended);
 end;

Procedure TQuestion.Execute;
Var CMD : String;
  PS : TProcess;
  TheWord : String;
Begin
repeat
If (Not Terminated) and
(ThisGame.QuestionList.Count < 20) and
(length(ThisGame.ModuleName) > 0) and
(Length(ThisGame.Option) > 0) then
Begin
if pos('tappywords',ThisGame.ModuleName) <> 0 then
Begin
     TheWord :=GetQuestion(ThisGame.Level);
   If ThisGame.QuestionList.IndexOf(TheWord) = -1 then
          ThisGame.QuestionList.Add(TheWord);
end else
Begin
 S.Clear;
 Ps := TProcess.create(nil);;
     CMD := ThisGame.ModuleName' "'ThisGAme.Option'" 'intToStr(ThisGame.Level)' --getquestion';
 PS.CommandLine :=  cmd;
  Ps.Options := [poNoConsole,poUsePipes,poWaitOnExit];
  Ps.Execute;
   S.LoadFromStream(PS.OutPut);
   PS.Free;
   If ThisGame.QuestionList.IndexOf(S[0]) = -1 then
          ThisGame.QuestionList.Add(S[0]);
end;
end;
 until Terminated;
S.Free;
end;

constructor TSong.Create(isSuspended : boolean);
begin
  FreeOnTerminate := True;
  inherited Create(isSuspended);
end;
 
Procedure TSong.Execute;
var
  Process: TProcess;
begin
  {To prevent ESD clashes - we slow this down on first run}
  sleep(5000);
  with ThisGame do
  begin
    Process := TProcess.create(nil);
    while (NextSong <> 'NONE') and (not Terminated) do
    begin
{$IFDEF Linux}
      Process.CommandLine := 'ogg123 -d esd "'NextSong'"' ;
      Process.Options := [poNoConsole,poWaitOnExit];
      Process.Execute;
{$ENDIF}
{$IFDEF Win32}
      sleep(5000);
{$ENDIF}
    end;

    Process.Free;
    SNDMusic := False;
  end;
end;

procedure TTappyGameData.Create;
begin
  BG := 0;
  
  if not (NextLevel > 0) then NextLevel := 100;
  
  if not (NextLife > 0) then NextLife := 325;
  
  if not(Score > 0) then Score := 0;
  
  Lives := 5;
  SearchFiles(SongList,TPTDirpathdelim'music'pathdelim,'*.ogg','');
  
  If Scale = 640 then
   SearchFiles(BGS,TPTDirpathdelim'levels','*.jpg','');
  
  If Scale = 800 then
   SearchFiles(BGS,TPTDirpathdelim'levels'pathdelim'800'pathdelim,'*.jpg','');
  
  If scale = 1024 then
   SearchFiles(BGS,TPTDirpathdelim'levels'pathdelim'1024'pathdelim,'*.jpg','');
end;

Function TTappyGameData.GetQuestion:String;
Var
TheQ:String;
Begin
While QuestionList.Count < 1 do
      sleep (100);
     TheQ := QuestionList[0];
     GetQuestion := TheQ;
     QuestionList.Delete(0);
end;



Function TTappyGameData.CheckAnswer(Question,Answer:String):Integer;
Var S: TStringList;
Begin
if (length(Question) <> 0) and (length(Answer) <> 0) then
begin
If ThisGame.ModuleName <> 'tappywords' then
begin
try
     execute(ModuleName' "'Option'" 'intToStr(Level)' --checkquestion "'Question'" "'answer'"',S);
     CheckAnswer := StrToInt(S[0]);
except
     CheckAnswer := 0;
end;
end else
  CheckAnswer := CheckQuestion(Question,Answer)
end else
    CheckAnswer := -1;
end;


Procedure TTappyGameData.LevelUp;
Var I : Integer;
Begin
For I := (QuestionList.Count - 1) downto 5 do
 QuestionList.Delete(I);
SchroedingersCat := True;
Inc(Level);
NextLevel := NextLevel  100;
End;


Procedure TTappyGameData.ScoreUp(ScorInc:Integer);
Begin
   If (Score  ScorInc > NextLevel) and (Score > NextLevel) then
           LevelUp;
     Score := Score  ScorInc;
End;


Procedure TTappyGameData.LoseLife;
Begin
Dec(Lives);
End;

Function TTappyGameData.NextSong: String;
Var SongNum : Integer;
Begin
if SongList.Count > 0 then
begin
     SongNum := Random(songList.Count -1);
     NextSong:=SongList[SongNum];
end else
    NextSong := 'NONE';
end;

Function TTappyGameData.NextBG: String;
Begin
     If BG  1 = BGS.Count then BG := 0 else
      inc(BG);
     NextBG:=BGS[BG];
end;




Function HammerQue.addTarget(newTarget : Integer):Boolean;
Var I : Integer;
    New : Boolean;
    
begin
New := True;
for I := 1 to Count do
begin
if (Target[I] = NewTarget) then
   New := False;
end;
If New then
begin
    Inc(Count);
    Target[Count] := NewTarget;
    AddTarget := True;
end else
    AddTarget := False;
end;

Procedure HammerQue.delTarget;
Var X : Integer;
Begin
    For X := 1 to Count do
        Target[X] := Target[X  1];
    Dec(Count);

end;

end.
