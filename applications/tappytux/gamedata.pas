unit GameData;

{$mode objfpc}{$H+}

interface
+
+uses
+  Classes, Forms, SysUtils, process, SynRegExpr, LCLPRoc, tappywords, util
+  {$IFDEF Linux}
+  ,oldlinux
+  {$ENDIF}
+  ;
+  
+Type
+    TTappyGameData = object
+              SndFX : Boolean;
+              SndMusic: Boolean;
+              ModuleName : String;
+              Option : String;
+              Level : Integer;
+              NextLevel : Integer;
+              NextLife : Integer;
+              Speed : Integer;
+              Score : Integer;
+              Lives : Integer;
+              SongList : TStringList;
+              QuestionList :TStringList;
+              Procedure Create;
+              Function GetQuestion:String;
+              Function CheckAnswer(Question,Answer:String):Integer;
+              Procedure ScoreUp(ScorInc:Integer);
+              Procedure LevelUp;
+              Procedure LoseLife;
+              Function NextSong: String;
+              BGS : TStringList;
+              BG : Integer;
+              Function NextBG:String;
+    end;
+    
+Type HammerQue = Object
+               Target : Array [1..10] of Integer;
+               Function addTarget(newTarget : Integer):Boolean;
+               Procedure delTarget;
+               Count : Integer;
+ end;
+ 
+
+   TSong = class(TThread)
+   protected
+     procedure Execute; override;
+   public
+     Constructor Create(isSuspended : boolean);
+   end;
+   
+    TQuestion = class(TThread)
+   private
+     S : TStringList;
+   protected
+     procedure Execute; override;
+   public
+     Constructor Create(isSuspended : boolean);
+   published
+     property terminated;
+   end;
+
+Var
+    ThisGame: TTappyGameData;

+    Question : TQuestion;
+    Scale : Integer;
+    TPTDIR: string;
+
+implementation
+
+constructor TQuestion.Create(isSuspended : boolean);
+ begin
+   S := TSTringlist.Create;
+   FreeOnTerminate := True;
+   inherited Create(isSuspended);
+ end;
+
+Procedure TQuestion.Execute;
+Var CMD : String;
+  PS : TProcess;
+  TheWord : String;
+Begin
+repeat
+If (Not Terminated) and
+(ThisGame.QuestionList.Count < 20) and
+(length(ThisGame.ModuleName) > 0) and
+(Length(ThisGame.Option) > 0) then
+Begin
+if pos('tappywords',ThisGame.ModuleName) <> 0 then
+Begin
+     TheWord :=GetQuestion(ThisGame.Level);
+   If ThisGame.QuestionList.IndexOf(TheWord) = -1 then
+          ThisGame.QuestionList.Add(TheWord);
+end else
+Begin
+ S.Clear;
+ Ps := TProcess.create(nil);;
+     CMD := ThisGame.ModuleName+' "'+ThisGAme.Option+'" '+intToStr(ThisGame.Level)+' --getquestion';
+ PS.CommandLine :=  cmd;
+  Ps.Options := [poNoConsole,poUsePipes,poWaitOnExit];
+  Ps.Execute;
+   S.LoadFromStream(PS.OutPut);
+   PS.Free;
+   If ThisGame.QuestionList.IndexOf(S[0]) = -1 then
+          ThisGame.QuestionList.Add(S[0]);
+end;
+end;
+ until Terminated;
+S.Free;
+end;
+
+constructor TSong.Create(isSuspended : boolean);
+begin
+  FreeOnTerminate := True;
+  inherited Create(isSuspended);
+end;
+ 
+Procedure TSong.Execute;
+var
+  Process: TProcess;
+begin
+  {To prevent ESD clashes - we slow this down on first run}
+  sleep(5000);
+  with ThisGame do
+  begin
+    Process := TProcess.create(nil);
+    while (NextSong <> 'NONE') and (not Terminated) do
+    begin
+{$IFDEF Linux}
+      Process.CommandLine := 'ogg123 -d esd "'+NextSong+'"' ;
+      Process.Options := [poNoConsole,poWaitOnExit];
+      Process.Execute;
+{$ENDIF}
+{$IFDEF Win32}
+      sleep(5000);
+{$ENDIF}
+    end;
+
+    Process.Free;
+    SNDMusic := False;
+  end;
+end;
+
+procedure TTappyGameData.Create;
+begin
+  BG := 0;
+  
+  if not (NextLevel > 0) then NextLevel := 100;
+  
+  if not (NextLife > 0) then NextLife := 325;
+  
+  if not(Score > 0) then Score := 0;
+  
+  Lives := 5;
+  SearchFiles(SongList,TPTDir+pathdelim+'music'+pathdelim,'*.ogg','');
+  
+  If Scale = 640 then
+   SearchFiles(BGS,TPTDir+pathdelim+'levels','*.jpg','');
+  
+  If Scale = 800 then
+   SearchFiles(BGS,TPTDir+pathdelim+'levels'+pathdelim+'800'+pathdelim,'*.jpg','');
+  
+  If scale = 1024 then
+   SearchFiles(BGS,TPTDir+pathdelim+'levels'+pathdelim+'1024'+pathdelim,'*.jpg','');
+end;
+
+Function TTappyGameData.GetQuestion:String;
+Var
+TheQ:String;
+Begin
+While QuestionList.Count < 1 do
+      sleep (100);
+     TheQ := QuestionList[0];
+     GetQuestion := TheQ;
+     QuestionList.Delete(0);
+end;
+
+
+
+Function TTappyGameData.CheckAnswer(Question,Answer:String):Integer;
+Var S: TStringList;
+Begin
+if (length(Question) <> 0) and (length(Answer) <> 0) then
+begin
+If ThisGame.ModuleName <> 'tappywords' then
+begin
+try
+     execute(ModuleName+' "'+Option+'" '+intToStr(Level)+' --checkquestion "'+Question+'" "'+answer+'"',S);
+     CheckAnswer := StrToInt(S[0]);
+except
+     CheckAnswer := 0;
+end;
+end else
+  CheckAnswer := CheckQuestion(Question,Answer)
+end else
+    CheckAnswer := -1;
+end;
+
+
+Procedure TTappyGameData.LevelUp;
+Var I : Integer;
+Begin
+For I := (QuestionList.Count - 1) downto 5 do
+ QuestionList.Delete(I);
+SchroedingersCat := True;
+Inc(Level);
+NextLevel := NextLevel + 100;
+End;
+
+
+Procedure TTappyGameData.ScoreUp(ScorInc:Integer);
+Begin
+   If (Score + ScorInc > NextLevel) and (Score > NextLevel) then
+           LevelUp;
+     Score := Score + ScorInc;
+End;
+
+
+Procedure TTappyGameData.LoseLife;
+Begin
+Dec(Lives);
+End;
+
+Function TTappyGameData.NextSong: String;
+Var SongNum : Integer;
+Begin
+if SongList.Count > 0 then
+begin
+     SongNum := Random(songList.Count -1);
+     NextSong:=SongList[SongNum];
+end else
+    NextSong := 'NONE';
+end;
+
+Function TTappyGameData.NextBG: String;
+Begin
+     If BG + 1 = BGS.Count then BG := 0 else
+      inc(BG);
+     NextBG:=BGS[BG];
+end;
+
+
+
+
+Function HammerQue.addTarget(newTarget : Integer):Boolean;
+Var I : Integer;
+    New : Boolean;
+    
+begin
+New := True;
+for I := 1 to Count do
+begin
+if (Target[I] = NewTarget) then
+   New := False;
+end;
+If New then
+begin
+    Inc(Count);
+    Target[Count] := NewTarget;
+    AddTarget := True;
+end else
+    AddTarget := False;
+end;
+
+Procedure HammerQue.delTarget;
+Var X : Integer;
+Begin
+    For X := 1 to Count do
+        Target[X] := Target[X + 1];
+    Dec(Count);
+
+end;
+
+initialization
+
+{$IFDEF Linux}
+  TPTDIR := '/usr/share/tappytux';
+{$ELSE}
+  TPTDIR := ExtractFileDir(Application.EXEName);
+{$ENDIF}
+
+end.
+
Index: tappytux.lpr
===================================================================
--- tappytux.lpr	(revision 67)
+++ tappytux.lpr	(working copy)
@@ -3,8 +3,8 @@
 {$mode objfpc}{$H+}
 
 uses
-{$IFDEF Linux}
-cthreads,
+{$IFDEF UNIX}
+  cthreads,
 {$ENDIF}
   Interfaces, // this includes the LCL widgetset
   Forms
@@ -14,9 +14,9 @@
   Application.Title:='TappyTux';
   Application.Initialize;
   Application.CreateForm(TForm1, Form1);
-    Application.CreateForm(TForm2, Form2);
-   Application.CreateForm(TForm3, Form3);
-   Application.CreateForm(TForm4, Form4);
-   Application.Run;
+  Application.CreateForm(TForm2, Form2);
+  Application.CreateForm(TForm3, Form3);
+  Application.CreateForm(TForm4, Form4);
+  Application.Run;
 end.
 
Index: unit1.pas
===================================================================
--- unit1.pas	(revision 67)
+++ unit1.pas	(working copy)
@@ -1,892 +1,854 @@
-unit unit1;
-
-{$mode objfpc}{$H+}
-
-interface
-
-uses
-  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,unit2,
-  doublebuffer, StdCtrls, ExtCtrls,  Buttons,Sprite,GameData
- ,unit3,unit4, screensize,util
-    {$IFDEF Linux}
-  ,oldlinux, esdsound
-  {$ENDIF}
-  {$IFDEF Win32}
-  ,Windows,ShellAPI
-  {$ENDIF}
-  ;
-
-type
-
-  { TForm1 }
-
-  TForm1 = class(TForm)
-    BitBtn1: TBitBtn;
-    BG: TDoubleBuffer;
-    Button1: TButton;
-    Edit1: TEdit;
-    Edit2: TEdit;
-    Edit3: TEdit;
-    Edit4: TEdit;
-    {$IFDEF Linux}
-    ESDSound1: TESDSound;
-    {$ENDIF}
-    Image1: TImage;
-    DanceTimer: TTimer;
-    Label1: TLabel;
-    ScreenSize1: TScreenSize;
-    SnowManTimer: TTimer;
-    ScreenUpdateTimer: TTimer;
-    HammerTimer: TTimer;
-    BoomTimer: TTimer;
-    SplashTimer: TTimer;
-    ThrowTimer: TTimer;
-    procedure BitBtn1Click(Sender: TObject);
-    procedure BoomTimerStartTimer(Sender: TObject);
-    procedure BoomTimerTimer(Sender: TObject);
-    procedure Button1Click(Sender: TObject);
-    procedure DanceTimerTimer(Sender: TObject);
-    procedure Edit1KeyPress(Sender: TObject; var Key: char);
-    procedure Edit2Enter(Sender: TObject);
-    procedure FormActivate(Sender: TObject);
-    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
-    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
-    procedure FormCreate(Sender: TObject);
-    procedure FormResize(Sender: TObject);
-    procedure FormShow(Sender: TObject);
-    procedure FormWindowStateChange(Sender: TObject);
-    procedure HammerTimerTimer(Sender: TObject);
-    procedure ScreenUpdateTimerTimer(Sender: TObject);
-    procedure SnowManTimerTimer(Sender: TObject);
-    procedure SplashTimerTimer(Sender: TObject);
-    procedure ThrowTimerStartTimer(Sender: TObject);
-    procedure ThrowTimerTimer(Sender: TObject);
-  private
-    { private declarations }
-  public
-    { public declarations }
-  end; 
-
-
-
-var
-
-  Music : TSong;
-  Form1: TForm1; 
-    SnowMan : Array[1..5] of TSprite;
-    NextQuestion : Array[1..5] Of String;
-    Hammer : HammerQue;
-   HammerPic : Tsprite;
-   Boom : TSprite;
-   Splash:TSprite;
-   DancingTux : TSprite;
-   HurtTux: TSprite;
-   ThrowTux: TSprite;
-   GameOver : Boolean;
-
-implementation
-
-{ TForm1 }
-
-
-Procedure Play(Name : String);
-Begin
-{$IFDEF Linux}
-If ThisGame.SNDFX then
- Form1.esdSound1.Play(Name);
-{$ENDIF}
-end;
-
-
-Procedure ThrowHammer(GoLeft:Boolean);
-Begin
-With Form1 do
-begin
-If GoLeft then
-begin
-   ThrowTux.Frame := 0;
-   HammerPic.X := ThrowTux.X - HammerPic.FrameWidth;
-   HammerPic.Y := ThrowTux.Y;
-end else
-begin
-   ThrowTux.Frame := 1;
-   HammerPic.X := ThrowTux.X + ThrowTux.FrameWidth;
-   HammerPic.Y := ThrowTux.Y;
-end;
-ThrowTimer.Enabled := True;
-end;
-end;
-
-Function textY(I:Integer):Integer;
-Begin
-TextY := SnowMan[I].Y - 35;
-end;
-
-//Why larry why is this different between platforms ?
-{$IFDEF Win32}
-function MyRect(X,Y,A,B:Integer):Rect;
-Var Z : Rect;
-Begin
- Z.Top := Y;
- Z.Left := X;
- Z.Right := A;
- Z.Bottom := B;
- MyRect := Z;
-end;
-{$ENDIF}
-
-procedure textMask(I :Integer);
-Begin
-With Form1 do
-begin
-{$IFDEF Win32}
-BG.MemBuff.Canvas.CopyRect(MyRect(SnowMan[I].X +1,textY(I),SnowMan[I].X + 99,textY(I)+35),
- BG.BackGround.BitMap.Canvas,MyRect(SnowMan[I].X +1,textY(I),SnowMan[I].X + 99,textY(I)+35));
-{$ENDIF}
-{$IFDEF Linux}
-BG.MemBuff.Canvas.CopyRect(Rect(SnowMan[I].X +1,textY(I),SnowMan[I].X + 99,textY(I)+35),
- BG.BackGround.BitMap.Canvas,Rect(SnowMan[I].X +1,textY(I),SnowMan[I].X + 99,textY(I)+35));
-{$ENDIF}
-
-end;
-end;
-
-
-procedure updateScoreboard;
-Begin
-With Form1 do
-Begin
-     Edit2.Text := intToStr(ThisGame.Level);
-     Edit3.Text := intToStr(ThisGame.Score);
-     Edit4.Text := IntToStr(ThisGame.Lives);
-     If ThisGame.Lives <=2 then
-        Form1.Caption := 'TappyTux - Warning: '+IntToStr(ThisGame.Lives)+' Left';
-
-end;
-end;
-
-procedure SnowQuestion(I:Integer);
-Var X : Integer;
-Unique : Boolean;
-begin
-      repeat
-            NextQuestion[I] := ThisGame.GetQuestion;
-            Unique := True;
-            for X := 1 to 5 do
-            if (length(NextQuestion[X]) <> 0) and (X <> I) then
-              if (thisgame.CheckAnswer(NextQuestion[X],NextQuestion[I]) <> 0) then
-                  Unique := False;
-      until Unique;
-end;
-
-Procedure InitSnowMen;
-Var I,X : integer;
-    Unique: Boolean;
-Begin
-
-  For I := 1 to 5 do
-      SnowQuestion(I);
-
-  for I := 1 to 5 do
-      repeat
-            Unique := True;
-            for X := 1 to 5 do
-            SnowMan[I].Y := 50 - (random(3000) div 10);
-            if (SnowMan[I].Y = SnowMan[X].Y) And (I <> X) then
-                  Unique := False;
-      until Unique;
-end;
-
-Procedure LoadSnowMen;
-Var I,X,Y : integer;
-Begin
-Case Scale of
-640:Y := 100;
-800:Y := 130;
-1024:Y := 180;
-end;
-For I  := 1 to random(30) do
-     X := random(100);
-  For I := 5 downto 1 do
-  begin
-       SnowMan[I] := TSprite.Create(nil);
-      {$IFDEF Linux}
-      SnowMan[I].loadFromFile('/usr/share/tappytux/sprites/snowmen.xpm');
-      {$ENDIF}
-      {$IFDEF Win32}
-      SnowMan[I].loadFromFile('c:\program files\tappytux\sprites\snowmen.xpm');
-      {$ENDIF}
-
-      SnowMan[I].FrameWidth := 94;
-      SnowMan[I].Frame := I -1;
-      SnowMan[I].X := 10+((I -1) * Y);
-
-  end;
-InitSnowMen;
-end;
-
-Procedure BlitSnowMen;
-Var I : Integer;
-Begin
-     For I := 5 downto 1 do
-     With form1 do
-     begin
-          BG.Mask(SnowMan[I]);
-          BG.Blit(SnowMan[I]);
-     end;
-end;
-
-
-procedure TForm1.BitBtn1Click(Sender: TObject);
-Var
-   Browser : String;
-begin
-{$IFDEF Linux}
-Browser := 'none';
-if shell('if which mozilla ; then exit 0 ; else exit 1 ; fi') = 0 then
-   Browser := 'mozilla';
-if shell('if which konqueror ; then exit 0 ; else exit 1 ; fi') = 0 then
-   Browser := 'konqueror';
-if shell('if which opera ; then exit 0 ; else exit 1 ; fi') = 0 then
-   Browser := 'opera';
-if shell('if which firefox ; then exit 0 ; else exit 1 ; fi') = 0 then
-   Browser := 'firefox';
-Writeln(browser);
-if browser <> 'none' then
-   shell(browser+' "http://www.getopenlab.com"&');
-{$ENDIF}
-{$IFDEF Win32}
-    ShellExecute(Handle,'open','http://www.getopenlab.com', nil, nil, SW_SHOWNORMAL);
-{$ENDIF}
-
-end;
-
-procedure TForm1.BoomTimerStartTimer(Sender: TObject);
-begin
-HammerTimer.Enabled := False;
-  SnowManTimer.Enabled := True;
-    Play('hit');
-     Boom.X := SnowMan[Hammer.Target[1]].X - 20;
-     Boom.Y := SnowMan[Hammer.Target[1]].Y - 20;
-             BG.Mask(SnowMan[Hammer.Target[1]]);
-             BG.Mask(HammerPic);
-
-               SnowMan[Hammer.Target[1]].Y := -150;
-     BG.Blit(Boom);
-   //  BG.Flip;
-
-end;
-
-procedure TForm1.BoomTimerTimer(Sender: TObject);
-
-begin
-
-     BG.Mask(Boom);
- SnowQuestion(Hammer.Target[1]);
-     BoomTimer.Enabled := False;
-                  Hammer.DelTarget;
-                  If Hammer.Count <> 0 then
-             begin
-                If SnowMan[Hammer.Target[1]].X <= DancingTux.X then
-                   ThrowHammer(True) else
-                   ThrowHammer(False);
-             end;
-               BG.Mask(HammerPic);
-SnowManTimer.Enabled := True;
-     BG.Mask(HammerPic);
-     HammerTimer.Enabled := True;
-end;
-
-Procedure PauseGame;
-Begin
-With Form1 do
-begin
-  HammerTimer.Enabled := False;
-  SnowManTimer.Enabled := False;
-  Button1.Caption := 'Play';
-  Form1.Caption := 'TappyTux - PAUSED';
-end;
-end;
-
-Procedure UnPauseGame;
-Begin
-With Form1 do
-begin
-  Edit1.SetFocus;
-  HammerTimer.Enabled := True;
-  SnowManTimer.Enabled := True;
-  Button1.Caption := 'Pause';
-    Form1.Caption := 'TappyTux';
-end;
-end;
-
-
-procedure TForm1.Button1Click(Sender: TObject);
-begin
-If Button1.Caption = 'Pause' then
-PauseGame
-else
-UnPauseGame;
-end;
-
-
-
-procedure TForm1.DanceTimerTimer(Sender: TObject);
-begin
- if DancingTux.Frame = 0 then
-  DancingTux.Frame := 1 else
-  DancingTux.Frame := 0;
-
-BG.Mask(DancingTux);
-BG.Blit(Dancingtux);
-{BG.Flip;  }
-end;
-
-
-
-procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: char);
-Var I,Y : Integer;
-    Gotcha : Boolean;
-begin
- Edit1.Font.Color := ClBlack;
-  If Key = #13 then
-  begin
-    Gotcha := False;
-    For I := 1 to 5 do
-     begin
-         Y :=  ThisGame.CheckAnswer(NextQuestion[I],Edit1.Text);
-        If (Y > 0) and (Hammer.AddTarget(I)) then
-        begin
-             Gotcha := True;
-             If (ThisGame.Score + Y > ThisGame.NextLevel) and
-              (ThisGame.Score > ThisGame.NextLevel)
-               then
-             Begin
-                  Play('levelup');
-                  BG.LoadFromFile(ThisGame.NextBG);
-                    For I := 1 to 5 do
-                      begin
-                           BG.Mask(SnowMan[I]);
-                           BG.Blit(SnowMan[I]);
-                           BG.Mask(ThrowTux);
-                      end;
-                      BG.Blit(DancingTux);
-            end;
-             ThisGame.ScoreUp(Y);
-             If thisgame.Score > ThisGame.NextLife then
-             begin
-                  play ('life');
-                  ThisGame.NextLife := ThisGame.NextLife + 300;
-                  inc(ThisGame.Lives);
-             end else
-             play ('match');
-             updateScoreBoard;
-             Edit1.Text := '';
-             If Hammer.Count = 1 then
-             begin
-                If SnowMan[I].X <= DancingTux.X then
-                   ThrowHammer(True) else
-                   ThrowHammer(False);
-             end;
-        end;
-     end;
-       if Not Gotcha then
-       begin
-           play('error');
-           edit1.Font.Color := ClRed;
-       end;
-     end;
-end;
-
-
-
-
-
-procedure TForm1.Edit2Enter(Sender: TObject);
-begin
-  Edit1.SetFocus;
-end;
-
-procedure TForm1.FormActivate(Sender: TObject);
-begin
-  Edit1.SetFocus;
-end;
-
-procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
-Var
-I : Integer;
-begin
-  ThisGame.Level := 1;
-  ThisGame.Score := 0;
-  ThisGame.Lives := 5;
-  SnowManTimer.Enabled := False;
-  ScreenUpdateTimer.Enabled := False;
-  HammerTimer.Enabled := FAlse;
-  BoomTimer.Enabled := False;
-  SplashTimer.Enabled := FAlse;
-  ThrowTimer.Enabled := False;
-If Music <> Nil then
-begin
- Music.Terminate;
- execute ('killall -9 ogg123');
-end;
-
-   DanceTimer.Enabled := False;
-   if ThisGame.SndMusic then
-    For I := 5 downto 1 do
-        SnowMan[I].Free;
-try
-If Music <> Nil then
-begin
-  Music.Free;
-    BG.Free;
-end;
-  ThisGame.QuestionList.Free;
-except
-  writeln ('Exiting');
-for I := 1 to 5 do
- SnowMan[I].free;
-  DancingTux.free;
-   HurtTux.free;
-   ThrowTux.free;
-   HammerPic.free;
-   Boom.free;
-   Splash.free;
-  BG.Free;
-Application.Terminate;
-end;
-end;
-
-procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
-begin
-If Not GameOver then
-begin
-PauseGame;
-  Form3.ShowModal;
-  If Form3.ModalResult = MrYes then
-       CanClose := True
-  else
-      Begin
-      CanClose := False;
-      UnPauseGame;
-      end;
-end;
-end;
-
-Procedure InitSprites;
-Begin
-With form1 do
-begin
-   DancingTux := TSprite.Create(nil);
-      {$IFDEF Linux}
-      DancingTux.LoadFromFile('/usr/share/tappytux/sprites/tuxfront.xpm');
-      {$ENDIF}
-      {$IFDEF Win32}
-      DancingTux.loadFromFile('c:\program files\tappytux\sprites\tuxfront.xpm');
-      {$ENDIF}
-   DancingTux.X := (BG.Width div 2) - (DancingTux.Width div 2);
-   DancingTux.Y := BG.Height - DancingTux.Height;
-   DancingTux.Visible := True;
-   DancingTux.Frame := 0;
-   DancingTux.FrameWidth := 54;
-
-   HurtTux := TSprite.Create(nil);
-      {$IFDEF Linux}
-      HurtTux.LoadFromFile('/usr/share/tappytux/sprites/hurt.xpm');
-      {$ENDIF}
-      {$IFDEF Win32}
-      HurtTux.loadFromFile('c:\program files\tappytux\sprites\hurt.xpm');
-      {$ENDIF}
-
-   HurtTux.X := (BG.Width div 2) - (HurtTux.Width div 2);
-   HurtTux.Y := Edit1.Top - DancingTux.Height;
-   HurtTux.Visible := True;
-   HurtTux.Frame := 0;
-   HurtTux.FrameWidth := 45;
-
-   ThrowTux := TSprite.Create(nil);
-      {$IFDEF Linux}
-      ThrowTux.LoadFromFile('/usr/share/tappytux/sprites/tuxside.xpm');
-      {$ENDIF}
-      {$IFDEF Win32}
-      ThrowTux.loadFromFile('c:\program files\tappytux\sprites\tuxside.xpm');
-      {$ENDIF}
-   ThrowTux.X := (BG.Width div 2) - (ThrowTux.Width div 2);
-   ThrowTux.Y := Edit1.Top - DancingTux.Height;
-   ThrowTux.Visible := True;
-   ThrowTux.Frame := 0;
-   ThrowTux.FrameWidth := 58;
-
-
-   HammerPic := TSprite.Create(nil);
-      {$IFDEF Linux}
-      HammerPic.LoadFromFile('/usr/share/tappytux/sprites/hammer.xpm');
-      {$ENDIF}
-      {$IFDEF Win32}
-      HammerPic.loadFromFile('c:\program files\tappytux\sprites\hammer.xpm');
-      {$ENDIF}
-   HammerPic.X := DancingTux.X - 30;
-   HammerPic.Y := DancingTux.Y;
-   HammerPic.Visible := False;
-   HammerPic.Frame := 0;
-   HammerPic.FrameWidth := 41;
-
-   Boom := TSprite.Create(nil);
-      {$IFDEF Linux}
-      Boom.LoadFromFile('/usr/share/tappytux/sprites/crash.xpm');
-      {$ENDIF}
-      {$IFDEF Win32}
-      Boom.loadFromFile('c:\program files\tappytux\sprites\crash.xpm');
-      {$ENDIF}
-   Boom.X := DancingTux.X - 30;
-   Boom.Y := DancingTux.Y;
-   Boom.Visible := True;
-   Boom.Frame := 0;
-   Boom.FrameWidth := 208;
-   
-   Splash := TSprite.Create(nil);
-      {$IFDEF Linux}
-      Splash.LoadFromFile('/usr/share/tappytux/sprites/splash.xpm');
-      {$ENDIF}
-      {$IFDEF Win32}
-      Splash.loadFromFile('c:\program files\tappytux\sprites\splash.xpm');
-      {$ENDIF}
-   Splash.X := DancingTux.X - 30;
-   Splash.Y := BG.Height - DancingTux.Height;
-   Splash.Visible := True;
-   Splash.Frame := 0;
-   Splash.FrameWidth := 198;
-
-end;
-end;
-
-procedure startGame;
-Begin
-With Form1 Do
-begin
-   ThisGame.Create;
-   LoadSnowMen;
-   SnowManTimer.Enabled := True;
-   HammerTimer.Enabled := True;
-   BlitSnowMen;
-   UpdateScoreBoard;
-End;
-end;
-
-
-procedure TForm1.FormCreate(Sender: TObject);
-
-begin
-{$IFDEF Linux}
-// EsdSound1.Enabled := True;
-{$ENDIF}
-ThisGame.QuestionList := TStringList.Create;
-randomize;
-          SchroedingersCat := False;
-ScreenSize1.GetScreenSize;
-If (ScreenSize1.X = 640) or (paramcount <> 0) then
-begin
-      {$IFDEF Linux}
-      BG.LoadFromFile('/usr/share/tappytux/levels/levelp.jpg');
-      {$ENDIF}
-      {$IFDEF Win32}
-      BG.LoadFromFile('c:\program files\tappytux\levels\levelp.jpg');
-      {$ENDIF}
-   Scale := 640;
-   Form1.Width := BG.Width + Image1.Width;
-   Form1.Height := BG.Height + Edit1.Height;
-   SnowManTimer.Interval := 200;
-   HammerTimer.Interval := 180;
-end;
-if paramcount = 0 then
-begin
-If ScreenSize1.X = 800 then
-Begin
-   Scale := 800;
-      {$IFDEF Linux}
-      BG.LoadFromFile('/usr/share/tappytux/levels/800/levelp.jpg');
-      {$ENDIF}
-      {$IFDEF Win32}
-      BG.LoadFromFile('c:\program files\tappytux\levels\800\levelp.jpg');
-      {$ENDIF}
-   Form1.Width := BG.Width + Image1.Width;
-   Form1.Height := BG.Height + Edit1.Height;
-   SnowManTimer.Interval := 150;
-   HammerTimer.Interval := 100;
-end;
-If ScreenSize1.X >= 1024 then
-Begin
-   Scale := 1024;
-      {$IFDEF Linux}
-      BG.LoadFromFile('/usr/share/tappytux/levels/1024/levelp.jpg');
-      {$ENDIF}
-      {$IFDEF Win32}
-      BG.LoadFromFile('c:\program files\tappytux\levels\1024\levelp.jpg');
-      {$ENDIF}
-   Form1.Width := BG.Width + Image1.Width;
-   Form1.Height := BG.Height + Edit1.Height;
-   SnowManTimer.Interval := 100;
-   HammerTimer.Interval := 80;
-End;
-end;
-   Hammer.Count := 0;
-   InitSprites;
-end;
-
-procedure TForm1.FormResize(Sender: TObject);
-begin
-     Form1.Width := BG.Width + Image1.Width;
-   Form1.Height := BG.Height + Edit1.Height;
-
-end;
-
-procedure TForm1.FormShow(Sender: TObject);
-Var
-   BTNFont : TFont;
-begin
-  BTNFont := TFont.Create;
-    BTNFont.Color := ClPurple;
-    BTNFont.Size := 47;
-   BTNFont.Name := 'TeachersPet';
-    BTNFont.Style := [FSBold];
-    Edit1.Font.Assign(BTNFont);
-
-GameOver := False;
-edit1.SetFocus;
- Form2.ShowModal;
- if not DoLoad then
-     Application.Terminate
- else
- Begin
- {$IFDEF Linux}
-  Play('/usr/share/tappytux/sounds/startup.wav');
- {$ENDIF}
-     if ThisGame.SndMusic then
-   begin
-         Music := TSong.Create(false);
-    if Assigned(Music.FatalException) then
-      raise Music.FatalException;
-      Music.Resume;
-   end;
-  {$IFDEF Linux}
-  ESDSound1.CacheSample('/usr/share/tappytux/sounds/gameover.wav','gameover');
-  ESDSound1.CacheSample('/usr/share/tappytux/sounds/level_up.wav','levelup');
-  ESDSound1.CacheSample('/usr/share/tappytux/sounds/missed_word.wav','missed');
-  ESDSound1.CacheSample('/usr/share/tappytux/sounds/ready.wav','ready');
-  ESDSound1.CacheSample('/usr/share/tappytux/sounds/word_hit.wav','hit');
-  ESDSound1.CacheSample('/usr/share/tappytux/sounds/word_error.wav','error');
-  ESDSound1.CacheSample('/usr/share/tappytux/sounds/word_match.wav','match');
-  ESDSound1.CacheSample('/usr/share/tappytux/sounds/life.wav','life');
-  {$ENDIF}
-  Edit1.SetFocus;
- StartGame;
- end;
-end;
-
-procedure TForm1.FormWindowStateChange(Sender: TObject);
-begin
-  Edit1.SetFocus;
-end;
-
-
-procedure TForm1.HammerTimerTimer(Sender: TObject);
-Var xdif,ydif : Integer;
-    GoLeft: Boolean;
-
-Procedure HammerLeft;
-Begin
-        if HammerPic.Frame <> 0 then
-           HammerPic.Frame := 0 else
-           HammerPic.Frame := 1;
-end;
-
-Procedure HammerRight;
-Begin
-    if HammerPic.Frame <> 2 then
-           HammerPic.Frame := 2 else
-           HammerPic.Frame := 3;
-end;
-
-begin
-  If Hammer.Count <> 0 then
-  begin
-  HammerPic.VIsible := True;
-
-     BG.Mask(HammerPic);
-     if HammerPic.X > SnowMan[Hammer.Target[1]].X + 30 then
-     begin
-        GoLeft := True;
-        HammerPic.X := HammerPic.X - 20;
-      end;
-     if HammerPic.X < SnowMan[Hammer.Target[1]].X + 30 then
-     begin
-        GoLeft := False;
-        HammerPic.X := HammerPic.X + 20;
-
-     end;
-     If GoLeft then HammerLeft else HammerRight;
-
-     if HammerPic.Y < SnowMan[Hammer.Target[1]].Y + 50 then
-          HammerPic.Y := HammerPic.Y + 20;
-     if HammerPic.Y > SnowMan[Hammer.Target[1]].Y + 50 then
-          HammerPic.Y := HammerPic.Y -20;
-     BG.Blit(HammerPic);
-
-     xdif := abs(SnowMan[Hammer.Target[1]].X +30 - HammerPic.X);
-     ydif := abs(SnowMan[Hammer.Target[1]].Y +50- HammerPic.Y);
-     If (xdif < 20) and (ydif < 50)
-     then
-     begin
-     snowManTimer.Enabled := False;
-     BoomTimer.Enabled := True;
-     end;
-
-  end else
-  HammerPic.Visible := False;
-end;
-
-procedure TForm1.ScreenUpdateTimerTimer(Sender: TObject);
-Var I : Integer;
-begin
-  bg.flip;
-end;
-
-Function SnowColor(X : Integer):Integer;
-Begin
-  Case X of
-  1: SnowColor := ClRed;
-  2: SnowColor := ClBlue;
-  3: SnowColor := ClPurple;
-  4: SnowColor := ClGreen;
-  5: SnowColor := ClOlive;
- end;
-end;
-
-procedure TForm1.SnowManTimerTimer(Sender: TObject);
-Var I,J : Integer;
-    Hit : Boolean;
-begin
-  for I := 5 downto 1 do
-  begin
-       TextMask(I);
-       BG.Mask(SnowMan[I]);
-       Hit := False;
-       for J := 0 to Hammer.Count do
-           If I = Hammer.Target[J] then Hit := True;
-
-       if Not Hit then
-       begin
-        SnowMan[I].Y := SnowMan[I].Y + (thisgame.Level div 4) +1;
-
-         With BG.MemBuff.Canvas do
-         begin
-             Brush.Style := BsSolid;
-              Brush.color := clWhite;
-              Font.Size := 28;
-              Font.Color := SnowColor(I);
-              Font.Name := 'TeachersPet';
-              {$IFDEF Win32}
-              FillRect(MyRect(SnowMan[I].X +1,TextY(I),SnowMan[I].X + 99,textY(I)+ 35));
-              {$ENDIF}
-              {$IFDEF Linux}
-              FillRect(Rect(SnowMan[I].X +1,TextY(I),SnowMan[I].X + 99,textY(I)+ 35));
-              {$ENDIF}
-              if Scale = 1024 then
-              TextOut(SnowMan[I].X +2,TextY(I),NextQuestion[I]) else
-              TextOut(SnowMan[I].X +2,TextY(I) + 3,NextQuestion[I]);
-         end;
-         end;
-  If SnowMan[I].Y >= BG.Height then
-  begin
-   DanceTimer.Enabled := False;
-   HammerTimer.Enabled:= false;
-   SnowManTimer.Enabled := False;
-   Play('missed');
-   
-      If SnowMan[I].X <= DancingTux.X then
-           HurtTux.Frame := 0 else
-           HurtTux.Frame := 1;
-      DanceTimer.Enabled := False;
-      BG.Mask(DancingTux);
-      textMask(I);
-      BG.Mask(SnowMan[I]);
-        SnowMan[I].Y := -150;
-      BG.Blit(HurtTux);
-
-      Splash.X := SnowMan[I].X - 50;
-      Splash.Y := BG.Height - Splash.Height;
-      If Splash.Frame = 0 then
-         Splash.Frame := 1 else
-         Splash.Frame := 0;
-      BG.Blit(Splash);
-    //  BG.Flip;
-      SplashTimer.Enabled := True;
-  end;
-  end;
-
-  BlitSnowMen;
-  
-end;
-
-
-
-procedure TForm1.SplashTimerTimer(Sender: TObject);
-begin
-  ThisGame.LoseLife;
-    BG.Load;
-  BG.Mask(Splash);
-  BG.Mask(HurtTux);
-  BG.Blit(DancingTux);
-  //BG.Flip;
-  DanceTimer.Enabled := True;
-    SnowManTimer.Enabled := True;
-    HammerTimer.Enabled := True;
-    SplashTimer.Enabled := False;
-    UpdateScoreBoard;
-  if StrToInt(Edit4.Text) <> ThisGame.Lives then
-  begin
-    Edit4.Text := IntToStr(ThisGame.Lives);
-    play ('life');
-  end;
-  If ThisGame.Lives = 0 then
-     begin
-         GameOver := True;
-         PauseGame;
-         Form4.ShowModal;
-         If Form4.ModalResult = MrYes then
-         begin
-            GameOver := False;
-            ThisGame.Create;
-            Edit3.Text := '0';
-            ThisGame.Score := 0;
-            ThisGame.Level := 1;
-            Edit2.Text := '1';
-            Form2.ShowModal;
-            InitSnowMen;
-            UnPauseGame;
-         end else
-             Form1.Close;
-     end;
-end;
-
-procedure TForm1.ThrowTimerStartTimer(Sender: TObject);
-begin
-  DanceTimer.Enabled := False;
-  BG.Mask(DancingTux);
-  BG.Blit(ThrowTux);
- // BG.Flip;
-end;
-
-procedure TForm1.ThrowTimerTimer(Sender: TObject);
-begin
-  BG.Mask(ThrowTux);
-  BG.Blit(DancingTux);
-  DanceTimer.Enabled := True;
-  ThrowTimer.Enabled := False;
-end;
-
-initialization
-  {$I unit1.lrs}
-
-end.
-
+unit unit1;
+
+{$mode objfpc}{$H+}
+
+interface
+
+uses
+  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,unit2,
+  doublebuffer, StdCtrls, ExtCtrls, Buttons, Sprite, GameData,
+  unit3, unit4, screensize, util, esdsound
+  {$IFDEF UNIX}
+  ,oldlinux
+  {$ENDIF}
+  {$IFDEF Win32}
+  ,Windows,ShellAPI
+  {$ENDIF}
+  ;
+
+type
+
+  { TForm1 }
+
+  TForm1 = class(TForm)
+    BitBtn1: TBitBtn;
+    BG: TDoubleBuffer;
+    Button1: TButton;
+    Edit1: TEdit;
+    Edit2: TEdit;
+    Edit3: TEdit;
+    Edit4: TEdit;
+    ESDSound1: TESDSound;
+    Image1: TImage;
+    DanceTimer: TTimer;
+    Label1: TLabel;
+    ScreenSize1: TScreenSize;
+    SnowManTimer: TTimer;
+    ScreenUpdateTimer: TTimer;
+    HammerTimer: TTimer;
+    BoomTimer: TTimer;
+    SplashTimer: TTimer;
+    ThrowTimer: TTimer;
+    procedure BitBtn1Click(Sender: TObject);
+    procedure BoomTimerStartTimer(Sender: TObject);
+    procedure BoomTimerTimer(Sender: TObject);
+    procedure Button1Click(Sender: TObject);
+    procedure DanceTimerTimer(Sender: TObject);
+    procedure Edit1KeyPress(Sender: TObject; var Key: char);
+    procedure Edit2Enter(Sender: TObject);
+    procedure FormActivate(Sender: TObject);
+    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
+    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
+    procedure FormCreate(Sender: TObject);
+    procedure FormResize(Sender: TObject);
+    procedure FormShow(Sender: TObject);
+    procedure FormWindowStateChange(Sender: TObject);
+    procedure HammerTimerTimer(Sender: TObject);
+    procedure ScreenUpdateTimerTimer(Sender: TObject);
+    procedure SnowManTimerTimer(Sender: TObject);
+    procedure SplashTimerTimer(Sender: TObject);
+    procedure ThrowTimerStartTimer(Sender: TObject);
+    procedure ThrowTimerTimer(Sender: TObject);
+  private
+    { private declarations }
+  public
+    { public declarations }
+  end; 
+
+
+
+var
+  Music: TSong;
+  Form1: TForm1; 
+  SnowMan: Array[1..5] of TSprite;
+  NextQuestion: Array[1..5] Of String;
+  Hammer: HammerQue;
+  HammerPic: Tsprite;
+  Boom: TSprite;
+  Splash: TSprite;
+  DancingTux: TSprite;
+  HurtTux: TSprite;
+  ThrowTux: TSprite;
+  GameOver: Boolean;
+
+implementation
+
+{ TForm1 }
+
+
+Procedure Play(Name : String);
+Begin
+  If ThisGame.SNDFX then Form1.esdSound1.Play(Name);
+end;
+
+
+Procedure ThrowHammer(GoLeft:Boolean);
+Begin
+  With Form1 do
+  begin
+  If GoLeft then
+  begin
+    ThrowTux.Frame := 0;
+    HammerPic.X := ThrowTux.X - HammerPic.FrameWidth;
+    HammerPic.Y := ThrowTux.Y;
+  end else
+  begin
+    ThrowTux.Frame := 1;
+    HammerPic.X := ThrowTux.X + ThrowTux.FrameWidth;
+    HammerPic.Y := ThrowTux.Y;
+  end;
+  ThrowTimer.Enabled := True;
+  end;
+end;
+
+Function textY(I:Integer):Integer;
+Begin
+  TextY := SnowMan[I].Y - 35;
+end;
+
+//Why larry why is this different between platforms ?
+{$IFDEF Win32}
+function MyRect(X,Y,A,B:Integer):Rect;
+Var
+ Z : Rect;
+Begin
+  Z.Top := Y;
+  Z.Left := X;
+  Z.Right := A;
+  Z.Bottom := B;
+  MyRect := Z;
+end;
+{$ENDIF}
+
+procedure textMask(I :Integer);
+Begin
+  With Form1 do
+  begin
+{$IFDEF Win32}
+  BG.MemBuff.Canvas.CopyRect(MyRect(SnowMan[I].X +1,textY(I),SnowMan[I].X + 99,textY(I)+35),
+   BG.BackGround.BitMap.Canvas,MyRect(SnowMan[I].X +1,textY(I),SnowMan[I].X + 99,textY(I)+35));
+{$ENDIF}
+{$IFDEF Linux}
+  BG.MemBuff.Canvas.CopyRect(Rect(SnowMan[I].X +1,textY(I),SnowMan[I].X + 99,textY(I)+35),
+   BG.BackGround.BitMap.Canvas,Rect(SnowMan[I].X +1,textY(I),SnowMan[I].X + 99,textY(I)+35));
+{$ENDIF}
+  end;
+end;
+
+
+procedure updateScoreboard;
+Begin
+With Form1 do
+Begin
+     Edit2.Text := intToStr(ThisGame.Level);
+     Edit3.Text := intToStr(ThisGame.Score);
+     Edit4.Text := IntToStr(ThisGame.Lives);
+     If ThisGame.Lives <=2 then
+        Form1.Caption := 'TappyTux - Warning: '+IntToStr(ThisGame.Lives)+' Left';
+
+end;
+end;
+
+procedure SnowQuestion(I:Integer);
+Var X : Integer;
+Unique : Boolean;
+begin
+      repeat
+            NextQuestion[I] := ThisGame.GetQuestion;
+            Unique := True;
+            for X := 1 to 5 do
+            if (length(NextQuestion[X]) <> 0) and (X <> I) then
+              if (thisgame.CheckAnswer(NextQuestion[X],NextQuestion[I]) <> 0) then
+                  Unique := False;
+      until Unique;
+end;
+
+Procedure InitSnowMen;
+Var I,X : integer;
+    Unique: Boolean;
+Begin
+
+  For I := 1 to 5 do
+      SnowQuestion(I);
+
+  for I := 1 to 5 do
+      repeat
+            Unique := True;
+            for X := 1 to 5 do
+            SnowMan[I].Y := 50 - (random(3000) div 10);
+            if (SnowMan[I].Y = SnowMan[X].Y) And (I <> X) then
+                  Unique := False;
+      until Unique;
+end;
+
+procedure LoadSnowMen;
+var
+  I, X, Y: integer;
+begin
+  case Scale of
+   640: Y := 100;
+   800: Y := 130;
+   1024:Y := 180;
+  end;
+
+  For I := 1 to random(30) do X := random(100);
+  
+  for I := 5 downto 1 do
+  begin
+    SnowMan[I] := TSprite.Create(nil);
+
+    SnowMan[I].loadFromFile(TPTDir+pathdelim+'sprites'+pathdelim+'snowmen.xpm');
+
+    SnowMan[I].FrameWidth := 94;
+    SnowMan[I].Frame := I -1;
+    SnowMan[I].X := 10+((I -1) * Y);
+  end;
+  
+  InitSnowMen;
+end;
+
+Procedure BlitSnowMen;
+Var I : Integer;
+Begin
+     For I := 5 downto 1 do
+     With form1 do
+     begin
+          BG.Mask(SnowMan[I]);
+          BG.Blit(SnowMan[I]);
+     end;
+end;
+
+
+procedure TForm1.BitBtn1Click(Sender: TObject);
+Var
+   Browser : String;
+begin
+{$IFDEF Linux}
+Browser := 'none';
+if shell('if which mozilla ; then exit 0 ; else exit 1 ; fi') = 0 then
+   Browser := 'mozilla';
+if shell('if which konqueror ; then exit 0 ; else exit 1 ; fi') = 0 then
+   Browser := 'konqueror';
+if shell('if which opera ; then exit 0 ; else exit 1 ; fi') = 0 then
+   Browser := 'opera';
+if shell('if which firefox ; then exit 0 ; else exit 1 ; fi') = 0 then
+   Browser := 'firefox';
+Writeln(browser);
+if browser <> 'none' then
+   shell(browser+' "http://www.getopenlab.com"&');
+{$ENDIF}
+{$IFDEF Win32}
+    ShellExecute(Handle,'open','http://www.getopenlab.com', nil, nil, SW_SHOWNORMAL);
+{$ENDIF}
+
+end;
+
+procedure TForm1.BoomTimerStartTimer(Sender: TObject);
+begin
+HammerTimer.Enabled := False;
+  SnowManTimer.Enabled := True;
+    Play('hit');
+     Boom.X := SnowMan[Hammer.Target[1]].X - 20;
+     Boom.Y := SnowMan[Hammer.Target[1]].Y - 20;
+             BG.Mask(SnowMan[Hammer.Target[1]]);
+             BG.Mask(HammerPic);
+
+               SnowMan[Hammer.Target[1]].Y := -150;
+     BG.Blit(Boom);
+   //  BG.Flip;
+
+end;
+
+procedure TForm1.BoomTimerTimer(Sender: TObject);
+
+begin
+
+     BG.Mask(Boom);
+ SnowQuestion(Hammer.Target[1]);
+     BoomTimer.Enabled := False;
+                  Hammer.DelTarget;
+                  If Hammer.Count <> 0 then
+             begin
+                If SnowMan[Hammer.Target[1]].X <= DancingTux.X then
+                   ThrowHammer(True) else
+                   ThrowHammer(False);
+             end;
+               BG.Mask(HammerPic);
+SnowManTimer.Enabled := True;
+     BG.Mask(HammerPic);
+     HammerTimer.Enabled := True;
+end;
+
+Procedure PauseGame;
+Begin
+With Form1 do
+begin
+  HammerTimer.Enabled := False;
+  SnowManTimer.Enabled := False;
+  Button1.Caption := 'Play';
+  Form1.Caption := 'TappyTux - PAUSED';
+end;
+end;
+
+Procedure UnPauseGame;
+Begin
+With Form1 do
+begin
+  Edit1.SetFocus;
+  HammerTimer.Enabled := True;
+  SnowManTimer.Enabled := True;
+  Button1.Caption := 'Pause';
+    Form1.Caption := 'TappyTux';
+end;
+end;
+
+
+procedure TForm1.Button1Click(Sender: TObject);
+begin
+If Button1.Caption = 'Pause' then
+PauseGame
+else
+UnPauseGame;
+end;
+
+
+
+procedure TForm1.DanceTimerTimer(Sender: TObject);
+begin
+ if DancingTux.Frame = 0 then
+  DancingTux.Frame := 1 else
+  DancingTux.Frame := 0;
+
+BG.Mask(DancingTux);
+BG.Blit(Dancingtux);
+{BG.Flip;  }
+end;
+
+
+
+procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: char);
+Var I,Y : Integer;
+    Gotcha : Boolean;
+begin
+ Edit1.Font.Color := ClBlack;
+  If Key = #13 then
+  begin
+    Gotcha := False;
+    For I := 1 to 5 do
+     begin
+         Y :=  ThisGame.CheckAnswer(NextQuestion[I],Edit1.Text);
+        If (Y > 0) and (Hammer.AddTarget(I)) then
+        begin
+             Gotcha := True;
+             If (ThisGame.Score + Y > ThisGame.NextLevel) and
+              (ThisGame.Score > ThisGame.NextLevel)
+               then
+             Begin
+                  Play('levelup');
+                  BG.LoadFromFile(ThisGame.NextBG);
+                    For I := 1 to 5 do
+                      begin
+                           BG.Mask(SnowMan[I]);
+                           BG.Blit(SnowMan[I]);
+                           BG.Mask(ThrowTux);
+                      end;
+                      BG.Blit(DancingTux);
+            end;
+             ThisGame.ScoreUp(Y);
+             If thisgame.Score > ThisGame.NextLife then
+             begin
+                  play ('life');
+                  ThisGame.NextLife := ThisGame.NextLife + 300;
+                  inc(ThisGame.Lives);
+             end else
+             play ('match');
+             updateScoreBoard;
+             Edit1.Text := '';
+             If Hammer.Count = 1 then
+             begin
+                If SnowMan[I].X <= DancingTux.X then
+                   ThrowHammer(True) else
+                   ThrowHammer(False);
+             end;
+        end;
+     end;
+       if Not Gotcha then
+       begin
+           play('error');
+           edit1.Font.Color := ClRed;
+       end;
+     end;
+end;
+
+
+
+
+
+procedure TForm1.Edit2Enter(Sender: TObject);
+begin
+  Edit1.SetFocus;
+end;
+
+procedure TForm1.FormActivate(Sender: TObject);
+begin
+  Edit1.SetFocus;
+end;
+
+procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
+Var
+I : Integer;
+begin
+  ThisGame.Level := 1;
+  ThisGame.Score := 0;
+  ThisGame.Lives := 5;
+  SnowManTimer.Enabled := False;
+  ScreenUpdateTimer.Enabled := False;
+  HammerTimer.Enabled := FAlse;
+  BoomTimer.Enabled := False;
+  SplashTimer.Enabled := FAlse;
+  ThrowTimer.Enabled := False;
+If Music <> Nil then
+begin
+ Music.Terminate;
+ execute ('killall -9 ogg123');
+end;
+
+   DanceTimer.Enabled := False;
+   if ThisGame.SndMusic then
+    For I := 5 downto 1 do
+        SnowMan[I].Free;
+try
+If Music <> Nil then
+begin
+  Music.Free;
+    BG.Free;
+end;
+  ThisGame.QuestionList.Free;
+except
+  writeln ('Exiting');
+for I := 1 to 5 do
+ SnowMan[I].free;
+  DancingTux.free;
+   HurtTux.free;
+   ThrowTux.free;
+   HammerPic.free;
+   Boom.free;
+   Splash.free;
+  BG.Free;
+Application.Terminate;
+end;
+end;
+
+procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
+begin
+If Not GameOver then
+begin
+PauseGame;
+  Form3.ShowModal;
+  If Form3.ModalResult = MrYes then
+       CanClose := True
+  else
+      Begin
+      CanClose := False;
+      UnPauseGame;
+      end;
+end;
+end;
+
+Procedure InitSprites;
+Begin
+  With form1 do
+  begin
+    DancingTux := TSprite.Create(nil);
+
+    DancingTux.LoadFromFile(TPTDir+pathdelim+'sprites'+pathdelim+'tuxfront.xpm');
+
+    DancingTux.X := (BG.Width div 2) - (DancingTux.Width div 2);
+    DancingTux.Y := BG.Height - DancingTux.Height;
+    DancingTux.Visible := True;
+    DancingTux.Frame := 0;
+    DancingTux.FrameWidth := 54;
+
+    HurtTux := TSprite.Create(nil);
+
+    HurtTux.LoadFromFile(TPTDir+pathdelim+'sprites'+pathdelim+'hurt.xpm');
+
+    HurtTux.X := (BG.Width div 2) - (HurtTux.Width div 2);
+    HurtTux.Y := Edit1.Top - DancingTux.Height;
+    HurtTux.Visible := True;
+    HurtTux.Frame := 0;
+    HurtTux.FrameWidth := 45;
+
+    ThrowTux := TSprite.Create(nil);
+
+    ThrowTux.LoadFromFile(TPTDir+pathdelim+'sprites'+pathdelim+'tuxside.xpm');
+
+    ThrowTux.X := (BG.Width div 2) - (ThrowTux.Width div 2);
+    ThrowTux.Y := Edit1.Top - DancingTux.Height;
+    ThrowTux.Visible := True;
+    ThrowTux.Frame := 0;
+    ThrowTux.FrameWidth := 58;
+
+
+    HammerPic := TSprite.Create(nil);
+    
+    HammerPic.LoadFromFile(TPTDir+pathdelim+'sprites'+pathdelim+'hammer.xpm');
+
+    HammerPic.X := DancingTux.X - 30;
+    HammerPic.Y := DancingTux.Y;
+    HammerPic.Visible := False;
+    HammerPic.Frame := 0;
+    HammerPic.FrameWidth := 41;
+
+    Boom := TSprite.Create(nil);
+    
+    Boom.LoadFromFile(TPTDir+pathdelim+'sprites'+pathdelim+'crash.xpm');
+
+    Boom.X := DancingTux.X - 30;
+    Boom.Y := DancingTux.Y;
+    Boom.Visible := True;
+    Boom.Frame := 0;
+    Boom.FrameWidth := 208;
+   
+    Splash := TSprite.Create(nil);
+
+    Splash.LoadFromFile(TPTDir+pathdelim+'sprites'+pathdelim+'splash.xpm');
+
+    Splash.X := DancingTux.X - 30;
+    Splash.Y := BG.Height - DancingTux.Height;
+    Splash.Visible := True;
+    Splash.Frame := 0;
+    Splash.FrameWidth := 198;
+  end;
+end;
+
+procedure startGame;
+Begin
+With Form1 Do
+begin
+   ThisGame.Create;
+   LoadSnowMen;
+   SnowManTimer.Enabled := True;
+   HammerTimer.Enabled := True;
+   BlitSnowMen;
+   UpdateScoreBoard;
+End;
+end;
+
+
+procedure TForm1.FormCreate(Sender: TObject);
+begin
+  // EsdSound1.Enabled := True;
+
+  ThisGame.QuestionList := TStringList.Create;
+  Randomize;
+
+  SchroedingersCat := False;
+  ScreenSize1.GetScreenSize;
+  
+  if (ScreenSize1.X = 640) or (paramcount <> 0) then
+  begin
+    BG.LoadFromFile(TPTDir + pathdelim + 'levels' + pathdelim + 'levelp.jpg');
+
+    Scale := 640;
+    Form1.Width := BG.Width + Image1.Width;
+    Form1.Height := BG.Height + Edit1.Height;
+    SnowManTimer.Interval := 200;
+    HammerTimer.Interval := 180;
+  end;
+
+  if paramcount = 0 then
+  begin
+    If ScreenSize1.X = 800 then
+    Begin
+      Scale := 800;
+
+      BG.LoadFromFile(TPTDir + pathdelim + 'levels' + pathdelim + '800' + pathdelim + 'levelp.jpg');
+
+      Form1.Width := BG.Width + Image1.Width;
+      Form1.Height := BG.Height + Edit1.Height;
+      SnowManTimer.Interval := 150;
+      HammerTimer.Interval := 100;
+    end;
+    if ScreenSize1.X >= 1024 then
+    begin
+      Scale := 1024;
+      BG.LoadFromFile(TPTDir + pathdelim + 'levels' + pathdelim + '1024' + pathdelim + 'levelp.jpg');
+
+      Form1.Width := BG.Width + Image1.Width;
+      Form1.Height := BG.Height + Edit1.Height;
+      SnowManTimer.Interval := 100;
+      HammerTimer.Interval := 80;
+    end;
+  end;
+  Hammer.Count := 0;
+  InitSprites;
+end;
+
+procedure TForm1.FormResize(Sender: TObject);
+begin
+     Form1.Width := BG.Width + Image1.Width;
+   Form1.Height := BG.Height + Edit1.Height;
+
+end;
+
+procedure TForm1.FormShow(Sender: TObject);
+Var
+   BTNFont : TFont;
+begin
+  BTNFont := TFont.Create;
+  BTNFont.Color := ClPurple;
+  BTNFont.Size := 47;
+  BTNFont.Name := 'TeachersPet';
+  BTNFont.Style := [FSBold];
+  Edit1.Font.Assign(BTNFont);
+
+  GameOver := False;
+  edit1.SetFocus;
+  Form2.ShowModal;
+
+  if not DoLoad then
+   Application.Terminate
+  else
+  begin
+    Play('/usr/share/tappytux/sounds/startup.wav');
+
+    if ThisGame.SndMusic then
+    begin
+         Music := TSong.Create(false);
+    if Assigned(Music.FatalException) then
+      raise Music.FatalException;
+      Music.Resume;
+    end;
+
+    ESDSound1.CacheSample('/usr/share/tappytux/sounds/gameover.wav','gameover');
+    ESDSound1.CacheSample('/usr/share/tappytux/sounds/level_up.wav','levelup');
+    ESDSound1.CacheSample('/usr/share/tappytux/sounds/missed_word.wav','missed');
+    ESDSound1.CacheSample('/usr/share/tappytux/sounds/ready.wav','ready');
+    ESDSound1.CacheSample('/usr/share/tappytux/sounds/word_hit.wav','hit');
+    ESDSound1.CacheSample('/usr/share/tappytux/sounds/word_error.wav','error');
+    ESDSound1.CacheSample('/usr/share/tappytux/sounds/word_match.wav','match');
+    ESDSound1.CacheSample('/usr/share/tappytux/sounds/life.wav','life');
+
+    Edit1.SetFocus;
+    StartGame;
+  end;
+end;
+
+procedure TForm1.FormWindowStateChange(Sender: TObject);
+begin
+  Edit1.SetFocus;
+end;
+
+
+procedure TForm1.HammerTimerTimer(Sender: TObject);
+var xdif,ydif : Integer;
+    GoLeft: Boolean;
+
+Procedure HammerLeft;
+Begin
+        if HammerPic.Frame <> 0 then
+           HammerPic.Frame := 0 else
+           HammerPic.Frame := 1;
+end;
+
+Procedure HammerRight;
+Begin
+    if HammerPic.Frame <> 2 then
+           HammerPic.Frame := 2 else
+           HammerPic.Frame := 3;
+end;
+
+begin
+  If Hammer.Count <> 0 then
+  begin
+  HammerPic.VIsible := True;
+
+     BG.Mask(HammerPic);
+     if HammerPic.X > SnowMan[Hammer.Target[1]].X + 30 then
+     begin
+        GoLeft := True;
+        HammerPic.X := HammerPic.X - 20;
+      end;
+     if HammerPic.X < SnowMan[Hammer.Target[1]].X + 30 then
+     begin
+        GoLeft := False;
+        HammerPic.X := HammerPic.X + 20;
+
+     end;
+     If GoLeft then HammerLeft else HammerRight;
+
+     if HammerPic.Y < SnowMan[Hammer.Target[1]].Y + 50 then
+          HammerPic.Y := HammerPic.Y + 20;
+     if HammerPic.Y > SnowMan[Hammer.Target[1]].Y + 50 then
+          HammerPic.Y := HammerPic.Y -20;
+     BG.Blit(HammerPic);
+
+     xdif := abs(SnowMan[Hammer.Target[1]].X +30 - HammerPic.X);
+     ydif := abs(SnowMan[Hammer.Target[1]].Y +50- HammerPic.Y);
+     If (xdif < 20) and (ydif < 50)
+     then
+     begin
+     snowManTimer.Enabled := False;
+     BoomTimer.Enabled := True;
+     end;
+
+  end else
+  HammerPic.Visible := False;
+end;
+
+procedure TForm1.ScreenUpdateTimerTimer(Sender: TObject);
+Var I : Integer;
+begin
+  bg.flip;
+end;
+
+Function SnowColor(X : Integer):Integer;
+Begin
+  Case X of
+  1: SnowColor := ClRed;
+  2: SnowColor := ClBlue;
+  3: SnowColor := ClPurple;
+  4: SnowColor := ClGreen;
+  5: SnowColor := ClOlive;
+ end;
+end;
+
+procedure TForm1.SnowManTimerTimer(Sender: TObject);
+Var I,J : Integer;
+    Hit : Boolean;
+begin
+  for I := 5 downto 1 do
+  begin
+       TextMask(I);
+       BG.Mask(SnowMan[I]);
+       Hit := False;
+       for J := 0 to Hammer.Count do
+           If I = Hammer.Target[J] then Hit := True;
+
+       if Not Hit then
+       begin
+        SnowMan[I].Y := SnowMan[I].Y + (thisgame.Level div 4) +1;
+
+         With BG.MemBuff.Canvas do
+         begin
+             Brush.Style := BsSolid;
+              Brush.color := clWhite;
+              Font.Size := 28;
+              Font.Color := SnowColor(I);
+              Font.Name := 'TeachersPet';
+              {$IFDEF Win32}
+              FillRect(MyRect(SnowMan[I].X +1,TextY(I),SnowMan[I].X + 99,textY(I)+ 35));
+              {$ENDIF}
+              {$IFDEF Linux}
+              FillRect(Rect(SnowMan[I].X +1,TextY(I),SnowMan[I].X + 99,textY(I)+ 35));
+              {$ENDIF}
+              if Scale = 1024 then
+              TextOut(SnowMan[I].X +2,TextY(I),NextQuestion[I]) else
+              TextOut(SnowMan[I].X +2,TextY(I) + 3,NextQuestion[I]);
+         end;
+         end;
+  If SnowMan[I].Y >= BG.Height then
+  begin
+   DanceTimer.Enabled := False;
+   HammerTimer.Enabled:= false;
+   SnowManTimer.Enabled := False;
+   Play('missed');
+   
+      If SnowMan[I].X <= DancingTux.X then
+           HurtTux.Frame := 0 else
+           HurtTux.Frame := 1;
+      DanceTimer.Enabled := False;
+      BG.Mask(DancingTux);
+      textMask(I);
+      BG.Mask(SnowMan[I]);
+        SnowMan[I].Y := -150;
+      BG.Blit(HurtTux);
+
+      Splash.X := SnowMan[I].X - 50;
+      Splash.Y := BG.Height - Splash.Height;
+      If Splash.Frame = 0 then
+         Splash.Frame := 1 else
+         Splash.Frame := 0;
+      BG.Blit(Splash);
+    //  BG.Flip;
+      SplashTimer.Enabled := True;
+  end;
+  end;
+
+  BlitSnowMen;
+  
+end;
+
+
+
+procedure TForm1.SplashTimerTimer(Sender: TObject);
+begin
+  ThisGame.LoseLife;
+    BG.Load;
+  BG.Mask(Splash);
+  BG.Mask(HurtTux);
+  BG.Blit(DancingTux);
+  //BG.Flip;
+  DanceTimer.Enabled := True;
+    SnowManTimer.Enabled := True;
+    HammerTimer.Enabled := True;
+    SplashTimer.Enabled := False;
+    UpdateScoreBoard;
+  if StrToInt(Edit4.Text) <> ThisGame.Lives then
+  begin
+    Edit4.Text := IntToStr(ThisGame.Lives);
+    play ('life');
+  end;
+  If ThisGame.Lives = 0 then
+     begin
+         GameOver := True;
+         PauseGame;
+         Form4.ShowModal;
+         If Form4.ModalResult = MrYes then
+         begin
+            GameOver := False;
+            ThisGame.Create;
+            Edit3.Text := '0';
+            ThisGame.Score := 0;
+            ThisGame.Level := 1;
+            Edit2.Text := '1';
+            Form2.ShowModal;
+            InitSnowMen;
+            UnPauseGame;
+         end else
+             Form1.Close;
+     end;
+end;
+
+procedure TForm1.ThrowTimerStartTimer(Sender: TObject);
+begin
+  DanceTimer.Enabled := False;
+  BG.Mask(DancingTux);
+  BG.Blit(ThrowTux);
+ // BG.Flip;
+end;
+
+procedure TForm1.ThrowTimerTimer(Sender: TObject);
+begin
+  BG.Mask(ThrowTux);
+  BG.Blit(DancingTux);
+  DanceTimer.Enabled := True;
+  ThrowTimer.Enabled := False;
+end;
+
+initialization
+  {$I unit1.lrs}
+
+end.
+
Index: unit2.pas
===================================================================
--- unit2.pas	(revision 67)
+++ unit2.pas	(working copy)
@@ -1,328 +1,334 @@
-unit Unit2; 
-
-{$mode objfpc}{$H+}
-
-interface
-
-uses
-  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,gamedata,
-  StdCtrls, ExtCtrls,util,tappywords
-  {$IFDEF Linux}
-  ,oldlinux
-  {$ENDIF}
-  ;
-
-type
-
-  { TForm2 }
-
-  TForm2 = class(TForm)
-    Button1: TButton;
-    Button2: TButton;
-    ComboBox1: TComboBox;
-    ComboBox2: TComboBox;
-    ComboBox3: TComboBox;
-    GroupBox5: TGroupBox;
-    Image1: TImage;
-    ListBox1: TListBox;
-    Memo1: TMemo;
-    Memo2: TMemo;
-    sndfx: TComboBox;
-    GroupBox1: TGroupBox;
-    GroupBox2: TGroupBox;
-    GroupBox3: TGroupBox;
-    GroupBox4: TGroupBox;
-    Label1: TLabel;
-    Label2: TLabel;
-    Label3: TLabel;
-    procedure Button1Click(Sender: TObject);
-    procedure Button2Click(Sender: TObject);
-    procedure ComboBox1Select(Sender: TObject);
-    procedure ComboBox2Change(Sender: TObject);
-    procedure ComboBox3Change(Sender: TObject);
-    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
-    procedure FormCreate(Sender: TObject);
-    procedure FormResize(Sender: TObject);
-    procedure FormShow(Sender: TObject);
-    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
-    procedure sndfxChange(Sender: TObject);
-  private
-    { private declarations }
-  public
-    { public declarations }
-  end; 
-
-var
-  Form2: TForm2; 
-  Extra : String;
-  Config : TStringList;
-  DoLoad,IsLoaded : Boolean;
-implementation
-
-{ TForm2 }
-
-procedure TForm2.Button1Click(Sender: TObject);
-var
-E : STring;
-begin
-ThisGame.Level := StrToInt(ComboBox3.Text);
-DoLoad := True;
-Config.Clear;
-If ThisGame.SNDMusic then
-   Config.Add('MUSIC=1') else
-   Config.Add('MUSIC=0');
-If ThisGame.SNDFX then
-   Config.Add('SOUNDFX=1') else
-   Config.Add('SOUNDFX=0');
-{$IFDEF Linux}
-Config.SaveToFile(getEnv('HOME')+'/.tappytux');
-{$ENDIF}
-{$IFDEF Win32}
-Config.SaveToFile('c:\program files\tappytux\tappytux.conf');
-{$ENDIF}
-  IsLoaded := True;
-  Form2.CLose;
-end;
-
-procedure TForm2.Button2Click(Sender: TObject);
-begin
-  execute(extra+' "'+ThisGame.Option+'"');
-end;
-
-
-procedure TForm2.ComboBox1Select(Sender: TObject);
-Var Op,Ex : TStringList;
-    Path : String;
-begin
-ThisGame.QuestionList.Clear;
-  If Length(ComboBox1.Text) > 0 then
-  try
-  {$IFDEF Linux}
-   Path := '/usr/share/tappytux/modules/'+ComboBox1.text;
-  {$ENDIF}
-  {$IFDEF Win32}
-  Path := 'c:\program files\tappytux\modules\'+ComboBox1.text;
-  {$ENDIF}
-  Memo1.Lines.LoadFromFile(Path+PathDelim+'description.txt');
-
-  If ComboBox1.text <> 'tappywords' then
-  begin
-       execute (PAth+PathDelim+ComboBox1.text+' --options',Op);
-       execute (PAth+PathDelim+ComboBox1.text+' --extra',Ex);
-  end else
-  begin
-       options(op);
-       tappyextra(ex);
-  end;
-  
-  If Ex.Count = 2 then
-   begin
-       Button2.Visible := True;
-       Button2.Caption := Ex[0];
-       Extra := Ex[1];
-   end else
-   begin
-        Button2.Visible := False;
-        Button2.Caption := 'Add/Edit';
-   end;
-
-  GroupBox2.Caption := Op[0];
-  Op.Delete(0);
-  ListBox1.Items.Assign(Op);
-  If ListBox1.ItemIndex = -1 then
-   Button1.Enabled := False;
-    If ComboBox1.text <> 'tappywords' then
-   {$IFDEF Linux}
-  ThisGame.ModuleName := '/usr/share/tappytux/modules/'+ComboBox1.Text+'/'+ComboBox1.Text
-   {$ENDIF}
-   {$IFDEF Win32}
-  ThisGame.ModuleName := 'c:\program files\tappytux\modules\'+ComboBox1.Text+'\'+ComboBox1.Text+'.exe'
-   {$ENDIF}
-   else
-  ThisGame.ModuleName := 'tappywords';
-  except
-        writeln ('except');
-  end;
-end;
-
-procedure TForm2.ComboBox2Change(Sender: TObject);
-begin
-  If ComboBox2.Text = 'On' then
-     ThisGame.SNDMusic := True
-  else
-      ThisGame.SNDMusic := False;
-end;
-
-procedure TForm2.ComboBox3Change(Sender: TObject);
-begin
-ThisGame.QuestionList.Clear;
-If length(ComboBox3.Text) > 0 then
-Begin
-  ThisGame.Level := StrToInt(ComboBox3.Text);
-  ThisGame.Score := StrToInt(ComboBox3.Text) * 100;
-  ThisGame.NextLevel := ThisGame.Score + 100;
-  ThisGame.NextLife := ThisGame.Score + 325;
-end;
-end;
-
-
-
-
-procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
-begin
-  If not IsLoaded then Application.Terminate;
-end;
-
-
-
-
-procedure TForm2.FormCreate(Sender: TObject);
-Var S : TSTringList;
-    St,Sa:String;
-    I,V : Integer;
-begin
-  DoLoad := False;
- {$IFDEF Linux}
- if not SearchFiles(S,'/usr/share/tappytux/modules/','*','') then
- {$ENDIF}
- {$IFDEF Win32}
- if not SearchFiles(S,'c:\program files\tappytux\modules\','*','') then
- {$ENDIF}
- begin
-  ShowMessage('Installation Error - no modules found');
-  application.Terminate;
- end;
-  For I := 0 to S.Count -1 do
-  begin
-  St := ExtractFilePath(S[I]);
-  Delete(St,Length(St),Length(St));
-  While (pos('\',ST) <>0) do
-        Delete(ST,1,pos('\',ST));
-  While (pos('/',ST) <>0) do
-        Delete(ST,1,pos('/',ST));
-   If Combobox1.Items.IndexOf(St) = -1 then
-       ComboBox1.Items.Add(St);
-  end;
-  ThisGame.SNDFX := True;
-  ThisGame.SNDMusic := True;
-  ThisGame.Option := '';
-  ThisGame.Level := 1;
-  ComboBox3.Text := '1';
-  ThisGame.QuestionList := TStringList.Create;
- Config := TStringList.Create;
-{$IFDEF Linux}
-If FileExists(getEnv('HOME')+'/.tappytux') then
-{$ENDIF}
-{$IFDEF Win32}
-If FileExists('c:\program files\tappytux\tappytux.conf') then
-{$ENDIF}
- Begin
-{$IFDEF Linux}
-Config.LoadFromFile(getEnv('HOME')+'/.tappytux');
-{$ENDIF}
-{$IFDEF Win32}
-Config.LoadFromFile('c:\program files\tappytux\tappytux.conf');
-{$ENDIF}
-  For I := 0 to Config.Count -1 do
-   begin
-      if pos('MUSIC',Config[I]) <> 0 then
-         begin
-              V := strToInt(Copy(Config[I],pos('=',Config[I])+1,length(Config[I])));
-              if V <> 0 then
-              Begin
-                ComboBox2.Text := 'On';
-                ThisGame.SNDMusic := True;
-              end else
-              begin
-                ComboBox2.Text := 'Off';
-                ThisGame.SNDMusic := False;
-              end;
-         end;
-      if pos('SOUNDFX',Config[I]) <> 0 then
-         begin
-              V := strToInt(Copy(Config[I],pos('=',Config[I])+1,length(Config[I])));
-              if V <> 0 then
-              Begin
-                SNDFX.Text := 'On';
-                ThisGame.SndFX := True;
-              end else
-              begin
-                SNDFX.Text := 'Off';
-                ThisGame.SndFX := False;
-              end;
-         end;
-    end;
-end;
-end;
-
-
-
-procedure TForm2.FormResize(Sender: TObject);
-begin
-  Form2.Width := 583;
-  Form2.Height := 400;
-end;
-
-procedure TForm2.FormShow(Sender: TObject);
-Var
-   BTNFont : TFont;
-begin
-   Memo2.Left := 6;
-   Memo2.Top := 0;
-   Memo2.Width :=  172;
-   Memo2.Height := 160;
-   {$IFDEF Linux}
-  Memo2.Lines.LoadFromFile('/usr/share/tappytux/CREDITS');
-   {$ENDIF}
-   {$IFDEF Win32}
-  Memo2.Lines.LoadFromFile('c:\program files\tappytux\CREDITS');
-   {$ENDIF}
-
-  BTNFont := TFont.Create;
-    BTNFont.Color := ClPurple;
-    BTNFont.Size := 20;
-   BTNFont.Name := 'TeachersPet';
-    BTNFont.Style := [FSBold];
-    Button1.Font.Assign(BTNFont);
-    BTNFont.FRee;
-  ComboBox3.Text := '1';
-  ComboBox1.Text := Combobox1.Items[0];
-  ComboBox1Select(Form2);
-  Button1.Invalidate;
-  IsLoaded := False;
-  Memo1.Top := 9;
-  Memo1.Left := 6;
-  Memo1.Width := 360;
-  Memo1.Height := 136;
-end;
-
-procedure TForm2.ListBox1SelectionChange(Sender: TObject; User: boolean);
-begin
-ThisGame.QuestionList.Clear;
-If (ListBox1.ItemIndex <> -1) AND (length(combobox1.Text) > 0 ) then
-begin
- ThisGame.Option := ListBox1.Items[Listbox1.ItemIndex];
- Button1.Enabled := True;
-if Combobox1.Text = 'tappywords' then
-   SetOption(ListBox1.Items[Listbox1.ItemIndex]);
-   Question := TQuestion.Create(false);
-    if Assigned(Question.FatalException) then
-      raise Question.FatalException;
-      Question.Resume;
-end else
-  Button1.Enabled := False;
-end;
-
-procedure TForm2.sndfxChange(Sender: TObject);
-begin
-  If SndFX.Text = 'On' then
-     ThisGame.SndFX := True
-  else
-      ThisGame.SndFX := False;
-end;
-
-initialization
-  {$I unit2.lrs}
-
-end.
-
+unit Unit2; 
+
+{$mode objfpc}{$H+}
+
+interface
+
+uses
+  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,gamedata,
+  StdCtrls, ExtCtrls,util,tappywords
+  {$IFDEF Linux}
+  ,oldlinux
+  {$ENDIF}
+  ;
+
+type
+
+  { TForm2 }
+
+  TForm2 = class(TForm)
+    Button1: TButton;
+    Button2: TButton;
+    ComboBox1: TComboBox;
+    ComboBox2: TComboBox;
+    ComboBox3: TComboBox;
+    GroupBox5: TGroupBox;
+    Image1: TImage;
+    ListBox1: TListBox;
+    Memo1: TMemo;
+    Memo2: TMemo;
+    sndfx: TComboBox;
+    GroupBox1: TGroupBox;
+    GroupBox2: TGroupBox;
+    GroupBox3: TGroupBox;
+    GroupBox4: TGroupBox;
+    Label1: TLabel;
+    Label2: TLabel;
+    Label3: TLabel;
+    procedure Button1Click(Sender: TObject);
+    procedure Button2Click(Sender: TObject);
+    procedure ComboBox1Select(Sender: TObject);
+    procedure ComboBox2Change(Sender: TObject);
+    procedure ComboBox3Change(Sender: TObject);
+    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
+    procedure FormCreate(Sender: TObject);
+    procedure FormResize(Sender: TObject);
+    procedure FormShow(Sender: TObject);
+    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
+    procedure sndfxChange(Sender: TObject);
+  private
+    { private declarations }
+  public
+    { public declarations }
+  end; 
+
+var
+  Form2: TForm2; 
+  Extra : String;
+  Config : TStringList;
+  DoLoad,IsLoaded : Boolean;
+implementation
+
+{ TForm2 }
+
+procedure TForm2.Button1Click(Sender: TObject);
+var
+  E: STring;
+begin
+  ThisGame.Level := StrToInt(ComboBox3.Text);
+  DoLoad := True;
+  Config.Clear;
+  If ThisGame.SNDMusic then Config.Add('MUSIC=1')
+  else Config.Add('MUSIC=0');
+  
+  If ThisGame.SNDFX then Config.Add('SOUNDFX=1')
+  else Config.Add('SOUNDFX=0');
+
+  {$IFDEF Linux}
+  Config.SaveToFile(getEnv('HOME')+'/.tappytux');
+  {$ENDIF}
+  {$IFDEF Win32}
+  Config.SaveToFile(TPTDir + pathdelim + 'tappytux.conf');
+  {$ENDIF}
+  
+  IsLoaded := True;
+  Form2.CLose;
+end;
+
+procedure TForm2.Button2Click(Sender: TObject);
+begin
+  execute(extra+' "'+ThisGame.Option+'"');
+end;
+
+
+procedure TForm2.ComboBox1Select(Sender: TObject);
+Var
+  Op, Ex: TStringList;
+  Path: String;
+begin
+  ThisGame.QuestionList.Clear;
+
+  If Length(ComboBox1.Text) > 0 then
+  try
+
+    Path := TPTDir + pathdelim + 'modules'+pathdelim+ComboBox1.text;
+
+    Memo1.Lines.LoadFromFile(Path+PathDelim+'description.txt');
+
+    If ComboBox1.text <> 'tappywords' then
+    begin
+      execute(Path + PathDelim + ComboBox1.text
+      {$IFDEF Win32}
+       + '.exe'
+      {$ENDIF}
+       + ' --options', Op);
+       
+      execute(Path + PathDelim + ComboBox1.text
+      {$IFDEF Win32}
+       + '.exe'
+      {$ENDIF}
+       + ' --extra', Ex);
+    end
+    else
+    begin
+      options(op);
+      tappyextra(ex);
+    end;
+  
+  If Ex.Count = 2 then
+   begin
+       Button2.Visible := True;
+       Button2.Caption := Ex[0];
+       Extra := Ex[1];
+   end else
+   begin
+        Button2.Visible := False;
+        Button2.Caption := 'Add/Edit';
+   end;
+
+  GroupBox2.Caption := Op[0];
+  Op.Delete(0);
+  ListBox1.Items.Assign(Op);
+  If ListBox1.ItemIndex = -1 then
+   Button1.Enabled := False;
+
+    If ComboBox1.text <> 'tappywords' then
+    begin
+      ThisGame.ModuleName := TPTDir + pathdelim + 'modules'+
+       pathdelim + ComboBox1.text + pathdelim + ComboBox1.Text;
+   
+      {$IFDEF Win32}
+      ThisGame.ModuleName := ThisGame.ModuleName + '.exe';
+      {$ENDIF}
+    end
+    else ThisGame.ModuleName := 'tappywords';
+   
+  except
+    writeln('except');
+  end;
+end;
+
+procedure TForm2.ComboBox2Change(Sender: TObject);
+begin
+  If ComboBox2.Text = 'On' then
+     ThisGame.SNDMusic := True
+  else
+      ThisGame.SNDMusic := False;
+end;
+
+procedure TForm2.ComboBox3Change(Sender: TObject);
+begin
+ThisGame.QuestionList.Clear;
+If length(ComboBox3.Text) > 0 then
+Begin
+  ThisGame.Level := StrToInt(ComboBox3.Text);
+  ThisGame.Score := StrToInt(ComboBox3.Text) * 100;
+  ThisGame.NextLevel := ThisGame.Score + 100;
+  ThisGame.NextLife := ThisGame.Score + 325;
+end;
+end;
+
+
+
+
+procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
+begin
+  If not IsLoaded then Application.Terminate;
+end;
+
+
+
+
+procedure TForm2.FormCreate(Sender: TObject);
+var
+  S: TSTringList;
+  St, Sa: String;
+  I, V: Integer;
+begin
+  DoLoad := False;
+
+ if not SearchFiles(S, TPTDir + pathdelim + 'modules'+pathdelim,'*','') then
+ begin
+  ShowMessage('Installation Error - no modules found');
+  application.Terminate;
+ end;
+  For I := 0 to S.Count -1 do
+  begin
+  St := ExtractFilePath(S[I]);
+  Delete(St,Length(St),Length(St));
+  While (pos('\',ST) <>0) do
+        Delete(ST,1,pos('\',ST));
+  While (pos('/',ST) <>0) do
+        Delete(ST,1,pos('/',ST));
+   If Combobox1.Items.IndexOf(St) = -1 then
+       ComboBox1.Items.Add(St);
+  end;
+  ThisGame.SNDFX := True;
+  ThisGame.SNDMusic := True;
+  ThisGame.Option := '';
+  ThisGame.Level := 1;
+  ComboBox3.Text := '1';
+  ThisGame.QuestionList := TStringList.Create;
+ Config := TStringList.Create;
+{$IFDEF Linux}
+If FileExists(getEnv('HOME')+'/.tappytux') then
+{$ENDIF}
+{$IFDEF Win32}
+If FileExists(TPTDir + pathdelim + 'tappytux.conf') then
+{$ENDIF}
+ Begin
+{$IFDEF Linux}
+Config.LoadFromFile(getEnv('HOME')+'/.tappytux');
+{$ENDIF}
+{$IFDEF Win32}
+Config.LoadFromFile(TPTDir + pathdelim + 'tappytux.conf');
+{$ENDIF}
+  For I := 0 to Config.Count -1 do
+   begin
+      if pos('MUSIC',Config[I]) <> 0 then
+         begin
+              V := strToInt(Copy(Config[I],pos('=',Config[I])+1,length(Config[I])));
+              if V <> 0 then
+              Begin
+                ComboBox2.Text := 'On';
+                ThisGame.SNDMusic := True;
+              end else
+              begin
+                ComboBox2.Text := 'Off';
+                ThisGame.SNDMusic := False;
+              end;
+         end;
+      if pos('SOUNDFX',Config[I]) <> 0 then
+         begin
+              V := strToInt(Copy(Config[I],pos('=',Config[I])+1,length(Config[I])));
+              if V <> 0 then
+              Begin
+                SNDFX.Text := 'On';
+                ThisGame.SndFX := True;
+              end else
+              begin
+                SNDFX.Text := 'Off';
+                ThisGame.SndFX := False;
+              end;
+         end;
+    end;
+end;
+end;
+
+
+
+procedure TForm2.FormResize(Sender: TObject);
+begin
+  Form2.Width := 583;
+  Form2.Height := 400;
+end;
+
+procedure TForm2.FormShow(Sender: TObject);
+var
+  BTNFont : TFont;
+begin
+  Memo2.Left := 6;
+  Memo2.Top := 0;
+  Memo2.Width :=  172;
+  Memo2.Height := 160;
+
+  Memo2.Lines.LoadFromFile(TPTDir + pathdelim + 'CREDITS');
+
+  BTNFont := TFont.Create;
+  BTNFont.Color := ClPurple;
+  BTNFont.Size := 20;
+  BTNFont.Name := 'TeachersPet';
+  BTNFont.Style := [FSBold];
+  Button1.Font.Assign(BTNFont);
+  BTNFont.FRee;
+  ComboBox3.Text := '1';
+//  ComboBox1.Text := Combobox1.Items[0]; // Crashes on win32
+  ComboBox1Select(Form2);
+  Button1.Invalidate;
+  IsLoaded := False;
+  Memo1.Top := 9;
+  Memo1.Left := 6;
+  Memo1.Width := 360;
+  Memo1.Height := 136;
+end;
+
+procedure TForm2.ListBox1SelectionChange(Sender: TObject; User: boolean);
+begin
+ThisGame.QuestionList.Clear;
+If (ListBox1.ItemIndex <> -1) AND (length(combobox1.Text) > 0 ) then
+begin
+ ThisGame.Option := ListBox1.Items[Listbox1.ItemIndex];
+ Button1.Enabled := True;
+if Combobox1.Text = 'tappywords' then
+   SetOption(ListBox1.Items[Listbox1.ItemIndex]);
+   Question := TQuestion.Create(false);
+    if Assigned(Question.FatalException) then
+      raise Question.FatalException;
+      Question.Resume;
+end else
+  Button1.Enabled := False;
+end;
+
+procedure TForm2.sndfxChange(Sender: TObject);
+begin
+  If SndFX.Text = 'On' then
+     ThisGame.SndFX := True
+  else
+      ThisGame.SndFX := False;
+end;

initialization
  {$I unit2.lrs}

end.

