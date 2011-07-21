
{
Skin Support for Cactus Jukebox

written by Sebastian Kraft, <c> 2006

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL






}


Unit skin;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, xmlcfg, Menus, graphics, config;

Type 

  TImageData = Record
    Img : string;
    x,y : integer;
  End;
  TButtonImageData = Record
    Img, MouseOver, Clicked, Activated : string;
    x,y : integer;
  End;

  { TSkin }

  TSkin = Class
   Private
    FSkinPrefix: string;
    FSKinName: string;
    FSkinPath: string;
    FDefaultPath: string;
    Function getpath: string;
   Public
    play, previous, next, pause, stop: TButtonImageData;
    mute, info, open, view : TButtonImageData;
    Pplay, Pprevious, Pnext, Ppause, Pstop: TButtonImageData;
    Background, Time, Title, Toolbar, Trackbar, TitleBack: TImageData;
    destructor destroy;
    constructor create(DefaultSkin, SkinPrefix: String);
    Procedure load_skin(skinfile: String);
    property SkinPath: string read getpath;
    property DefaultPath: string read FDefaultPath;
    property Name: string read FSkinName;
  End;


Procedure register_skins;

Var SkinData: TSkin;

  Implementation

  Uses mainform;

Var skinxml: TXMLConfig;


Procedure register_skins;

Var skinsrch: TSearchrec;
  i: byte;
Begin
  i := 1;
  If FindFirst(CactusConfig.DataPrefix+'skins'+DirectorySeparator+'*.xml',faAnyFile,skinsrch)=0 Then
    Begin
      Repeat
        Begin
          Main.skinmenuitems[i] := TMenuItem.Create(Nil);
          Main.skinmenuitems[i].Caption := skinsrch.Name;
          Main.skinmenuitems[i].RadioItem := true;
          Main.skinmenuitems[i].GroupIndex := 200;
          Main.skinmenuitems[i].OnClick := @Main.loadskin;
          //                  if skinsrch.Name='default.xml' then Main.skinmenuitems[i].:=true;
          Main.skinmenu.Add(Main.skinmenuitems[i]);
          inc(i);
        End;
      Until (FindNext(skinsrch)<>0) Or (i>16);
    End;

End;

Function TSkin.getpath: string;
Begin
  result := FSkinPath;
End;

destructor TSkin.destroy;
Begin
  skinxml.Free;
End;

constructor TSkin.create(DefaultSkin, SkinPrefix: String);
Begin
  skinxml := TXMLConfig.Create(Nil);
  FSkinPrefix := SkinPrefix;

{    First load default skin, later ovveride with user skin }

  skinxml.Filename := SkinPrefix+'skins'+DirectorySeparator+DefaultSkin;
  FSkinPath := SkinPrefix+'skins'+DirectorySeparator+skinxml.GetValue('RelativePath/Value','');
  FDefaultpath := FSkinPath;
  FSKinName := skinxml.GetValue('Name/Value','');

  //Mainwindow buttons

  play.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Play/Normal','');
  play.x := StrToInt(skinxml.GetValue('Main/Buttons/Play/x','0'));
  play.y := StrToInt(skinxml.GetValue('Main/Buttons/Play/y','0'));

  stop.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Stop/Normal','');
  stop.x := StrToInt(skinxml.GetValue('Main/Buttons/Stop/x','0'));
  stop.y := StrToInt(skinxml.GetValue('Main/Buttons/Stop/y','0'));

  previous.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Previous/Normal','');
  previous.x := StrToInt(skinxml.GetValue('Main/Buttons/Previous/x','0'));
  previous.y := StrToInt(skinxml.GetValue('Main/Buttons/Previous/y','0'));

  next.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Next/Normal','');
  next.x := StrToInt(skinxml.GetValue('Main/Buttons/Next/x','0'));
  next.y := StrToInt(skinxml.GetValue('Main/Buttons/Next/y','0'));

  pause.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Pause/Normal','');
  pause.x := StrToInt(skinxml.GetValue('Main/Buttons/Pause/x','0'));
  pause.y := StrToInt(skinxml.GetValue('Main/Buttons/Pause/y','0'));

  previous.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue(
                        'Main/Buttons/Previous/MouseOver','');
  next.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Next/MouseOver','');
  pause.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Pause/MouseOver',''
                     );
  stop.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Stop/MouseOver','');
  play.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Play/MouseOver','');

  previous.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Previous/Clicked',
                      '');
  next.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Next/Clicked','');
  pause.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Pause/Clicked','');
  stop.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Stop/Clicked','');
  play.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue('Main/Buttons/Play/Clicked','');



  //Player Buttons
  Time.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Images/Time/Path','');
  time.x := StrToInt(skinxml.GetValue('Player/Images/Time/x','0'));
  time.y := StrToInt(skinxml.GetValue('Player/Images/Time/y','0'));
  Background.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Images/Background/Path',''
                    );
  Background.x := StrToInt(skinxml.GetValue('Player/Images/Background/x','0'));
  Background.y := StrToInt(skinxml.GetValue('Player/Images/Background/y','0'));
  Title.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Images/Title/Path','');
  Title.x := StrToInt(skinxml.GetValue('Player/Images/Title/x','0'));
  title.y := StrToInt(skinxml.GetValue('Player/Images/Title/y','0'));
  Toolbar.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Images/Toolbar/Path','');
  Toolbar.x := StrToInt(skinxml.GetValue('Player/Images/Toolbar/x','0'));
  Toolbar.y := StrToInt(skinxml.GetValue('Player/Images/Toolbar/y','0'));
  Trackbar.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Images/Trackbar/Path','');
  Trackbar.x := StrToInt(skinxml.GetValue('Player/Images/Trackbar/x','0'));
  trackbar.y := StrToInt(skinxml.GetValue('Player/Images/Trackbar/y','0'));
  TitleBack.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Images/TextBackground/Path'
                   ,'');
  TitleBack.x := StrToInt(skinxml.GetValue('Player/Images/TextBackground/x','0'));
  titleback.y := StrToInt(skinxml.GetValue('Player/Images/TextBackground/y','0'));

  Mute.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Mute/Normal','');
  mute.x := StrToInt(skinxml.GetValue('Player/Buttons/Mute/x','0'));
  mute.y := StrToInt(skinxml.GetValue('Player/Buttons/Mute/y','0'));

  view.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/View/Normal','');
  view.x := StrToInt(skinxml.GetValue('Player/Buttons/View/x','0'));
  view.y := StrToInt(skinxml.GetValue('Player/Buttons/View/y','0'));

  Info.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Info/Normal','');
  info.x := StrToInt(skinxml.GetValue('Player/Buttons/Info/x','0'));
  info.y := StrToInt(skinxml.GetValue('Player/Buttons/Info/y','0'));

  Open.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Open/Normal','');
  open.x := StrToInt(skinxml.GetValue('Player/Buttons/Open/x','0'));
  open.y := StrToInt(skinxml.GetValue('Player/Buttons/Open/y','0'));

  Mute.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Mute/MouseOver',''
                    );
  View.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/View/MouseOver',''
                    );
  info.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Info/MouseOver',''
                    );
  Open.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Open/MouseOver',''
                    );

  Pplay.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Play/Normal','');
  Pplay.x := StrToInt(skinxml.GetValue('Player/Buttons/Play/x','0'));
  Pplay.y := StrToInt(skinxml.GetValue('Player/Buttons/Play/y','0'));

  Pstop.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Stop/Normal','');
  Pstop.x := StrToInt(skinxml.GetValue('Player/Buttons/Stop/x','0'));
  Pstop.y := StrToInt(skinxml.GetValue('Player/Buttons/Stop/y','0'));

  Pprevious.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Previous/Normal',''
                   );
  Pprevious.x := StrToInt(skinxml.GetValue('Player/Buttons/Previous/x','0'));
  Pprevious.y := StrToInt(skinxml.GetValue('Player/Buttons/Previous/y','0'));

  Pnext.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Next/Normal','');
  Pnext.x := StrToInt(skinxml.GetValue('Player/Buttons/Next/x','0'));
  Pnext.y := StrToInt(skinxml.GetValue('Player/Buttons/Next/y','0'));

  Ppause.Img := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Pause/Normal','');
  Ppause.x := StrToInt(skinxml.GetValue('Player/Buttons/Pause/x','0'));
  Ppause.y := StrToInt(skinxml.GetValue('Player/Buttons/Pause/y','0'));

  Pprevious.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue(
                         'Player/Buttons/Previous/MouseOver','');
  Pnext.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Next/MouseOver',
                     '');
  Ppause.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Pause/MouseOver'
                      ,'');
  Pstop.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Stop/MouseOver',
                     '');
  Pplay.MouseOver := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Play/MouseOver',
                     '');

  Pprevious.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue(
                       'Player/Buttons/Previous/Clicked','');
  Pnext.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Next/Clicked','');
  Ppause.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Pause/Clicked','')
  ;
  Pstop.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Stop/Clicked','');
  Pplay.Clicked := FSkinPath+DirectorySeparator+skinxml.GetValue('Player/Buttons/Play/Clicked','');
End;

Procedure Tskin.load_skin(skinfile: String);

Var tmpstr: string;
Begin

{     bgcolor:=clBtnFace;
     main.panel1.Color:=bgcolor;
     main.panel4.Color:=bgcolor;
     main.Splitter1.Color:=bgcolor;
     main.Panel2.Color:=bgcolor;
     main.Panel3.Color:=bgcolor;
     main.Color:=bgcolor; }

{    now override with use skin if user sets something                                             }

  skinxml.Filename := FSkinPrefix+'skins'+DirectorySeparator+skinfile;
  FSkinPath := FSkinPrefix+'skins'+DirectorySeparator+skinxml.GetValue('RelativePath/Value','');
  FSKinName := skinxml.GetValue('Name/Value','');

  tmpstr := skinxml.GetValue('Main/Buttons/Play/Normal','');
  If tmpstr<>'' Then play.Img := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Stop/Normal','');
  If tmpstr<>'' Then  stop.Img := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Previous/MouseOverrmal','');
  If tmpstr<>'' Then previous.Img := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Next/Normal','');
  If tmpstr<>'' Then next.Img := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Pause/Normal','');
  If tmpstr<>'' Then pause.Img := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Previous/MouseOver','');
  If tmpstr<>'' Then previous.MouseOver := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Next/MouseOver','');
  If tmpstr<>'' Then next.MouseOver := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Pause/MouseOver','');
  If tmpstr<>'' Then pause.MouseOver := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Stop/MouseOver','');
  If tmpstr<>'' Then stop.MouseOver := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Play/MouseOver','');
  If tmpstr<>'' Then play.MouseOver := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Previous/Clicked','');
  If tmpstr<>'' Then previous.Clicked := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Next/Clicked','');
  If tmpstr<>'' Then next.Clicked := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Pause/Clicked','');
  If tmpstr<>'' Then pause.Clicked := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Stop/Clicked','');
  If tmpstr<>'' Then stop.Clicked := FSkinPath+DirectorySeparator+tmpstr;

  tmpstr := skinxml.GetValue('Main/Buttons/Play/Clicked','');
  If tmpstr<>'' Then play.Clicked := FSkinPath+DirectorySeparator+tmpstr;


{
     tmpstr:=skinxml.GetValue('Database/Pause/Icon','icon/pause.xpm');
     Main.pause.Glyph.LoadFromFile(FSkinPath+DirectorySeparator+tmpstr);

     tmpstr:=skinxml.GetValue('Database/Play/Icon','icon/play.xpm');
     Main.play.Glyph.LoadFromFile(FSkinPath+DirectorySeparator+tmpstr);

     tmpstr:=skinxml.GetValue('Database/Stop/Icon','icon/stop.xpm');
     Main.stop.Glyph.LoadFromFile(FSkinPath+DirectorySeparator+tmpstr);

     tmpstr:=skinxml.GetValue('Database/Next/Icon','icon/next.xpm');
     Main.next.Glyph.LoadFromFile(FSkinPath+DirectorySeparator+tmpstr);

     tmpstr:=skinxml.GetValue('Database/Previous/Icon','icon/previous.xpm');
     Main.prev.Glyph.LoadFromFile(FSkinPath+DirectorySeparator+tmpstr);
     }
  //     tmpbmp:=TBitmap.Create;


{  tmpstr:=skinxml.GetValue('Database/Upload/Icon','icon/upload.xpm');
     tmpbmp.LoadFromFile(FSkinPath+DirectorySeparator+tmpstr);}

{ Main.ImageList1.Replace(3, tmpbmp, nil);
     Main.ImageList1.Replace(2, tmpbmp, nil);
     Main.ImageList1.Replace(1, tmpbmp, nil);}
  //     Main.ImageList1.Delete(4);




{ tmpstr:=skinxml.GetValue('Database/Remove/Icon','icon/remove.xpm');
     tmpbmp.LoadFromFile(FSkinPath+DirectorySeparator+tmpstr);
     i:=Main.ImageList1.add(tmpbmp, nil);
     Main.openfile.ImageIndex:=i;

     tmpbmp:=TBitmap.Create;
     tmpstr:=skinxml.GetValue('Database/Upload/Icon','icon/remove.xpm');
     tmpbmp.LoadFromFile(FSkinPath+DirectorySeparator+tmpstr);
     i:=Main.ImageList1.add(tmpbmp, nil);
     Main.player_lib.ImageIndex:=i;}

     {Main.ImageList1.Insert(0, tmpbmp, nil);
     Main.ImageList1.Insert(1, tmpbmp, nil);}

  // writeln(skinxml.GetValue('Database/Play/Icon','ss'));

  With main Do
    Begin
      PlayButtonImg.Picture.LoadFromFile(play.Img);
      StopButtonImg.Picture.LoadFromFile(stop.Img);
      PauseButtonImg.Picture.LoadFromFile(pause.Img);
      NextButtonImg.Picture.LoadFromFile(next.Img);
      PreviousButtonImg.Picture.LoadFromFile(previous.Img);
    End;


End;


Begin
End.
