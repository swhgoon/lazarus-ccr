
{
Minimized Player View for Cactus Jukebox

written by Sebastian Kraft, <c> 2006-2008

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL






}


Unit player;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
Buttons, playerclass, mainform, mediacol, {messages,} ComCtrls, Menus;

Type 

  { Tplaywin }

  Tplaywin = Class(TForm)
    BackgroundImg: TImage;
    AlbumCoverImg: TImage;
    ViewImg: TImage;
    PlayImg: TImage;
    StopImg: TImage;
    PauseImg: TImage;
    backImg: TImage;
    NextImg: TImage;
    ToolbarImg: TImage;
    OpenImg: TImage;
    InfoImg: TImage;
    MuteImg: TImage;
    textBackImg: TImage;
    TitleImg: TImage;
    TimeImg: TImage;
    TrackbarImg: TImage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    popup_open: TMenuItem;
    file_info: TMenuItem;
    PopupMenu1: TPopupMenu;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormDestroy(Sender: TObject);
    Procedure InfoImgClick(Sender: TObject);
    Procedure InfoImgMouseEnter(Sender: TObject);
    Procedure InfoImgMouseLeave(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    //   procedure WMEraseBkgnd(var message:TWMEraseBkgnd); message  WM_ERASEBKGND;
    Procedure Image1Click(Sender: TObject);
    Procedure Image1Paint(Sender: TObject);
    Procedure MuteImgClick(Sender: TObject);
    Procedure MuteImgMouseEnter(Sender: TObject);
    Procedure NextImgClick(Sender: TObject);
    Procedure NextImgMouseDown(Sender: TOBject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Integer);
    Procedure NextImgMouseEnter(Sender: TObject);
    Procedure NextImgMouseLeave(Sender: TObject);
    Procedure NextImgMouseUp(Sender: TOBject; Button: TMouseButton;
                             Shift: TShiftState; X, Y: Integer);
    Procedure OpenImgClick(Sender: TObject);
    Procedure PauseImgClick(Sender: TObject);
    Procedure PauseImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    Procedure PauseImgMouseEnter(Sender: TObject);
    Procedure PauseImgMouseLeave(Sender: TObject);
    Procedure PauseImgMouseUp(Sender: TOBject; Button: TMouseButton;
                              Shift: TShiftState; X, Y: Integer);
    Procedure PlayImgClick(Sender: TObject);
    Procedure PlayImgMouseDown(Sender: TOBject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Integer);
    Procedure PlayImgMouseEnter(Sender: TObject);
    Procedure PlayImgMouseLeave(Sender: TObject);
    Procedure PlayImgMouseUp(Sender: TOBject; Button: TMouseButton;
                             Shift: TShiftState; X, Y: Integer);
    Procedure StopImgClick(Sender: TObject);
    Procedure StopImgMouseDown(Sender: TOBject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Integer);
    Procedure StopImgMouseEnter(Sender: TObject);
    Procedure StopImgMouseLeave(Sender: TObject);
    Procedure StopImgMouseUp(Sender: TOBject; Button: TMouseButton;
                             Shift: TShiftState; X, Y: Integer);
    Procedure ViewImgClick(Sender: TObject);
    Procedure ViewImgMouseEnter(Sender: TObject);
    Procedure ViewImgMouseLeave(Sender: TObject);
    Procedure backImgClick(Sender: TObject);
    Procedure backImgMouseDown(Sender: TOBject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Integer);
    Procedure backImgMouseEnter(Sender: TObject);
    Procedure backImgMouseLeave(Sender: TObject);
    Procedure backImgMouseUp(Sender: TOBject; Button: TMouseButton;
                             Shift: TShiftState; X, Y: Integer);
    Procedure r(Sender: TObject);
    Procedure MuteImgMouseLeave(Sender: TObject);
    Procedure OpenImgMouseEnter(Sender: TObject);
    Procedure OpenImgMouseLeave(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure SpeedButton2Click(Sender: TObject);
    Procedure file_infoClick(Sender: TObject);
    Procedure openClick(Sender: TObject);
    Procedure pauseClick(Sender: TObject);
    Procedure playClick(Sender: TObject);
    Procedure playwinClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure playwinCreate(Sender: TObject);
    Procedure playwinKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
    );
    Procedure playwinKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure prevClick(Sender: TObject);
    Procedure stopClick(Sender: TObject);
    Procedure toggle_viewClick(Sender: TObject);
    Procedure trackbarMouseDown(Sender: TOBject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    Private 
    { private declarations }
    strg: boolean;
    Public 
    Procedure draw_artist(a:String);
    Procedure draw_title(t:String);

    { public declarations }
  End;

Var 
  playwin: Tplaywin;

  Implementation

  Uses skin;

{ Tplaywin }

{procedure TPlaywin.WMEraseBkgnd(var message:TWMEraseBkgnd);
begin
 message.result:=1;
end;}

Procedure Tplaywin.MenuItem1Click(Sender: TObject);
Begin

End;

Procedure Tplaywin.FormDestroy(Sender: TObject);
Begin

End;

Procedure Tplaywin.InfoImgClick(Sender: TObject);
Begin
  main.TrackInfoClick(Nil);
End;

Procedure Tplaywin.InfoImgMouseEnter(Sender: TObject);
Begin
  InfoImg.Picture.LoadFromFile(SkinData.info.MouseOver);
End;

Procedure Tplaywin.InfoImgMouseLeave(Sender: TObject);
Begin
  InfoImg.Picture.LoadFromFile(SkinData.info.Img);
End;

Procedure Tplaywin.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin

End;

Procedure Tplaywin.MenuItem2Click(Sender: TObject);
Begin
  main.MenuItem27Click(Nil);
End;

Procedure Tplaywin.MenuItem3Click(Sender: TObject);
Begin
  main.save_listClick(Nil);
End;

Procedure Tplaywin.Image1Click(Sender: TObject);
Begin
End;

Procedure Tplaywin.Image1Paint(Sender: TObject);
Begin

End;

Procedure Tplaywin.MuteImgClick(Sender: TObject);
Begin
  main.muteClick(Nil);
End;

Procedure Tplaywin.MuteImgMouseEnter(Sender: TObject);
Begin
  MuteImg.Picture.LoadFromFile(SkinData.mute.MouseOver);
End;

Procedure Tplaywin.NextImgClick(Sender: TObject);
Begin
  main.nextClick(Nil);
End;

Procedure Tplaywin.NextImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
Begin
  NextImg.Picture.LoadFromFile(SkinData.Pnext.Clicked);
End;

Procedure Tplaywin.NextImgMouseEnter(Sender: TObject);
Begin
  NextImg.Picture.LoadFromFile(SkinData.Pnext.MouseOver);
End;

Procedure Tplaywin.NextImgMouseLeave(Sender: TObject);
Begin
  NextImg.Picture.LoadFromFile(SkinData.Pnext.Img);
End;

Procedure Tplaywin.NextImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
Begin
  NextImg.Picture.LoadFromFile(SkinData.Pnext.MouseOver);
End;

Procedure Tplaywin.OpenImgClick(Sender: TObject);
Begin
  main.openfileClick(Nil);
End;

Procedure Tplaywin.PauseImgClick(Sender: TObject);
Begin
  main.pauseClick(Nil);
End;

Procedure Tplaywin.PauseImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                     Shift: TShiftState; X, Y: Integer);
Begin
  PauseImg.Picture.LoadFromFile(SkinData.Ppause.Clicked);
End;

Procedure Tplaywin.PauseImgMouseEnter(Sender: TObject);
Begin
  PauseImg.Picture.LoadFromFile(SkinData.Ppause.MouseOver);
End;

Procedure Tplaywin.PauseImgMouseLeave(Sender: TObject);
Begin
  PauseImg.Picture.LoadFromFile(SkinData.Ppause.Img);
End;

Procedure Tplaywin.PauseImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                   Shift: TShiftState; X, Y: Integer);
Begin
  PauseImg.Picture.LoadFromFile(SkinData.Ppause.MouseOver);
End;

Procedure Tplaywin.PlayImgClick(Sender: TObject);
Begin
  main.playClick(Nil);
End;

Procedure Tplaywin.PlayImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
Begin
  PlayImg.Picture.LoadFromFile(SkinData.Pplay.Clicked);
End;

Procedure Tplaywin.PlayImgMouseEnter(Sender: TObject);
Begin
  PlayImg.Picture.LoadFromFile(SkinData.Pplay.MouseOver);
End;

Procedure Tplaywin.PlayImgMouseLeave(Sender: TObject);
Begin
  PlayImg.Picture.LoadFromFile(SkinData.Pplay.Img);
End;

Procedure Tplaywin.PlayImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
Begin
  PlayImg.Picture.LoadFromFile(SkinData.Pplay.MouseOver);
End;

Procedure Tplaywin.StopImgClick(Sender: TObject);
Begin
  main.stopClick(Nil);
End;

Procedure Tplaywin.StopImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
Begin
  StopImg.Picture.LoadFromFile(SkinData.Pstop.Clicked);
End;

Procedure Tplaywin.StopImgMouseEnter(Sender: TObject);
Begin
  StopImg.Picture.LoadFromFile(SkinData.Pstop.MouseOver);
End;

Procedure Tplaywin.StopImgMouseLeave(Sender: TObject);
Begin
  StopImg.Picture.LoadFromFile(SkinData.Pstop.Img);
End;

Procedure Tplaywin.StopImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
Begin
  StopImg.Picture.LoadFromFile(SkinData.Pstop.MouseOver);
End;

Procedure Tplaywin.ViewImgClick(Sender: TObject);
Begin
  main.player_libClick(Nil);
End;

Procedure Tplaywin.ViewImgMouseEnter(Sender: TObject);
Begin
  ViewImg.Picture.LoadFromFile(SkinData.view.MouseOver);
End;

Procedure Tplaywin.ViewImgMouseLeave(Sender: TObject);
Begin
  ViewImg.Picture.LoadFromFile(SkinData.view.Img);
End;

Procedure Tplaywin.backImgClick(Sender: TObject);
Begin
  main.prevClick(Nil);
End;

Procedure Tplaywin.backImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
Begin
  backImg.Picture.LoadFromFile(SkinData.Pprevious.Clicked);
End;

Procedure Tplaywin.backImgMouseEnter(Sender: TObject);
Begin
  backImg.Picture.LoadFromFile(SkinData.Pprevious.MouseOver);
End;

Procedure Tplaywin.backImgMouseLeave(Sender: TObject);
Begin
  backImg.Picture.LoadFromFile(SkinData.Pprevious.Img);
End;

Procedure Tplaywin.backImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
Begin
  backImg.Picture.LoadFromFile(SkinData.Pprevious.MouseOver);
End;

Procedure Tplaywin.r(Sender: TObject);
Begin

End;

Procedure Tplaywin.MuteImgMouseLeave(Sender: TObject);
Begin
  MuteImg.Picture.LoadFromFile(SkinData.mute.Img);
End;

Procedure Tplaywin.OpenImgMouseEnter(Sender: TObject);
Begin
  OpenImg.Picture.LoadFromFile(SkinData.open.MouseOver);
End;

Procedure Tplaywin.OpenImgMouseLeave(Sender: TObject);
Begin
  OpenImg.Picture.LoadFromFile(SkinData.open.Img);
End;

Procedure Tplaywin.SpeedButton1Click(Sender: TObject);
Begin
  main.nextClick(Nil);
End;

Procedure Tplaywin.SpeedButton2Click(Sender: TObject);
Begin

End;

Procedure Tplaywin.file_infoClick(Sender: TObject);
Begin
  If (PlayerObj.CurrentTrack)>=0 Then
    Begin
      main.playlist.selected := main.playlist.Items[PlayerObj.CurrentTrack-1];
      Main.MenuItem10Click(Nil);
    End;
End;

Procedure Tplaywin.openClick(Sender: TObject);
Begin
  Main.openfileClick(Nil);
End;

Procedure Tplaywin.pauseClick(Sender: TObject);
Begin
  main.pauseClick(Nil);
  BackgroundImg.Canvas.Refresh;
End;

Procedure Tplaywin.playClick(Sender: TObject);
Begin
  Main.playClick(Nil);
End;

Procedure Tplaywin.playwinClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin


{  BackgroundImg.Free;
  timeimage.Free;
  titleimg1.Free;
  titleimg2.Free;
  trackbar.Free;
  timeimage.Free;}
  AlbumCoverImg.Free;

  If (main<>Nil) And main.playermode Then
    Begin
      main.Close;
    End;
End;

Procedure Tplaywin.playwinCreate(Sender: TObject);
Begin
  With SkinData Do
    Begin
      BackgroundImg.canvas.Font.Color := CLRED;

      BackgroundImg.AutoSize := true;
      BackgroundImg.Picture.LoadFromFile(SkinData.Background.Img);

      ToolbarImg.Top := Toolbar.y;
      ToolbarImg.Left := Toolbar.x;
      ToolbarImg.Picture.LoadFromFile(SkinData.Toolbar.Img);
      ToolbarImg.AutoSize := true;

      TrackbarImg.Top := Trackbar.y;
      TrackbarImg.Left := Trackbar.x;
      TrackbarImg.Picture.LoadFromFile(SkinData.Trackbar.Img);
      TrackbarImg.AutoSize := true;

      TimeImg.Top := Time.y;
      TimeImg.Left := Time.x;
      TimeImg.Picture.LoadFromFile(SkinData.Time.Img);
      TimeImg.AutoSize := true;

      TitleImg.Top := Title.y;
      TitleImg.Left := Title.x;
      TitleImg.Picture.LoadFromFile(SkinData.Title.Img);
      TitleImg.AutoSize := true;

      AlbumCoverImg.top := Title.y+3;
      AlbumCoverImg.left := Title.x+ TitleImg.Picture.Width-55;
      AlbumCoverImg.Width := TitleImg.Picture.Height-6;
      AlbumCoverImg.Height := TitleImg.Picture.Height-6;
      AlbumCoverImg.Stretch := true;

      textBackImg.Top := TitleBack.y;
      textBackImg.Left := TitleBack.x;
      textBackImg.Picture.LoadFromFile(SkinData.TitleBack.Img);
      textBackImg.AutoSize := true;

      MuteImg.Top := mute.y;
      MuteImg.Left := mute.x;
      MuteImg.Picture.LoadFromFile(SkinData.mute.Img);
      MuteImg.AutoSize := true;

      ViewImg.Top := view.y;
      ViewImg.Left := view.x;
      ViewImg.Picture.LoadFromFile(SkinData.view.Img);
      ViewImg.AutoSize := true;

      OpenImg.Top := Open.y;
      OpenImg.Left := open.x;
      OpenImg.Picture.LoadFromFile(SkinData.open.Img);
      OpenImg.AutoSize := true;

      InfoImg.Top := info.y;
      InfoImg.Left := info.x;
      InfoImg.Picture.LoadFromFile(SkinData.info.Img);
      InfoImg.AutoSize := true;

      PlayImg.Top := Pplay.y;
      PlayImg.Left := Pplay.x;
      PlayImg.AutoSize := true;
      PlayImg.Picture.LoadFromFile(Pplay.Img);

      PauseImg.Top := Ppause.y;
      PauseImg.Left := Ppause.x;
      PauseImg.AutoSize := true;
      PauseImg.Picture.LoadFromFile(Ppause.Img);

      StopImg.Top := Pstop.y;
      StopImg.Left := Pstop.x;
      StopImg.AutoSize := true;
      StopImg.Picture.LoadFromFile(PStop.Img);

      NextImg.Top := Pnext.y;
      NextImg.Left := Pnext.x;
      NextImg.AutoSize := true;
      NextImg.Picture.LoadFromFile(Pnext.Img);

      backImg.Top := Pprevious.y;
      backImg.Left := Pprevious.x;
      backImg.AutoSize := true;
      backImg.Picture.LoadFromFile(Previous.Img);

      AutoSize := true;
      DoubleBuffered := true;
    End;
End;

Procedure Tplaywin.playwinKeyDown(Sender: TObject; Var Key: Word;
                                  Shift: TShiftState);
Begin
  writeln(key);
  If key=113 Then Main.player_libClick(Nil);
  If key = 17 Then strg := true;
  If (strg=true) And (key=78) Then Main.nextClick(Nil);
  If (key=32) Or ((strg=true) And (key=80)) Then If PlayerObj.playing Then pauseClick(Nil)
  Else Main.playClick(Nil);
  If (key=77) Or ((strg=true) And (key=77)) Then Main.muteClick(Nil);
  If (strg=true) And (key=66) Then Main.prevClick(Nil);
End;

Procedure Tplaywin.playwinKeyUp(Sender: TObject; Var Key: Word;
                                Shift: TShiftState);
Begin
  If key=17 Then strg := false;
End;

Procedure Tplaywin.prevClick(Sender: TObject);
Begin
  main.prevClick(Nil);
End;

Procedure Tplaywin.stopClick(Sender: TObject);
Begin
  main.stopClick(Nil);

{  playwin.titleimg1.Picture.LoadFromFile(SKIN_DIR+'title.bmp');
  playwin.titleimg2.Picture.LoadFromFile(SKIN_DIR+'title.bmp');
  playwin.trackbar.Picture.LoadFromFile(SKIN_DIR+'trackbar.bmp');
  playwin.timeimage.Picture.LoadFromFile(SKIN_DIR+'time.bmp');}
End;

Procedure Tplaywin.toggle_viewClick(Sender: TObject);
Begin
  main.player_libClick(Nil);
End;

Procedure Tplaywin.trackbarMouseDown(Sender: TOBject; Button: TMouseButton;
                                     Shift: TShiftState; X, Y: Integer);

Var spos,slength: integer;
Begin
  slength := PlayerObj.get_filelength;
  spos := (x*slength) Div (200);
  PlayerObj.set_fileposition(spos);

End;

Procedure Tplaywin.draw_artist(a: String);
Begin

{titleimg1.Picture.LoadFromFile(SKIN_DIR+'title.bmp');
   titleimg1.canvas.Font.Color:=Clnavy;
   titleimg1.canvas.textout(5,5,a);}

End;

Procedure Tplaywin.draw_title(t: String);
Begin

{titleimg2.Picture.LoadFromFile(SKIN_DIR+'title.bmp');
   titleimg2.canvas.Font.Color:=Clnavy;
   titleimg2.canvas.textout(5,5,t);}
End;


initialization
  {$I player.lrs}

End.
