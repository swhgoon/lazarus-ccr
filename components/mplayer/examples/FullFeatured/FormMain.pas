Unit FormMain;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, MPlayerCtrl;

Type

  { TfrmMain }

  TfrmMain = Class(TForm)
    btnRunCommand: TButton;
    cboCommand: TComboBox;
    ilTools: TImageList;
    lblPos: TLabel;
    memResults: TMemo;
    MPlayerControl1: TMPlayerControl;
    OpenDialog1: TOpenDialog;
    pnlTrackbar: TPanel;
    pnlPos: TPanel;
    pnlCommands: TPanel;
    pnlFeedback: TPanel;
    pnlVideo: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    tbMain: TToolBar;
    btnLoad: TToolButton;
    btnFrameGrab: TToolButton;
    btnNudgeBack: TToolButton;
    ToolButton2: TToolButton;
    btnPlay: TToolButton;
    btnStop: TToolButton;
    btnPause: TToolButton;
    btnNudgeForward: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    btnRewind: TToolButton;
    btnFWD: TToolButton;
    ToolButton9: TToolButton;
    TrackBarPlaying: TTrackBar;
    TrackBarVolume: TTrackBar;
    Procedure btnLoadClick(Sender: TObject);
    Procedure btnPauseClick(Sender: TObject);
    Procedure btnPlayClick(Sender: TObject);
    Procedure btnRunCommandClick(Sender: TObject);
    Procedure btnStopClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure OnError(ASender: TObject; AStrings: TStringList);
    Procedure OnFeedback(ASender: TObject; AStrings: TStringList);
    Procedure OnPlay(Sender: TObject);
    Procedure OnPlaying(ASender: TObject; APosition: Single);
    Procedure OnStop(Sender: TObject);
    Procedure TrackBarPlayingChange(Sender: TObject);

    Procedure TrackBarPlayingMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    Procedure TrackBarPlayingMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure TrackBarVolumeChange(Sender: TObject);
  Private
    Function GetUpdatingPosition: Boolean;
    Procedure SetUpdatingPosition(AValue: Boolean);
  Private
    FUpdatingPosition: Integer;
    FLastPosition: Integer;

    Property UpdatingPosition: Boolean read GetUpdatingPosition write SetUpdatingPosition;
  End;

Var
  frmMain: TfrmMain;

Implementation

{$R *.lfm}

{ TfrmMain }

Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  FUpdatingPosition := 0;
  FLastPosition := -1;
  TrackBarPlaying.Max := 50;

  MPlayerControl1.Volume := 50;

  {$IFDEF Linux}
  MPlayerControl1.MPlayerPath := '';
  MPlayerControl1.StartParam := '-vo x11 -zoom -fs';
  {$else $IFDEF Windows}
  // Download MPlayer generic for Windows and save under Programm Folder Directory
  // http://sourceforge.net/projects/mplayer-win32/
  MPlayerControl1.MPlayerPath :=
    IncludeTrailingBackslash(ExtractFileDir(Application.ExeName)) + 'mplayer\mplayer.exe';
  MPlayerControl1.StartParam := '-vo gl_nosw';
  //MPlayerControl1.StartParam := '-vo direct3d';
  //MPlayerControl1.StartParam := '-vf screenshot';
  {$ENDIF}
End;

Procedure TfrmMain.btnLoadClick(Sender: TObject);
Begin
  If OpenDialog1.Execute Then
  Begin
    MPlayerControl1.Stop;
    memResults.Lines.Clear;
    MPlayerControl1.Filename := OpenDialog1.Filename;
    MPlayerControl1.Play;
  End;
End;

Procedure TfrmMain.btnPauseClick(Sender: TObject);
Begin
  MPlayerControl1.Paused := Not MPlayerControl1.Paused;
  btnPause.Down := MPlayerControl1.Paused;
End;

Procedure TfrmMain.btnPlayClick(Sender: TObject);
Begin
  MPlayerControl1.Play;
End;

Procedure TfrmMain.btnRunCommandClick(Sender: TObject);
Begin
  memResults.Lines.Add(cboCommand.Text);
  MPlayerControl1.SendMPlayerCommand(cboCommand.Text);
End;

Procedure TfrmMain.btnStopClick(Sender: TObject);
Begin
  MPlayerControl1.Stop;
End;

Procedure TfrmMain.OnFeedback(ASender: TObject; AStrings: TStringList);
Begin
  memResults.Lines.AddStrings(AStrings);

  memResults.SelStart := Length(memResults.Text);
  //memResults.SelLength := 0;
End;

Procedure TfrmMain.OnError(ASender: TObject; AStrings: TStringList);
Var
  i: Integer;
Begin
  For i := 0 To AStrings.Count - 1 Do
    memResults.Lines.Add(' Err: ' + AStrings[i]);
End;

Procedure TfrmMain.OnPlaying(ASender: TObject; APosition: Single);
Begin
  If (MPlayerControl1.Duration <> -1) Then
  Begin
    UpdatingPosition := True;
    Try
      btnPause.Down := MPlayerControl1.Paused;

      TrackBarPlaying.SelEnd := Trunc(TrackBarPlaying.Max * APosition / MPlayerControl1.Duration);
      If ActiveControl <> TrackBarPlaying Then
        TrackBarPlaying.Position := TrackBarPlaying.SelEnd;

      lblPos.Caption := FormatDateTime('nnn:ss', APosition / (24 * 60 * 60)) +
        ' / ' + FormatDateTime('nnn:ss', MPlayerControl1.Duration / (24 * 60 * 60));

      pnlPos.Width := lblPos.Width + 3;

      // Reversed := True doesn't seem to apply for SelStart/SelEnd...
      // TODO: Talk about on Forum/Consider lodging item on Bugtracker...
      TrackBarVolume.SelEnd := TrackBarVolume.Max;
      TrackBarVolume.SelStart := TrackBarVolume.Max - Trunc(TrackBarVolume.Max *
        MPlayerControl1.Volume / 100);

      If ActiveControl <> TrackBarVolume Then
        TrackBarVolume.Position := TrackBarVolume.SelEnd - TrackBarVolume.SelStart;
    Finally
      UpdatingPosition := False;
    End;
  End;
End;

Procedure TfrmMain.TrackBarPlayingChange(Sender: TObject);
Begin
  If (MPlayerControl1.Duration <> -1) And Not UpdatingPosition Then
    If TrackBarPlaying.Position <> FLastPosition Then
    Begin
      MPlayerControl1.Position := MPlayerControl1.Duration * TrackBarPlaying.Position /
        TrackBarPlaying.Max;
      FLastPosition := TrackBarPlaying.Position;
    End;
End;

Procedure TfrmMain.TrackBarPlayingMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  MPlayerControl1.Paused := True;
End;

Procedure TfrmMain.TrackBarPlayingMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  MPlayerControl1.Paused := False;
  Self.ActiveControl := memResults;
End;

Procedure TfrmMain.TrackBarVolumeChange(Sender: TObject);
Begin
  If (TrackBarVolume.Position <> TrackBarVolume.Tag) And Not UpdatingPosition Then
  Begin
    MPlayerControl1.Volume := Trunc(100 * TrackBarVolume.Position / TrackBarVolume.Max);

    TrackBarVolume.Tag := TrackBarVolume.Position;
  End;
End;

Function TfrmMain.GetUpdatingPosition: Boolean;
Begin
  Result := FUpdatingPosition <> 0;
End;

Procedure TfrmMain.SetUpdatingPosition(AValue: Boolean);
Begin
  If AValue Then
    Inc(FUpdatingPosition)
  Else
    Dec(FUpdatingPosition);
End;

Procedure TfrmMain.OnPlay(Sender: TObject);
Begin
  memResults.Lines.Add('OnPlay message received');
  StatusBar1.SimpleText := 'Playing ' + MPlayerControl1.Filename;

  btnStop.Enabled := MPlayerControl1.Running;
  btnPause.Enabled := MPlayerControl1.Running;
End;

Procedure TfrmMain.OnStop(Sender: TObject);
Begin
  If csDestroying In ComponentState Then
    exit;

  memResults.Lines.Add('OnStop message received');
  StatusBar1.SimpleText := '';

  UpdatingPosition := True;
  Try
    TrackBarPlaying.Position := 0;
    TrackBarPlaying.SelStart := 0;
  Finally
    UpdatingPosition := False;
  End;

  btnStop.Enabled := MPlayerControl1.Running;
  btnPause.Enabled := MPlayerControl1.Running;

  lblPos.Caption := '';
End;

End.
