
Unit editid3;


{

Edit/show file info dialog for Cactus Jukebox

written by Sebastian Kraft, <c> 2006-2008
Contact the author at: sebastian_kraft@gmx.de
This Software is published under the GPL

}





{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  {ExtCtrls,} Buttons, ComCtrls, lcltype, mediacol, ExtCtrls, skin, last_fm, streamcol,
  settings, debug, mp3file;


Const ALBUM_MODE = 1;

Const ARTIST_MODE = 2;

Const STREAM_MODE = 2;

Type 
  PLabel = ^TLabel;
  PEdit = ^TEdit;

  { TEditID3 }

  TEditID3 = Class(TForm)
    albumedit1: TEdit;
    albumedit2: TEdit;
    albumedit3: TEdit;
    artistedit1: TEdit;
    artistedit2: TEdit;
    artistedit3: TEdit;
    btnOptions: TButton;
    Button1: TButton;
    cancelbut1: TButton;
    cmbYear: TComboBox;
    cmbComment: TComboBox;
    GenreBox: TComboBox;
    commentedit1: TEdit;
    Edit1: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    NameEdit: TEdit;
    URLEdit: TEdit;
    guessname1: TButton;
    Filelogo: TImage;
    AlbumCoverImg: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblTrack: TLabel;
    Label21: TLabel;
    idlabel: TLabel;
    indexlabel: TLabel;
    DescEdit: TMemo;
    mtype: TLabel;
    bitrate: TLabel;
    fsize: TLabel;
    btnReset: TButton;
    srate: TLabel;
    plength: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    savebut1: TButton;
    id3v1tab: TTabSheet;
    fileinfo: TTabSheet;
    PicDownloadTimer: TTimer;
    StreamTab: TTabSheet;
    titleedit2: TEdit;
    titleedit3: TEdit;
    trackedit1: TEdit;
    trackedit2: TEdit;
    yearEdit1: TEdit;
    lblArtist: TLabel;
    lblTitle: TLabel;
    lblAlbum: TLabel;
    lblYear: TLabel;
    lblGenre: TLabel;
    lblComment: TLabel;
    lblPath: TLabel;
    metacontrol: TPageControl;
    pathedit1: TEdit;
    metatab: TTabSheet;
    id3v2tab: TTabSheet;
    titleedit1: TEdit;
    yearEdit2: TEdit;
    yearEdit3: TEdit;
    procedure ac(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure btnResetClick(Sender: TObject);
    Procedure EditID3Close(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure FormHide(Sender: TObject);
    procedure metacontrolChange(Sender: TObject);
    Procedure PicDownloadTimerStartTimer(Sender: TObject);
    Procedure PicDownloadTimerTimer(Sender: TObject);
    Procedure cancelbutClick(Sender: TObject);
    Procedure guessnameClick(Sender: TObject);
    Procedure savebutClick(Sender: TObject);
    Procedure yearEdit1Change(Sender: TObject);
    Procedure cmbYearChange(Sender: TObject);
    Procedure activateEMode(Sender: TObject);
  Private
    { private declarations }
    artist_only, album_only: Boolean;
    timer_loop_count: integer;
    request_send: boolean;
    picrequest_send: boolean;
    LastFMAPI: TLastfmAPIObject;
    bEModeActive: boolean;

    MedFileObj: TMediaFileClass;
    MedColObj: TMediaCollectionClass;
    StreamInfoObj: TStreamInfoItemClass;

    GenreIDtoCBIndex: array[0..255, 0..255] of integer;  //used to translate genre id to checkbox item index

    // Edit-mode specific variable
    ptrControls: array Of array Of ^TControl;
    // ..
    Procedure show_tags();
  Public
    { public declarations }
    fileid: integer;
    Procedure display_window(MedFile:TMediaFileClass; intMode: Integer = 0);
    Procedure display_window(StreamInfo: TStreamInfoItemClass);
  End;

Var 
  EditID3win: TEditID3;

  Implementation

  Uses mainform, config, functions;
{ TEditID3 }

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.savebutClick(Sender: TObject);

Var curartist, newart, oldart, oldalbum, newalbum, strNewYear, strNewComment: string;
  z: integer;
  bYearLongEnough: Boolean;
Begin
  // only save if s.th. has been changed
  If bEModeActive = false
    Then
    Begin
      EditID3win.Hide;
      exit;
    End;

  // check if the file exists and is writable
  If (FileGetAttr(MedFileObj.path)<>faReadOnly)
    Then
    Begin
      // write changes (artist-mode)
      If artist_only=true
        Then
        Begin
          oldart := lowercase(MedFileObj.artist);
          newart := artistedit1.text;
          strNewComment := self.cmbComment.Caption;

          strNewYear := self.cmbYear.Caption;
          If Length(strNewYear) = 4 Then
            bYearLongEnough := true;

          z := MedColObj.getTracks(oldart, MedFileObj.index);
          Repeat
            Begin
              MedFileObj := MedColObj.Items[z];
              DebugOutLn('artist_mode: '+ artistedit1.Text +' #'+ IntToStr(z),3);
              MedFileObj.artist := newart;
              if GenreBox.ItemIndex>=0 then MedFileObj.GenreID:= GenreIDtoCBIndex[0, GenreBox.ItemIndex];
              If bYearLongEnough Then MedColObj.items[z].year := self.cmbYear.Caption;
              MedFileObj.comment := strNewComment;
              MedFileObj.write_tag;
              z := MedColObj.getNext;
            End;
          Until z<0;

        End
        // write changes (album-mode)
      Else If album_only=true
             Then
             Begin
               curartist := lowercase(MedFileObj.artist);
               oldalbum := lowercase(MedFileObj.album);
               newalbum := albumedit1.text;
               newart := artistedit1.text;
               strNewComment := self.cmbComment.Caption;

               strNewYear := self.cmbYear.Caption;
               If Length(strNewYear) = 4 Then
                 bYearLongEnough := true;

               z := MedColObj.getTracks(curartist, oldalbum, MedFileObj.index);

               Repeat
                 Begin
                   MedFileObj := MedColObj.items[z];
                   MedFileObj.album := newalbum;
                   MedFileObj.Artist := newart;
                   If bYearLongEnough Then MedFileObj.year := self.cmbYear.Caption;
                   MedFileObj.comment := strNewComment;
                   if GenreBox.ItemIndex>=0 then MedFileObj.GenreID:= GenreIDtoCBIndex[0, GenreBox.ItemIndex];
                   MedFileObj.write_tag;
                   z := MedColObj.getNext;
                 End;
               Until z<0;

             End
             // write changes (title-mode)
      Else
        Begin
          MedFileObj.artist := artistedit1.text;
          MedFileObj.title := titleedit1.text;
          MedFileObj.album := albumedit1.text;
          MedFileObj.year := yearedit1.text;
          MedFileObj.comment := commentedit1.text;
          MedFileObj.track := trackedit1.text;
          if GenreBox.ItemIndex>=0 then MedFileObj.GenreID:= GenreIDtoCBIndex[0, GenreBox.ItemIndex];

          MedFileObj.write_tag;

          RenameFile(MedFileObj.path, editid3win.pathedit1.text);
          MedFileObj.path := editid3win.pathedit1.text;
        End;


      If main.player_connected Then PlayerCol.SaveToFile(CactusConfig.DAPPath+'cactuslib');

      main.update_artist_view;
      update_title_view;
      main.update_playlist;
    End
  Else
    ShowMessage('Error: File(s) is/are read-only');

  EditID3win.Hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.cancelbutClick(Sender: TObject);
Begin
  EditID3win.Hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.yearEdit1Change(Sender: TObject);
Begin
  // ensure only YYYY (years of four digits) are entered (if anything is entered)
  If self.yearEdit1.Visible = true
    Then
    Begin
      Case Length(self.yearEdit1.Caption) Of 
        0: self.savebut1.Enabled := true;
        4:
           Try
             self.savebut1.Enabled := true;
             StrToInt(self.yearEdit1.Caption);
           Except
             self.savebut1.Enabled := false;
           End;
        otherwise self.savebut1.Enabled := false;
      End;
      activateEMode(Sender);
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.cmbYearChange(Sender: TObject);
Begin
  // ensure only YYYY (years of four digits) are entered (if anything is entered)
  If self.cmbYear.Visible = true
    Then
    Begin
      Case Length(self.cmbYear.Caption) Of 
        0: self.savebut1.Enabled := true;
        4:
           Try
             self.savebut1.Enabled := true;
             StrToInt(self.cmbYear.Caption);
           Except
             self.savebut1.Enabled := false;
           End;
        otherwise self.savebut1.Enabled := false;
      End;
      activateEMode(Sender);
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.activateEMode(Sender: TObject);

Var 
  i, j: Integer;
  ptrLabel: ^TLabel;
  ptrEdit: ^TEdit;
Begin
  // disable all labels
  If bEModeActive = false
    Then
    Begin
      For i := 0 To Length(ptrControls) -1 Do
        Begin
          ptrLabel := PLabel(ptrControls[i,0]);
          ptrLabel^.Enabled := false;
        End;
      bEModeActive := true;
      btnReset.Enabled := true;
    End;

  // enable label if sender (a text-box) belongs to it
  For i := 0 To Length(ptrControls) -1 Do
    Begin
      ptrLabel := PLabel(ptrControls[i,0]);
      For j := 1 To Length(ptrControls[i]) -1 Do
        Begin
          ptrEdit := PEdit(ptrControls[i,j]);
          If ptrEdit^ = Sender Then ptrLabel^.Enabled := true;
        End
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.FormHide(Sender: TObject);
Begin
  // reset form components
  self.guessname1.Enabled := true;
  self.Button1.Enabled := true;

  self.pathedit1.Caption := '';
  self.pathedit1.Enabled := true;
  self.artistedit1.Caption := '';
  self.artistedit1.Enabled := true;
  self.titleedit1.Caption := '';
  self.titleedit1.Enabled := true;
  self.albumedit1.Caption := '';
  self.albumedit1.Enabled := true;
  self.trackedit1.Caption := '';
  self.trackedit1.Enabled := true;
  self.yearEdit1.Caption := '';
  self.yearEdit1.Enabled := true;
  self.commentedit1.Caption := '';
  self.commentedit1.Enabled := true;

  self.AlbumCoverImg.Canvas.Clear;
  self.AlbumCoverImg.Picture.Clear;
  self.PicDownloadTimer.Enabled := false;

  self.cmbYear.Visible := false;
  self.yearEdit1.Visible := true;
  self.cmbComment.Visible := false;
  self.commentedit1.Visible := true;

 // self.ShowInTaskBar := stNever;
End;


procedure TEditID3.metacontrolChange(Sender: TObject);
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.PicDownloadTimerStartTimer(Sender: TObject);
Begin
  timer_loop_count := 0;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.PicDownloadTimerTimer(Sender: TObject);

Begin
  inc(timer_loop_count);
  If (timer_loop_count Mod 8)=0 Then AlbumCoverImg.Canvas.Clear
  Else AlbumCoverImg.Canvas.TextOut(10,10, 'Loading...');
  If (timer_loop_count>20) Then
    Begin
      DebugOutLn('TIMEOUT while loading album cover image from Internet', 2);
      AlbumCoverImg.Canvas.Clear;
      AlbumCoverImg.Canvas.TextOut(10,10, 'No cover found :(');
      LastFMAPI.free;
      picrequest_send := false;
      PicDownloadTimer.Enabled := false;
    End;

  If (picrequest_send) And LastFMAPI.data_ready Then
    Begin
      writeln(MedFileObj.CoverPath);
      AlbumCoverImg.Canvas.Clear;
      Try
        AlbumCoverImg.Picture.LoadFromFile(MedFileObj.CoverPath);
      Except
        DebugOutLn('EXCEPTION', 1);
      End;
      LastFMAPI.free;
      picrequest_send := false;
      PicDownloadTimer.Enabled := false;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.EditID3Close(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  //    Filelogo.free;
  //    AlbumCoverImg.free;
  //   PicDownloadTimer.Enabled:=false;
  //    PicDownloadTimer.Free;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.FormCreate(Sender: TObject);

Var 
  i: integer;
  GenreList, numberlist: TStringlist;
  Aname, Avalue: string;
Begin
  // initialize index of labels and text boxes on form - used for edit-mode
  SetLength(ptrControls, 8);
  For i := 0 To Length(ptrControls) -1 Do
    SetLength(ptrControls[i], 2);

  ptrControls[0,0] := @self.lblPath;
  ptrControls[0,1] := @self.pathedit1;

  ptrControls[1,0] := @self.lblArtist;
  ptrControls[1,1] := @self.artistedit1;

  ptrControls[2,0] := @self.lblTitle;
  ptrControls[2,1] := @self.titleedit1;

  ptrControls[3,0] := @self.lblAlbum;
  ptrControls[3,1] := @self.albumedit1;

  ptrControls[4,0] := @self.lblTrack;
  ptrControls[4,1] := @self.trackedit1;

  ptrControls[5,0] := @self.lblGenre;
  ptrControls[5,1] := @self.GenreBox;

  SetLength(ptrControls[6], 3);
  ptrControls[6,0] := @self.lblComment;
  ptrControls[6,1] := @self.commentedit1;
  ptrControls[6,2] := @self.cmbComment;

  SetLength(ptrControls[7], 3);
  ptrControls[7,0] := @self.lblYear;
  ptrControls[7,1] := @self.yearEdit1;
  ptrControls[7,2] := @self.cmbYear;

  // (FIXME) ressourcestring translations need to be added here
  //TODO: translation for editid3 window

  Icon.LoadFromFile(CactusConfig.DataPrefix+'icon'+DirectorySeparator+'cactus-icon.ico');

  GenreList:=TStringList.Create;
  for i:= 0 to high(ID3Genre) do begin
      GenreList.Add(ID3Genre[i]+'='+IntToStr(i));
  end;
  GenreList.Sorted:=true;

  for i:= 0 to high(ID3Genre) do begin
      GenreList.GetNameValue(i, Aname, Avalue);

      GenreIDtoCBIndex[0,i]:=StrToInt(Avalue);

      GenreIDtoCBIndex[StrToInt(Avalue),0]:=i;

      GenreBox.Items.Add(Aname);
  end;


End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.show_tags();

Var 
  strYears: Array Of String[4];
  strComments: Array Of String;
  bExists: Boolean;
  i, j: Integer;
  ptrLabel: ^TLabel;
Begin
  // reset all labels indicating edit-mode and changes
  GenreBox.ItemIndex:=-1;
  For i := 0 To Length(ptrControls) -1 Do
    Begin
      ptrLabel := PLabel(ptrControls[i,0]);
      ptrLabel^.Enabled := true;
    End;

  // display tags...
  artistedit1.Text := MedFileObj.artist;

  // artist and album(-mode) specific actions
  If (artist_only = true) Or (album_only = true)
    Then
    Begin
      // collect all "years" set for files of the chosen artist/album
      // and display them
      SetLength(strYears, 0);
      For i := 0 To MedColObj.ItemCount -1 Do
        If MedColObj.items[i].artist = MedFileObj.artist
          Then
          Begin
            If album_only = true Then
              If MedColObj.items[i].album <> MedFileObj.album Then continue;
            // ensure "year" is added only once
            bExists := false;
            For j := 0 To Length(strYears) -1 Do
              If strYears[j] = MedColObj.items[i].year
                Then
                Begin
                  bExists := true;
                  break;
                End;
            If bExists = true Then continue;
            // add "year"
            SetLength(strYears, Length(strYears) +1);
            strYears[Length(strYears) -1] := MedColObj.items[i].year;
          End;
      // and display...

      yearEdit1.Visible := false;
      cmbYear.Visible := true;
      cmbYear.Clear;
      For i := 0 To Length(strYears) -1 Do
        cmbYear.Items.Add(strYears[i]);

      // collect all "comments" set for files of the chosen artist/album
      // and display them
      SetLength(strComments, 0);
      For i := 1 To self.MedColObj.ItemCount-1 Do
        If MedColObj.items[i].artist = MedFileObj.artist
          Then
          Begin
            If album_only = true Then
              If MedColObj.items[i].album <> self.MedFileObj.album Then continue;
            // ensure "comment" is added only once
            bExists := false;
            For j := 0 To Length(strComments) -1 Do
              If strComments[j] = MedColObj.items[i].comment
                Then
                Begin
                  bExists := true;
                  break;
                End;
            If bExists = true Then continue;
            // add "comment"
            SetLength(strComments, Length(strComments) +1);
            strComments[Length(strComments) -1] := MedColObj.items[i].comment;
          End;
      // and display...
      commentedit1.Visible := false;
      cmbComment.Visible := true;
      cmbComment.Clear;
      For i := 0 To Length(strComments) -1 Do
        cmbComment.Items.Add(strComments[i]);

      // album(-mode) specific actions
      If album_only = true
        Then
        Begin
          albumedit1.text := MedFileObj.album;
          // select first entry from combobox as default for the album
          If cmbYear.Items.Count > 0 Then cmbYear.ItemIndex := 0;
          If cmbComment.Items.Count > 0 Then cmbComment.ItemIndex := 0;
        End;
    End
    // title(-mode) specific actions
  Else
    Begin
      pathedit1.text := MedFileObj.path;
      //TODO: scroll TEdit to end of path
      titleedit1.text := MedFileObj.title;
      albumedit1.text := MedFileObj.album;
      commentedit1.text := MedFileObj.comment;
      yearedit1.text := MedFileObj.year;
      trackedit1.text := MedFileObj.track;
      GenreBox.ItemIndex:=GenreIDtoCBIndex[MedFileObj.GenreID, 0];
    End;

  btnReset.Enabled := false;
  bEModeActive := false;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.display_window(MedFile: TMediaFileClass; intMode: Integer);
Begin
  // set up gui elements

  metacontrol.Pages[0].TabVisible := true;
  metacontrol.Pages[1].TabVisible := true;
  metacontrol.Pages[2].TabVisible := false;

  metacontrol.ActivePage := metacontrol.Pages[0];

  btnReset.Enabled := true;

  MedFileObj := MedFile;
  MedColObj := MedFileObj.Collection;
  fileid := 0;

  self.artist_only := false;
  self.album_only := false;
  Case intMode Of 
    ALBUM_MODE: self.album_only := true;
    ARTIST_MODE: self.artist_only := true;
  End;

  Button1.Hint := '( ' + CactusConfig.strTagToNameFormatString + ' )';
  Button1.Enabled := true;

  // artist and album(-mode) specific actions
  If (artist_only = true) Or (album_only = true)
    Then
    Begin
      self.guessname1.Enabled := false;
      self.pathedit1.Enabled := false;
      self.titleedit1.Enabled := false;

      self.indexlabel.Caption := 'File-Index: ' + IntToStr(MedFileObj.index);

      // artist(-mode) specific actions
      If artist_only = true
        Then
        self.albumedit1.Enabled := false;

      // album(-mode) specific actions
      If album_only = true
        Then
        Begin
          DebugOutLn('########AlbumCover', 5);
          If MedFileObj.album<>''
            Then
            Begin
              MedFileObj.CoverPath := CactusConfig.ConfigPrefix+DirectorySeparator+'covercache'+
                                      DirectorySeparator+MedFileObj.artist+'_'+MedFileObj.album+
                                      '.jpeg';
              If FileExists(MedFileObj.CoverPath) Then
                Begin
                  Try
                    AlbumCoverImg.Picture.LoadFromFile(MedFileObj.CoverPath);
                  Except
                    writeln('EXCEPTION loading cover file from '+MedFileObj.CoverPath);
                  End;
                End
              Else
                Begin
                  If CactusConfig.CoverDownload
                    Then
                    Begin
                      LastFMAPI := TLastfmAPIObject.Create;
                      if CactusConfig.CoverSize='large' then LastFMAPI.CoverSize:=ExtralargeImage
                            else LastFMAPI.CoverSize:=LargeImage;
                      LastFMAPI.album_downloadCover(MedFileObj.artist, MedFileObj.album, MedFileObj.CoverPath);
                      picrequest_send := true;
                      AlbumCoverImg.Canvas.TextOut(10,10, 'Loading...');
                      PicDownloadTimer.Enabled := true;
                      Application.ProcessMessages;
                    End;
                End;
            End;
        End;
    End
    // title(-mode) specific actions
  Else
    Begin
      self.guessname1.Enabled := true;

      mtype.caption := 'Mediatype:  '+MedFileObj.filetype;
      If MedFileObj.filetype='.mp3' Then
        Filelogo.Picture.LoadFromFile(SkinData.DefaultPath+DirectorySeparator+'icon'+
                                      DirectorySeparator+'mp3_64.png');
      If MedFileObj.filetype='.ogg' Then
        Filelogo.Picture.LoadFromFile(SkinData.DefaultPath+DirectorySeparator+'icon'+
                                      DirectorySeparator+'ogg_64.png');
      If MedFileObj.filetype='.wav' Then
        Filelogo.Picture.LoadFromFile(SkinData.DefaultPath+DirectorySeparator+'icon'+
                                      DirectorySeparator+'wav_64.png');
      If MedFileObj.filetype='.wma' Then
        Filelogo.Picture.LoadFromFile(SkinData.DefaultPath+DirectorySeparator+'icon'+
                                      DirectorySeparator+'wma_64.png');
      If (MedFileObj.filetype='.flac') or (MedFileObj.filetype='.fla') Then
        Filelogo.Picture.LoadFromFile(SkinData.DefaultPath+DirectorySeparator+'icon'+
                                      DirectorySeparator+'flac_64.png');
      plength.caption := 'Length:  '+MedFileObj.playtime;



      fsize.caption := 'Size:  '+ByteToFmtString(MedFileObj.size, 2, 2);
      srate.Caption := 'Samplerate:  ' + IntToStr(MedFileObj.samplerate) + ' Hz';
      bitrate.Caption := 'Bitrate:  ' + IntToStr(MedFileObj.bitrate) + ' kbps';
      idlabel.Caption := 'File-Id: ' + IntToStr(MedFileObj.id);
      indexlabel.Caption := 'File-Index: ' + IntToStr(MedFileObj.index);

      writeln('########AlbumCover');
      // DEBUG-INFO
      If MedFileObj.album<>''
        Then
        Begin
          MedFileObj.CoverPath := CactusConfig.ConfigPrefix+DirectorySeparator+'covercache'+
                                  DirectorySeparator+MedFileObj.artist+'_'+MedFileObj.album+'.jpeg';
          If FileExists(MedFileObj.CoverPath)
            Then
            Begin
              Try
                AlbumCoverImg.Picture.LoadFromFile(MedFileObj.CoverPath);
              Except
                writeln('EXCEPTION');
              End;
            End
          Else
            Begin
              If CactusConfig.CoverDownload
                Then
                Begin
                  LastFMAPI := TLastfmAPIObject.Create;
                  if CactusConfig.CoverSize='large' then LastFMAPI.CoverSize:=ExtralargeImage
                            else LastFMAPI.CoverSize:=LargeImage;
                  LastFMAPI.album_downloadCover(MedFileObj.artist, MedFileObj.album, MedFileObj.CoverPath);
                  picrequest_send := true;
                  AlbumCoverImg.Canvas.TextOut(10,10, 'Loading...');
                  PicDownloadTimer.Enabled := true;
                  Application.ProcessMessages;
                End;
            End;
        End;
    End;

  show_tags();
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.display_window(StreamInfo: TStreamInfoItemClass);

Begin
  StreamInfoObj := StreamInfo;
  metacontrol.Pages[0].TabVisible := false;
  metacontrol.Pages[1].TabVisible := false;
  metacontrol.Pages[2].TabVisible := true;

  metacontrol.ActivePage := metacontrol.Pages[2];

  btnReset.Enabled := true;

  NameEdit.Text := StreamInfoObj.Name;
  URLEdit.Text := StreamInfoObj.URL;
  DescEdit.Text := StreamInfoObj.Description;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.Button1Click(Sender: TObject);

Var
 //s: string;
 z: integer;

Begin
  {
        // write changes (artist-mode)
      If artist_only=true
        Then
        Begin
          oldart := lowercase(MedFileObj.artist);
          newart := artistedit1.text;
          strNewComment := self.cmbComment.Caption;

          strNewYear := self.cmbYear.Caption;
          If Length(strNewYear) = 4 Then
            bYearLongEnough := true;

          z := MedColObj.getTracks(oldart, MedFileObj.index);
          Repeat
            Begin
              MedFileObj := MedColObj.Items[z];
              writeln('artist_mode: '+ artistedit1.Text +' #'+ IntToStr(z));
              // DEBUG-INFO
              MedFileObj.artist := newart;
              If bYearLongEnough Then MedColObj.items[z].year := self.cmbYear.Caption;
              MedFileObj.comment := strNewComment;
              MedFileObj.write_tag;
              z := MedColObj.getNext;
            End;
          Until z<0;

        End
        // write changes (album-mode)
}

  if artist_only = true then     // title-mode
  begin
     z := MedColObj.getTracks(lowercase(MedFileObj.artist), MedFileObj.index);
     repeat
     begin
       MedFileObj := MedColObj.items[z];
       MedFileObj.PathNameFromTag(CactusConfig.strTagToNameFormatString);
       z := MedColObj.getNext;
     End;
     Until z < 0;
  end
  else if album_only = true then    // album-mode
  begin
     z := MedColObj.getTracks(lowercase(MedFileObj.artist), lowercase(MedFileObj.album),
                              MedFileObj.index);
     repeat
     begin
       MedFileObj := MedColObj.items[z];
       MedFileObj.PathNameFromTag(CactusConfig.strTagToNameFormatString);
       z := MedColObj.getNext;
     End;
     Until z < 0;
  end
  else  // title-mode
  begin
    MedFileObj.PathNameFromTag(CactusConfig.strTagToNameFormatString);
    EditID3win.pathedit1.text := MedFileObj.Path;

  end;
            

//  EditID3win.pathedit1.text := s;
End;

procedure TEditID3.btnOptionsClick(Sender: TObject);
Var
  setupwin: TSettings;
begin
  Enabled := false;
  setupwin := Tsettings.Create(Application);
  setupwin.ShowModal(TSETTINGS_SELECT_ID3);
  setupwin.Free;
  Enabled := true;
end;

procedure TEditID3.ac(Sender: TObject);
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.btnResetClick(Sender: TObject);
Begin
  show_tags();
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TEditID3.guessnameClick(Sender: TObject);

Var z: integer;
  tmps: string;
Begin
  tmps := extractfilename(pathedit1.Text);
  If ((tmps[1]<#60) And (tmps[2]<#60) And (tmps[4]=#45)) Then
    Begin
      trackedit1.text := copy(tmps,1,2);
      delete(tmps, 1, 5);
    End;

  z := pos(' - ', tmps)+3;
  If z<>3 Then
    Begin
      titleedit1.text := TrimRight(copy(tmps,z,length(tmps)-z-3));
      artistedit1.text := TrimRight(copy(tmps,1,z-3));
    End
  Else
    Begin
      artistedit1.text := '';
      titleedit1.text := '';
    End;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

initialization
  {$I editid3.lrs}

End.
