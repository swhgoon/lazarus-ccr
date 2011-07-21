
Unit cdrip;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
StdCtrls, Buttons, ExtCtrls, cddb, dos, Grids, DBCtrls, process;


resourcestring
rsEncodeToMp3 = 'Encode to mp3';
rsQuerryCDDB = 'Query CDDB';
rsLoad = 'Load';
rsEject = 'Eject';
rsStart = 'Start';
rsBack = 'Back';
rsSetID3Tag = 'Write ID3-Tags';
rsCrSubfolders = 'Create artist subfolders';
rsOutfileNamin = 'Outfile naming scheme';

Type 

  { Tcdrip }

  Tcdrip = Class(TForm)
    bitratebox: TComboBox;
    FileNameType: TComboBox;
    LNameScheme: TLabel;
    paranoia: TCheckBox;
    startbtn: TButton;
    backbtn: TButton;
    querrybtn: TButton;
    ejectbtn: TButton;
    loadbtn: TButton;
    browsedirectorybtn: TButton;
    encodecheck: TCheckBox;
    writetagscheck: TCheckBox;
    artistedit: TEdit;
    albumedit: TEdit;
    subfoldercheck: TCheckBox;
    drivebox: TComboBox;
    outputfolderbox: TComboBox;
    Label1: TLabel;
    LArtist: TLabel;
    LAlbum: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Trackgrid: TStringGrid;
    Panel1: TPanel;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    procedure browsedirectorybtnClick(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure TrackgridHeaderClick(Sender: TObject; IsColumn: Boolean;
                                   index: Integer);
    Procedure TrackgridMouseDown(Sender: TOBject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
    Procedure TrackgridSelectCell(Sender: TObject; Col, Row: Integer;
                                  Var CanSelect: Boolean);
    Procedure encodecheckChange(Sender: TObject);
    Procedure startButClick(Sender: TObject);
    Private 
    { private declarations }
    Outputstring: TStringlist;
    Outputstream: TMemoryStream;
    RipProcess, EncodeProcess: TProcess;
    Public 
    { public declarations }
    ToRemove, ToRip, ToEncode: array[1..100] Of boolean;
    OutFileNames: array[1..100] Of string;
    RipTrack, EncodeTrack: byte;
    ripping, encoding: boolean;
    outfolder: string;
    CDDBcon: TCddbObject;
  End;

Var 
  cdripwin: Tcdrip;


  Implementation

  Uses mainform, mediacol, translations, functions, config;


  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{ Tcdrip }

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.Button1Click(Sender: TObject);
Begin
  Button5Click(Nil);
  If CDDBcon=Nil Then
    CDDBcon := TCddbObject.create
  Else
    Begin
      Timer1.Enabled := false;
      CDDBcon.destroy;
      CDDBcon := TCddbObject.create;
    End;
  If CDDBcon.ReadTOC(drivebox.Text) Then
    Begin
      CDDBcon.query(drivebox.Text, 'freedb.org', 8880);
      Timer1.Enabled := true;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.Button2Click(Sender: TObject);
Begin
  close;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.Button4Click(Sender: TObject);
Begin
{$ifdef linux}
  Exec('/usr/bin/eject',CDDBcon.CDromDrives[drivebox.ItemIndex+1]);
{$endif linux}
{$ifdef win32}
  Exec('eject.exe',CDDBcon.CDromDrives[drivebox.ItemIndex+1]);
{$endif win32}
  writeln('ATTENTION!! DIRTY! ejecting cdrom drive... ');
  Trackgrid.Clean([gzNormal, gzFixedCols]);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.Button5Click(Sender: TObject);

Var b, z: byte;
  tmps, t1, t2: string;
  ti1, ti2: integer;
Begin
  Trackgrid.Clean([gzNormal, gzFixedCols]);
  If CDDBcon.ReadTOC(drivebox.Text) Then
    Begin
      artistedit.Text := 'Unknown';
      albumedit.Text := 'Unknown';
      Trackgrid.RowCount := 1;
      Trackgrid.ColWidths[0] := 20;
      For b := 1 To CDDBcon.NrTracks Do
        Begin
          str(b, tmps);
          Trackgrid.RowCount := Trackgrid.RowCount+1;
          Trackgrid.Cells[0,b] := tmps;
          Trackgrid.Cells[1,b] := 'Track '+tmps;
          z := b;
          ti1 := (CDDBcon.TOCEntries[z+1].min)-(CDDBcon.TOCEntries[z].min);
          ti2 := (CDDBcon.TOCEntries[z+1].sec)-(CDDBcon.TOCEntries[z].sec);
          If ti2<0 Then dec(ti1);
          ti2 := abs(ti2);
          str(ti1, t1);
          str(ti2, t2);
          If ti2<10 Then t2 := '0'+t2;
          Trackgrid.Cells[3,b] := t1+':'+t2;
        End;
    End;
  Timer1.Enabled := true;
End;

procedure Tcdrip.browsedirectorybtnClick(Sender: TObject);
begin
  if DirectoryExists(outputfolderbox.Text) then SelectDirectoryDialog1.FileName := outputfolderbox.Text;
  if SelectDirectoryDialog1.Execute Then
     begin
       if outputfolderbox.Items.IndexOf(SelectDirectoryDialog1.FileName) < 0 then outputfolderbox.Items.Insert(0, SelectDirectoryDialog1.FileName);
       outputfolderbox.ItemIndex := outputfolderbox.Items.IndexOf(SelectDirectoryDialog1.FileName);
     end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.CheckBox1Change(Sender: TObject);
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  Timer1.Enabled := false;
  RipProcess.free;
  EncodeProcess.free;
  Outputstream.free;
  Outputstring.free;
  Timer1.free;
  CDDBcon.destroy;
  main.Enabled := true;
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.FormCreate(Sender: TObject);

Var b, z: byte;
  tmps, t1, t2: string;
  ti1, ti2, i: integer;
Begin
  TranslateUnitResourceStrings('cdrip', cactusconfig.DataPrefix+'languages/cactus.%s.po',
                               CactusConfig.language, copy(CactusConfig.language, 0, 2));

  encodecheck.Caption := rsEncodeToMp3;
  LArtist.Caption := rsArtist;
  LAlbum.Caption := rsAlbum;
  querrybtn.Caption := rsQuerryCDDB;
  loadbtn.Caption := rsLoad;
  ejectbtn.Caption := rsEject;
  startbtn.Caption := rsStart;
  backbtn.Caption := rsBack;
  writetagscheck.Caption := rsSetID3Tag;
  subfoldercheck.Caption := rsCrSubfolders;
  LNameScheme.Caption := rsOutfileNamin;



  ripping := false;
  RipProcess := TProcess.Create(Nil);
  EncodeProcess := TProcess.Create(Nil);
  Outputstring := TStringList.Create;
  Outputstream := TMemoryStream.Create;

  For i:= 0 To MediaCollection.DirList.Count-1 Do
    //
    Begin
      outputfolderbox.AddItem(MediaCollection.DirList[i], Nil);
    End;

  outputfolderbox.ItemIndex := 0;
  CDDBcon := TCddbObject.create;
  If (CDDBcon.DriveCount>0) Then
    Begin
      For b:=1 To CDDBcon.DriveCount Do
        drivebox.AddItem(CDDBcon.CDromDrives[b], Nil);
      drivebox.ItemIndex := 0;
      If CDDBcon.ReadTOC(CDDBcon.CDromDrives[drivebox.ItemIndex+1]) Then
        Begin
          artistedit.Text := 'Unknown';
          albumedit.Text := 'Unknown';
          Trackgrid.Clean([gzNormal, gzFixedCols]);
          Trackgrid.Cells[1,0] := 'Title';
          Trackgrid.Cells[2,0] := 'Rip';
          Trackgrid.Cells[3,0] := 'Length';
          Trackgrid.RowCount := 1;
          Trackgrid.ColWidths[0] := 20;
          For b := 1 To CDDBcon.NrTracks Do
            Begin
              str(b, tmps);
              Trackgrid.RowCount := Trackgrid.RowCount+1;
              Trackgrid.Cells[0,b] := tmps;
              Trackgrid.Cells[1,b] := 'Track '+tmps;
              z := b;
              ti1 := (CDDBcon.TOCEntries[z+1].min)-(CDDBcon.TOCEntries[z].min);
              ti2 := (CDDBcon.TOCEntries[z+1].sec)-(CDDBcon.TOCEntries[z].sec);
              If ti2<0 Then dec(ti1);
              ti2 := abs(ti2);
              str(ti1, t1);
              str(ti2, t2);
              If ti2<10 Then t2 := '0'+t2;
              Trackgrid.Cells[3,b] := t1+':'+t2;
            End;
        End;
      Timer1.Enabled := true;

    End
  Else
    Begin
      ShowMessage('No CDROM-Drive found on this computer');

    End;



End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


{This timerevent is too confusing, big and buggy. split in 2 timer objects -> one for IP communication and one for rip/encode status }

Procedure Tcdrip.Timer1Timer(Sender: TObject);

Var
  b: byte;
  tmps: string;
  ti1, ti2: integer;
  t1,t2, EncPercent: string;
  i: integer;
Begin

  If (ripping=false) And (encoding=false) Then
    Begin
      CDDBcon.callevents;
      If CDDBcon.data_ready Then
        Begin
          artistedit.Text := CDDBcon.artist;
          albumedit.Text := CDDBcon.album;
          Trackgrid.Clean([gzNormal, gzFixedCols]);
          Trackgrid.RowCount := 1;
          Trackgrid.ColWidths[0] := 20;
          For i:= 1 To length(CDDBcon.album) Do
            Begin
              write(byte(CDDBcon.album[i]));
              write('-');
            End;
          writeln;
          For b:= 1 To CDDBcon.NrTracks Do
            Begin
              str(b, tmps);
              Trackgrid.RowCount := Trackgrid.RowCount+1;
              Trackgrid.Cells[0,b] := tmps;
              Trackgrid.Cells[1,b] := CDDBcon.title[b];
              ti1 := (CDDBcon.TOCEntries[b+1].min)-(CDDBcon.TOCEntries[b].min);
              ti2 := (CDDBcon.TOCEntries[b+1].sec)-(CDDBcon.TOCEntries[b].sec);
              If ti2<0 Then dec(ti1);
              ti2 := abs(ti2);
              str(ti1, t1);
              str(ti2, t2);
              If ti2<10 Then t2 := '0'+t2;
              Trackgrid.Cells[3,b] := t1+':'+t2;
            End;
          CDDBcon.data_ready := false;
        End;
    End;
  If ripping And RipProcess.Running Then
    Begin
      Outputstream.Clear;
      Outputstream.SetSize(2048);
      i := (RipProcess.OutPut.Read(Outputstream.Memory^, 2048));
      Outputstream.SetSize(i);
      If i>5 Then
        Begin
          Outputstring.clear;
          Outputstring.LoadFromStream(Outputstream);
          for i:=0 to Outputstring.Count-1 do begin
              tmps := copy(Outputstring.Strings[i], pos('%', Outputstring.Strings[i])-3, 3);
              if tmps<>'' then EncPercent:=tmps;
          end;

          Trackgrid.Cells[2,RipTrack] := EncPercent+'%';

        End;
    End;
  If (ripping) And (RipProcess.Running=false) Then
    Begin
      If ToEncode[RipTrack] Then
        Begin
          encoding := true;
          EncodeTrack := RipTrack;
          str(EncodeTrack, tmps);
          If EncodeTrack<10 Then tmps := '0'+tmps;
          If FileNameType.ItemIndex=0 Then OutFileNames[EncodeTrack] := tmps+' - '+
                                                                        artistedit.Text+' - '+
                                                                        Trackgrid.Cells[1,
                                                                        EncodeTrack]+'.mp3';
          If FileNameType.ItemIndex=1 Then OutFileNames[EncodeTrack] := artistedit.
                                                                        Text+' - '+Trackgrid.Cells[1
                                                                        , EncodeTrack]+'.mp3';
          If FileNameType.ItemIndex=2 Then OutFileNames[EncodeTrack] := artistedit.
                                                                        Text+' - '+inttostr(
                                                                        EncodeTrack)+' - '+Trackgrid
                                                                        .Cells[1, EncodeTrack]+
                                                                        '.mp3';
          OutFileNames[EncodeTrack] := StringReplace(OutFileNames[EncodeTrack], '/', '_', [rfReplaceAll]);
          OutFileNames[EncodeTrack] := StringReplace(OutFileNames[EncodeTrack], '\', '_', [rfReplaceAll]);
          OutFileNames[EncodeTrack] := IncludeTrailingPathDelimiter(outfolder)+OutFileNames[EncodeTrack];
          OutFileNames[EncodeTrack] := StringReplace(OutFileNames[EncodeTrack], #39, '', [
                                       rfReplaceAll]);
          writeln(OutFileNames[EncodeTrack]);
          EncodeProcess.CommandLine := '/usr/bin/lame -h -b'+bitratebox.Items[bitratebox.ItemIndex]+
                                       ' --tt "'+UTF8toLatin1(Trackgrid.Cells[1, EncodeTrack])+
                                       '" --ta "'+UTF8toLatin1(artistedit.Text)+'" --tl "'+
                                       UTF8toLatin1(albumedit.Text)+'" --tn '+tmps+' "'+outfolder+
                                       '/Track'+tmps+'.wav"'+' "'+OutFileNames[EncodeTrack]+'"';
          writeln(EncodeProcess.CommandLine);
          Caption := 'Encoding Track '+inttostr(EncodeTrack)+' ...';
          EncodeProcess.Options := [poUsePipes, poStderrToOutPut];
          EncodeProcess.Execute;
          encoding := true;
          ripping := false;
          Timer1.Enabled := true;
        End
      Else
        Begin
          encoding := false;
          i := 0;
          str(Riptrack, tmps);
          If RipTrack<10 Then tmps := '0'+tmps;
          If ToRemove[RipTrack] Then
            Begin
              DeleteFile(outfolder+'/Track'+tmps+'.wav');
              DeleteFile(outfolder+'/Track'+tmps+'.inf');
              writeln('delete '+outfolder+'/Track'+tmps+'.wav');
            End;
          Repeat
            inc(i)
          Until (ToRip[i]=true) Or (i>CDDBcon.NrTracks);


          If i<=CDDBcon.NrTracks Then
            Begin
              Trackgrid.Cells[2,i] := '0%';
              ToRip[i] := false;
              ToRemove[RipTrack] := false;
              Trackgrid.Cells[2,RipTrack] := '100%';
              RipTrack := i;
              str(i, tmps);
              If i<10 Then tmps := '0'+tmps;
              If paranoia.Checked Then
                RipProcess.CommandLine := '/usr/bin/cdda2wav -paranoia -D'+CDDBcon.Device+' -t '+
                                          tmps+' '''+outfolder+'/Track'+tmps+'.wav'''
              Else
                RipProcess.CommandLine := '/usr/bin/cdda2wav -D'+CDDBcon.Device+' -t '+tmps+' '''+
                                          outfolder+'/Track'+tmps+'.wav''';
              RipProcess.Options := [poUsePipes,poStderrToOutPut, poDefaultErrorMode];
              Caption := 'Ripping Track '+tmps+' ...';
              writeln('Ripping Track '+tmps);
              RipProcess.Execute;
              Timer1.Enabled := true;
            End
          Else
            Begin
              writeln('Finished all tracks');
              Trackgrid.Cells[2,RipTrack] := '100%';
              Caption := 'CD Rip... < Finished >';
              Trackgrid.Options:= Trackgrid.Options + [goEditing];
              Timer1.Enabled := false;
              main.update_artist_view;
              update_title_view;
              ripping := false;
              encoding := false;
              ShowMessage('Ripping and encoding finished');
              MediaCollection.SaveToFile;
            End;
        End;
    End;
  If encoding And EncodeProcess.Running And ToEncode[RipTrack] Then
    Begin
      Outputstream.Clear;
      Outputstream.SetSize(1024);
      i := (EncodeProcess.OutPut.Read(Outputstream.Memory^, 1024));
      Outputstream.SetSize(i);
      If i>0 Then
        Begin
          Outputstring.clear;
          Outputstring.LoadFromStream(Outputstream);
          for i:=0 to Outputstring.Count-1 do begin
              tmps := copy(Outputstring.Strings[i], pos('%', Outputstring.Strings[i])-2, 2);
              if tmps<>'' then EncPercent:=tmps;
          end;
          writeln(EncPercent);
          Trackgrid.Cells[2,EncodeTrack] := EncPercent+'%';
          Application.ProcessMessages;
        End;
    End;

  If encoding And (EncodeProcess.Running=false) {and (ToEncode[RipTrack]=false)} Then
    Begin
      ripping := true;
      encoding := false;
      ToEncode[RipTrack] := false;
      writeln('adding file');
      MediaCollection.add(OutFileNames[EncodeTrack]);
    End;
End;

Procedure Tcdrip.TrackgridHeaderClick(Sender: TObject; IsColumn: Boolean;
                                      index: Integer);

Var row: integer;
Begin
  If index = 2 Then
    Begin
      For row:=1 To Trackgrid.RowCount-1 Do
        If Trackgrid.Cells[2, row]='' Then Trackgrid.Cells[2, row] := 'x'
        Else Trackgrid.Cells[2, row] := '';
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.TrackgridMouseDown(Sender: TOBject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.TrackgridSelectCell(Sender: TObject; Col, Row: Integer;
                                     Var CanSelect: Boolean);
Begin
  If col=2 Then If Trackgrid.Cells[2, row]='' Then Trackgrid.Cells[2, row] := 'x'
  Else Trackgrid.Cells[2, row] := '';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.encodecheckChange(Sender: TObject);
Begin
  If EncodeCheck.Checked Then
    Begin
      writetagscheck.Enabled := true;
      subfoldercheck.Enabled := true;
      bitratebox.enabled := true;
    End
  Else
    Begin
      writetagscheck.Enabled := false;
      subfoldercheck.Enabled := false;
      bitratebox.enabled := false;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tcdrip.startButClick(Sender: TObject);

Var row, i: integer;
  tmps : string;
Begin
  outfolder := outputfolderbox.Items[outputfolderbox.ItemIndex];
  If subfoldercheck.Checked Then outfolder := outfolder+DirectorySeparator+artistedit.Text;
  If DirectoryExists(outfolder)=false Then mkdir(outfolder);
  For i:= 1 To CDDBcon.NrTracks Do
    Begin
      If Trackgrid.Cells[2,i]='x' Then ToRip[i] := true;
    End;
  i := 0;
  If encodecheck.Checked Then
     begin
       ToEncode := ToRip;
       ToRemove := ToRip;
     end;
  Repeat
    inc(i)
  Until (ToRip[i]=true) Or (i>CDDBcon.NrTracks);
  If i<=CDDBcon.NrTracks Then
    Begin
      If FileExists('/usr/bin/cdda2wav') Then
        Begin {NOT PORTABLE!!!}
          Trackgrid.Options:= Trackgrid.Options - [goEditing];
         // Trackgrid.Enabled := false;
          Trackgrid.Cells[2,i] := '0%';
          ToRip[i] := false;
          RipTrack := i;
          str(i, tmps);
          If i<10 Then tmps := '0'+tmps;

          If paranoia.Checked Then
            RipProcess.CommandLine := '/usr/bin/cdda2wav -paranoia -D'+CDDBcon.Device+' -t '+tmps+
                                      ' "'+outfolder+'/Track'+tmps+'.wav"'
          Else
            RipProcess.CommandLine := '/usr/bin/cdda2wav -D'+CDDBcon.Device+' -t '+tmps+' "'+
                                      outfolder+'/Track'+tmps+'.wav"';
          RipProcess.Options := [poUsePipes, poStderrToOutPut, poDefaultErrorMode];
          writeln('Ripping Track '+tmps);
          Caption := 'Ripping Track '+tmps+' ...';
          RipProcess.Execute;
          Timer1.Enabled := true;
          ripping := true;
        End
      Else ShowMessage(
                    'ERROR: cdda2wav executable not found. Please install cdda2wav package first...'
        );
    End
  Else If MessageDlg('No tracks selected. Rip complete disc?', mtWarning, mbOKCancel, 0)=mrOK Then
         Begin
           For row:=1 To Trackgrid.RowCount-1 Do
             If Trackgrid.Cells[2, row]='' Then Trackgrid.Cells[2, row] := 'x'
             Else Trackgrid.Cells[2, row] := '';
           startButClick(Nil);
         End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

initialization
  {$I cdrip.lrs}

End.
