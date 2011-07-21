unit mp3file;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;


Const ID3Genre: array[0..147] of string[32] = ('', 'Classic Rock', 'Country', 'Dance',
       'Disco', 'Funk', 'Grunge',
       'Hip-Hop', 'Jazz', 'Metal', 'New Age', 'Oldies', 'Other', 'Pop', 'R&B',
       'Rap', 'Reggae', 'Rock', 'Techno', 'Industrial', 'Alternative', 'Ska',
       'Death Metal', 'Pranks', 'Soundtrack', 'Euro-Techno', 'Ambient', 'Trip-Hop',
       'Vocal', 'Jazz&Funk', 'Fusion', 'Trance', 'Classical', 'Instrumental',
       'Acid', 'House', 'Game', 'Sound Clip', 'Gospel', 'Noise', 'Alternative Rock',
       'Bass', 'Soul', 'Punk', 'Space', 'Meditative', 'Instrumental Pop',
       'Instrumental Rock', 'Ethnic', 'Gothic', 'Darkwave', 'Techno-Industrial',
       'Electronic', 'Pop-Folk', 'Eurodance', 'Dream', 'Southern Rock', 'Comedy',
       'Cult', 'Gangsta', 'Top 40', 'Christian Rap', 'Pop/Funk', 'Jungle',
       'Native US', 'Cabaret', 'New Wave', 'Psychedelic', 'Rave', 'Showtunes',
       'Trailer', 'Lo-Fi', 'Tribal', 'Acid Punk', 'Acid Jazz', 'Polka', 'Retro',
       'Musical', 'Rock & Roll', 'Hard Rock', 'Folk', 'Folk-Rock', 'National Folk',
       'Swing', 'Fast Fusion', 'Bebob', 'Latin', 'Revival', 'Celtic', 'Bluegrass',
       'Avantgarde', 'Gothic Rock', 'Progressive Rock', 'Psychedelic Rock',
       'Symphonic Rock', 'Slow Rock', 'Big Band', 'Chorus', 'Easy Listening',
       'Acoustic', 'Humour', 'Speech', 'Chanson', 'Opera', 'Chamber Music',
       'Sonata', 'Symphony', 'Booty Bass', 'Primus', 'Porn Groove', 'Satire',
       'Slow Jam', 'Club', 'Tango', 'Samba', 'Folklore', 'Ballad', 'Power Ballad',
       'Rhythmic Soul', 'Freestyle', 'Duet', 'Punk Rock', 'Drum Solo', 'A capella',
       'Euro-House', 'Dance Hall', 'Goa', 'Drum’n’Bass', 'Club-House', 'Hardcore',
       'Terror', 'Indie', 'BritPop', 'Negerpunk', 'Polsk Punk', 'Beat',
       'Christian Gangsta', 'Heavy Metal', 'Black Metal', 'Crossover',
       'Contemporary Christian', 'Christian Rock', 'Merengue', 'Salsa',
       'Thrash Metal', 'Anime', 'JPop', 'SynthPop');

type
    
    { TMP3File }

TMP3File = class
   private
    FArtist, FTitle, FAlbum, FTrack, FYear, FComment, FPlaytime: string;
    FSamplerate, FBitrate, FPlaylength: integer;
    FGenreID: byte;
    FFileName: string;
    procedure ReadHeader;
   public
    constructor create;
    function ReadTag(Filename: string):boolean;
    function WriteTag(Filename: string):boolean;
    property Artist: string read FArtist write FArtist;
    property Album: string read FAlbum write FAlbum;
    property Title: string read FTitle write FTitle;
    property Track: string read FTrack write FTrack;
    property Year: string read FYear write FYear;
    property Comment: string read FComment write FComment;
    property GenreID: byte read FGenreID write FGenreID;
    property Playtime: string read FPlaytime;

    property Playlength: integer read FPlaylength;
    property Bitrate: integer read FBitrate;
    property Samplerate: integer read FSamplerate;

  end;

implementation

uses functions, config;

Const BitratesTable: array[0..15] Of integer = (0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192,
                                               224, 256, 320, 999);
      SampleratesTable: array[0..3] Of integer = (44100, 48000, 32100, 0);

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TMP3File }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TMP3File.ReadHeader;
var FileStream: TFileStream;
    buf: array [0..255] of byte;
    b: byte;
    i, z: integer;
    size: int64;
begin
    FileStream:=TFileStream.Create(FFileName, fmOpenRead);

{calculating playtime}
    FileStream.Seek(0,fsfrombeginning);
    FileStream.Read(buf, SizeOf(buf));
    size:=FileStream.Size;

    i := 0;
    z := 1;
    Repeat
      Begin
        // iterate buf to finde mpeg header start sequenz
        // start sequenz is 1111 1111 1111 10xx = $FF $Fx
        inc(i);
        If i=high(buf)-2 Then
          Begin
            // if reached end of buf, read next part of file to buf
            FileStream.Seek((i*z)-8, fsFromBeginning);
            FileStream.Read(buf,high(buf));
            inc(z);
            i := 1;
          End;
      End;
    Until ((buf[i]=$FF) And ((buf[i+1] Or $3)=$FB)) Or (z>256);
    // until mpgeg header start seuqenz found
    FileStream.Free;


    If (buf[i]=$FF) And ((buf[i+1] Or $3)=$FB) Then
      Begin
        //if header found then do
        b := buf[i+2] shr 4;
        //shift the byte containing the bitrate right by 4 positions
        // bbbb xxxx -> 0000 bbbb
        // b : bitrate bits, x: any other bits
        If b > 15 Then b := 0;
        FBitrate := BitratesTable[b];
        // select bitrate at index b from table bitrates

        b := buf[i+2] And $0F;
        // the same with samplerate byte
        b := b shr 2;
        If b > 3 Then b := 3;
        FSamplerate := SampleratesTable[b];
        // select samplerate at index b from table samplerates
        If FBitrate>0 Then
          Begin
            FPlaylength := round(size / (bitrate*125));
            //calculate playlength from bitrate and samplerate
            FPlaytime := SecondsToFmtStr(FPlaylength);
            // doesn't work with VBR files !
          End;
      End
    Else writeln(FFileName+' -> no valid mpeg header found');
end;

constructor TMP3File.create; 
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TMP3File.ReadTag(Filename: string): boolean; 

Var i, z, tagpos: integer;
  b: byte;
  //TODO: Implement reading as TFilestream to reduce buffering/buffer size
  buf: array[1..2048] Of byte;
  artistv2, albumv2, titlev2, commentv2, yearv2, trackv2: string;
  bufstr: string;
  mp3filehandle: longint;
Begin
  FFileName:=Filename;
  ReadHeader;
  Try
    mp3filehandle := fileopen(Filename, fmOpenRead);

{reading ID3-tags}
    fileseek(mp3filehandle,0,fsfrombeginning);
    fileread(mp3filehandle,buf,high(buf));
    bufstr := '';
    For i:= 1 To high(buf) Do
      If (buf[i]<>0) and (buf[i]<>10) Then bufstr := bufstr+char(buf[i])
      Else bufstr := bufstr+' ';
    // filter #10 and 0, replace by ' '
{id3v2}
    albumv2 := '';
    artistv2 := '';
    titlev2 := '';
    trackv2 := '';
    yearv2 := '';
    If pos('ID3',bufstr)<> 0 Then
      Begin
        i := pos('TPE1',bufstr);
        If i<> 0 Then artistv2 := copy(bufstr,i+11,buf[i+7]-1);
        i := pos('TP1',bufstr);
        If i<> 0 Then artistv2 := copy(bufstr,i+7,buf[i+5]-1);

        i := pos('TIT2',bufstr);
        If i<> 0 Then titlev2 := copy(bufstr,i+11,buf[i+7]-1);

        i := pos('TT2',bufstr);
        If i<> 0 Then titlev2 := copy(bufstr,i+7,buf[i+5]-1);
        i := pos('TRCK',bufstr);
        If i<> 0 Then trackv2 := copy(bufstr,i+11,buf[i+7]-1);

        i := pos('TRK',bufstr);
        If i<> 0 Then trackv2 := copy(bufstr,i+7,buf[i+5]-1);

        If length(trackv2)>3 Then trackv2 := '';

        i := pos('TAL',bufstr);
        If i<> 0 Then albumv2 := copy(bufstr,i+7,buf[i+5]-1);
        i := pos('TALB',bufstr);
        If i<> 0 Then albumv2 := copy(bufstr,i+11,buf[i+7]-1);

        i := pos('TYE',bufstr);
        If i<> 0 Then yearv2 := copy(bufstr,i+7,buf[i+5]-1);

        i := pos('TYER',bufstr);
        If i<> 0 Then yearv2 := copy(bufstr,i+11,buf[i+7]-1);
        artistv2 := (artistv2);
        titlev2 := (titlev2);
        albumv2 := (albumv2);
        yearv2 := (yearv2);
        trackv2 := (trackv2);
        If length(yearv2)>5 Then yearv2 := '';
      End;
   except WriteLn(Filename+' -> exception while reading id3v2 tag... skipped!!'); end;
{id3v1}
   try
    fileseek(mp3filehandle,-128, fsfromend);
    fileread(mp3filehandle,buf,128);
    bufstr := '';
    For i:= 1 To 128 Do bufstr := bufstr+char(buf[i]);

    For i:= 1 To 128 Do
      Begin
        b := byte(bufstr[i]);
        If (b<32) Then bufstr[i] := #32;
        //TODO: This also replaces line breaks!!
        //      Done here because line breaks in tags collapse cactus database
        //      file which uses a fixed linecount for one entry
      End;
    tagpos := pos('TAG',bufstr)+3;
    If tagpos<>3 Then
      Begin
        ftitle := (copy(bufstr,tagpos,30));
        fartist := (copy(bufstr,tagpos+30,30));
        falbum := (copy(bufstr,tagpos+60,30));
        fyear := copy(bufstr,tagpos+90,4);

        FGenreID := buf[tagpos+124];
        if FGenreID>high(ID3Genre) then FGenreID:=0;
        If buf[125]<>0 Then                             {check for id3v1.1}
          fcomment := (copy(bufstr,tagpos+94,30))
        Else
          Begin
            fcomment := (copy(bufstr,tagpos+94,28));
            If (buf[tagpos+123])<>0 Then ftrack := IntToStr(buf[tagpos+123])
            Else ftrack := '';
          End;
      End; // else writeln('no id3v1 tag');
  except WriteLn(Filename+' -> exception while reading id3v1 tag... skipped!!');  end;
    If ((artistv2<>'')) And (CactusConfig.id3v2_prio Or (artist='')) Then Fartist := TrimRight(
                                                                                    artistv2);
    If ((titlev2<>'')) And (CactusConfig.id3v2_prio Or (title=''))  Then Ftitle := TrimRight(
                                                                                  titlev2);
    If ((albumv2<>'')) And (CactusConfig.id3v2_prio Or (album='')) Then Falbum := TrimRight(
                                                                                 albumv2);
    If ((commentv2<>'')) And (CactusConfig.id3v2_prio Or (comment='')) Then Fcomment := TrimRight
                                                                                       (
                                                                                       commentv2
                                                                                       );
    If ((yearv2<>'')) And (CactusConfig.id3v2_prio Or (year='')) Then Fyear := TrimRight(yearv2);
    If ((trackv2<>'')) And (CactusConfig.id3v2_prio Or (track='')) Then ftrack := TrimRight(
                                                                                 trackv2);

    Fartist := TrimRight(Fartist);
    Ftitle := TrimRight(Ftitle);
    Falbum := TrimRight(FAlbum);
    Fcomment := TrimRight(FComment);
    Fyear := TrimRight(FYear);
    fileclose(mp3filehandle);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TMP3File.WriteTag(Filename: string): boolean;
Var
  buf: array[1..1024] Of byte;
  bufstr, tmptag, tmps: string;
  i, z: integer;
  id3v1str: string[31];
  mp3filehandle: longint;
Begin
{id3v2}
  mp3filehandle := fileopen(Filename,fmOpenRead);
  fileseek(mp3filehandle, 0, fsfrombeginning);
  fileread(mp3filehandle, buf, high(buf));
  fileclose(mp3filehandle);
  For i:= 1 To high(buf) Do
    bufstr := bufstr+char(buf[i]);

  If (pos('ID3',bufstr) <> 0) Or (length(artist)>30) Or (length(title)>30) Or (length(album)>30)
    Then
    Begin
      If pos('ID3',bufstr) = 0 Then
        Begin                                               {create new ID3v2 Tag skeleton}
          bufstr := '';
          bufstr := 'ID3'+char($03)+char(0)+char(0)+char(0)+char(0)+char(0)+char(0);
          {ID3 03 00 00 00 00 00 00}
          tmps := char(0)+char(0)+char(0)+char(2)+char(0)+char(0)+char(0)+' ';
          bufstr := bufstr+'TPE1'+tmps+'TIT2'+tmps+'TRCK'+tmps+'TYER'+tmps+'TALB'+tmps+char(0)+
                    char(0);
          writeln('creating new ID3v2 tag!');
          writeln(bufstr);
          z := length(bufstr)-1;
          For i:= z To high(buf) Do
            bufstr := bufstr+char(0);
        End;

      // Now lets write the tags
      i := pos('TPE1',bufstr);
      If i<> 0 Then
        Begin
          tmptag := (artist);
          If length(tmptag)>0 Then
            Begin
              Delete(bufstr, i+11, byte(bufstr[i+7])-1);
              Insert(tmptag, bufstr, i+11);
              bufstr[i+7] := char(length(tmptag)+1);
            End
          Else Delete(bufstr, i, byte(bufstr[i+7])+10);
        End
      Else
        Begin
          tmptag := (artist);
          If length(tmptag)>0 Then
            Begin
              tmps := char(0)+char(0)+char(0)+char(length(tmptag)+1)+char(0)+char(0)+char(0);
              Insert('TPE1'+tmps+(tmptag), bufstr, pos('ID3',bufstr)+10);
            End;
        End;

      i := pos('TP1',bufstr);
      If i<> 0 Then
        Begin
          tmptag := (artist);
          Delete(bufstr, i, byte(bufstr[i+5])+6);
          //delete whole TP1 tag
        End;

      i := pos('TIT2',bufstr);
      If i<> 0 Then
        Begin
          tmptag := (title);
          If length(tmptag)>0 Then
            Begin
              Delete(bufstr, i+11, byte(bufstr[i+7])-1);
              Insert(tmptag, bufstr, i+11);
              bufstr[i+7] := char(length(tmptag)+1);
            End
          Else Delete(bufstr, i, byte(bufstr[i+7])+10);
        End
      Else
        Begin
          tmptag := UTF8toLatin1(title);
          If length(tmptag)>0 Then
            Begin
              tmps := char(0)+char(0)+char(0)+char(length(tmptag)+1)+char(0)+char(0)+char(0);
              Insert('TIT2'+tmps+tmptag, bufstr, pos('ID3',bufstr)+10);
            End;
        End;

      i := pos('TRCK',bufstr);
      If i<> 0 Then
        Begin
          tmptag := (track);
          If length(tmptag)>0 Then
            Begin
              Delete(bufstr, i+11, byte(bufstr[i+7])-1);
              Insert(tmptag, bufstr, i+11);
              bufstr[i+7] := char(length(tmptag)+1);
            End
          Else Delete(bufstr, i, byte(bufstr[i+7])+10);
        End
      Else
        Begin
          tmptag := UTF8toLatin1(track);
          If length(tmptag)>0 Then
            Begin
              tmps := char(0)+char(0)+char(0)+char(length(tmptag)+1)+char(0)+char(0)+char(0);
              Insert('TRCK'+tmps+tmptag, bufstr, pos('ID3',bufstr)+10);
            End;
        End;

      i := pos('TYER',bufstr);
      If i<> 0 Then
        Begin
          tmptag := (year);
          If length(tmptag)>0 Then
            Begin
              Delete(bufstr, i+11, byte(bufstr[i+7])-1);
              Insert(tmptag, bufstr, i+11);
              bufstr[i+7] := char(length(tmptag)+1);
            End
          Else Delete(bufstr, i, byte(bufstr[i+7])+10);
        End
      Else
        Begin
          tmptag := (year);
          If length(tmptag)>0 Then
            Begin
              tmps := char(0)+char(0)+char(0)+char(length(tmptag)+1)+char(0)+char(0)+char(0);
              Insert('TYER'+tmps+tmptag, bufstr, pos('ID3',bufstr)+10);
            End;
        End;

      i := pos('TALB',bufstr);
      If i<> 0 Then
        Begin
          tmptag := (album);
          If length(tmptag)>0 Then
            Begin
              Delete(bufstr, i+11, byte(bufstr[i+7])-1);
              Insert(tmptag, bufstr, i+11);
              bufstr[i+7] := char(length(tmptag)+1);
            End
          Else Delete(bufstr, i, byte(bufstr[i+7])+10);
        End
      Else
        Begin
          tmptag := (album);
          If length(tmptag)>0 Then
            Begin
              tmps := char(0)+char(0)+char(0)+char(length(tmptag)+1)+char(0)+char(0)+char(0);
              Insert('TALB'+tmps+tmptag, bufstr, pos('ID3',bufstr)+10);
            End;
        End;

      z := length(bufstr)-1;
      For i:= 1 To high(buf) Do
        If (i<z) Then buf[i] := byte(bufstr[i])
        Else buf[i] := 0;
      mp3filehandle := fileopen(Filename,fmOpenWrite);
      If mp3filehandle<>-1 Then
        Begin
          fileseek(mp3filehandle,0,fsfrombeginning);
          filewrite(mp3filehandle,buf,high(buf));
          fileclose(mp3filehandle);
        End
      Else writeln('ERROR: cant write tag. file not found');
    End;
{id3v1}
  writeln('#####ID3V1#######');
  For i:=1 To 128 Do
    buf[i] := 0;
  buf[1] := 84;
  buf[2] := 65;
  buf[3] := 71; {TAG}

  FillChar(id3v1str, SizeOf(id3v1str), #0);
  id3v1str := (title);
  For i:= 4 To 33 Do
    buf[i] := byte(id3v1str[i-3]);

  FillChar(id3v1str, SizeOf(id3v1str), #0);
  id3v1str := (artist);
  For i:= 34 To 63 Do
    buf[i] := byte(id3v1str[i-33]);

  FillChar(id3v1str, SizeOf(id3v1str), #0);
  id3v1str := (album);
  For i:= 64 To 93 Do
    buf[i] := byte(id3v1str[i-63]);

  FillChar(id3v1str, SizeOf(id3v1str), #0);
  id3v1str := (year);
  For i:= 94 To 97 Do
    buf[i] := byte(id3v1str[i-93]);

  FillChar(id3v1str, SizeOf(id3v1str), #0);
  id3v1str := (comment);
  For i:= 98 To 127 Do
    buf[i] := byte(id3v1str[i-97]);

  If length(track)>0 Then
    Begin
      buf[126] := 0;
      buf[127] := StrToInt(track);
    End;

  mp3filehandle := fileopen(Filename,fmOpenWrite);
  If mp3filehandle<>-1 Then
    Begin
      If FileGetAttr(Filename)=faReadOnly Then writeln('file is read only');
      fileseek(mp3filehandle,-128,fsfromend);
      writeln(ftitle);
      writeln(fartist);
      filewrite(mp3filehandle,buf,128);
      fileclose(mp3filehandle);
    End
  Else writeln('ERROR: cant write tag. file not found');

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end.

