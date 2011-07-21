{
Mediacollection Object for Cactus Jukebox

written by Sebastian Kraft, <c> 2006-2008

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL




}
Unit mediacol;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils,
//Tagreader:
WMAfile, OggVorbis, FLACfile, mp3file, debug, lconvencoding, guesstag;

Type
  // PMediaCollectionClass = ^TMediaCollectionClass;
  TSrchType = ( FTrackSrch_Artist, FTrackSrch_ArtistAlbum, FAlbumSrch, FArtistSrch, FAllArtist );
  TMediaType = (MTAudioFile, MTStream, MTCDAudio);
  TPathFmt = ( FRelative, FDirect );

  TMediaCollectionClass = Class;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TMediaFileClass }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    TMediaFileClass = Class
      Private 
       FTitle, FAlbum, FArtist: string;
       FStreamUrl: string;
       FMediaType: TMediaType;

       Procedure read_tag_ogg;
       Procedure read_tag_flac;
       Procedure read_tag_wma;
       Procedure read_tag_wave;
       Procedure read_tag_mp3;

       procedure write_tag_mp3;

       Procedure SetArtist(aValue: String);
       Procedure SetAlbum(aValue: String);
       Procedure SetTitle(aValue: String);
       Procedure setStreamUrl(aValue: String);

      Public
       Path: String;
       CoverPath: ansistring;
       Collection: TMediaCollectionClass;
       Comment: ansistring;
       GenreID: Byte;
       Year, Track: string[4];
       Filetype: string[5];
       Size: int64;
       ID, Bitrate, Samplerate, Playlength, Action: longint;
       Playtime: string;
       index: integer;

       constructor create(filepath:String; ParentCollection: TMediaCollectionClass);
       constructor create(ParentCollection: TMediaCollectionClass);
       destructor destroy;
       Procedure Write_Tag;
       Procedure Read_Tag;
       Procedure assign(SourceObject: TMediaFileClass);
       Function PathNameFromTag(var strFormat: string): Boolean;
       Function PathNameFromTag_dryrun(var strFormat: string): string;
       Function FullPathNameFromTag_dryrun(var strFormat: string): string;
       Function move2path(strFilePath: string): Boolean;
       Function LibraryPath(): string;

       property Artist: string read FArtist write SetArtist;
       property Album: string read FAlbum write SetAlbum;
       property Title: string read FTitle write SetTitle;
       property StreamUrl: string read FStreamUrl write SetStreamUrl;
       property MediaType: TMediaType read FMEdiaType;

      End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TMediaCollectionClass }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    TMediaCollectionClass = Class(Tlist)
     Private
      FSorted, FEnumerated, FAutoEnum: Boolean;
      FSrchAscending: Boolean;
      FSrchPos: Integer;
      FSrchArtist, FSrchAlbum: String;
      FSrchType: TSrchType;
      Function GetItems(index: integer): TMediaFileClass;
      Procedure Recursive_AddDir(dir: String);
      Procedure SetAutoEnum(Const AValue: boolean);
      Procedure SetSorted(Const AValue: boolean);
     Public
      syncronize: Procedure (dir: String) Of object;
      Savepath: string;
      DirList: TStringList;
      PathFmt: TPathFmt;
      property Items[index: integer]: TMediaFileClass read GetItems;
      property sorted: boolean read FSorted write SetSorted;
      // when Sorted is set true, add/insert always adds at right position
      // on changing state from false to true whole collection is getting sorted once
      property AutoEnum: boolean read FAutoEnum write SetAutoEnum;
      property enumerated: boolean read FEnumerated;

      constructor create;
      destructor destroy;
      Procedure Assign(SourceCol:TMediaCollectionClass);
      Function  LoadFromFile(path: String): boolean;
      Function SaveToFile(path: String): boolean;
      Function SaveToFile: boolean;
      Procedure clear;
      Procedure insert(path: String; atInd: integer);
      Function add(path: String): integer;
      Function add(MedFileObj: TMediaFileClass): integer;
      Procedure add_directory(dir: String);
      Procedure remove(ind: integer);
      Procedure move(dest, target: integer);
      Function ItemCount: integer;
      Procedure enumerate;
      Procedure enumerate(StartFrom: integer);

      Function getTracks(artist: String): integer;
      Function getTracks(artist: String; StartFrom: integer): integer;

      Function getTracks(artist, album: String): integer;
      Function getTracks(artist, album: String; StartFrom: integer): integer;

      Function getAlbums(artist: String): TStringList;
      Function getAlbums(artist: String; StartFrom: integer): TStringList;

      Function getNext: integer;

      Function getArtists: integer;
      Function getNextArtist: integer;

      Function getIndexByPath(path: String): integer;

    End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Var MediaCollection, PlayerCol : TMediaCollectionClass;

implementation

Uses functions, config;

{$i cactus_const.inc}


{ TMediaCollectionClass }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.GetItems(index: integer): TMediaFileClass;
Begin
   if (index>=0) and (index < Count) then
      Result := (TMediaFileClass(Inherited Items [Index]));
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.Recursive_AddDir(dir: String);

Var mp3search,dirsearch: TSearchrec;
  tmps: string;
  fHandle: file Of byte;
Begin
  dir := IncludeTrailingPathDelimiter(dir);
  writeln('scanning through:  '+dir);
  syncronize(dir);
  If FindFirst(dir+'*.*',faAnyFile,mp3search)=0 Then
    Begin
      Repeat
        Begin
          tmps := lowercase(ExtractFileExt(mp3search.name));
          syncronize(dir);
          If (tmps='.mp3') Or (tmps='.wav') Or (tmps='.ogg') Or (tmps='.wma')
          Or (tmps='.flac') Or (tmps='.fla') Then
            Begin
              // Files with bad filenames may suddenly vanish from samba
              // mounts when accessed. This will fiter them out.
              system.assign(fHandle, dir+mp3search.name);
              {$I-}
              reset(fHandle);
              close(fHandle);
              {$I+}
              if IOResult = 0
              then
                add(dir+mp3search.name);
            End;
        End;
      Until FindNext(mp3search)<>0;
    End;
  Findclose(mp3search);
  If Findfirst(dir+'*',faanyfile,dirsearch)=0 Then
    Begin
      syncronize(dir);
      Repeat
        Begin
          If (dirsearch.attr And FaDirectory)=FaDirectory Then
            Begin
              If (dirsearch.name<>'..') And (dirsearch.name<>'.') Then
                    Recursive_AddDir(IncludeTrailingPathDelimiter(dir+dirsearch.name));
            End;
        End;
      Until FindNext(dirsearch)<>0;
    End;
  Findclose(dirsearch);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.SetAutoEnum(Const AValue: boolean);
Begin
  FAutoEnum := AValue;
  enumerate;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.SetSorted(Const AValue: boolean);
Begin
  Fsorted := AValue;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TMediaCollectionClass.create;
Begin
  Inherited create;
  FSorted := true;

  DirList := TStringList.Create;
  PathFmt := FDirect;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

destructor TMediaCollectionClass.destroy;
Begin

End;

Procedure TMediaCollectionClass.Assign(SourceCol: TMediaCollectionClass);
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.LoadFromFile(path: String): boolean;

Var i: integer;
  linecount: longint;
  lfile: textfile;
  RPath, tmps: String;
  NumEntries: Integer;
  MedFileObj: TMediaFileClass;
  sortState: boolean;
  tmpstring: string;
Begin
  savepath := path;
  sortState := FSorted;
  linecount:=0;
  Try
    system.assign(lfile,path);
    reset(lfile);

    readln(lfile, tmps);
    readln(lfile, tmps);

    readln(lfile, tmps);
    NumEntries := StrToInt(tmps);
    writeln(NumEntries);
    readln(lfile, tmps);
    If tmps[length(tmps)]=';' Then System.Delete(tmps, length(tmps), 1);
    i := pos(';', tmps);
    While i<>0 Do
      Begin
        DirList.Add(IncludeTrailingPathDelimiter(copy(tmps, 1, i-1)));
        system.delete(tmps, 1, i);
        i := pos(';', tmps);
      End;
    DirList.Add(tmps);
    If PathFmt = FRelative Then RPath := IncludeTrailingPathDelimiter(DirList[0])
    Else RPath := '';
    readln(lfile);
      linecount:=5;
    fsorted := false;
    For i:= 0 To  NumEntries-1 Do
      Begin
        MedFileObj := TMediaFileClass.create(self);
        MedFileObj.action := ANOTHING;
        readln(lfile, MedFileObj.path);
        inc(linecount);
        If PathFmt = FRelative Then MedFileObj.Path := RPath+MedFileObj.Path;
        readln(lfile, MedFileObj.id);
        inc(linecount);
        readln(lfile, tmpstring);
        MedFileObj.artist:=tmpstring;
        inc(linecount);
        readln(lfile, tmpstring);
        MedFileObj.album:=tmpstring;
        inc(linecount);
        readln(lfile, tmpstring);
        MedFileObj.title:=tmpstring;
        inc(linecount);
        readln(lfile, MedFileObj.year);
        inc(linecount);
        readln(lfile, MedFileObj.comment);
        inc(linecount);
        readln(lfile, MedFileObj.track);
        inc(linecount);
        readln(lfile, MedFileObj.size);
        inc(linecount);
        readln(lfile, MedFileObj.filetype);
        inc(linecount);
        readln(lfile, MedFileObj.bitrate);
        inc(linecount);
        readln(lfile, MedFileObj.samplerate);
        inc(linecount);
        readln(lfile, MedFileObj.playlength);
        inc(linecount);
        readln(lfile, MedFileObj.GenreID);
        inc(linecount);
        readln(lfile, MedFileObj.playtime);
        inc(linecount);
        add(MedFileObj);
      End;
    fsorted := sortState;
    AutoEnum := true;
    close(lfile);
    writeln('library sucessfully loaded');
    result := true;
  Except
    fsorted := sortState;
    writeln('lib seems corupted');
    write('exception at entry ');
    writeln(i);
    write('line: ');writeln(linecount);
    result := false;
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.SaveToFile(path: String): boolean;

Var lfile: textfile;
  i: integer;
  tmps: string;
Begin
  savepath := path;
  writeln('saving library to -> '+path);
  Try
    system.assign(lfile,path);
    rewrite(lfile);
    writeln(lfile,
            '#####This config file is created by Cactus Jukebox. NEVER(!!) edit by hand!!!####')
    ;
    writeln(lfile,'++++Config++++');
    writeln(lfile, ItemCount);
    For i:= 0 To DirList.Count-1 Do
      tmps := tmps+DirList.Strings[i]+';';
    writeln(lfile, tmps);
    writeln(lfile,'++++Files++++');
    tmps := '';
    For i:= 0 To ItemCount-1 Do
      Begin
        If PathFmt = FDirect Then
          tmps := items[i].Path
        Else
          Begin
            tmps := copy(items[i].path, length(DirList[0]), length(items[i].path) - length(
                    DirList[0])+1);
            If tmps[1]=DirectorySeparator Then system.Delete(tmps, 1, 1);
          End;
        //writeln(tmps);
        writeln(lfile,tmps);
        writeln(lfile,items[i].id);
        writeln(lfile,items[i].artist);
        writeln(lfile, items[i].album);
        writeln(lfile, items[i].title);
        writeln(lfile, items[i].year);
        writeln(lfile, items[i].comment);
        writeln(lfile, items[i].track);
        writeln(lfile,items[i].size);
        writeln(lfile,items[i].filetype);
        writeln(lfile,items[i].bitrate);
        writeln(lfile,items[i].samplerate);
        writeln(lfile,items[i].playlength);
        writeln(lfile,items[i].GenreID);
        writeln(lfile,items[i].playtime);
      End;
    close(lfile);
    write('written ');
    write(i);
    write(' of ');
    writeln(ItemCount);
  Except
    writeln('error writing library to disk: check permissions!');
    write('written ');
    write(i);
    write(' of ');
    writeln(ItemCount);
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.SaveToFile: boolean;
Begin
  result := SaveToFile(Savepath);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.clear;
var AutoEnumStatus: boolean;
Begin
  AutoEnumStatus:=AutoEnum;
  AutoEnum:=false;
  While count>0 Do
    remove(0);
  AutoEnum:=AutoEnumStatus;
  DirList.Clear;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.insert(path: String; atInd: integer);

Var i: integer;
Begin
  inherited Insert(atInd, TMediaFileClass.create(path, self));
  items[atInd].index := atInd;
  items[atInd].Collection := self;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.add(path: String): integer;

Var i,z: integer;
  MedFileObj: TMediaFileClass;
  SortedState: boolean;
Begin
  i := 0;
  SortedState := FSorted;
  FSorted := false;
  MedFileObj := TMediaFileClass.create(path, self);
  If (SortedState) Then
    Begin
      If (MedFileObj.Artist<>'') and (ItemCount>0) Then
        Begin
          While (i<ItemCount) And (CompareText(items[i].Artist, MedFileObj.Artist)<0)
            Do
            inc(i);
          While (i<=ItemCount-1) And (CompareText(items[i].Artist, MedFileObj.Artist)=0) And
                (CompareText(items[i].Title, MedFileObj.Title)<0)
            Do
            inc(i);
        End
      Else i := 0;
      inherited Insert(i, MedFileObj);
      If AutoEnum Then enumerate(i)
      Else FEnumerated := false;
    End
  Else
    Begin
      i := Inherited Add(MedFileObj);
      items[i].index := i;
    End;
  items[i].Collection := self;
  result := i;
  FSorted := SortedState;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.add(MedFileObj: TMediaFileClass): integer;

Var i: integer;
  SortedState: boolean;
Begin
  SortedState := FSorted;
  FSorted := false;
  i := 0;
  If SortedState Then
    Begin
      try
        If MedFileObj.Artist<>'' Then
          Begin
            While (i<ItemCount) And (CompareText(items[i].Artist, MedFileObj.Artist)<0)
              Do
              inc(i);

            While (i<=ItemCount-1) And (CompareText(items[i].Artist, MedFileObj.Artist)=0) And
                  (CompareText(items[i].Title, MedFileObj.Title)<0)
              Do
              inc(i);
          End;
        inherited Insert(i, MedFileObj);
        If AutoEnum Then enumerate(i)
        Else FEnumerated := false;

      except
        writeln('exception sorting object in library');
        writeln(MedFileObj.Path);
      end;
    End
  Else
    Begin
      i := Inherited Add(MedFileObj);
      items[i].index := i;
    End;
  items[i].Collection := self;
  result := i;
  FSorted := SortedState;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.add_directory(dir: String);
Begin
  DirList.Add(dir);
  Recursive_AddDir(dir);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.remove(ind: integer);

Var i: integer;
Begin
  If (ind>=0) And (ind < Count) Then
    Begin
      Items[ind].free;
      inherited Delete(ind);
      dec(FSrchPos);
{          If (FSrchPos<=ind) Then dec(FSrchPos);
      If (FSrchPos>ind) And (FSrchAscending) Then dec(FSrchPos);}
      If AutoEnum Then enumerate(ind)
      Else FEnumerated := false;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.move(dest, target: integer);
Begin
  //TODO: Test move operation for all FsrchPos cases
  inherited move(dest, target);
  //  if (FSrchPos>=dest) and (FSrchPos>target) then inc(FSrchPos);
  If (FSrchPos>=dest) And (FSrchPos<target) Then dec(FSrchPos);


  //  if (FSrchPos>ind) and (FSrchAscending) then dec(FSrchPos);

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.ItemCount: integer;
Begin
  Result := Inherited Count;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.enumerate;
Begin
  enumerate(0);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaCollectionClass.enumerate(StartFrom: integer);

Var i: integer;
Begin
  For i:=StartFrom To ItemCount-1 Do
    items[i].index := i;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.GetTracks(artist: String): integer;
Begin
  result := getTracks(artist, 0);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.getTracks(artist, album: String): integer;
Begin
  result := getTracks(artist, album, 0);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.GetTracks(artist: String; StartFrom: integer): integer;

Var i: integer;
Begin
  FSrchType := FTrackSrch_Artist;

  artist := lowercase(artist);
  FSrchArtist := artist;
  If StartFrom<>0 Then
    Begin
      If (sorted=false) Then
        StartFrom := 0
      Else
        Begin
          i := AnsiCompareText(Items[StartFrom].Artist, artist);
          If (i=0) Or (i>0) Then FSrchAscending := true
          Else FSrchAscending := false;
        End;
    End;

  i := StartFrom;
  If (i<>0) And (FSrchAscending) Then
    Begin
      While (lowercase(Items[i].Artist)<>artist) And (i>=0) Do
        dec(i);
      While (lowercase(Items[i].Artist)=artist) And (i>0) Do
        dec(i);
      inc(i);
      FSrchPos := i;
    End
  Else
    Begin
      While (i<Count) And (lowercase(Items[i].Artist)<>artist)  Do
        inc(i);
      If i<>Count Then FSrchPos := i
      Else FSrchPos := -1;
    End;

  Result := FSrchPos;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.getTracks(artist, album: String;
                                         StartFrom: integer): integer;

Var i: integer;
Begin
  album:=LowerCase(album);
  artist:=LowerCase(artist);
  FSrchArtist := artist;
  FSrchAlbum := album;
  i := getTracks(artist, StartFrom);
  FSrchType := FTrackSrch_ArtistAlbum;
  While lowercase(items[i].Album)<>album Do
    inc(i);

  FSrchPos := i;
  Result := i;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.getAlbums(artist: String): TStringList;
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.getAlbums(artist: String; StartFrom: integer
): TStringList;

Var tmplist : TStringList;
  i: integer;
Begin
  artist := lowercase(artist);
  If StartFrom<>0 Then
    Begin
      If (sorted=false) Then
        StartFrom := 0
      Else
        Begin
          i := AnsiCompareText(Items[StartFrom].Artist, artist);
          If (i=0) Or (i>0) Then FSrchAscending := true
          Else FSrchAscending := false;
        End;
    End;

  i := StartFrom;
  If (i<>0) And (FSrchAscending) Then
    Begin
      While (lowercase(Items[i].Artist)<>artist) And (i>=0) Do
        dec(i);
      While (lowercase(Items[i].Artist)=artist) And (i>0) Do
        dec(i);
      inc(i);
    End
  Else
    Begin
      While (i<count) And (lowercase(Items[i].Artist)<>artist) Do
        inc(i);
      If i=Count Then i := -1;
    End;
  tmplist := TStringList.Create;
  tmplist.Sorted := true;
  tmplist.CaseSensitive := false;
  tmplist.Duplicates := dupIgnore;
  If (i>=0) And (i<Count) Then
    Begin

      While (i<Count) And (lowercase(Items[i].Artist)=artist) Do
        Begin
          If Items[i].Album<>'' Then tmplist.AddObject(items[i].Album, Items[i])
          Else tmplist.AddObject('Unknown', Items[i]);
          inc(i);
        End;
      Result := tmplist;
    End;

End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.getArtists: integer;
Begin
  FSrchPos := 0;
  FSrchArtist := lowercase(Items[0].Artist);
  result := FSrchPos;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.getNextArtist: integer;

Var i: integer;
Begin
  i := FSrchPos;
  Repeat
    inc(i)
  Until (i>=Count) Or (lowercase(items[i].Artist)<>FSrchArtist);
  If i<Count Then
    Begin
      FSrchArtist := lowercase(Items[i].Artist);
      FSrchPos := i;
      Result := i;
    End
  Else result := -1;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.GetNext: integer;

Var i: integer;
Begin
  Case FSrchType Of
    FTrackSrch_Artist:
                       Begin
                         Repeat
                           inc(FSrchPos)
                         Until (FSrchPos>=ItemCount) Or (lowercase(Items[FSrchPos].Artist)=
                               FSrchArtist);
                         If (FSrchPos<>ItemCount) And (lowercase(Items[FSrchPos].Artist)=
                            FSrchArtist)Then
                           result := FSrchPos
                         Else
                           result := -1;
                         exit;
                       End;
    FTrackSrch_ArtistAlbum:
                            Begin
                              Repeat
                                inc(FSrchPos)
                              Until (FSrchPos>=ItemCount) Or ((lowercase(Items[FSrchPos].Album)=
                                    FSrchAlbum) And (lowercase(Items[FSrchPos].Artist)=
                                    FSrchArtist));
                              If (FSrchPos<>ItemCount) And ((lowercase(Items[FSrchPos].Album)=
                                 FSrchAlbum) And (lowercase(Items[FSrchPos].Artist)=FSrchArtist)
                                 ) Then
                                result := FSrchPos
                              Else
                                result := -1;
                              exit;
                            End;
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaCollectionClass.GetIndexByPath(path: String): integer;

Var i: integer;
Begin
  i := ItemCount;
  Repeat
    dec(i)
  Until (i<0) Or (items[i].Path=path);
  result := i;
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TMediaFileClass }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ogg-Support changed by monta
Procedure TMediaFileClass.read_tag_ogg;
Var OGGFile: TOggVorbis;
Begin
  OGGFile := TOggVorbis.Create;
  try
  if FileExists(Path) then
  begin
    writeln(path);
    OGGFile.ReadFromFile(Path);
    artist := UTF8Encode(OGGFile.Artist);
    title := UTF8Encode(OGGFile.Title);
    album := UTF8Encode(OGGFile.Album);
    Bitrate := OGGFile.BitRate;
    Year := OGGFile.Date;
    Samplerate := OGGFile.SampleRate;
    Comment := OGGFile.Comment;
    Track := IntToStr(OGGFile.Track);
    Playlength := round(OGGFile.Duration);
    Playtime := SecondsToFmtStr(Playlength);
    GenreID := StrToIntDef(OGGFile.Genre, 0);
  end;
  finally
    OGGFile.Free;
  end;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// flac-Support added by monta
Procedure TMediaFileClass.read_tag_flac;
Var FLACFile: TFLACfile;
Begin
  FLACfile := TFLACfile.Create;
  try
  if FileExists(Path) then
  begin
    FLACFile.ReadFromFile(Path);
    artist := UTF8Encode(FLACFile.Artist);
    title := UTF8Encode(FLACFile.Title);
    album := UTF8Encode(FLACFile.Album);
    Bitrate := FLACFile.BitRate;
    Year := FLACFile.Year;
    Samplerate := FLACFile.SampleRate;
    Comment := FLACFile.Comment;
    Track := FLACFile.TrackString;
    Playlength := round(FLACFile.Duration);
    Playtime := SecondsToFmtStr(Playlength);
    GenreID := StrToIntDef(FLACFile.Genre, 0);
  end;
  finally
    FLACFile.Free;
  end;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// wma-Support added by monta
Procedure TMediaFileClass.read_tag_wma;
Var WMAFile: TWMAfile;
Begin
  WMAFile := TWMAfile.Create;
  try
  if FileExists(Path) then
  begin
    WMAFile.ReadFromFile(Path);
    artist := UTF8Encode(WMAFile.Artist);
    title := UTF8Encode(WMAFile.Title);
    album := UTF8Encode(WMAFile.Album);
    Bitrate := WMAFile.BitRate;
    Year := WMAFile.Year;
    Samplerate := WMAFile.SampleRate;
    Comment := WMAFile.Comment;
    Track := IntToStr(WMAFile.Track);
    Playlength := round(WMAFile.Duration);
    Playtime := SecondsToFmtStr(Playlength);
    GenreID := StrToIntDef(WMAFile.Genre, 0);
  end;
  finally
    WMAFile.Free;
  end;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaFileClass.read_tag_wave;

Var li: cardinal;
  b: byte;
  z: integer;
  mp3filehandle: longint;
  tmps: string;
Begin
  Try
    mp3filehandle := fileopen(path, fmOpenRead);
    fileseek(mp3filehandle,8,fsfrombeginning);
    b := 0;
    Repeat
      Begin
        inc(b);
        fileread(mp3filehandle,li,4);
      End;
    Until (li=$20746D66) Or (b=15);
    fileread(mp3filehandle,li,4);
    fileread(mp3filehandle,li,4);
    fileread(mp3filehandle,li,4);
    samplerate := li;
    fileread(mp3filehandle,li,4);
    bitrate := (li Div 1024)*8;
    playlength := size Div li;

    playtime := SecondsToFmtStr(Playlength);
  Except
    writeln('**EXCEPTION reading wave file '+path+'**');
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMediaFileClass.read_tag_mp3;
Var MP3File: TMP3File;
Begin
  MP3File := TMP3File.Create;
  try
  if FileExists(Path) then
  begin
   // writeln(path);
    MP3File.ReadTag(Path);
    artist := ISO_8859_1ToUTF8(MP3File.Artist);
    title := ISO_8859_1ToUTF8(MP3File.Title);
    album := ISO_8859_1ToUTF8(MP3File.Album);
    Bitrate := MP3File.BitRate;
    Year := MP3File.Year;
    Samplerate := MP3File.SampleRate;
    Comment := ISO_8859_1ToUTF8(MP3File.Comment);
    Track := (MP3File.Track);
    Playlength := round(MP3File.Playlength);
    Playtime := SecondsToFmtStr(Playlength);
    GenreID := (MP3File.GenreID);
  end;
  finally
    MP3File.Free;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMediaFileClass.write_tag_mp3;
var MP3File: TMP3File;
begin
  MP3File:=TMP3File.create;

  MP3File.Artist:=UTF8ToISO_8859_1(FArtist);
  MP3File.Album:=UTF8ToISO_8859_1(FAlbum);
  MP3File.Title:=UTF8ToISO_8859_1(FTitle);
  MP3File.Track:= Track;
  MP3File.Comment:=UTF8ToISO_8859_1(Comment);
  MP3File.Year:= Year;
  MP3File.GenreID:=(GenreID);

  MP3File.WriteTag(Path);

  MP3File.Free;

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaFileClass.Read_Tag;
var TagFromName: TTagFromFilename;
Begin
  If filetype='.wav' Then read_tag_wave;
  If filetype='.ogg' Then read_tag_ogg;
  If filetype='.mp3' Then read_tag_mp3;
  If filetype='.wma' Then read_tag_wma;
  If (filetype='.flac') or (filetype='.fla') Then read_tag_flac;

  if (Artist='') and CactusConfig.GuessTag then begin
     TagFromName:=TTagFromFilename.create;
     TagFromName.ReadTag(Path);
     if TagFromName.Artist<>'' then Artist:=TagFromName.Artist+'*';
     if (Title='') and (TagFromName.Artist<>'') then Title:=TagFromName.Title+'*';
     if (Track='') and (TagFromName.Track<>'') then Track:=TagFromName.Track+'*';
     TagFromName.Free;
  end;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaFileClass.SetArtist(aValue: String);

Var i, start: integer;
Begin
  i := index;
  start := index;
  FArtist := aValue;
  If (Collection<>nil) and Collection.sorted Then
    Begin
      If (i<Collection.Count-1) And (CompareText(FArtist, Collection.Items[i+1].Artist)>0) Then
        Begin
          inc(i);
          While (i<=Collection.Count-1) And (compareText(FArtist, Collection.Items[i].Artist)>0)
            Do
            Begin
              inc(i);
            End;
          While (i<=Collection.Count-1) And (compareText(FTitle, Collection.Items[i].Title)>0)
                And (CompareText(FArtist, Collection.Items[i].Artist)=0) Do
            Begin
              inc(i);
            End;
          Collection.Move(index, i-1);
          If Collection.AutoEnum Then Collection.enumerate(start);
        End;
      If (i>0) And (CompareText(FArtist, Collection.Items[i-1].Artist)<0) Then
        Begin
          dec(i);
          While (i>=0) And (compareText(FArtist, Collection.Items[i].Artist)<0) Do
            Begin
              dec(i);
            End;

          While ((i>=0) And (compareText(FTitle, Collection.Items[i].Title)<0))
                And (CompareText(FArtist, Collection.Items[i].Artist)=0) Do
            Begin
              dec(i);
            End;
          Collection.Move(index, i+1);
          If Collection.AutoEnum Then Collection.enumerate;
        End;
    End;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaFileClass.SetAlbum(aValue: String);
Begin
  FAlbum := aValue;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaFileClass.SetTitle(aValue: String);

Var i, start: integer;
Begin
  FTitle := aValue;
  i := index;
  start := index;
  If (Collection<>nil) and Collection.sorted Then
    Begin
      writeln(i);
      If (i<Collection.Count-1) And (CompareText(FTitle, Collection.Items[i+1].Title)>0)
         And (CompareText(FArtist, Collection.Items[i+1].Artist)=0) Then
        Begin
          inc(i);
          While ((i<=Collection.Count-1) And (compareText(FTitle, Collection.Items[i].Title)>0))
                And (CompareText(FArtist, Collection.Items[i].Artist)=0) Do
            Begin
              inc(i);
            End;
          Collection.Move(index, i-1);
          If Collection.AutoEnum Then Collection.enumerate(start);
        End;

      If (i>0) And (CompareText(FTitle, Collection.Items[i-1].Title)<0)
         And (CompareText(FArtist, Collection.Items[i-1].Artist)=0) Then
        Begin
          dec(i);
          While ((i>=0) And (compareText(FTitle, Collection.Items[i].Title)<0))
                And (compareText(FArtist, Collection.Items[i].Artist)=0) Do
            Begin
              dec(i);
            End;
          Collection.Move(index, i+1);
          If Collection.AutoEnum Then Collection.enumerate;
        End;
    End;

End;

Procedure TMediaFileClass.setStreamUrl(aValue: String);
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TMediaFileClass.create(filepath: String; ParentCollection: TMediaCollectionClass);

Var tmpfile: file Of byte;
Begin
  Collection := ParentCollection;
  path := filepath;
  action := ANOTHING;
  If pos(URLID, filepath)=0 Then FMediaType := MTStream
  Else FMediaType := MTAudioFile;
  Filemode := 0;
  try
     system.assign(tmpfile, path);
     //Open file temporaly to get some information about it
     reset(tmpfile);
     size := filesize(tmpfile);
     //get filesize
     ID := crc32(path);
     // calc unique file ID
     filetype := lowercase(ExtractFileExt(filepath));
     close(tmpfile);
  except debugoutln('ERROR reading file '+filepath,2);
  end;
  Filemode := 2;
  read_tag;
  //finally read tag information
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TMediaFileClass.create(ParentCollection: TMediaCollectionClass);
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

destructor TMediaFileClass.destroy;
Begin
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaFileClass.Write_Tag;
begin
  if Filetype='.mp3' then write_tag_mp3;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMediaFileClass.assign(SourceObject: TMediaFileClass);
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaFileClass.move2path(strFilePath: string): Boolean;
var
  i: integer;
  strSrc, strDest, strTmp: string;
begin
  // did the filename change at all?
  strSrc := Path;
  strDest := strFilePath;
  if strSrc = strDest then
  begin
    Result := true;
    exit;
  end;

  // has the folder changed?
  strSrc := ExtractFilePath(Path);
  strDest := ExtractFilePath(strFilePath);
  if strSrc <> strDest then
    if NOT DirectoryExists(strDest) then
      ForceDirectories(strDest);

  // does the target file alredy existe?
  strDest := strFilePath;
  if FileExists(strDest) then
  begin
    while strDest[Length(strDest)-1] <> '.' do
      strDest := Copy(strDest, 1, Length(strDest)-1);
    strDest := Copy(strDest, 1, Length(strDest)-2);
    i := 2;
    repeat
    begin
      strTmp := '(' + IntToStr(i) + ')' + Filetype;
      i += 1;
    end
    until NOT FileExists(strDest + strTmp)
  end;
  strDest += strTmp;

  // move the file
  strSrc := Path;
  RenameFile(strSrc, strDest);

  // remove old folder and folders above if empty
  strSrc := ExtractFilePath(Path);
  while DirectoryIsEmpty(strSrc) do
  begin
    RemoveDir(strSrc);
    i := LastDelimiter(PathDelim,ExcludeTrailingPathDelimiter(strSrc));
    Delete(strSrc, i, Length(strSrc)-i+1);
  end;

  result := true; // FIXME write error detection needed

  if result then
    Path := strDest;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaFileClass.PathNameFromTag_dryrun(var strFormat: string): string;
var
  strArtist, strAlbum, strTitle, strTrack, strYear: string;
  strLeft, strRight, strMid: string;
  chrLetter: char;
  intPos, intPos2: integer;
  bNonEmpty: Boolean;
begin
  // format string could be '%a/%a - %b - %n - %t'

  // if existant, replace unwanted chars in tags
  strArtist := MakeValidFilename(Artist);
  strAlbum := MakeValidFilename(Album);
  strTitle := MakeValidFilename(Title);
  strTrack := MakeValidFilename(Track);
  strYear := MakeValidFilename(Year);


  result := strFormat;
//  result := '%a/%a - %b%? - ?%n - %t%? in ?%y';

  while (Pos('%?', result) >0) and (Pos('?%', result) >0) do
  begin
    intPos := Pos('%?', result);
    intPos2 := Pos('?%', result);
    if length(result) < intPos2+2 then break;
    if intPos2 < intPos then break;  // FIXME  could be more elegant

    strLeft := Copy(result, 1, intPos -1);
    strRight := Copy(result, intPos2 +3, Length(result) - intPos2 +3);
    strMid := Copy (result, intPos +2, Length(result) - intPos -1 -(Length(result) - intPos2) -1);
    chrLetter := result[intPos2 +2];

    bNonEmpty := false;
    case chrLetter of
      'a': if strArtist <> '' then bNonEmpty := true;
      'b': if strAlbum <> '' then bNonEmpty := true;
      't': if strTitle <> '' then bNonEmpty := true;
      'n': if strTrack <> '' then bNonEmpty := true;
      'y': if strYear <> '' then bNonEmpty := true;
    end;

    if bNonEmpty then
      result := strLeft + strMid + '%' + chrLetter + strRight
    else
      result := strLeft + strRight;
  end;

  result := StringReplace(result, '%a', strArtist, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%b', strAlbum, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%t', strTitle, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%n', strTrack, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%y', strYear, [rfReplaceAll, rfIgnoreCase]);
  result += FileType;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaFileClass.FullPathNameFromTag_dryrun(var strFormat: string): string;
begin
  result := LibraryPath() + PathNameFromTag_dryrun(strFormat);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaFileClass.PathNameFromTag(var strFormat: string): Boolean;
begin
  result := move2path(FullPathNameFromTag_dryrun(strFormat));
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMediaFileClass.LibraryPath(): string;
var
  i: integer;
begin
  result := '';
  For i:= 0 To MediaCollection.dirlist.Count-1 Do
    if Pos(Collection.dirlist[i], Path) > 0 then
    begin
      result := IncludeTrailingPathDelimiter(Collection.dirlist[i]);
      break;
    end;
end;


End.
