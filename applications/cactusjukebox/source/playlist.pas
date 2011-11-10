unit playlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mediacol, debug;


{ TPlaylistitemClass }
type
  TPlaylistitemClass = class
    Artist, Title, Path, Album: string;
    LengthMS, id: longint;
    Played: boolean;
    constructor Create;
    destructor Destroy;
    procedure update(MedFileObj: TMediaFileClass);
  end;

  PPlaylistItemClass = ^TPlaylistitemClass;

type

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  { TPlaylistClass }

  TPlaylistClass = class(TList)
  private
    function GetItems(index: integer): TPlaylistitemClass;
  public
    CurrentTrack: integer;
    property Items[index: integer]: TPlaylistitemClass read GetItems;

    constructor Create;
    destructor Destroy;
    function TotalPlayTime: int64;
    function TotalPlayTimeStr: string;
    procedure move(dest, target: integer);
    procedure remove(index: integer);
    procedure Clear; override;
    function add(filepath: string): integer;       //Read track info out of file at path
    function add(MedFileObj: TMediaFileClass): integer; //Get track info from FileObj
    procedure insert(index: integer; MedFileObj: TMediaFileClass);

    function update(index: integer; filepath: string): integer;
    //update track info out of file at path
    function update(index: integer; MedFileObj: TMediaFileClass): integer;
    //update track info from FileObj
    function RandomIndex: integer;
    procedure reset_random;
    function ItemCount: integer;
    function LoadFromFile(path: string): byte;
    function SaveToFile(path: string): byte;
  end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

implementation

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TPlaylistitemClass }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TPlaylistitemClass.Create;
begin
  played := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

destructor TPlaylistitemClass.Destroy;
begin
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TPlaylistitemClass.update(MedFileObj: TMediaFileClass);
begin

  Artist := MedFileObj.Artist;
  Title := MedFileObj.Title;
  Album := MedFileObj.Album;

  ID := MedFileObj.ID;
  LengthMS := MedFileObj.Playlength;
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TPlaylistClass }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.GetItems(index: integer): TPlaylistitemClass;
begin
  if (index >= 0) and (index < Count) then
    Result := (TPlaylistitemClass(inherited Items[Index]));
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TPlaylistClass.Create;
begin
  inherited Create;
end;

destructor TPlaylistClass.Destroy;
begin
  Clear;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.TotalPlayTime: int64;
  // returns total playtime of playlist in milliseconds
var
  i: integer;
  PPlaylistItem: PPlaylistItemClass;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    Result := Result + Items[i].LengthMS;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.TotalPlayTimeStr: string;
  // returns total playtime of playlist in string
  // format. i.e. '2h 20min'
var
  s1, s2: string;
  i: int64;
begin
  i := TotalPlayTime;
  s2 := IntToStr((i div 60) mod 60);
  s1 := IntToStr((i div 60) div 60);
  Result := s1 + 'h ' + s2 + 'min';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TPlaylistClass.move(dest, target: integer);
var
  current_track_tmp: integer;
begin
  if (dest < ItemCount) and (target < ItemCount) and (dest >= 0) and (target >= 0) then
  begin
    inherited Move(dest, target);
    current_track_tmp := CurrentTrack;
    if (CurrentTrack > dest) and (CurrentTrack <= target + 1) then
      Dec(current_track_tmp);

    if (CurrentTrack < dest) and (CurrentTrack >= target) then
      Inc(current_track_tmp);

    if (CurrentTrack = dest) then
    begin
      current_track_tmp := target;
      //  if dest<target then current_track_tmp:=target+1 else current_track_tmp:=target;
    end;
    CurrentTrack := current_track_tmp;
    DebugOutLn(Format('dest=%d target=%d curtrack_before=%d', [dest, target, CurrentTrack]), 0);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TPlaylistClass.remove(index: integer);
begin
  if (index >= 0) and (index < Count) then
  begin
    Items[index].Free;
    inherited Delete(index);
    if CurrentTrack > index then
      Dec(CurrentTrack);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TPlaylistClass.Clear;
begin
  while Count > 0 do
    remove(0);
  CurrentTrack := -1;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.add(filepath: string): integer;
begin
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.add(MedFileObj: TMediaFileClass): integer;
var
  Playlistitem: TPlaylistitemClass;
  index: integer;
begin

  index := (inherited Add(TPlaylistitemClass.Create));


  Items[index].Path := MedFileObj.path;
  Items[index].Artist := MedFileObj.Artist;
  Items[index].Title := MedFileObj.Title;
  Items[index].Album := MedFileObj.Album;

  Items[index].ID := MedFileObj.ID;
  Items[index].LengthMS := MedFileObj.Playlength;
  Result := index;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TPlaylistClass.insert(index: integer; MedFileObj: TMediaFileClass);
begin
  inherited insert(index, TPlaylistitemClass.Create);

  Items[index].Path := MedFileObj.path;
  Items[index].Artist := MedFileObj.Artist;
  Items[index].Title := MedFileObj.Title;
  Items[index].Album := MedFileObj.Album;

  Items[index].ID := MedFileObj.ID;
  Items[index].LengthMS := MedFileObj.Playlength;
  if index < CurrentTrack then
    Inc(CurrentTrack);

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.update(index: integer; filepath: string): integer;
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.update(index: integer; MedFileObj: TMediaFileClass): integer;
begin
  if (index >= 0) and (index < Count) then
  begin

    Items[index].update(MedFileObj);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.RandomIndex: integer;
  // Returns a random index of playlist entry that has not been played yet. -1 if all has been played.
  // reset_random resets it
var
  x, i: integer;
  s: boolean;
begin
  s := False;
  for i := 0 to Count - 1 do
    if Items[i].played = False then
      s := True;
  randomize;
  if s then
  begin
    i := 0;
    repeat
      begin
        x := random(Count - 1);
        Inc(i);
      end;
    until (Items[x].played = False) or (i > 4096);
    // i is for timeout to prevent an endless loop

    if i > 4096 then
    begin
      x := -1;
      repeat
        Inc(x)
      until Items[x].played = False;
    end;

    Result := x;
  end
  else
  begin
    Result := -1;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TPlaylistClass.reset_random;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].played := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.ItemCount: integer;
begin
  Result := inherited Count;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.LoadFromFile(path: string): byte;       //Load .m3u Playlist
var
  s, tmps, fpath, fartist, ftitle: string;
  pos1, pos2, i, lengthS: integer;
  PlaylistItem: TPlaylistItemClass;
  fileobj: TMediaFileClass;
  filehandle: Text;
begin
  try
    system.Assign(Filehandle, path);
    Reset(filehandle);
    readln(filehandle, tmps);
    if pos('#EXTM3U', tmps) <> 0 then
    begin
      repeat
        begin
          repeat
            readln(filehandle, tmps)
          until ((pos('#EXTINF', tmps) <> 0) or EOF(filehandle));
          pos1 := pos(':', tmps) + 1;
          pos2 := pos(',', tmps);
          s := copy(tmps, pos1, pos2 - pos1);

          val(s, LengthS);

          pos1 := pos2 + 1;
          pos2 := pos(' - ', tmps);

          fartist := copy(tmps, pos1, pos2 - pos1);
          pos2 := pos2 + 3;
          ftitle := copy(tmps, pos2, (length(tmps)) - pos2 + 1);
          readln(filehandle, fpath);

          i := (inherited Add(TPlaylistitemClass.Create));
          Items[i].Title := ftitle;
          Items[i].Artist := fartist;
          Items[i].Path := fpath;
          Items[i].LengthMS := lengthS * 1000;

        end;
      until EOF(filehandle);
    end
    else
      debugoutln(path + ' is not a valid m3u playlist', 4);
    Close(filehandle);
    Result := 0;
  except
    Result := 1;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TPlaylistClass.SaveToFile(path: string): byte;
var
  i: integer;
  temps: string;
  filehandle: Text;
begin
  try
    system.Assign(Filehandle, path);
    Rewrite(filehandle);
    writeln(Filehandle, '#EXTM3U');
    for i := 0 to Count - 1 do
    begin
      str(Items[i].LengthMS div 1000, temps);
      writeln(filehandle, '#EXTINF:' + temps + ',' + Items[i].artist + ' - ' + Items[i].title);
      writeln(filehandle, Items[i].path);
    end;
    Close(filehandle);
    Result := 0;
  except
    Result := 1;
  end;
end;


end.

