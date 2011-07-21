
{

  classes that implement the player functions for FMOD library
  written by Sebastian Kraft
  sebastian_kraft@gmx.de

  This software is free under the GNU Public License

  (c)2005-2008
}

unit fmodplayer;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,
  {$ifdef win32} fmoddyn, {$endif}
  {$ifdef unix} fmoddyn, {$endif}
  fmodtypes, mediacol, playlist, playerclass;

type
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{ TFModPlayerClass }

TFModPlayerClass = class(TPlayerClass)
   Private
     Soundhandle: PFSoundStream;

   Public
     destructor destroy;
     constructor create; override;

     function play(index:integer):byte;override;
     function play(url: string):byte;override;
     procedure pause;override;
     procedure stop;override;
     function next_track:byte;override;
     function prev_track:byte;override;
     
     function Get_Stream_Status:TStreamStatus;override;

     function Get_TrackLength:longint;override;
     function Get_Time:longint;override;
     function Get_TimeStr:string;override;
     function Get_TimeRemainingStr: string; override;

     function Get_FilePosition:longint;override;
     function get_FileLength:longint;override;

     procedure Set_Time(ms: longint);override;
     procedure Set_FilePosition(fpos:longint);override;
     procedure Set_Volume(vol:byte);override;
     
     procedure Mute;override;
     function Muted:boolean;override;

     property CurrentTrack: Integer read GetCurrentTrack;
     property playing: boolean read FPlaying;
     property paused: boolean read FPaused;
     property volume:byte read FVolume write Set_Volume;
     property PlaybackMode: TPlaybackMode read FPlaybackMode;
  end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

implementation
uses functions;
var tmpp: PChar;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


destructor TFModPlayerClass.destroy;
var i:integer;
begin
     fplaying:=false;

     Playlist.Free;

end;

constructor TFModPlayerClass.create;
begin
  inherited create;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.play(index:integer):byte;
{startplay=0 -> succesful
 startplay=1 -> file not found
 startplay=2 -> soundcard init failed
 startplay=3 -> index out of bounds
 startplay=4 -> Last song in List}
var z: integer;
   pPlaylistItem: pPlaylistItemClass;
begin
 if (index<Playlist.ItemCount) and (index>=0) then begin
   if (fplaying=true) then begin
         if FSOUND_Stream_Stop(Soundhandle)=false then writeln('ERROR stop stream');
         if FSOUND_Stream_Close(Soundhandle)=false then writeln('ERROR on closing stream');
         fplaying:=false;
      end;
   if (fplaying=false) then begin
   //FSOUND_Close;
 {$ifdef linux}
    if OutputMode = OSSOUT then begin
           if FSOUND_SetOutput(FSOUND_OUTPUT_OSS) then writeln('Oss output openend') else writeln('failed opening oss output')
         end
         else begin
           if FSOUND_SetOutput(FSOUND_OUTPUT_ALSA) then writeln('alsa output openend') else writeln('failed opening alsa output')
         end;
  {$endif}

    if FSOUND_Init(44100, 32, 0)=true then begin

      FPlaybackMode:=FILE_MODE;
      writeln('playing  -> '+playlist.items[index].path);
      if (FileExists(playlist.items[index].path)) then begin
         tmpp:=StrAlloc(length(playlist.items[index].path)+1);
         StrPCopy(tmpp,playlist.items[index].path);

       // Open the stream
         write(' openingstream... ');
         Soundhandle:=FSOUND_Stream_Open(tmpp, FSOUND_MPEGACCURATE or FSOUND_NONBLOCKING {FSOUND_NORMAL}, 0, 0);    //Fixes Bug when starting VBR files first, FSOUND_NORMAL is faster!!
         z:=0;
         repeat begin   //Wait until it is loaded and ready
                z:=FSOUND_Stream_GetOpenState(soundhandle);
              end;
          until (z=0) or (z=-3);
         write(' ready... ');
         if z = 0 then begin //If loading was succesful
            write(' start playing... ');
            FSOUND_Stream_Play (FSOUND_FREE,Soundhandle);      //   Start playing
            writeln(' ready... ');
            FSOUND_SetVolume(0, FVolume);
            playlist.items[index].played:=true;
            fplaying:=true;
            result:=0;
            FCurrentTrack:=index;
           end else begin
               write('error: can''t play file');writeln(z);
              end;
       end else result:=1;
     end else result:=2;
   end;
  end
  else begin
         writeln('INTERNAL error: playlistindex out of bound');
         Result:=3;
       end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.play(url: string): byte;
{startplay=0 -> succesful
 startplay=1 -> invalid stream URL
 startplay=2 -> soundcard init failed}
var z: integer;

begin
   if (fplaying=true) then begin
         if FSOUND_Stream_Stop(Soundhandle)=false then writeln('ERROR stop stream');
         if FSOUND_Stream_Close(Soundhandle)=false then writeln('ERROR on closing stream');
         fplaying:=false;
      end;
   if (fplaying=false) then begin
   //FSOUND_Close;
 {$ifdef linux}
    if OutputMode = OSSOUT then begin
           if FSOUND_SetOutput(FSOUND_OUTPUT_OSS) then writeln('Oss output openend') else writeln('failed opening oss output')
         end
         else begin
           if FSOUND_SetOutput(FSOUND_OUTPUT_ALSA) then writeln('alsa output openend') else writeln('failed opening alsa output')
         end;
  {$endif}
    if FSOUND_Init(44100, 32, 0)=true then begin
      FPlaybackMode:=STREAMING_MODE;
      writeln('playing  -> '+url);
         tmpp:=StrAlloc(length(url)+1);
         StrPCopy(tmpp,url);

       // Open the stream
         write(' openingstream... ');
         Soundhandle:=FSOUND_Stream_Open(tmpp, FSOUND_NONBLOCKING, 0, 0);


       end else result:=1;
     end else result:=2;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFModPlayerClass.pause;
begin
   if FSOUND_Getpaused(0)=false then
        FSOUND_Setpaused(0, true)
     else   FSOUND_Setpaused(0, false);
   fpaused:=FSOUND_Getpaused(0);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFModPlayerClass.stop;
begin
     if fplaying=true then begin
         FSOUND_Stream_Stop(Soundhandle);
         FSOUND_Stream_Close(Soundhandle);
         fplaying:=false;
         FSOUND_Close;
      //   reset_random;
         FCurrentTrack:=-1;
       end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.prev_track: byte;
var r:byte;
begin
  r:=127;
  if fplaying then begin
    if (FCurrentTrack<Playlist.ItemCount) and (FCurrentTrack>0) then begin
       r:=play(FCurrentTrack-1);
     end else
         if (FCurrentTrack<Playlist.ItemCount) and (FCurrentTrack=0) then begin
             r:=play(FCurrentTrack);
           end;
   end;
  result:=r;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.Get_Stream_Status: TStreamStatus;
var    StreamStatus:TFSoundStreamNetStatus;
    BufferUsed, Bitrate: integer;
    Flags: Cardinal;
begin
if FSOUND_Stream_GetOpenState(Soundhandle)=0 then begin
    FSOUND_Stream_Net_GetStatus(Soundhandle, StreamStatus, BufferUsed, Bitrate, Flags);
   //writeln(BufferUsed);
    case StreamStatus of
        FSOUND_STREAM_NET_READY: Result:=STREAM_READY;
        FSOUND_STREAM_NET_ERROR: Result:=STREAM_NOTFOUND;
        FSOUND_STREAM_NET_CONNECTING: Result:=STREAM_BUFFERING;
        FSOUND_STREAM_NET_BUFFERING:Result:=STREAM_BUFFERING;
        FSOUND_STREAM_NET_NOTCONNECTED: Result:=STREAM_NOTFOUND;
     end;
     
     if Result=STREAM_READY then begin
            FSOUND_Stream_Play (FSOUND_FREE,Soundhandle);      //   Start playing
            FSOUND_SetVolume(0, volume);
            fplaying:=true;
       end else begin
               write('error: can''t open stream');
              end;
  end else result:=STREAM_BUFFERING;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.next_track: byte;
var r:byte;
begin
  r:=127;
  if fplaying then begin
    if FCurrentTrack<Playlist.ItemCount-1 then begin
       r:=play(CurrentTrack+1);
     end;
   end;
  result:=r;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFModPlayerClass.mute;
begin
  if FSOUND_GetMute(0)=false then FSOUND_SetMute(FSOUND_ALL, true) else FSOUND_SetMute(FSOUND_ALL, false);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.Get_TrackLength:longint;
begin
  if (Soundhandle<>nil) and (FSOUND_Stream_GetOpenState(soundhandle)=0) then begin
     result:=FSOUND_Stream_GetLengthMs(Soundhandle);
   end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.get_time: longint;
begin
  if (Soundhandle<>nil) and (FSOUND_Stream_GetOpenState(soundhandle)=0) then begin
     get_time:=FSOUND_Stream_GetTime(Soundhandle);
   end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.get_timestr:string;
begin
  if (Soundhandle<>nil) and (FSOUND_Stream_GetOpenState(soundhandle)=0) then begin
     result:=MSecondsToFmtStr(FSOUND_Stream_GetTime(Soundhandle));
   end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.Get_TimeRemainingStr: string;
begin
  if (Soundhandle<>nil) and (FSOUND_Stream_GetOpenState(soundhandle)=0)
  then
  begin
     result:= '-' + MSecondsToFmtStr(
              FSOUND_Stream_GetLengthMs(Soundhandle) -
              FSOUND_Stream_GetTime(Soundhandle));
   end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.get_fileposition: longint;
begin
  result:=(FSOUND_Stream_GetPosition(Soundhandle)*100) div FSOUND_Stream_GetLength(soundhandle);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.get_filelength: longint;
begin
  get_filelength:=FSOUND_Stream_GetLength(soundhandle);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFModPlayerClass.muted: boolean;
begin
  if FSOUND_GetMute(0)=true then result:=true else result:=false;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFModPlayerClass.set_time(ms: longint);
begin
  if fplaying then FSOUND_Stream_SetTime(Soundhandle,ms);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFModPlayerClass.set_fileposition(fpos: longint);
begin
  if fplaying then FSOUND_Stream_SetPosition(Soundhandle,(fpos*FSOUND_Stream_GetLength(soundhandle)) div 100);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFModPlayerClass.set_volume(vol: byte);
{set volume from 0 to 100}
var i:integer;
begin
  FVolume:=vol;
  i:=((vol*255) div 100);
  FSOUND_SetVolume(0, i);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

initialization
  fmod_load('');
  
finalization
  FMOD_Unload;
  
end.
