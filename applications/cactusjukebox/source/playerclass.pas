
{
Parent class for all Cactus Jukebox player backends

written by Sebastian Kraft, <c> 2006-2008

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL




}

unit playerclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, playlist;

type
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
TStreamStatus=(STREAM_READY, STREAM_BUFFERING, STREAM_NOTFOUND);
TPlaybackMode=(STREAMING_MODE, FILE_MODE);

TOutputMode=(ALSAOUT, OSSOUT, DIRECTX, WIN32);
TAudioBackend=(MPLAYERBACK, FMODBACK);


{ TPlayerClass }
type
TPlayerClass = class
   Protected
     fTotalLength: int64;
     FPlaying, FPaused: Boolean;
     FVolume: Byte;
     FPlaybackMode:TPlaybackMode;
     function GetCurrentTrack: integer;
     procedure SetCurrentTrack(index: integer);
     property FCurrentTrack: Integer read GetCurrentTrack write SetCurrentTrack;
   Public
     OutputMode: TOutputMode;
     Playlist: TPlaylistClass;

     constructor create;virtual;
     destructor destroy;virtual;

     function play(index:integer):byte;virtual; abstract;
     function play(url: string):byte;virtual; abstract;
     procedure pause;virtual; abstract;
     procedure stop;virtual; abstract;
     function next_track:byte;virtual; abstract;
     function prev_track:byte;virtual; abstract;

     function Get_Stream_Status:TStreamStatus;virtual; abstract;

     function Get_TrackLength:longint;virtual; abstract;  // Get track length in ms
     function Get_Time:longint;virtual; abstract;   // Get current time position in ms
     function Get_TimeStr:string;virtual; abstract; // Get current time position as a 00:00 formatted string
     function Get_TimeRemainingStr:string;virtual; abstract; // Get time remaining as a -00:00 formated stting

     function Get_FilePosition:longint;virtual; abstract;   // Get current position in percent
     function get_FileLength:longint;virtual; abstract;    // Get current file size in bytes

     procedure Set_Time(ms: longint);virtual; abstract;  //Set vurrent time position in ms
     procedure Set_FilePosition(fpos:longint);virtual; abstract;  //set file position in percent
     procedure Set_Volume(vol:byte);virtual; abstract;  // Volume, value range in percent

     procedure Mute;virtual; abstract;    // Mute audio output
     function Muted:boolean;virtual; abstract;   //Get mute state

     property CurrentTrack: Integer read GetCurrentTrack;
     property playing: boolean read FPlaying;
     property paused: boolean read FPaused;
     property volume:byte read FVolume write Set_Volume;
     property PlaybackMode: TPlaybackMode read FPlaybackMode;

  end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

var PlayerObj: TPlayerClass;

implementation

{ TPlayerClass }

function TPlayerClass.GetCurrentTrack: integer;
begin
    result:=Playlist.CurrentTrack;
end;

procedure TPlayerClass.SetCurrentTrack(index: integer);
begin
  Playlist.CurrentTrack:=index;
end;

constructor TPlayerClass.create;
begin
     Playlist:=TPlaylistclass.create;
     fplaying:=false;
     FCurrentTrack:=-1;
     FVolume:=100;
end;

destructor TPlayerClass.destroy;
begin

end;

end.

