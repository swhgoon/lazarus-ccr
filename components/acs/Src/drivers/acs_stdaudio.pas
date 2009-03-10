(*
  this file is a part of audio components suite v 2.3 (delphi version).
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at acs@compiler4.net
  this is the acs for delphi (windows) version of the unit.
*)

{
$Log: acs_stdaudio.pas,v $
Revision 1.6  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.4  2006/01/01 18:46:40  z0m3ie
*** empty log message ***

Revision 1.3  2005/12/30 11:10:57  z0m3ie
some corrections to lazarus-linux depending things

Revision 1.2  2005/12/26 17:31:39  z0m3ie
fixed some problems in acs_dsfiles
fixed some problems in acs_vorbis
reworked all buffers

Revision 1.1  2005/12/19 18:36:05  z0m3ie
*** empty log message ***

Revision 1.8  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.7  2005/11/27 16:50:33  z0m3ie
add ACS VolumeQuerry
make ACS_VolumeQuerry localizeable
some little errorfixes (buffersize for linuxdrivers was initially 0)
make TAudioIn workable

Revision 1.6  2005/10/05 20:26:36  z0m3ie
Linux changes

Revision 1.5  2005/10/02 16:51:31  z0m3ie
*** empty log message ***

Revision 1.4  2005/09/23 14:04:58  z0m3ie
*** empty log message ***

Revision 1.3  2005/09/18 19:28:59  z0m3ie
more progress on driver handling

Revision 1.2  2005/09/14 21:19:37  z0m3ie
*** empty log message ***

Revision 1.1  2005/09/13 21:53:45  z0m3ie
maked seperat driver (not complete jet)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.3  2005/09/02 16:27:49  z0m3ie
*** empty log message ***

Revision 1.2  2005/08/28 20:31:17  z0m3ie
linux restructuring for 2.4

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

Revision 1.4  2005/08/22 20:17:02  z0m3ie
changed Headers to log
changed mail adress

}

unit acs_stdaudio;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes,ACS_Audio,ACS_Strings
  {$IFDEF MSWINDOWS}
  , Windows, MMSystem
  {$ELSE}
  , Soundcard
  {$ENDIF}
  ;

const
  LATENCY = 110;

type
  {$IFDEF MSWINDOWS}
  {$IFDEF FPC}
  TWaveInCapsA = WAVEINCAPSA;
  TWaveInCaps = TWaveInCapsA;

  TWaveHdr = WAVEHDR;
  {$ENDIF}

  PPWaveHdr = ^PWaveHdr;
  {$ENDIF}

  { TStdAudioOut }

  TStdAudioOut = class(TACSBaseAudioOut)
  private
    {$IFDEF MSWINDOWS}
    BlockChain : PWaveHdr;
    aBlock : Integer;
    EOC : PPWaveHdr;
    FReadChunks : Integer;
    {$ENDIF}
    _audio_fd : Integer;
    procedure SetDevice(Ch : Integer);override;
    function GetDeviceInfo : TACSDeviceInfo;override;
    {$IFDEF MSWINDOWS}
    procedure WriteBlock(P : Pointer; Len : Integer);
    procedure AddBlockToChain(WH : PWaveHdr);
    {$ENDIF}
    function GetDeviceCount : Integer;override;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TStdAudioIn = class(TACSBaseAudioIn)
  private
    {$IFDEF MSWINDOWS}
    BlockChain : PWaveHdr;
    FBlocksCount : Integer;
    aBlock : Integer;
    EOC : PPWaveHdr;
    {$ENDIF}
    _audio_fd : Integer;
    FOpened : Integer;
    FRecBytes : Integer;
    procedure OpenAudio;
    procedure CloseAudio;
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    procedure SetDevice(Ch : Integer);override;
    function GetDeviceInfo : TACSDeviceInfo;override;
    function GetTotalTime : real; override;
    {$IFDEF MSWINDOWS}
    procedure NewBlock;
    procedure AddBlockToChain(WH : PWaveHdr);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; oBufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  end;

var
  InputChannelsCount : Integer;
  OutputChannelsCount : Integer;

function GetAudioDeviceInfo(DevID : Integer; OutputDev : Boolean) : TACSDeviceInfo;

implementation

var
  CrSecI, CrSecO : TRTLCriticalSection;
  
{$IFDEF MSWINDOWS}
{$I windows\acs_audio.inc}
{$ELSE}
{$I linux\acs_audio.inc}
{$ENDIF}

function TStdAudioOut.GetDeviceInfo : TACSDeviceInfo;
begin
  Result := GetAudioDeviceInfo(FBaseChannel, True);
end;

function TStdAudioIn.GetDeviceInfo : TACSDeviceInfo;
begin
  Result := GetAudioDeviceInfo(FBaseChannel, False);
end;

function TStdAudioIn.GetTotalTime : real;
begin
  Result := RecTime;
end;

constructor TStdAudioIn.Create;
begin
  inherited Create(AOwner);
  FBPS := 8;
  FChan := 1;
  FFreq := 8000;
  FSize := -1;
  FRecTime := 600;
  BufferSize := $1000;
  {$IFDEF MSWINDOWS}
  FBlocksCount := 4;
  {$ENDIF}
end;

function TStdAudioOut.GetDeviceCount : Integer;
begin
  Result := OutputChannelsCount;
end;

initialization
  {$IFDEF MSWINDOWS}
  InitializeCriticalSection(CrSecI);
  InitializeCriticalSection(CrSecO);
  {$ENDIF}
  CountChannels;
  RegisterAudioOut('Wavemapper',TStdAudioOut,LATENCY);
  RegisterAudioIn('Wavemapper',TStdAudioIn,LATENCY);

finalization
  {$IFDEF MSWINDOWS}
  DeleteCriticalSection(CrSecI);
  DeleteCriticalSection(CrSecO);
  {$ENDIF}

end.
