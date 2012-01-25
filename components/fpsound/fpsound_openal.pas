{
}
unit fpsound_openal;

{$mode objfpc}

interface

uses
  Classes, SysUtils, openal, fpsound_wav, fpsound;

// openal
const
  // Note: if you lower the al_bufcount, then you have to modify the al_polltime also!
  al_bufcount           = 4;
  al_polltime           = 100;

var
  al_device   : PALCdevice;
  al_context  : PALCcontext;

type

  { TOpenALPlayer }

  TOpenALPlayer = class(TSoundPlayer)
  private
    source     : TStream;
    codec_bs   : Longword;
    OPCSoundWasInitialized: Boolean;
    OPCSoundStreamIsLoaded: Boolean;
    al_source   : ALuint;
    al_format   : Integer;
    al_buffers  : array[0..al_bufcount-1] of ALuint;
    al_bufsize  : Longword;
    al_readbuf  : Pointer;
    al_rate     : Longword;
    wave       : TWaveReader;
  public
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Play(ASound: TSoundDocument); override;
    procedure AdjustToKeyElement(ASound: TSoundDocument; AKeyElement: TSoundKeyElement);
    procedure alStop;
    function alProcess(ASound: TSoundDocument; AKeyElement: TSoundKeyElement): Boolean;
    function alFillBuffer(ASound: TSoundDocument; AKeyElement: TSoundKeyElement): Integer;
  end;


implementation

procedure TOpenALPlayer.alStop;
begin
  alSourceStop(al_source);
  alSourceRewind(al_source);
  alSourcei(al_source, AL_BUFFER, 0);
end;

function TOpenALPlayer.alProcess(ASound: TSoundDocument; AKeyElement: TSoundKeyElement): Boolean;
var
  processed : ALint;
  buffer    : ALuint;
  sz        : Integer;
begin
  alGetSourcei(al_source, AL_BUFFERS_PROCESSED, processed);
  while (processed > 0) and (processed <= al_bufcount) do
  begin
    alSourceUnqueueBuffers(al_source, 1, @buffer);
    sz := alFillBuffer(ASound, AKeyELement);
    if sz <= 0 then
    begin
      Exit(False);
    end;
    alBufferData(buffer, al_format, al_readbuf, sz, al_rate);
    alSourceQueueBuffers(al_source, 1, @buffer);
    Dec(processed);
  end;
  Result := True;
end;

function TOpenALPlayer.alFillBuffer(ASound: TSoundDocument; AKeyElement: TSoundKeyElement): Integer;
var
  lCurElement: TSoundElement;
  lReadCount: Integer = 0;
begin
  Result := 0;
  while lReadCount < al_bufsize do
  begin
    lCurElement := ASound.GetNextSoundElement();
    if lCurElement = nil then Exit;

    Inc(Result);
    lReadCount := lReadCount + AKeyElement.BitsPerSample div 8;

    if AKeyElement.BitsPerSample = 8 then
      PByte(al_readbuf)[lReadCount] := Lo((lCurElement as TSoundSample8).ChannelValues[0])
    else
      PWord(al_readbuf)[lReadCount div 2] := Word((lCurElement as TSoundSample16).ChannelValues[0])
  end;
end;

procedure TOpenALPlayer.Initialize;
begin
//  alutInit(0, NULL);

{  alListenerfv(AL_POSITION,listenerPos);
  alListenerfv(AL_VELOCITY,listenerVel);
  alListenerfv(AL_ORIENTATION,listenerOri);}

  alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED);
  alGenSources(1, @al_source);
  alGenBuffers(al_bufcount, @al_buffers);
end;

procedure TOpenALPlayer.Finalize;
begin
  // finalize openal
  alDeleteSources(1, @al_source);
  alDeleteBuffers(al_bufcount, @al_buffers);
  if al_readbuf <> nil then FreeMem(al_readbuf);

//  wave.fStream := nil;
//  source := nil;

  // finalize codec
//  wave.Free;

  // close file
//  source.Free;
end;

procedure TOpenALPlayer.Play(ASound: TSoundDocument);
var
  i: Integer;
  queued  : Integer;
  done    : Boolean;
  lKeyElement: TSoundKeyElement;
  //
{  buffer : Cardinal;
  sourcepos: array [0..2] of Single=(0.0, 0.0, 0.0);
  sourcevel: array [0..2] of Single=(0.0, 0.0, 0.0);
  listenerpos: array [0..2] of Single=(0.0, 0.0, 0.0);
  listenervel: array [0..2] of Single=(0.0, 0.0, 0.0);
  listenerori: array [0..5] of Single=(0.0, 0.0, -1.0, 0.0, 1.0, 0.0);
  Context: PALCcontext;
  Device: PALCdevice;  }
begin
  Initialize();

  // First adjust to the first key element
  lKeyElement := ASound.GetFirstSoundElement();
  AdjustToKeyElement(ASound, lKeyElement);

  // Now clean up the source
  alSourceStop(al_source);
  alSourceRewind(al_source);
  alSourcei(al_source, AL_BUFFER, 0);

  for i := 0 to al_bufcount - 1 do
  begin
    // Fill the buffer
    AlFillBuffer(ASound, lKeyElement);

    alBufferData(al_buffers[i], al_format, al_readbuf, al_bufsize, al_rate);
    alSourceQueueBuffers(al_source, 1, @al_buffers[i]);
  end;

  // Under windows, AL_LOOPING = AL_TRUE breaks queueing, no idea why
  alSourcei(al_source, AL_LOOPING, AL_FALSE);
  alSourcePlay(al_source);

  done:=False;
  queued:=0;
  repeat
    if alProcess(ASound, lKeyElement) then
    begin
      alGetSourcei(al_source, AL_BUFFERS_QUEUED, queued);
      done:=queued=0;
    end;
    Sleep(al_polltime);
  until done;

  {
  AlSourceStop(source);
  AlGenBuffers(1, @buffer);
  loop:=0;
  LoadWavStream(AStream, format, data, size, freq, loop);
  AlBufferData(buffer, format, data, size, freq);

  if data<>nil then freemem(data);

  AlGenSources(1, @source);
  AlSourcei(source, AL_BUFFER, buffer);
  AlSourcef(source, AL_PITCH, 1.0);
  AlSourcef(source, AL_GAIN, 1.0);
  AlSourcefv(source, AL_POSITION, @sourcepos);
  AlSourcefv(source, AL_VELOCITY, @sourcevel);
  // Under windows, AL_LOOPING = AL_TRUE breaks queueing, no idea why
  // AlSourcei(source, AL_LOOPING, AL_TRUE);

  AlListenerfv(AL_POSITION, @listenerpos);
  AlListenerfv(AL_VELOCITY, @listenervel);
  AlListenerfv(AL_ORIENTATION, @listenerori);
  AlSourcePlay(source);}
end;

procedure TOpenALPlayer.AdjustToKeyElement(ASound: TSoundDocument; AKeyElement: TSoundKeyElement);
begin
  // define codec
  //source := AStream;

  // inittialize codec
  if AKeyElement.Channels = 1 then
  begin
    if AKeyElement.BitsPerSample=8 then al_format:=AL_FORMAT_MONO8
    else al_format:=AL_FORMAT_MONO16
  end
  else
  begin
    if AKeyElement.BitsPerSample=8 then al_format := AL_FORMAT_STEREO8
    else al_format:=AL_FORMAT_STEREO16
  end;

  codec_bs:=2*AKeyElement.Channels;
  al_bufsize := 20000 - (20000 mod codec_bs);
  al_rate:=AKeyElement.SampleRate;
//  WriteLn('Blocksize    : ', codec_bs);
//  WriteLn('Rate         : ', wave.fmt.SampleRate);
//  WriteLn('Channels     : ', wave.fmt.Channels);
//  WriteLn('OpenAL Buffers     : ', al_bufcount);
//  WriteLn('OpenAL Buffer Size : ', al_bufsize);

  if al_readbuf <> nil then FreeMem(al_readbuf);
  GetMem(al_readbuf, al_bufsize);

  alProcess(ASound, AKeyElement);
end;

initialization
  RegisterSoundPlayer(TOpenALPlayer.Create, spOpenAL);
end.

