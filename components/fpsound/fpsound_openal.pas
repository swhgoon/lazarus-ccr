{
OpenAL player for the fpsound library

License: The same modified LGPL as the LCL

Authors:

JiXian Yang
Felipe Monteiro de Carvalho
}
unit fpsound_openal;

{$mode objfpc}

interface

uses
  Classes, SysUtils, OpenAL_NT, fpsound;

type

  { TOpenALPlayer }

  TOpenALPlayer = class(TSoundPlayer)
  private
    al_device: PALCdevice;
    al_context: PALCcontext;
    codec_bs: longword;
    al_source: TALuint;
    al_format: integer;
    al_buffers: TALuint;
    al_bufsize: longword;
    al_readbuf: Pointer;
    al_rate: longword;
    al_bufcount: integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Play(ASound: TSoundDocument); override;
    procedure Pause(ASound: TSoundDocument); override;
    procedure Stop(ASound: TSoundDocument); override;
    procedure AdjustToKeyElement(ASound: TSoundDocument; AKeyElement: TSoundKeyElement);
    procedure alStop;
    //function alProcess(ASound: TSoundDocument; AKeyElement: TSoundKeyElement): Boolean;
    function alFillBuffer(ASound: TSoundDocument;
      AKeyElement: TSoundKeyElement): integer;
  end;


implementation

constructor TOpenALPlayer.Create;
begin
  inherited;
  Initialize;
end;

destructor TOpenALPlayer.Destroy;
begin
  inherited;
  Finalize;
end;

procedure TOpenALPlayer.alStop;
begin
  alSourceStop(al_source);
  alSourceRewind(al_source);
  alSourcei(al_source, AL_BUFFER, 0);
end;

{function TOpenALPlayer.alProcess(ASound: TSoundDocument; AKeyElement: TSoundKeyElement): Boolean;
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
end;}

function TOpenALPlayer.alFillBuffer(ASound: TSoundDocument;
  AKeyElement: TSoundKeyElement): integer;
var
  lCurElement: TSoundElement;
  lReadCount: integer = 0;
  lBufferBytePtr: PByte;
  lBufferWordPtr: PWord;
  loop: TALInt;       aaa: TStringList;
begin
  GetMem(al_readbuf, al_bufsize);
  Result := 0;

  lBufferBytePtr := al_readbuf;
  lBufferWordPtr := al_readbuf;

  while lReadCount < al_bufsize do
  begin
    lCurElement := ASound.GetNextSoundElement();
    if lCurElement = nil then
      Exit;

    Inc(Result);
    lReadCount := lReadCount + AKeyElement.BitsPerSample div 8;

    if AKeyElement.BitsPerSample = 8 then
    begin
      lBufferBytePtr^ := Lo((lCurElement as TSoundSample8).ChannelValues[0]);
      Inc(lBufferBytePtr);
    end
    else
    begin
      lBufferWordPtr^ := word((lCurElement as TSoundSample16).ChannelValues[0]);
      Inc(lBufferWordPtr, 2);
    end;
  end;
  //AlutLoadWavFile('T:\fpsound\testsounds\test.wav', al_format, al_readbuf, al_bufsize, al_rate, loop);
  //alutLoadWAVMemory(ASound.GetSoundDocPtr, al_format, al_readbuf, al_bufsize, al_rate, loop);
  LoadWavStream(ASound.SoundDocStream, al_format, al_readbuf, al_bufsize, al_rate, loop);

aaa := TStringList.Create;
aaa.Add(IntToStr(ASound.SoundDocStream.Size));
aaa.SaveToFile(IntToStr(ASound.SoundDocStream.Size) + '.txt');
end;

procedure TOpenALPlayer.Initialize;
var
  argv: array of PALbyte;
begin
  if FInitialized then
    Exit;

  IsMultiThread := False;
  if not InitOpenAL then
    Exception.Create('Initialize OpenAL failed');
{  al_device := alcOpenDevice(nil);
  al_context := alcCreateContext(al_device, nil);
  alcMakeContextCurrent(al_context);  }

  alutInit(nil, argv);
  al_bufcount := 1;
  alGenBuffers(al_bufcount, @al_buffers);

  alListener3f(AL_POSITION, 0, 0, 0);
  alListener3f(AL_VELOCITY, 0, 0, 0);
  alListener3f(AL_ORIENTATION, 0, 0, -1);

  alGenSources(1, @al_source);

  alSourcef(al_source, AL_PITCH, 1);
  alSourcef(al_source, AL_GAIN, 1);
  alSource3f(al_source, AL_POSITION, 0, 0, 0);
  alSource3f(al_source, AL_VELOCITY, 0, 0, 0);
  alSourcei(al_source, AL_LOOPING, AL_FALSE);

  //  alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED);

  FInitialized := True;
end;

procedure TOpenALPlayer.Finalize;
begin
  // finalize openal
  alDeleteSources(1, @al_source);
  alDeleteBuffers(al_bufcount, @al_buffers);
  AlutExit();
  if al_readbuf <> nil then
    FreeMem(al_readbuf);
//  alcDestroyContext(al_context);
//  alcCloseDevice(al_device);
  FInitialized := False;
end;

procedure TOpenALPlayer.Play(ASound: TSoundDocument);
var
  queued: integer;
  done: boolean;
  lKeyElement: TSoundKeyElement;
begin
  // Initialize;

  // First adjust to the first key element
  lKeyElement := ASound.GetFirstSoundElement();
  AdjustToKeyElement(ASound, lKeyElement);

  // Now clean up the source
  alSourceStop(al_source);
  alSourceRewind(al_source);
//  alSourcei(al_source, AL_BUFFER, 0);
  if al_readbuf <> nil then
    FreeMem(al_readbuf);

  // Fill the buffer
  alFillBuffer(ASound, lKeyElement);
  alBufferData(al_buffers, al_format, al_readbuf, al_bufsize, al_rate);
  alSourceQueueBuffers(al_source, 1, @al_buffers);

  // Play the sound
  alSourcePlay(al_source);
end;

procedure TOpenALPlayer.Stop(ASound: TSoundDocument);
begin
  alSourceStop(al_source);
end;

procedure TOpenALPlayer.Pause(ASound: TSoundDocument);
begin
  alSourcePause(al_source);
end;

procedure TOpenALPlayer.AdjustToKeyElement(ASound: TSoundDocument;
  AKeyElement: TSoundKeyElement);
begin
  // initialize codec
  if AKeyElement.Channels = 1 then
  begin
    if AKeyElement.BitsPerSample = 8 then
      al_format := AL_FORMAT_MONO8
    else
      al_format := AL_FORMAT_MONO16;
  end
  else
  begin
    if AKeyElement.BitsPerSample = 8 then
      al_format := AL_FORMAT_STEREO8
    else
      al_format := AL_FORMAT_STEREO16;
  end;

  codec_bs := 2 * AKeyElement.Channels;
//  al_bufsize := 20000 - (20000 mod codec_bs);
  al_rate := AKeyElement.SampleRate;
  //  WriteLn('Blocksize    : ', codec_bs);
  //  WriteLn('Rate         : ', wave.fmt.SampleRate);
  //  WriteLn('Channels     : ', wave.fmt.Channels);
  //  WriteLn('OpenAL Buffers     : ', al_bufcount);
  //  WriteLn('OpenAL Buffer Size : ', al_bufsize);

 // if al_readbuf <> nil then
 //   FreeMem(al_readbuf);

  //alProcess(ASound, AKeyElement);
end;

initialization
  RegisterSoundPlayer(TOpenALPlayer.Create, spOpenAL);

end.

