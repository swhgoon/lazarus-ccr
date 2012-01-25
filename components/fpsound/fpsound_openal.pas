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
{    buffer : Cardinal;
    sourcepos: array [0..2] of Single=(0.0, 0.0, 0.0);
    sourcevel: array [0..2] of Single=(0.0, 0.0, 0.0);
    listenerpos: array [0..2] of Single=(0.0, 0.0, 0.0);
    listenervel: array [0..2] of Single=(0.0, 0.0, 0.0);
    listenerori: array [0..5] of Single=(0.0, 0.0, -1.0, 0.0, 1.0, 0.0);
    Context: PALCcontext;
    Device: PALCdevice;}
    //
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
    //procedure OPCSoundPlayStreamEx(AStream: TStream);
    procedure alStop;
    function alProcess(ASound: TSoundDocument; AKeyElement: TSoundKeyElement): Boolean;
    function alFillBuffer(ASound: TSoundDocument; AKeyElement: TSoundKeyElement): Integer;
  end;


implementation

(*type
  TWAVHeader = record
    RIFFHeader: array [1..4] of AnsiChar;
    FileSize: longint;
    WAVEHeader: array [1..4] of AnsiChar;
    FormatHeader: array [1..4] of AnsiChar;
    FormatHeaderSize: longint;
    FormatCode: Word;
    ChannelNumber: Word;
    SampleRate: longint;
    BytesPerSecond: longint;
    BytesPerSample: Word;
    BitsPerSample: Word;
  end;

function LoadWavStream(Stream: TStream; var format: integer; var data: Pointer;
  var size: LongInt; var freq: LongInt; var loop: LongInt): Boolean;
var
  WavHeader: TWavHeader;
  readname: pansichar;
  name: ansistring;
  readint: longint;
begin
    Result:=False;

    size:=0;
    data:=nil;

    //Read wav header
    stream.Read(WavHeader, sizeof(TWavHeader));

    //Determine SampleRate
    freq:=WavHeader.SampleRate;

    //Detemine waveformat
    if WavHeader.ChannelNumber = 1 then
    case WavHeader.BitsPerSample of
    8: format := AL_FORMAT_MONO8;
    16: format := AL_FORMAT_MONO16;
    end;

    if WavHeader.ChannelNumber = 2 then
    case WavHeader.BitsPerSample of
    8: format := AL_FORMAT_STEREO8;
    16: format := AL_FORMAT_STEREO16;
    end;

    //go to end of wavheader
    stream.seek((8-44)+12+4+WavHeader.FormatHeaderSize+4,soFromCurrent); //hmm crappy...

    getmem(readname,4); //only alloc memory once, thanks to zy.
    //loop to rest of wave file data chunks
    repeat
      //read chunk name
      stream.Read(readname^, 4);
      name := readname[0]+readname[1]+readname[2]+readname[3];
      if name='data' then
      begin
        //Get the size of the wave data
        stream.Read(readint,4);
        size:=readint;
        //if WavHeader.BitsPerSample = 8 then size:=size+8; //fix for 8bit???
        //Read the actual wave data
        getmem(data,size);
        stream.Read(Data^, size);

        //Decode wave data if needed
        if WavHeader.FormatCode=$0011 then
        begin
          //TODO: add code to decompress IMA ADPCM data
        end;
        if WavHeader.FormatCode=$0055 then
        begin
          //TODO: add code to decompress MP3 data
        end;
        Result:=True;
      end
      else
      begin
        //Skip unknown chunk(s)
        stream.Read(readint,4);
        stream.Position:=stream.Position+readint;
      end;
    until stream.Position>=stream.size;
    freemem(readname);

    loop:= 0;
end;

procedure alutLoadWAVFile(fname: string; var format: Integer; var data: Pointer;
  var size: LongInt; var freq: LongInt; var loop: LongInt);
var
  Stream : TFileStream;
begin
  Stream:=TFileStream.Create(fname,$0000);
  LoadWavStream(Stream, format, data, size, freq, loop);
  Stream.Free;
end;

procedure OPCSoundPlayStreamEx(AStream: TStream);
var
  format: Integer;
  size: LongInt;
  freq: LongInt;
  loop: LongInt;
  data: Pointer;
begin
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
  AlSourcePlay(source);
end;

{$DEFINE OPC_SOUND_OPENAL_THREAD}

{$ifdef WINDOWS}uses Windows; {$endif}

type
  TOpenALThread = class(TThread)
  private

  public
    FAOwner: TStream;
    constructor Create(aOwner: TStream);
    destructor Destroy; override;
    procedure Execute; override;
    procedure DoOpenALPlay;
  end;

var
  TheLastThread: TOpenALThread;

procedure OPCSoundInitialize();
begin

end;

procedure ResetOpenALThread;
begin
  if (TheLastThread <> nil) and (not TheLastThread.Terminated) then
  begin
    try
      {$IFDEF MSWINDOWS}
      TerminateThread(TheLastThread.Handle, 0);
      {$ELSE}
      TheLastThread.Terminate;
      {$ENDIF}
    except
    end;
  end;
end;

procedure OPCSoundLoadWavFromStream(AStream: TStream);
begin
  {$IFDEF OPC_SOUND_OPENAL_THREAD}
  ResetOpenALThread;
  TheLastThread := TOpenALThread.Create(AStream);
  {$ELSE}
  OPCSoundPlayStreamEx(AStream);
  {$ENDIF}
end;

procedure OPCSoundPlay();
begin
  {$IFDEF OPC_SOUND_OPENAL_THREAD}

  {$ELSE}
  OPCAudioPlayer.OPCSoundOpenALInitialize();
  OPCAudioPlayer.OPCSoundOpenALPlay();
  OPCAudioPlayer.OPCSoundOpenALFinalize();
  {$ENDIF}
end;

procedure OPCSoundFinalize();
begin
  {$IFDEF OPC_SOUND_OPENAL_THREAD}
  ResetOpenALThread;
  {$ELSE}
  OPCAudioPlayer.OPCSoundOpenALFinalize();
  {$ENDIF}
end;

procedure OPCSoundPlayStream(AStream: TStream);
begin
  OPCSoundLoadWavFromStream(AStream);
end;

constructor TOpenALThread.Create(aOwner: TStream);
begin
  inherited Create(False);
  FAOwner := aOwner;
  FreeOnTerminate := True;
end;

destructor TOpenALThread.Destroy;
begin

  inherited Destroy;
end;

procedure TOpenALThread.Execute;
begin
  Synchronize(@DoOpenALPlay);
end;

procedure TOpenALThread.DoOpenALPlay;
begin
  OPCSoundPlayStreamEx(FAOwner);
end;*)

///

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
  alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED);
  alGenSources(1, @al_source);
  alGenBuffers(al_bufcount, @al_buffers);

  GetMem(al_readbuf, al_bufsize);
end;

procedure TOpenALPlayer.Finalize;
begin
  // finalize openal
  alDeleteSources(1, @al_source);
  alDeleteBuffers(al_bufcount, @al_buffers);
  FreeMem(al_readbuf);

//  wave.fStream := nil;
//  source := nil;

  // finalize codec
  wave.Free;

  // close file
//  source.Free;
end;

procedure TOpenALPlayer.Play(ASound: TSoundDocument);
var
  i: Integer;
  queued  : Integer;
  done    : Boolean;
  lKeyElement: TSoundKeyElement;
begin
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

  alProcess(ASound, AKeyElement);
end;

end.

