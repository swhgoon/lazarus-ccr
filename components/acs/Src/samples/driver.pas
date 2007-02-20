(*
  this file is a part of audio components suite
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
  
  this is an sample unit for an driver
*)

unit driver;

interface

uses
  ACS_Audio,SysUtils, Classes, ACS_Types, ACS_Classes, ACS_Strings;

const
  LATENCY = 25;

type
  TOwnAudioOut = class(TACSBaseAudioOut)
  private
    EndOfInput, StartInput : Boolean;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    procedure SetDevice(Ch : Integer);override;
    function GetDeviceInfo : TACSDeviceInfo;override;
    function GetDeviceCount : Integer;override;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Pause;override;
    procedure Resume;override;
  end;

  TOwnAudioIn = class(TACSBaseAudioIn)
  private
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    FBPS, FChan, FFreq : Integer;
    FOpened : Integer;
    FRecTime : Integer;
    procedure SetDevice(i : Integer);override;
    procedure OpenAudio;
    procedure CloseAudio;
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    function GetTotalTime : real; override;
    procedure SetRecTime(aRecTime : Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  end;

implementation

procedure TOwnAudioOut.Prepare;
begin
  FInput.Init;
  FBuffer := AllocMem(FBufferSize);
  StartInput := True;
  EndOfInput := False;
  
  //Init your output device here
end;

procedure TOwnAudioOut.Done;
begin
  //Flush your output device here
  
  Finput.Flush;
  FreeMem(FBuffer);
end;

function TOwnAudioOut.DoOutput(Abort : Boolean):Boolean;
var
  Len, offs, lb : Integer;
  Stat : LongWord;
  Res : HRESULT;
  PlayTime, CTime : LongWord;
begin
  Result := True;
  if not Busy then Exit;
  if not CanOutput then
  begin
    Result := False;
    Exit;
  end;
  if StartInput then
  begin
    Len := 0;
    while Len < FBufferSize do
    begin
      offs := FInput.GetData(@FBuffer^[Len], FBufferSize-Len);
      if offs = 0 then
      begin
        EndOfInput := True;
        Break;
      end;
      Inc(Len, offs);
    end;
    //Do Output Len bytes from FBuffer^

    StartInput := False;
  end;
  if Abort then
  begin
    //Stop your output device

    CanOutput := False;
    Result := False;
    Exit;
  end;
  if EndOfInput then
  begin
    CanOutput := False;
    
    //Stop your output device
    
    Result := False;
    Exit;
  end;
end;

constructor TOwnAudioOut.Create;
begin
  inherited Create(AOwner);
  FBufferSize := $40000;
  
  //enumerate devices and set Devicecount
  FDeviceCount := 0;
end;

destructor TOwnAudioOut.Destroy;
begin
  //Wahtever
end;

procedure TOwnAudioOut.Pause;
begin
  if EndOfInput then Exit;
  //Pause
end;

procedure TOwnAudioOut.Resume;
begin
  if EndOfInput then Exit;
  //waht schoud i say ?
end;

procedure TOwnAudioOut.SetDevice(Ch: Integer);
begin
  FBaseChannel := Ch;
end;

function TOwnAudioOut.GetDeviceInfo: TACSDeviceInfo;
begin
  if (FBaseChannel >= FDeviceCount) then
    exit;
  //return an Deviceinfo
end;

function TOwnAudioOut.GetDeviceCount: Integer;
begin
  Result := FDeviceCount;
end;

constructor TOwnAudioIn.Create;
begin
  inherited Create(AOwner);
  FBPS := 8;
  FChan := 1;
  FFreq := 8000;
  FSize := -1;
  BufferSize := $2000;
  //enumerate devices and set Devicecount
  FDeviceCount := 0;
end;

destructor TOwnAudioIn.Destroy;
begin
  //whatever
  inherited Destroy;
end;

procedure TOwnAudioIn.OpenAudio;
var
  Res : HResult;
  BufSize : Integer;
begin
  BufSize := BufferSize;
  if FOpened = 0 then
  begin
    //Init your In put device
  end;
  Inc(FOpened);
end;

procedure TOwnAudioIn.CloseAudio;
begin
  if FOpened = 1 then
    begin
      //Flush your Input device
    end;
  if FOpened > 0 then Dec(FOpened);
end;

function TOwnAudioIn.GetBPS : Integer;
begin
  Result := FBPS;
end;

function TOwnAudioIn.GetCh : Integer;
begin
  Result := FChan;
end;

function TOwnAudioIn.GetSR : Integer;
begin
  Result := FFreq;
end;

procedure TOwnAudioIn.Init;
begin
  if Busy then raise EACSException.Create(strBusy);
  if (FDeviceNumber >= FDeviceCount) then raise EACSException.Create(Format(strChannelnotavailable,[FDeviceNumber]));
  if FRecTime > 0 then FBytesToRead := FRecTime*FFreq*FChan*(FBPS div 8);
  BufEnd := 0;
  BufStart := 1;
  FPosition := 0;
  Busy := True;
  FSize := FBytesToRead;
  OpenAudio;
end;

procedure TOwnAudioIn.Flush;
begin
  CloseAudio;
  Busy := False;
end;

function TOwnAudioIn.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
var
  l : Integer;
begin
  if not Busy then  raise EACSException.Create(strStreamnotopen);
  if  (FBytesToRead >=0) and (FPosition >= FBytesToRead) then
  begin
    Result := 0;
    Exit;
  end;
  if BufStart >= BufEnd then
  begin
    BufStart := 0;
    
    //Read Buffersize bytes from your input device and convert to PCM
    //l is the real readed size maybe you cant ead the full buffer jet
    BufEnd := l;
  end;
  if BufferSize < (BufEnd - BufStart)
  then Result := BufferSize
  else Result := BufEnd - BufStart;
  Move(FBuffer[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

procedure TOwnAudioIn.SetRecTime;
begin
  FRecTime := aRecTime;
  if FRecTime > 0 then
    FBytesToRead := FRecTime*FFreq*FChan*(FBPS div 8)
  else
    FBytesToRead := -1;
end;

procedure TOwnAudioIn.SetDevice(i : Integer);
begin
  FDeviceNumber := i
end;

function TOwnAudioIn.GetTotalTime : real;
var
  BytesPerSec : Integer;
begin
  BytesPerSec := FFreq*FChan*(FBPS div 8);
  Result := FBytesToRead/BytesPerSec;
end;

initialization
  RegisterAudioOut('My Drivers Name',TOwnAudioOut,LATENCY);
  RegisterAudioIn('My Drivers Name',TOwnAudioIn,LATENCY);

finalization

end.
