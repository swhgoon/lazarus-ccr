(*
  this file is a part of audio components suite
  see the license file for more details.
  you can contact me at mail@z0m3ie.de

$Log: acs_audio.pas,v $
Revision 1.12  2006/07/07 15:51:19  z0m3ie
*** empty log message ***

Revision 1.11  2006/07/04 18:38:32  z0m3ie
*** empty log message ***

Revision 1.10  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.4  2006/01/02 18:54:46  z0m3ie
*** empty log message ***

Revision 1.3  2006/01/01 18:46:40  z0m3ie
*** empty log message ***

Revision 1.2  2005/12/26 17:31:38  z0m3ie
fixed some problems in acs_dsfiles
fixed some problems in acs_vorbis
reworked all buffers

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.14  2005/12/18 17:01:54  z0m3ie
delphi compatibility

Revision 1.13  2005/12/04 16:54:33  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.12  2005/11/28 21:57:24  z0m3ie
mostly FileOut fixes
moved PBuffer to PBuffer8
set all to dynamically Buffering

Revision 1.11  2005/11/27 16:50:33  z0m3ie
add ACS VolumeQuerry
make ACS_VolumeQuerry localizeable
some little errorfixes (buffersize for linuxdrivers was initially 0)
make TAudioIn workable

Revision 1.10  2005/10/02 16:51:46  z0m3ie
*** empty log message ***

Revision 1.9  2005/09/23 14:04:58  z0m3ie
*** empty log message ***

Revision 1.8  2005/09/18 19:28:59  z0m3ie
more progress on driver handling

Revision 1.7  2005/09/16 17:34:29  z0m3ie
*** empty log message ***

Revision 1.6  2005/09/15 20:59:37  z0m3ie
start translate the documentation in the source for pasdoc

Revision 1.5  2005/09/14 21:19:37  z0m3ie
*** empty log message ***

Revision 1.4  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.3  2005/09/13 20:14:52  z0m3ie
driver handling classes (basic audio class)
*)

{
@abstract(this unit introduces the base classes for acs)
@author(Andrei Borovsky (2003-2005))
@author(Christian Ulrich (2005))
}

unit acs_audio;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  ACS_Types, ACS_Classes,Classes,ACS_Strings,SysUtils;

type
  { Audio Formats used in DeviceInfo record
    format constants mask : af<SampleRate><Mono/Stereo><BitsPerSample>
    where 1, 2, 4 means sample rate of 11025, 22050, and 44100 Hz respectively
    M, S means mono or stereo, 08, 16 means 8 or 16 bits per sample.
    For example, af4S16 corresponds to 44100 Hz stereo 16 bit format.
  }

  TACSAudioFormat = (af1M08, af1M16, af1S08, af1S16, af2M08, af2M16, af2S08, af2S16,
                  af4M08, af4M16, af4S08, af4S16);
  TACSAudioFormats = set of TACSAudioFormat;
  TACSBaseAudioOut = class;
  TACSBaseAudioIn = class;

  { This record is used to get an deviceinfo from the Drivers
  }
  TACSDeviceInfo = record
    DeviceName : String;
    DrvVersion : LongWord;
    Formats : TACSAudioFormats;
    Stereo : Boolean;
  end;
  
  { This introduces an base class for the drivers
  }

  { TAudioOut }

  { TACSAudioOut }

  TACSAudioOut = class(TComponent)
  private
    FDriver: string;
    FOutput : TACSBaseAudioOut;
    FInput : TACSCustomInput;
    FOnDone: TACSOutputDoneEvent;
    FOnProgress: TACSOutputProgressEvent;
    FOnThreadException: TACSThreadExceptionEvent;
    FLatency : Integer;
    FBufferSize : Integer;
    function GetBufferSize: Integer;
    function GetBusy: Boolean;
    function GetDelay: Integer;
    function GetPriority: TTPriority;
    function GetProgress: real;
    function GetStatus: TACSOutputStatus;
    function GetSuspend: Boolean;
    function GetTE: Integer;
    procedure SetBufferSize(const AValue: Integer);
    procedure SetDelay(const AValue: Integer);
    procedure SetPriority(const AValue: TTPriority);
    procedure SetSuspend(const AValue: Boolean);

    procedure ThreadException(Sender : TComponent;E : Exception);
    procedure OutputDone(Sender : TComponent);
    procedure OutputProgress(Sender : TComponent);
  protected
    FBaseChannel: Integer;
    FVolume: Byte;
    
    procedure SetInput(Input : TACSCustomInput);

    procedure SetDevice(Ch : Integer);virtual;
    function GetDeviceInfo : TACSDeviceInfo;virtual;
    function GetDeviceCount : Integer;virtual;
    procedure SetDriver(Driver : string);virtual;

    function GetDriverName(idx : Integer) : string;
    function GetDriversCount : Integer;

    procedure Done;
    function DoOutput(Abort : Boolean):Boolean;
    procedure Prepare;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    { The result returns an deviceinfo record that can be used to enumerate devices
      just set device property from 0 to DeviceCount-1 and read deviceInfo to
      enumerate all Devices from current Driver
    }
    property DeviceInfo : TACSDeviceInfo read GetDeviceInfo;
    { Returns the count of devices supported by actual driver
    }
    property DeviceCount : Integer read GetDeviceCount;
    { This can be used to enumerate the Drivers
      just use Driverscount as index it returns the DriverName
    }
    property Drivers[idx : Integer] : string read GetDriverName;
    { Returns the total count of avalible drivers
    }
    property DriversCount : Integer read GetDriversCount;
    { pauses the output.
    }
    procedure Pause;virtual;
    { Resumes previously paused output.
    }
    procedure Resume;virtual;
    { This is the most important method in the output components.
      After an input component has been assigned, call Run to start audio-processing chain.
    }
    procedure Run;
    { Stops the running output process.
    }
    procedure Stop;
    { Output components perform output in their own threads.
      Use this property to set the priority for the thread.
    }
    property ThreadPriority :  TTPriority read GetPriority write SetPriority;
    { Read Progress to get the output progress in percents.
      This value is meaningful only after the input component has been set
      and only if the input component can tell the size of its stream.
    }
    property Progress : real read GetProgress;
    { This property indicates the output component's current status. Possible values are:

      tosPlaying: the component is working;

      tosPaused: the component is paused (the Pause method was called);

      tosIdle: the component is idle;
    }
    property Status : TACSOutputStatus read GetStatus;
    property TimeElapsed : Integer read GetTE;
    property Latency : Integer read FLatency;
  published
    { The output buffer size in bytes default is 4000
    }
    property Buffersize : Integer read GetBufferSize write SetBufferSize;
    { use this property to set an driver, on create of this component the driver
      with lowest latency is used for default
    }
    property Driver : string read FDriver write SetDriver stored True;
    { Use this property to set the output device
    }
    property Busy : Boolean read GetBusy;
    property Device : Integer read FBaseChannel write SetDevice stored True;
    property Volume : Byte read FVolume write FVolume;
    property Input : TACSCustomInput read FInput write SetInput;
    { Use this property to set the delay (in milliseconds) in output thread.
      This property allows the user to reduce the stress the output thread puts
      on the CPU (especially under Windows).
      Be careful with this property when using TAudioOut component.
      Assigning too large values to it can cause dropouts in audio playback.
    }
    property Delay : Integer read GetDelay write SetDelay;
    property SuspendWhenIdle : Boolean read GetSuspend write SetSuspend;
    property OnDone : TACSOutputDoneEvent read FOnDone write FOndone;
    property OnProgress : TACSOutputProgressEvent read FOnProgress write FOnProgress;
    property OnThreadException : TACSThreadExceptionEvent read FOnThreadException write FOnThreadException;
  end;

  { TAudioIn }

  TACSAudioIn = class(TACSCustomInput)
  private
    FInput : TACSBaseAudioIn;
    FDriver : string;
    function GetBPS : Integer;override;
    function GetCh : Integer;override;
    function GetSR : Integer;override;
    procedure SetDevice(Ch : Integer);
    function GetDeviceInfo : TACSDeviceInfo;
    function GetTotalTime : real;override;
    function GetDriverName(idx : Integer) : string;
    function GetDriversCount : Integer;
    procedure SetDriver(Driver : string);
  protected
    FBPS: Integer;
    FChan: Integer;
    FFreq: Integer;
    FRecTime: Integer;
    FBaseChannel: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer;override;
    procedure Init;override;
    procedure Flush;override;
    { The result returns an deviceinfo record that can be used to enumerate devices
      just set device property from 0 to DeviceCount-1 and read deviceInfo to
      enumerate all Devices from current Driver
    }
    property DeviceInfo : TACSDeviceInfo read GetDeviceInfo;
    { This can be used to enumerate the Drivers
      just use Driverscount as index it returns the DriverName
    }
    property Drivers[idx : Integer] : string read GetDriverName;
    { Returns the total count of avalible drivers
    }
    property DriversCount : Integer read GetDriversCount;
  published
    { use this property to set an driver, on create of this component the driver
      with lowest latency is used for default
    }
    property Driver : string read FDriver write SetDriver stored True;
    { Use this property to set the output device
    }
    property Device : Integer read FBaseChannel write SetDevice stored True;
    { Use this property to set the number of bits per sample for the input audio stream.
      Possible values are 8 and 16.
    }
    property InBitsPerSample : Integer read GetBPS write FBPS stored True;
    { Use this property to set the number of channels for the input audio stream.
      Possible values are 1 (mono) and 2 (stereo).
    }
    property InChannels : Integer read GetCh write FChan stored True;
    { Use this property to set the sample rate for the input audio stream.
      Possible values are determined by the soundcard hardware.
    }
    property InSampleRate : Integer read GetSR write FFreq stored True;
    { This property allow you to set the record duration time in seconds.
      If you assign -1 to this property TAudioIn will never stop recording by itself.
      In both cases you can stop recording at any time by calling Stop method of
      the respective output component.
    }
    property RecTime : Integer read FRecTime write FRecTime stored True;
  end;

  { This class is an abstract base class for the drivers
  }

  { TBaseAudioOut }

  TACSBaseAudioOut = class(TACSCustomOutput)
  private
    FDriver: string;
    FOutput : TACSAudioOut;
  protected
    FBaseChannel: Integer;
    FVolume: Byte;
    procedure SetDevice(Ch : Integer);virtual;abstract;
    function GetDeviceInfo : TACSDeviceInfo;virtual;abstract;
    function GetDeviceCount : Integer;virtual;abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property DeviceInfo : TACSDeviceInfo read GetDeviceInfo;
    property DeviceCount : Integer read GetDeviceCount;
    property Buffersize : Integer read FBufferSize write FBufferSize;
  published
    property Device : Integer read FBaseChannel write SetDevice stored True;
    property Volume : Byte read FVolume write FVolume;
  end;

  { This class is an abstract base class for the drivers
  }

  { TBaseAudioIn }

  TACSBaseAudioIn = class(TACSCustomInput)
  private
    FInput : TACSAudioIn;
    FDriver : string;
  protected
    FBPS: Integer;
    FChan: Integer;
    FFreq: Integer;
    FRecTime: Integer;
    FBaseChannel: Integer;
    procedure SetDevice(Ch : Integer);virtual;abstract;
    function GetDeviceInfo : TACSDeviceInfo;virtual;abstract;
    function GetDeviceCount : Integer;virtual;abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DeviceInfo : TACSDeviceInfo read GetDeviceInfo;
  published
    property Device : Integer read FBaseChannel write SetDevice stored True;
    property InBitsPerSample : Integer read GetBPS write FBPS stored True;
    property InChannels : Integer read GetCh write FChan stored True;
    property InSampleRate : Integer read GetSR write FFreq stored True;
    property RecTime : Integer read FRecTime write FRecTime stored True;
  end;

  TACSAudioOutClass = class of TACSBaseAudioOut;
  TACSAudioInClass = class of TACSBaseAudioIn;

  TACSOutDriverinfo = record
    DriverName : string;
    Latency : Integer;
    DrvClass : TACSAudioOutClass;
  end;

  TACSInDriverinfo = record
    DriverName : string;
    Latency : Integer;
    DrvClass : TACSAudioInClass;
  end;

  { This procedure must be used to register drivers to the system
    just call them at initialization of the driver main unit
  }
  procedure RegisterAudioOut(DrvName : string;OutClass : TACSAudioOutClass;Latency : Integer);
  { This procedure must be used to register drivers to the system
    just call them at initialization of the driver main unit
  }
  procedure RegisterAudioIn(DrvName : string;InClass : TACSAudioInClass;Latency : Integer);

var
  OutDriverInfos : array of TACSOutDriverInfo;
  InDriverInfos : array of TACSInDriverInfo;


implementation

{ TAudioOut }

function TACSAudioOut.GetDelay: Integer;
begin
  if Assigned(FOutput) then
    Result := FOutput.GetDelay
  else
    Result := -1;
end;

function TACSAudioOut.GetBufferSize: Integer;
begin
  if Assigned(FOutput) then
    Result := FOutput.BufferSize
  else
    Result := -1;
end;

function TACSAudioOut.GetBusy: Boolean;
begin
  if Assigned(FOutput) then
    Result := FOutput.Busy;
end;

function TACSAudioOut.GetPriority: TTPriority;
begin
  if not Assigned(FOutput) then
    raise EACSException(strNoDriverselected);
  Result := FOutput.GetPriority;
end;

function TACSAudioOut.GetProgress: real;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  Result := FOutput.GetProgress;
end;

function TACSAudioOut.GetStatus: TACSOutputStatus;
begin
  if not Assigned(FOutput) then
    Result := tosUndefined
  else
    Result := FOutput.Status;
end;

function TACSAudioOut.GetSuspend: Boolean;
begin
  if Assigned(FOutput) then
    Result := FOutput.GetSuspend;
end;

function TACSAudioOut.GetTE: Integer;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  Result := FOutput.GetTE;
end;

procedure TACSAudioOut.SetBufferSize(const AValue: Integer);
begin
  if Assigned(FOutput) then
    FOutput.BufferSize := AValue;
  FBufferSize := AValue;
end;

procedure TACSAudioOut.SetDelay(const AValue: Integer);
begin
  if Assigned(FOutput) then
    FOutput.SetDelay(AValue);
end;

procedure TACSAudioOut.SetPriority(const AValue: TTPriority);
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  FOutput.SetPriority(AValue);
end;

procedure TACSAudioOut.SetSuspend(const AValue: Boolean);
begin
  if Assigned(FOutput) then
    FOutput.SetSuspend(AValue);
end;

procedure TACSAudioOut.ThreadException(Sender: TComponent; E: Exception);
begin
  if Assigned(FOnThreadException) then
    FOnThreadException(Sender,E);
end;

procedure TACSAudioOut.OutputDone(Sender: TComponent);
begin
  if Assigned(FOnDone) then
    FOnDone(Sender);
end;

procedure TACSAudioOut.OutputProgress(Sender: TComponent);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;

procedure TACSAudioOut.SetInput(Input: TACSCustomInput);
begin
  FInput := Input;
  if Assigned(FOutput) then
    FOutput.Input := Input;
end;

procedure TACSAudioOut.SetDevice(Ch: Integer);
begin
  FBaseChannel := ch;
  if Assigned(FOutput) then
    FOutput.SetDevice(ch);
end;

function TACSAudioOut.GetDeviceInfo : TACSDeviceInfo;
begin
  if Assigned(FOutput) then
    Result := FOutput.DeviceInfo;
end;

function TACSAudioOut.GetDeviceCount : Integer;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  Result := FOutput.GetDeviceCount;
end;

procedure TACSAudioOut.SetDriver(Driver: string);
var
  i : Integer;
begin
  if Driver = '' then
    exit;
  if Assigned(Foutput) then
    FOutput.Free;
  FOutput := nil;
  for i := 0 to length(OutDriverInfos)-1 do
    if OutDriverInfos[i].DriverName = Driver then
      begin
        FOutput := OutDriverInfos[i].DrvClass.Create(nil);
        try
          FOutput.SetDevice(FBaseChannel);
        except
          FOutput.SetDevice(0);
        end;
        FDriver := OutDriverInfos[i].DriverName;
        FLatency := OutDriverInfos[i].Latency;
        if Assigned(FInput) then
          FOutput.Input := FInput;
        FOutput.OnDone := OutputDone;
        FOutput.OnProgress := OutputProgress;
        Foutput.OnThreadException := ThreadException;
        Foutput.Buffersize := FBuffersize;
        exit;
      end;
end;

function TACSAudioOut.GetDriverName(idx: Integer): string;
begin
  if (idx < 0) or (idx > length(OutDriverInfos)-1)then
    exit;
  Result := OutDriverInfos[idx].DriverName;
end;

function TACSAudioOut.GetDriversCount: Integer;
begin
  Result := length(OutDriverInfos);
end;

procedure TACSAudioOut.Done;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  Foutput.Done;
end;

function TACSAudioOut.DoOutput(Abort: Boolean): Boolean;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  FOutput.DoOutput(Abort);
end;

procedure TACSAudioOut.Prepare;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  FOutput.Prepare;
end;

constructor TACSAudioOut.Create(AOwner: TComponent);
var
  lowestindex,lowest,minlat,i : Integer;
  tmp : string;
  exc: Boolean;
label retry;
begin
  inherited Create(AOwner);
  minlat := 0;
retry:
  lowest := 99999;
  for i := 0 to length(OutDriverInfos)-1 do
    if (OutDriverInfos[i].Latency < lowest) and (OutDriverInfos[i].Latency > minlat) then
      begin
        lowest := OutDriverInfos[i].Latency;
        lowestindex := i;
      end;
  if lowest < 99999 then
    begin
      try
        SetDriver(OutDriverInfos[lowestindex].DriverName);
        exc := false;
      except
        minlat := lowest+1;
        exc := true;
      end;
      if exc then
        goto retry;
    end
  else
    FDriver := 'No Driver';
end;

destructor TACSAudioOut.Destroy;
begin
  FOutput.Free;
  inherited Destroy;
end;

procedure TACSAudioOut.Pause;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  FOutput.Pause;
end;

procedure TACSAudioOut.Resume;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  FOutput.Resume;
end;

procedure TACSAudioOut.Run;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoDriverselected);
  FOutput.Run;
end;

procedure TACSAudioOut.Stop;
begin
  if Assigned(FOutput) then
    FOutput.Stop;
end;

{ TACSAudioIn }

function TACSAudioIn.GetBPS: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoDriverselected);
  Result := FInput.GetBPS;
end;

function TACSAudioIn.GetCh: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoDriverselected);
  Result := FInput.GetCh;
end;

function TACSAudioIn.GetSR: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoDriverselected);
  Result := FInput.GetSr;
end;

procedure TACSAudioIn.SetDevice(Ch: Integer);
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoDriverselected);
  FInput.SetDevice(Ch);
end;

function TACSAudioIn.GetDeviceInfo : TACSDeviceInfo;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoDriverselected);
  //TODO: Complete
end;

function TACSAudioIn.GetTotalTime : real;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoDriverselected);
  Result := FInput.GetTotalTime;
end;

function TACSAudioIn.GetDriverName(idx: Integer): string;
begin
  if (idx < 0) or (idx > length(InDriverInfos)-1)then
    exit;
  Result := InDriverInfos[idx].DriverName;
end;

function TACSAudioIn.GetDriversCount: Integer;
begin
  Result := length(InDriverInfos);
end;

procedure TACSAudioIn.SetDriver(Driver: string);
var
  i : Integer;
begin
  if Assigned(FInput) then
    FInput.Free;
  for i := 0 to length(InDriverInfos)-1 do
    if InDriverInfos[i].DriverName = Driver then
      begin
        FDriver := InDriverInfos[i].DriverName;
        FInput := InDriverInfos[i].DrvClass.Create(nil);
        FInput.SetDevice(FBaseChannel);
        exit;
      end;
end;

constructor TACSAudioIn.Create(AOwner: TComponent);
var
  lowestindex,lowest,i : Integer;
  minlat: Integer;
  exc: Boolean;
label retry;
begin
  inherited Create(AOwner);
  minlat := 0;
retry:
  lowest := 99999;
  for i := 0 to length(InDriverInfos)-1 do
    if (InDriverInfos[i].Latency < lowest) and (InDriverInfos[i].Latency > minlat) then
      begin
        lowest := InDriverInfos[i].Latency;
        lowestindex := i;
      end;
  if lowest < 99999 then
    begin
      try
        SetDriver(InDriverInfos[lowestindex].DriverName);
        exc := false;
      except
        minlat := lowest+1;
        exc := true;
      end;
      if exc then
        goto retry;
    end
  else
    FDriver := 'No Driver';
end;

destructor TACSAudioIn.Destroy;
begin
  inherited Destroy;
end;

function TACSAudioIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoDriverselected);
  Result := FInput.GetData(Buffer,BufferSize);
end;

procedure TACSAudioIn.Init;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoDriverselected);
  FInput.Init;
end;

procedure TACSAudioIn.Flush;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoDriverselected);
  FInput.Flush;
end;

procedure RegisterAudioOut(DrvName : string;OutClass : TACSAudioOutClass;Latency : Integer);
begin
  Setlength(OutDriverInfos,length(OutdriverInfos)+1);
  OutDriverInfos[length(OutDriverInfos)-1].DriverName := DrvName;
  OutDriverInfos[length(OutDriverInfos)-1].Latency := Latency;
  OutDriverInfos[length(OutDriverInfos)-1].DrvClass := OutClass;
end;

procedure RegisterAudioIn(DrvName : string;InClass : TACSAudioInClass;Latency : Integer);
begin
  Setlength(InDriverInfos,length(IndriverInfos)+1);
  InDriverInfos[length(InDriverInfos)-1].DriverName := DrvName;
  InDriverInfos[length(InDriverInfos)-1].Latency := Latency;
  InDriverInfos[length(InDriverInfos)-1].DrvClass := InClass;
end;

{ TACSBaseAudioOut }

constructor TACSBaseAudioOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TACSBaseAudioOut.Destroy;
begin
  inherited Destroy;
end;

{ TBaseAudioIn }

constructor TACSBaseAudioIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TACSBaseAudioIn.Destroy;
begin
  inherited Destroy;
end;

initialization

  Setlength(OutDriverInfos,0);
  Setlength(InDriverInfos,0);

finalization

  Setlength(OutDriverInfos,0);
  Setlength(InDriverInfos,0);

end.
