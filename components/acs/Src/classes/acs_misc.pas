(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_misc.pas,v $
Revision 1.4  2006/08/31 20:10:54  z0m3ie
*** empty log message ***

Revision 1.3  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.4  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.3  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.2  2005/09/13 04:04:50  z0m3ie
First release without Components for Fileformats
only TFileIn and TFileOut are Visible

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.9  2005/09/07 20:53:22  z0m3ie
begon to add MPEG and WMA support using DirectX

Revision 1.8  2005/09/04 17:59:37  z0m3ie
moving CDIn support to AKRip mostly
begon to add mpegin support for Win with mpg123

Revision 1.7  2005/08/28 18:35:53  z0m3ie
created Delphi package for 2.4
more Mixer stuff
updated some things for Delphi

Revision 1.6  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}

unit acs_misc;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Strings
  {$IFDEF LINUX}
  ,baseunix, LibAO
  {$ENDIF};

const
  BUF_SIZE = $4000;

type
  TACSOnBufferDone = procedure(Sender : TComponent) of object;

  TACSAudioProcessorInitEvent = procedure(Sender : TComponent; var TotalSize : Integer) of object;
  TACSAudioProcessorFlushEvent = procedure(Sender : TComponent) of object;

  TACSGetParameterEvent = procedure(Sender : TComponent; var Param : Integer) of object;

  TACSGetRealParameterEvent = procedure(Sender : TComponent; var Param : real) of object;

  TACSGetDataEvent = procedure(Sender : TComponent; Data : Pointer; var n : Integer) of object;

  TACSMemoryIn = class(TACSCustomInput)
  private
    FBuffer : PACSBuffer8;
    FDataSize : Integer;
    FOnBufferDone : TACSOnBufferDone;
    Busy : Boolean;
    BufStart, BufEnd : Integer;
    FBPS, FSR, FChan : Integer;
    function GetBuffer : Pointer;
    procedure SetBuffer(v : Pointer);
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    property DataBuffer : Pointer read GetBuffer write SetBuffer;
    property DataSize : Integer read FDataSize write FDataSize;
  published
    property GlobalSize : Integer read FSize write FSize;
    property InBitsPerSample : Integer read GetBPS write FBPS;
    property InChannels : Integer read GetCh write FChan;
    property InSampleRate : Integer read GetSR write FSR;
    property OnBufferDone : TACSOnBufferDone read FOnBufferDone write FOnBufferDone;
  end;

  TACSAudioProcessor = class(TACSCustomConverter)
  private
    FOnInit : TACSAudioProcessorInitEvent;
    FOnFlush : TACSAudioProcessorFlushEvent;
    FOnGetData : TACSGetDataEvent;
    FOnGetSampleRate : TACSGetParameterEvent;
    FOnGetBitsPerSample : TACSGetParameterEvent;
    FOnGetChannels : TACSGetParameterEvent;
    FOnGetTotalTime : TACSGetRealParameterEvent;
    FOnGetSize : TACSGetParameterEvent;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    function GetTotalTime : real; override;
  public
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  published
    property OnFlush : TACSAudioProcessorFlushEvent read FOnFlush write FOnFlush;
    property OnGetBitsPerSample : TACSGetParameterEvent read FOnGetBitsPerSample write FOnGetBitsPerSample;
    property OnGetChannels : TACSGetParameterEvent read FOnGetChannels write FOnGetChannels;
    property OnGetData : TACSGetDataEvent read FOnGetData write FOnGetData;
    property OnGetSampleRate : TACSGetParameterEvent read FOnGetSampleRate write FOnGetSampleRate;
    property OnGetSize : TACSGetParameterEvent read FOnGetSize write FOnGetSize;
    property OnGetTotalTime : TACSGetrealParameterEvent read FOnGetTotalTime write FOnGetTotalTime;
    property OnInit : TACSAudioProcessorInitEvent read FOnInit write FOnInit;
  end;

  TACSNULLOut = class(TACSCustomOutput)
  private
    Buf : array[0..BUF_SIZE-1] of Byte;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  end;

  TACSInputItem = class(TCollectionItem)
  protected
    FInput : TACSCustomInput;
    function GetOwner : TPersistent; override;
  published
    property Input : TACSCustomInput read FInput write FInput;
  end;

  TACSInputItems = class(TOwnedCollection)
  end;

  TACSInputChangedEvent = procedure(Sender : TComponent; var Index : Integer; var Continue : Boolean) of object;

  TACSInputList = class(TACSCustomInput)
  private
    FCurrentInput : Integer;
    FInputItems : TACSInputItems;
    Lock : Boolean;
    FOnInputChanged : TACSInputChangedEvent;
    FIndicateProgress : Boolean;
    procedure SetCurrentInput(aInput : Integer);
    procedure SetInputItems(aItems : TACSInputItems);
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    property CurrentInput : Integer read FCurrentInput write SetCurrentInput;
  published
    property IndicateProgress : Boolean read FIndicateProgress write FIndicateProgress;
    property InputItems : TACSInputItems read FInputItems write SetInputItems;
    property OnInputChanged : TACSInputChangedEvent read FOnInputChanged write FOnInputChanged;
  end;


implementation

{$IFDEF LINUX}

var
  AOInitialized : Integer = 0;

{$ENDIF}

  constructor TACSMemoryIn.Create;
  begin
    inherited Create(AOwner);
    FSize := -1;
  end;

  destructor TACSMemoryIn.Destroy;
  begin
    inherited Destroy;
  end;

  function TACSMemoryIn.GetBPS : Integer;
  begin
    if (FBPS in [8, 16]) = False  then FBPS := 16;
    Result := FBPS;
  end;

  function TACSMemoryIn.GetCh : Integer;
  begin
    if (FChan in [1..2]) = False then FChan := 1;
    Result := FChan;
  end;

  function TACSMemoryIn.GetSR : Integer;
  begin
    if (FSR < 4000) or (FSR > 48000) then FSR := 8000;
    Result := FSR;
  end;

  procedure TACSMemoryIn.Init;
  begin
    FPosition := 0;
    BufEnd := FDataSize;
    BufStart := 1;
    Busy := True;
  end;

  procedure TACSMemoryIn.Flush;
  begin
    Busy := False;
    FDataSize := 0;
  end;

  function TACSMemoryIn.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
  begin
    if not Busy then  raise EACSException.Create(strStreamnotopen);
    if not Assigned(FBuffer) then
    begin
      Result := 0;
      Exit;
    end;
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      if FDataSize = 0 then
      begin
        if Assigned(FOnBufferDone) then FOnBufferDone(Self)
        else
        begin
          Result := 0;
          Exit;
        end;
      end;
      BufEnd := FDataSize;
      if FDataSize = 0 then
      begin
        Result := 0;
        Exit;
      end;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(FBuffer[BufStart-1], Buffer^, Result);
    Inc(BufStart, Result);
    Inc(FPosition, Result);
    Dec(FDataSize, Result);
  end;

  function TACSMemoryIn.GetBuffer : Pointer;
  begin
    Result := Pointer(FBuffer);
  end;

  procedure TACSMemoryIn.SetBuffer;
  begin
    FBuffer := PACSBuffer8(v);
  end;

  function TACSAudioProcessor.GetBPS : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    if Assigned(FOnGetBitsPerSample) then FOnGetBitsPerSample(Self, Result) else
    Result := FInput.BitsPerSample;
  end;

  function TACSAudioProcessor.GetSR : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    if Assigned(FOnGetSampleRate) then FOnGetSampleRate(Self, Result) else
    Result := FInput.SampleRate;
  end;

  function TACSAudioProcessor.GetCh : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    if Assigned(FOnGetChannels) then FOnGetChannels(Self, Result) else
    Result := FInput.Channels;
  end;

  function TACSAudioProcessor.GetTotalTime : real;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    if Assigned(FOnGetTotalTime) then FOnGetTotalTime(Self, Result) else
    Result := FInput.TotalTime;
  end;

  function TACSAudioProcessor.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    Result := BufferSize;
    if Assigned(FOnGetData) then FOnGetData(Self, Buffer, Result)
    else Result := FInput.GetData(Buffer, BufferSize);
   Inc(FPosition, Result);
//   if Result = 0 then
//   Result := Result shl 1;
  end;

  procedure TACSAudioProcessor.Init;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    if Assigned(FOnInit) then FOnInit(Self, FSize)
    else
    begin
      FInput.Init;
      if Assigned(FOnGetSize) then FOnGetSize(Self, FSize)
      else FSize := Finput.Size;
    end;
    FBusy := True;
    FPosition := 0;
  end;

  procedure TACSAudioProcessor.Flush;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    if Assigned(FOnFlush) then FOnFlush(Self)
    else FInput.Flush;
    FBusy := False;
  end;

procedure TACSNULLOut.Prepare;
begin
  if not Assigned(FInput) then
  raise EACSException.Create(strInputnotAssigned);
  FInput.Init;
end;

function TACSNULLOut.DoOutput(Abort : Boolean):Boolean;
begin
  Result := True;
  if not Busy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result := False;
    Exit;
  end;
  if Finput.GetData(@Buf[0], BUF_SIZE) > 0 then Result := True
  else
  begin
    Result := False;
    Exit;
  end;
end;

procedure TACSNULLOut.Done;
begin
  FInput.Flush;
end;

function TACSInputItem.GetOwner : TPersistent;
begin
  Result := Collection;
end;

constructor TACSInputList.Create;
begin
  inherited Create(AOwner);
  FInputItems := TACSInputItems.Create(Self, TACSInputItem);
  FPosition := 0;
  FSize := -1;
  FIndicateProgress := True;
end;

destructor TACSInputList.Destroy;
begin
  FInputItems.Free;
  Inherited Destroy;
end;

procedure TACSInputList.SetCurrentInput;
var
  I : TACSInputItem;
begin
  if aInput <> 0 then
  if (aInput < 0) or (aInput >= FInputItems.Count) then
  raise EACSException.Create(Format(strListIndexOOB,[aInput]));
  if Busy then
  begin
    while Lock do;
    Lock := True;
    I := TACSInputItem(InputItems.Items[FCurrentInput]);
    I.Input.Flush;
    I := TACSInputItem(InputItems.Items[aInput]);
    I.Input.Init;
    if FIndicateProgress then
    FSize := I.Input.Size
    else FSize := -1;
    FPosition := 0;
    Lock := False;
  end;
  FCurrentInput := aInput;
end;

function TACSInputList.GetBPS : Integer;
var
  I : TACSInputItem;
begin
  if Busy then
  begin
    I := TACSInputItem(InputItems.Items[FCurrentInput]);
    Result := I.Input.BitsPerSample;
  end else
  if InputItems.Count > 0 then
  begin
    I := TACSInputItem(InputItems.Items[0]);
    Result := I.Input.BitsPerSample;
  end;
end;

function TACSInputList.GetCh : Integer;
var
  I : TACSInputItem;
begin
  if Busy then
  begin
    I := TACSInputItem(InputItems.Items[FCurrentInput]);
    Result := I.Input.Channels;
  end else
  if InputItems.Count > 0 then
  begin
    I := TACSInputItem(InputItems.Items[0]);
    Result := I.Input.Channels;
  end;
end;

function TACSInputList.GetSR : Integer;
var
  I : TACSInputItem;
begin
  if Busy then
  begin
    I := TACSInputItem(InputItems.Items[FCurrentInput]);
    Result := I.Input.SampleRate;
  end else
  if InputItems.Count > 0 then
  begin
    I := TACSInputItem(InputItems.Items[0]);
    Result := I.Input.SampleRate;
  end;
end;

procedure TACSInputList.Init;
var
  I : TACSInputItem;
begin
  if Busy then
  raise EACSException.Create(strBusy);
  if InputItems.Count = 0 then
  raise EACSException.Create(strNoInputItems);
  I := TACSInputItem(InputItems.Items[FCurrentInput]);
  if not Assigned(I.Input) then
  raise EACSException.Create(Format(strNoInputAssigned,[FCurrentInput]));
  FBusy := True;
  I.Input.Init;
  if FIndicateProgress then
  FSize := I.Input.Size
  else FSize := -1;
  FPosition := 0;
end;

procedure TACSInputList.Flush;
var
  I : TACSInputItem;
begin
  I := TACSInputItem(InputItems.Items[FCurrentInput]);
  I.Input.Flush;
  FCurrentInput := 0;
  Lock := False;
  FBusy := False;
end;

function TACSInputList.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
var
  I : TACSInputItem;
  Continue : Boolean;
begin
  while Lock do;
  Lock := True;
  I := TACSInputItem(InputItems.Items[FCurrentInput]);
  Result :=  I.Input.GetData(Buffer, BufferSize);
  while Result = 0 do
  begin
    if FCurrentInput < InputItems.Count -1 then
    begin
      I.Input.Flush;
      Inc(FCurrentInput);
      Continue := True;
      if Assigned(FonInputChanged) then
      FonInputChanged(Self, FCurrentInput, Continue);
      if Continue then
      begin
        I := TACSInputItem(InputItems.Items[FCurrentInput]);
        if not Assigned(I.Input) then
        raise EACSException.Create(Format(strNoInputAssigned,[FCurrentInput]));
        I.Input.Init;
        if FIndicateProgress then
        FSize := I.Input.Size
        else FSize := -1;
        FPosition := 0;
        Result :=  I.Input.GetData(Buffer, BufferSize);
      end else Break;
    end else Break;
  end;
  if FIndicateProgress then
  FPosition := I.Input.Position;
  Lock := False;
end;

procedure TACSInputList.SetInputItems;
begin
  FInputItems.Assign(aItems);
end;
end.
