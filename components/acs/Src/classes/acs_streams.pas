(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_streams.pas,v $
Revision 1.5  2006/08/31 20:10:54  z0m3ie
*** empty log message ***

Revision 1.4  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.4  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.3  2005/11/27 16:50:33  z0m3ie
add ACS VolumeQuerry
make ACS_VolumeQuerry localizeable
some little errorfixes (buffersize for linuxdrivers was initially 0)
make TAudioIn workable

Revision 1.2  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.2  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}


unit acs_streams;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, ACS_Classes, ACS_Strings;

const

  OUTBUF_SIZE = $4000;


type

  TACSStreamOut = class(TACSStreamedOutput)
  private
    function GetSR : Integer;
    function GetBPS : Integer;
    function GetCh : Integer;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OutSampleRate : Integer read GetSR;
    property OutBitsPerSample : Integer read GetBPS;
    property OutChannles : Integer read GetCh;
  end;

  TACSStreamIn = class(TACSStreamedInput)
  private
    FBPS, FChan, FFreq : Integer;
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
  published
    property InBitsPerSample : Integer read FBPS write FBPS;
    property InChannels : Integer read FChan write FChan;
    property InSampleRate : Integer read FFreq write FFreq;
  end;


implementation

procedure TACSStreamOut.Prepare;
begin
  if not FStreamAssigned then
  raise EACSException.Create(strStreamObjectnotassigned);
  FInput.Init;
end;

procedure TACSStreamOut.Done;
begin
  FInput.Flush;
end;

function TACSStreamOut.DoOutput(Abort : Boolean):Boolean;
var
  Len : Integer;
  P : Pointer;
begin
  // No exceptions Here
  Result := True;
  if not Busy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result := False;
    Exit;
  end;
  GetMem(P, OUTBUF_SIZE);
  while InputLock do;
  InputLock := True;
  Len := Finput.GetData(P, OUTBUF_SIZE);
  InputLock := False;
  if Len > 0 then
  begin
    Result := True;
    FStream.WriteBuffer(P^, Len);
  end
  else Result := False;
  FreeMem(P);
end;

constructor TACSStreamOut.Create;
begin
  inherited Create(AOwner);
end;

destructor TACSStreamOut.Destroy;
begin
  inherited Destroy;
end;

constructor TACSStreamIn.Create;
begin
  inherited Create(AOwner);
  FBPS := 8;
  FChan := 1;
  FFreq := 8000;
  FSize := -1;
end;

destructor TACSStreamIn.Destroy;
begin
  inherited Destroy;
end;

procedure TACSStreamIn.Init;
begin
  if Busy then raise EACSException.Create(strBusy);
  if not Assigned(FStream) then raise EACSException.Create(strStreamObjectnotassigned);
  FPosition := FStream.Position;
  FBusy := True;
  FSize := FStream.Size;
end;

procedure TACSStreamIn.Flush;
begin
//  FStream.Position := 0;
  FBusy := False;
end;

function TACSStreamIn.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
begin
  Result := FStream.Read(Buffer^, BufferSize);
  FPosition := FStream.Position;
  //  Inc(FPosition, Result);
  if FPosition >= FSize then
  Result := 0;
end;

function TACSStreamOut.GetSR : Integer;
begin
  if not Assigned(Input) then
  raise EACSException.Create(strInputnotassigned);
  Result := FInput.SampleRate;
end;

function TACSStreamOut.GetBPS : Integer;
begin
  if not Assigned(Input) then
  raise EACSException.Create(strInputnotassigned);
  Result := FInput.BitsPerSample;
end;

function TACSStreamOut.GetCh : Integer;
begin
  if not Assigned(Input) then
  raise EACSException.Create(strInputnotassigned);
  Result := FInput.Channels;
end;

function TACSStreamIn.GetBPS : Integer;
begin
  Result := FBPS
end;

function TACSStreamIn.GetCh : Integer;
begin
  Result := FChan;
end;

function TACSStreamIn.GetSR : Integer;
begin
  Result := Self.FFreq;
end;


end.
