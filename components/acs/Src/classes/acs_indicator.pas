(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_indicator.pas,v $
Revision 1.2  2006/08/31 20:10:54  z0m3ie
*** empty log message ***

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.4  2005/12/04 16:54:33  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.3  2005/10/02 16:51:46  z0m3ie
*** empty log message ***

Revision 1.2  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.2  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}


unit acs_indicator;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
   Classes, ACS_Types, ACS_Classes, ACS_Procs, ACS_Strings;

type

  TACSSoundIndicator = class(TACSCustomConverter)
  private
    Lock : Boolean;
    Window : array[0..1023] of Double;
    FValues : array[0..31] of Double;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure GetValues(var Values : array of Double);
    procedure Init; override;
    procedure Flush; override;
  end;

implementation

  constructor TACSSoundIndicator.Create;
  begin
    inherited Create(AOwner);
    HannWindow(@Window, 1024, True);
  end;

  destructor TACSSoundIndicator.Destroy;
  begin
    inherited Destroy;
  end;

  function TACSSoundIndicator.GetBPS : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotassigned);
    Result := FInput.BitsPerSample;
  end;

  function TACSSoundIndicator.GetCh : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotassigned);
    Result := FInput.Channels;
  end;

  function TACSSoundIndicator.GetSR : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotassigned);
    Result := FInput.SampleRate;
  end;

  procedure TACSSoundIndicator.Init;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotassigned);
    FBusy := True;
    FInput.Init;
    FSize := FInput.Size;
    FillChar(FValues[0], SizeOf(Double)*32, 0);
    Lock := False;
    FPosition := 0;
  end;

  procedure TACSSoundIndicator.Flush;
  begin
    FInput.Flush;
    FBusy := False;
    Lock := False;
  end;

  function TACSSoundIndicator.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
  var
    i, j, k, NumSamples : Integer;
    P : Pointer;
    P8 : PACSBuffer8;
    P16 : PACSBuffer16;
    PS8 : PACSStereoBuffer8;
    PS16 : PACSStereoBuffer16;
    DA : array[0..63] of Double;
    C1 : array[0..63] of TACSComplex;
  begin
    if not Busy then  raise EACSException.Create(strStreamnotopen);
    while InputLock do;
    InputLock := True;
    Result := FInput.GetData(Buffer, BufferSize);
    InputLock := False;
    FPosition := Finput.Position;
    if Result = 0 then Exit;
    if Lock then Exit;
    Lock := True;
    k := Result;
    GetMem(P, k);
    Move(Buffer^, P^, k);
    if FInput.BitsPerSample = 8 then
    begin
      if FInput.Channels = 1 then NumSamples := k
      else NumSamples := k shr 1;
    end else
    begin
      if FInput.Channels = 1 then NumSamples := k shr 1
      else NumSamples := k shr 2;
    end;
    for i := 0 to (NumSamples div 64) - 1 do
    begin
      if FInput.BitsPerSample = 8 then
      begin
        if FInput.Channels = 1 then
        begin
          P8 := P;
          for j := 0 to 63 do DA[j] := P8[i*64+j];
        end else
        begin
          PS8 := P;
          for j := 0 to 63 do DA[j] := (PS8[i*64+j].Left+PS8[i*64+j].Right)/2;
        end
      end else
      begin
        if FInput.Channels = 1 then
        begin
          P16 := P;
          for j := 0 to 63 do DA[j] := P16[i*64+j];
        end else
        begin
          PS16 := P;
          for j := 0 to 63 do DA[j] := (PS16[i*64+j].Left+PS16[i*64+j].Right)/2;
        end
      end;
      MultDoubleArrays(@Window[0], @DA[0], 64);
      for j := 0 to 63 do
      begin
        C1[j].Re := DA[j];
        C1[j].Im := 0;
      end;
      ComplexFFT(@C1, 64, 1);
      LgMagnitude(@C1[0], @DA[0], 64, 0);
      try
        for j := 0 to 31 do FValues[j]:=FValues[j]+DA[j];
      except
        for j := 0 to 31 do FValues[j] := 0;
      end;
    end;
    for j := 0 to 31 do FValues[j]:=FValues[j]/(NumSamples div 64);
    FreeMem(P);
    Lock := False;
  end;

  procedure TACSSoundIndicator.GetValues;
  var
    i : Integer;
  begin
    while Lock do;
    Lock := True;
    for i := 0 to 31 do Values[i] := FValues[i]*0.4; //ValCount;
    for i := 0 to 31 do FValues[i] := 0;
    Lock := False;
  end;

end.
