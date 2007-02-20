(*
  this file is a part of audio components suite.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
  
  This is an sample unit for an converter
*)
unit converter;

interface

uses
  Classes, SysUtils; 

type

  { TACSNewConverter }

  TACSNewConverter = class(TACSCustomConverter)
  private
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
  end;

implementation

{ TACSNewConverter }

function TACSNewConverter.GetBPS: Integer;
begin
  Result:=FInput.GetBPS;
end;

function TACSNewConverter.GetCh: Integer;
begin
  Result:=FInput.GetCh;
end;

function TACSNewConverter.GetSR: Integer;
begin
  Result:=FInput.GetSR;
end;

constructor TACSNewConverter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TACSNewConverter.Destroy;
begin
  inherited Destroy;
end;

function TACSNewConverter.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
  var
    l : Integer;
  begin
    if not Busy then  raise EACSException.Create(strStreamnotopen);
    if BufStart > BufEnd then
    begin
      if EndOfInput then
      begin
        Result := 0;
        Exit;
      end;
      BufStart := 1;


      while InputLock do;
      InputLock := True;

      l := Finput.GetData(@Yourbuffer[0], WantedSize);

      InputLock := False;
      if l = 0 then
      begin
        Result := 0;
        Exit;
      end;
      InSize := l;
      while (l<>0) and (InSize < WantedSize) do
      begin
        while InputLock do;
        InputLock := True;
        l := Finput.GetData(@YourBuffer[InSize], WantedSize - InSize);
        InputLock := False;
        Inc(InSize, l);
      end;
      if l = 0 then EndOfInput := True;
      
      //Do Your DSP Stuff here
      
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;

    Move(YourOutBuffer[BufStart-1], Buffer^, Result);

    Inc(BufStart, Result);
    Inc(FPosition, Result);
end;

procedure TACSNewConverter.Init;
begin
  inherited Init;
end;

procedure TACSNewConverter.Flush;
begin
  inherited Flush;
end;

end.

