unit semaphore;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, syncobjs;
  
type

  ESemaphoreException = class(Exception);
  
  { TSemaphoreObject }

  TSemaphoreObject = class
  private
    FHandle : PRTLEvent;
    FLimit: Integer;
    FCurrentState : Integer;
    FCriticalSection : TCriticalSection;
  public
    constructor Create(const ALimit : Integer);
    destructor Destroy(); override;
    function WaitFor(ATimeout : Cardinal) : TWaitResult;
    procedure Release();
    property Limit : Integer read FLimit;
  end;

implementation

{ TSemaphoreObject }

constructor TSemaphoreObject.Create(const ALimit: Integer);
begin
  Assert(ALimit>0);
  FLimit := ALimit;
  FHandle := RTLEventCreate();
  FCriticalSection := TCriticalSection.Create();
  FCurrentState := FLimit;
  RTLeventSetEvent(FHandle);
end;

destructor TSemaphoreObject.Destroy();
begin
  RTLeventdestroy(FHandle);
  FreeAndNil(FCriticalSection);
  inherited Destroy();
end;

function TSemaphoreObject.WaitFor(ATimeout: Cardinal): TWaitResult;
var
  ok : Boolean;
begin
  Result := wrTimeout;
  ok := False;
  FCriticalSection.Acquire();
  try
    if ( FCurrentState > 0 ) then begin
      Dec(FCurrentState);
      ok := True;
      if ( FCurrentState = 0 ) then
        RTLeventResetEvent(FHandle);
    end;
  finally
    FCriticalSection.Release();
  end;
  if not ok then begin
    RTLeventWaitFor(FHandle,ATimeout);
    FCriticalSection.Acquire();
    try
      if ( FCurrentState > 0 ) then begin
        Dec(FCurrentState);
        ok := True;
      end;
    finally
      FCriticalSection.Release();
    end;
  end;
  if ok then
    Result := wrSignaled;
end;

procedure TSemaphoreObject.Release();
begin
  FCriticalSection.Acquire();
  try
    if ( FCurrentState < Limit ) then begin
      Inc(FCurrentState);
    end else begin
      raise ESemaphoreException.Create('Invalid semaphore operation.');
    end;
  finally
    FCriticalSection.Release();
  end;
  RTLeventSetEvent(FHandle);
end;

end.

