{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit semaphore;

interface

uses
  Classes, SysUtils, SyncObjs, wst_types
{$IFDEF WST_DELPHI}
  ,Windows
{$ENDIF}
{$IFDEF FPC}
  {$IFDEF WINDOWS}
    ,Windows
  {$ENDIF}
{$ENDIF};

type

  ESemaphoreException = class(Exception);

{$UNDEF FPC_TM}
{$IFDEF WST_DELPHI}
  TSemaphoreHandle = THandle;
{$ENDIF}
{$IFDEF FPC}
  {$IFDEF WINDOWS}
    TSemaphoreHandle = THandle;
  {$ELSE}
    {$DEFINE FPC_TM}
    TSemaphoreHandle = Pointer;
  {$ENDIF}
{$ENDIF}

  { TSemaphoreObject }

  TSemaphoreObject = class
  private
    FHandle : TSemaphoreHandle;
    FLimit: Integer;
  {$IFDEF FPC_TM}
    FTM : TThreadManager;
  {$ENDIF}
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
{$IFDEF FPC_TM}
var
  i : PtrInt;
{$ENDIF}  
begin
  if ( ALimit < 1 ) then
    raise ESemaphoreException.CreateFmt('Invalid semaphore maximum count : %d.',[ALimit]);
  FLimit := ALimit;
{$IFNDEF FPC_TM}
  FHandle := CreateSemaphore(nil,ALimit,ALimit,'');
  if ( FHandle = THandle(0) ) then
    RaiseLastOSError();
{$ELSE}
  if not GetThreadManager(FTM) then
    raise ESemaphoreException.Create('Unable to get the thread manager.');
  FHandle := FTM.SemaphoreInit();
  for i := 1 to FLimit do
    FTM.SemaphorePost(FHandle);
{$ENDIF}
end;

destructor TSemaphoreObject.Destroy();
begin
{$IFNDEF FPC_TM}
  if ( FHandle <> THandle(0) ) then
    CloseHandle(FHandle);
{$ELSE}
  if ( FHandle <> nil ) then
    FTM.SemaphoreDestroy(FHandle);
  FHandle := nil;
{$ENDIF}
  inherited Destroy();
end;

function TSemaphoreObject.WaitFor(ATimeout: Cardinal): TWaitResult;
{$IFNDEF FPC_TM}
var
  intRes : DWORD;
begin
  intRes := WaitForSingleObject(FHandle,ATimeout);
  case intRes of
    WAIT_OBJECT_0  : Result := wrSignaled;
    WAIT_TIMEOUT   : Result := wrTimeout;
    WAIT_ABANDONED : Result := wrAbandoned;
    else
                     Result := wrTimeout;
  end;
end;
{$ELSE}
begin
  FTM.SemaphoreWait(FHandle);
  Result := wrSignaled;
end;
{$ENDIF}

procedure TSemaphoreObject.Release();
begin
{$IFNDEF FPC_TM}
  ReleaseSemaphore(FHandle,1,nil);
{$ELSE}
  FTM.SemaphorePost(FHandle);
{$ENDIF}
end;

end.


