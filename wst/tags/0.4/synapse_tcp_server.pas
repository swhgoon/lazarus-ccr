{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit synapse_tcp_server;

{$INCLUDE wst.inc}

interface

uses
  Classes, SysUtils, blcksock, synsock;

const
  sSERVER_PORT = '1234';
  
type

  ILogger = interface
    ['{CA357B9A-604F-4603-96FA-65D445837E80}']
    procedure Log(const AMsg : string);overload;
    procedure Log(const AMsg : string;const AArgs : array of const);overload;
  end;
  
  { TClientHandlerThread }

  TClientHandlerThread = class(TThread)
  private
    FDefaultTimeOut: Integer;
    FSocketObject : TTCPBlockSocket;
    FSocketHandle : TSocket;
    FInputStream : TMemoryStream;
    FOutputStream : TMemoryStream;
  private
    procedure ClearBuffers();
    function ReadInputBuffer():Integer;
    procedure SendOutputBuffer();
  public
    constructor Create (ASocketHandle : TSocket);
    destructor Destroy();override;
    procedure Execute(); override;
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
  end;
  
  { TServerListnerThread }

  TServerListnerThread = class(TThread)
  private
    FDefaultTimeOut: Integer;
    FSocketObject : TTCPBlockSocket;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Execute(); override;
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
  end;
  
  { TConsoleLogger }

  TConsoleLogger = class(TInterfacedObject,IInterface,ILogger)
  protected
    procedure Log(const AMsg : string);overload;
    procedure Log(const AMsg : string;const AArgs : array of const);overload;
  end;
  
  function Logger():ILogger ;
  function SetLogger(ALogger : ILogger):ILogger ;

implementation
uses binary_streamer, server_service_intf, server_service_imputils;

var FLoggerInst : ILogger = nil;
function SetLogger(ALogger : ILogger):ILogger ;
begin
  Result := FLoggerInst;
  FLoggerInst := ALogger;
end;

function Logger():ILogger ;
begin
  Result := FLoggerInst;
end;

{ TConsoleLogger }

procedure TConsoleLogger.Log(const AMsg: string);
begin
  WriteLn(AMsg);
end;

procedure TConsoleLogger.Log(const AMsg: string; const AArgs: array of const);
begin
  WriteLn(Format(AMsg,AArgs));
end;

{ TClientHandlerThread }

procedure TClientHandlerThread.ClearBuffers();
begin
  FInputStream.Size := 0;
  FOutputStream.Size := 0;
end;

function TClientHandlerThread.ReadInputBuffer(): Integer;
var
  strBuff : string;
  bufferLen : LongInt;
  i, j, c : PtrInt;
begin
  FInputStream.Size := 0;
  Result := 0;
  bufferLen := 0;
  FSocketObject.RecvBufferEx(@bufferLen,SizeOf(bufferLen),DefaultTimeOut);
  FSocketObject.ExceptCheck();
  bufferLen := Reverse_32(bufferLen);
  FInputStream.Size := bufferLen;
  if ( bufferLen > 0 ) then begin
    c := 0;
    i := 1024;
    if ( i > bufferLen ) then
      i := bufferLen;
    SetLength(strBuff,i);
    repeat
      j := FSocketObject.RecvBufferEx(@(strBuff[1]),i,DefaultTimeOut);
      FSocketObject.ExceptCheck();
      FInputStream.Write(strBuff[1],j);
      Inc(c,j);
      if ( ( bufferLen - c ) > 1024 ) then
        i := 1024
      else
        i := bufferLen - c;
    until ( i = 0 ) or ( j <= 0 );
  end;
  FInputStream.Position := 0;
  Result := FInputStream.Size;
end;

procedure TClientHandlerThread.SendOutputBuffer();
begin
  FSocketObject.SendBuffer(FOutputStream.Memory,FOutputStream.Size);
end;

constructor TClientHandlerThread.Create(ASocketHandle: TSocket);
begin
  FSocketHandle := ASocketHandle;
  FreeOnTerminate := True;
  FDefaultTimeOut := 90000;
  inherited Create(False);
end;

destructor TClientHandlerThread.Destroy();
begin
  FreeAndNil(FOutputStream);
  FreeAndNil(FInputStream);
  inherited Destroy();
end;

procedure TClientHandlerThread.Execute();
var
  wrtr : IDataStore;
  rdr : IDataStoreReader;
  buff, trgt,ctntyp : string;
  rqst : IRequestBuffer;
  i : PtrUInt;
begin
  FInputStream := TMemoryStream.Create();
  FOutputStream := TMemoryStream.Create();
  FSocketObject := TTCPBlockSocket.Create();
  try
    FSocketObject.RaiseExcept := True;
    try
      FSocketObject.Socket := FSocketHandle;
      FSocketObject.GetSins();
      while not Terminated do begin
        FOutputStream.Size := 0;
        if ( ReadInputBuffer() >= SizeOf(LongInt) ) then begin
          rdr := CreateBinaryReader(FInputStream);
          trgt := rdr.ReadStr();
          ctntyp := rdr.ReadStr();
          buff := rdr.ReadStr();
          rdr := nil;
          FInputStream.Size := 0;
          FInputStream.Write(buff[1],Length(buff));
          FInputStream.Position := 0;
          rqst := TRequestBuffer.Create(trgt,ctntyp,FInputStream,FOutputStream);
          HandleServiceRequest(rqst);
          i := FOutputStream.Size;
          SetLength(buff,i);
          FOutputStream.Position := 0;
          FOutputStream.Read(buff[1],i);
          FOutputStream.Size := 0;
          wrtr := CreateBinaryWriter(FOutputStream);
          wrtr.WriteStr(buff);
          SendOutputBuffer();
          ClearBuffers();
        end;
      end;
    except
      on e : Exception do begin
        Logger().Log('Error : ThreadID = %d; Message = %s',[Self.ThreadID,e.Message]);
      end;
    end;
  finally
    FreeAndNil(FSocketObject);
  end;
end;

{ TServerListnerThread }

constructor TServerListnerThread.Create();
begin
  FSocketObject := TTCPBlockSocket.Create();
  FreeOnTerminate := True;
  FDefaultTimeOut := 1000;
  inherited Create(false);
end;

destructor TServerListnerThread.Destroy();
begin
  FreeAndNil(FSocketObject);
  inherited Destroy();
end;

procedure TServerListnerThread.Execute();
var
  ClientSock : TSocket;
begin
  try
    FSocketObject.RaiseExcept := True;
    FSocketObject.CreateSocket();
    FSocketObject.SetLinger(True,10);
    FSocketObject.Bind('127.0.0.1',sSERVER_PORT);
    FSocketObject.Listen();
    while not Terminated do begin
      if FSocketObject.CanRead(DefaultTimeOut) then begin
        ClientSock := FSocketObject.Accept();
        TClientHandlerThread.Create(ClientSock);
      end;
    end;
  except
    on e : Exception do begin
      Logger().Log('Listner Thread Error : ThreadID = %d; Message = %s',[Self.ThreadID,e.Message]);
      Logger().Log('Listner stoped.');
    end;
  end;
end;

end.

