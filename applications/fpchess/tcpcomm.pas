{
 fpChess - TCP communication module

 License: GPL version 2 or superior

 Copyright: Felipe Monteiro de Carvalho in 2010
}
unit tcpcomm;

{$mode objfpc}{$H+}{$packsets 1}

interface

uses
  // RTL, FLC, LCL
  Classes, SysUtils, Forms,
  //  TPacket declaration
  chessgame,
  // LNet
  lnet, lnetcomponents
  ;

type
  TProgressEvent = procedure (AStr: string; APorcent: Integer) of object;

  { TSimpleTcpConnection }

  TSimpleTcpConnection = class
  private
    FConnectionFinished: Boolean;
    FEvent: THandle;
    FIP: string;
    FPort: Word;
    FConnected: Boolean;
    FSocket: TLTcpComponent;
    ReceivedPacketsStart: TPacket;
    ReceivedPacketsEnd: TPacket;
    FOnProgress: TProgressEvent;
    // Variables to read fragmented packets
    UnfinishedPacket: TPacket;
    UnfinishedPacketPos: Word;
    procedure SendPacket(Packet: TPacket);
    function ReceivePacket(ATimeOut: Word = 10000): TPacket;
    function HashInList(APacket: TPacket; AHashes: array of Cardinal): Boolean;
  protected
    function Handshake: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPacketToList(Packet: TPacket);
    procedure RemovePacketFromList(Packet: TPacket);
    function GetConnected: Boolean;
    function PacketPending: Boolean;
    function GetNextMessage(AID: Cardinal): TPacket;
    function Reconnect: Integer;
    function Connect(AIP: string; APort: Integer; ATimeOut: Word = 10000): Boolean;
    procedure Disconnect(ATimeOut: Word = 1000);
    procedure Shutdown;
    procedure DebugOutputMessageList();
    // Events for LNet
    procedure OnConnect(aSocket: TLSocket);
    procedure OnErrorEvent(const msg: string; aSocket: TLSocket);
    procedure OnReceive(aSocket: TLSocket);
    procedure OnDisconnect(aSocket: TLSocket);
    // Properties
    property Connected: Boolean read GetConnected;
    {@@
      Indicates that the connection procedure, which is event-based, was finished.
      See Connected to check if the connection was successful.

      @see Connected
    }
    property ConnectionFinished: Boolean read FConnectionFinished write FConnectionFinished;
  end;

  ETcpTransportException = class(Exception);
  EHandshakeException = class(ETcpTransportException);
  ECryptoException = class(ETcpTransportException);

function GetConnection(): TSimpleTcpConnection;

implementation

resourcestring
  SInvalidDataPacket = 'Invalid data packet format';

var
  ClientConnection: TSimpleTcpConnection = nil;

procedure OPDebugLn(AStr: string);
begin
  {$ifndef Win32}
  WriteLn(AStr);
  {$endif}
end;

function GetConnection(): TSimpleTcpConnection;
begin
  if ClientConnection = nil then
    ClientConnection := TSimpleTcpConnection.Create();
  Result := ClientConnection;
end;

{ TOPCTcpConnection }

constructor TSimpleTcpConnection.Create;
begin
  {$ifdef Synapse}
  FSocket := TTCPBlockSocket.Create;
  {$else}
  FSocket := TLTcpComponent.Create(nil);
  FSocket.OnConnect := @OnConnect;
  FSocket.OnError := @OnErrorEvent;
  FSocket.OnReceive := @OnReceive;
  FSocket.OnDisconnect := @OnDisconnect;
  FSocket.Timeout:= 1000;
  {$endif}
end;

function TSimpleTcpConnection.PacketPending: Boolean;
begin
//  Result := FSocket.PacketPending(SizeOf(TPacketHeader));
end;

{@@
  Connects to a given server. This function is synchronous.

  @param API      Server name or IP
  @param APort    Server port
  @param ATimeOut The maximum time to wait for an answer of the server,
                  in miliseconds.
}
function TSimpleTcpConnection.Connect(AIP: string; APort: Integer;
  ATimeOut: Word = 10000): Boolean;
var
  Packet: TPacket;
  i: Integer;
begin
  OPDebugLn('[TSimpleTcpConnection.Connect] START');
  Result := False;
  FIP := AIP;
  FPort := APort;
  FConnectionFinished := False;

  if Assigned(FOnProgress) then FOnProgress('', 30); // Values between 20 and 60
  FSocket.Connect(FIP, FPort);

  // Wait for either OnConnect or OnErrorEvent
  for i := 0 to ATimeOut div 10 do
  begin
    if FConnectionFinished then
    begin
      if Assigned(FOnProgress) then
        FOnProgress('Conection Response arrived', 40); // Values between 20 and 60
      Break;
    end;
    Sleep(10);
    Application.ProcessMessages;
  end;

  if not Connected then
    raise Exception.Create('[TSimpleTcpConnection.Connect] Connection Timeout');

  if Assigned(FOnProgress) then FOnProgress('Executing Handshake', 60);
  Handshake;

  Result := True;

  OPDebugLn('[TSimpleTcpConnection.Connect] END');
end;

function TSimpleTcpConnection.Reconnect: Integer;
var
  Packet: TPacket;
begin
//  Result := RC_NOT_RESTORED;
{  if FSocket <> nil then FSocket.Free;
  FSocket := TWinSocket.Create;
  try
    FSocket.Connect(FHostName, FHostIP, FServiceName, FServicePort);
    Assert(FConnectionCookie <> nil);
    FConnectionCookie.ResetPosition;
    SendPacket(FConnectionCookie);
    Packet := ReceivePacket;
    case Packet.Action of

      asCookie:
      // positive response on reconnect - connection found by cookie
        begin
          FConnectionCookie := Packet;
          Result := RC_RESTORED
        end;

      asRestart:
      // No corresponding connection found on server. Client should be restarted
        begin
          FConnectionCookie := Packet;
          FConnectionCookie.Action := asCookie;
          Result := RC_FAIL
        end;

      else
        Assert(False);
    end;
  except
    FSocket.Free;
    FSocket := nil;
    Result := RC_NOT_RESTORED;
  end;}
end;

destructor TSimpleTcpConnection.Destroy;
begin
  if Connected then Disconnect;
  FSocket.Free;
  inherited;
end;

procedure TSimpleTcpConnection.Disconnect(ATimeOut: Word = 1000);
var
  i: Integer;
begin
  {$ifdef Synapse}
  FSocket.CloseSocket;
  {$else}
  FSocket.Disconnect();
  {$endif}

  for i := 0 to ATimeOut div 10 do
  begin
    if not FConnected then Break;
    Sleep(10);
    Application.ProcessMessages;
  end;

  if FConnected then
    OPDebugLn('[TSimpleTcpConnection.Disconnect] Disconection failed');
end;

function TSimpleTcpConnection.GetConnected: Boolean;
begin
  Result := (FSocket <> nil) and FConnected;
end;

function TSimpleTcpConnection.GetNextMessage(AID: Cardinal): TPacket;
var
  CurrentPacket: TPacket;
  PacketFound: Boolean = False;
begin
  Result := nil;

  // Search the packets in the linked list
  CurrentPacket := ReceivedPacketsStart;
  while CurrentPacket <> nil do
  begin
    if (CurrentPacket.ID = AID) then
    begin
      PacketFound := True;
      Break;
    end;

    CurrentPacket := CurrentPacket.Next;
  end;

  if not PacketFound then Exit;

  // Convert the Packet to a DataBlock
  Result := CurrentPacket;

  // Remove the packet from the list
  RemovePacketFromList(CurrentPacket);
end;

{@@
  First step when disconnecting from the server

  @see Disconnect
}
procedure TSimpleTcpConnection.Shutdown;
var
  Packet: TPacket;
begin
{  try
    Packet := TPacket.Create(asShutdown, nil^, 0);
    SendPacket(Packet);
    Packet.Free;
  except
    // eat exception for user pleasure
  end;}
end;

procedure TSimpleTcpConnection.DebugOutputMessageList();
var
  CurPacket: TPacket;
  lHash: LongWord;
begin
  OPDebugLn('[TSimpleTcpConnection.DebugOutputMessageList]');
  CurPacket := ReceivedPacketsStart;
  while CurPacket <> nil do
  begin
    lHash := CurPacket.ID;
    OPDebugLn(Format('[Packege] Hash %d', [lHash]));

    CurPacket := CurPacket.Next;
  end;
  // Variables to read fragmented packets
  if UnfinishedPacket <> nil then
    OPDebugLn('[There is an unfinished packege]');
end;

{@@
  Event called by LNet indicating that the connection was finished successfully
}
procedure TSimpleTcpConnection.OnConnect(aSocket: TLSocket);
begin
  FConnectionFinished := True;
  FConnected := True;
end;

{@@
  Event called by LNet when an error occured in the Connection
}
procedure TSimpleTcpConnection.OnErrorEvent(const msg: string; aSocket: TLSocket);
begin
  FConnectionFinished := True;
  FConnected := False;
end;

{@@
  Event called by LNet when data is available to be read
}
procedure TSimpleTcpConnection.OnReceive(aSocket: TLSocket);
var
  lPacket: TPacket;
  lFreePacket: Boolean;
  i, lPos, lRemaining, lSizeRead: Integer;
begin
  OPDebugLn('[TSimpleTcpConnection.OnReceive] BEGIN');
  repeat
    // Finishes reading a fragmented packet
    if UnfinishedPacket <> nil then
    begin
      OPDebugLn('[TSimpleTcpConnection.OnReceive] Another part of a fragmented packet');
{      lPacket := UnfinishedPacket;

      // Gets the data
      lRemaining := lPacket.DataSize - UnfinishedPacketPos;
      lSizeRead := ASocket.Get(lPacket.Data[UnfinishedPacketPos], lRemaining);
      if lSizeRead = lRemaining then
      begin
        OPDebugLn('[TSimpleTcpConnection.OnReceive] Read fragmented packet to the end');
        UnfinishedPacket := nil;
        AddPacketToList(lPacket);
      end
      else
      begin
        OPDebugLn('[TSimpleTcpConnection.OnReceive] Fragmented packet not yet finished, read: '
          + IntToStr(lSizeRead));
        UnfinishedPacketPos := UnfinishedPacketPos + lSizeRead;
        OPDebugLn('[TSimpleTcpConnection.OnReceive] END');
        Break;
      end;}
    end
    else
    // Reads a new packet
    begin
      lPacket := TPacket.Create;
      lFreePacket := True;

      try
        // Gets the header
        lSizeRead := ASocket.Get(lPacket.ID, 4);
        if lSizeRead < 4 then // Expected if there are no more packets
        begin
          OPDebugLn('[TSimpleTcpConnection.OnReceive] END');
          Exit;
        end;
        OPDebugLn('[TSimpleTcpConnection.OnReceive] ID: ' + IntToHex(Integer(lPacket.ID), 2));

        lSizeRead := ASocket.Get(lPacket.Kind, 1);
        if lSizeRead < 1 then
        begin
          OPDebugLn('[TSimpleTcpConnection.OnReceive] Packet ended in lPacket.Kind');
          Exit;
        end;
        OPDebugLn('[TSimpleTcpConnection.OnReceive] Kind: ' + IntToHex(
          {$ifdef VER2_4}Cardinal{$else}Byte{$endif}(lPacket.Kind), 2)); // Byte for FPC 2.5+

        if lPacket.Kind = pkMove then
        begin
          lSizeRead := ASocket.Get(lPacket.MoveStartX, 1);
{          if lSizeRead < 1 then
          begin
            OPDebugLn('[TSimpleTcpConnection.OnReceive] Packet ended in MoveStartX');
            Exit;
          end;
          OPDebugLn('[TSimpleTcpConnection.OnReceive] MoveStartX: ' + IntToStr(lPacket.MoveStartX));}

          lSizeRead := ASocket.Get(lPacket.MoveStartY, 1);
          lSizeRead := ASocket.Get(lPacket.MoveEndX, 1);
          lSizeRead := ASocket.Get(lPacket.MoveEndY, 1);
        end;

        // Because most packets are crypted, the raw data isn't very useful
  //      OPDebugData('[TSimpleTcpConnection.OnReceive]', lPacket.Data, lPacket.DataSize);

        // Updates the linked list
        lFreePacket := False;
        AddPacketToList(lPacket);
      finally
        if lFreePacket then lPacket.Free;
      end;
    end;
  until (lSizeRead = 0);
  OPDebugLn('[TSimpleTcpConnection.OnReceive] END');
end;

{@@
  Event of the LNet server. Happens when the disconnection procedure is
  finished or when a disconnection occurs for another reason.
}
procedure TSimpleTcpConnection.OnDisconnect(aSocket: TLSocket);
begin
  FConnectionFinished := True;
  FConnected := False;
  OPDebugLn('[TSimpleTcpConnection.OnDisconnect] Disconnected from server');
end;

function TSimpleTcpConnection.Handshake: Boolean;
begin
  OPDebugLn('[TSimpleTcpConnection.Handshake] START');

  Result := True;

  OPDebugLn('[TSimpleTcpConnection.Handshake] END');
end;

procedure CheckCryptoResult(Result: Integer);
begin
//  if Result <> 1 then
//    raise ECryptoException.Create(ERR_error_string(ERR_get_error, nil));
end;

{@@
  Returns the next received packet in the line or waits until one
  arrives to a maximum of ATimeOut miliseconds. Returns nil in case
  of a timeout of the packet otherwise.
}
function TSimpleTcpConnection.ReceivePacket(ATimeOut: Word = 10000): TPacket;
var
  i: Integer;
begin
  OPDebugLn('[TSimpleTcpConnection.ReceivePacket]');

  Result := nil;

  for i := 0 to ATimeOut div 10 do
  begin
    // Takes one Packet from the linked list
    if ReceivedPacketsStart <> nil then
    begin
      Result := ReceivedPacketsStart;

      if ReceivedPacketsStart = ReceivedPacketsEnd then
      begin
        ReceivedPacketsStart := nil;
        ReceivedPacketsEnd := nil;
      end
      else
        ReceivedPacketsStart := ReceivedPacketsStart.Next;

      Break;
    end;
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

function TSimpleTcpConnection.HashInList(APacket: TPacket;
  AHashes: array of Cardinal): Boolean;
var
  lHash: Cardinal;
  i: Integer;
begin
  Result := False;
  lHash := APacket.ID;
  for i := 0 to Length(AHashes) - 1 do
    if lHash = AHashes[i] then Exit(True);
end;

procedure TSimpleTcpConnection.AddPacketToList(Packet: TPacket);
begin
  if ReceivedPacketsStart = nil then
    ReceivedPacketsStart := Packet
  else
    ReceivedPacketsEnd.Next := Packet;
  ReceivedPacketsEnd := Packet;
end;

procedure TSimpleTcpConnection.RemovePacketFromList(Packet: TPacket);
var
  CurPacket, PreviousPacket: TPacket;
begin
  // First find the previous packet
  PreviousPacket := nil;
  CurPacket := ReceivedPacketsStart;
  while CurPacket <> nil do
  begin
    if CurPacket.Next = Packet then
    begin
      PreviousPacket := CurPacket;
      Break;
    end;

    CurPacket := CurPacket.Next;
  end;

  // Now fix the packets array
  if Packet = ReceivedPacketsStart then
    ReceivedPacketsStart := Packet.Next;

  if Packet = ReceivedPacketsEnd then
    ReceivedPacketsEnd := PreviousPacket;

  if PreviousPacket <> nil then
    PreviousPacket.Next := Packet.Next;

  // And finally free it
//  Packet.Free;
end;

procedure TSimpleTcpConnection.SendPacket(Packet: TPacket);
var
  lSize: Integer;
begin
  FSocket.Send(Packet.ID, 4);
  FSocket.Send(Packet.Kind, 1);
  if Packet.Kind = pkMove then
  begin
    FSocket.Send(Packet.MoveStartX, 1);
    FSocket.Send(Packet.MoveStartY, 1);
    FSocket.Send(Packet.MoveEndX, 1);
    FSocket.Send(Packet.MoveEndY, 1);
  end;
end;

end.

