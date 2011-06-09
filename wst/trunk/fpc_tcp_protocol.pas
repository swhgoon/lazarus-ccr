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
unit fpc_tcp_protocol;
                                                   
interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, client_utils,
  ssockets;

//{$DEFINE WST_DBG}

Const
  sTRANSPORT_NAME = 'TCP';

Type

  ETCPException = class(EServiceException);

{$M+}
  { TTCPTransport }
  TTCPTransport = class(TBaseTransport,ITransport)
  Private
    FFormat : string;
    FConnection : TInetSocket;
    FContentType : string;
    FTarget: string;
    FAddress : string;
    FPort : string;
    procedure ReadResponse(ADest: TStream);
    procedure SendRequest(ARequest: TStream);
  private
    procedure Connect();
  public
    constructor Create();override;
    destructor Destroy();override;      
    function GetTransportName() : string; override;      
    procedure SendAndReceive(ARequest,AResponse:TStream); override;
  Published
    property Target : string Read FTarget Write FTarget;
    property ContentType : string Read FContentType Write FContentType;
    property Address : string Read FAddress Write FAddress;
    property Port : string Read FPort Write FPort;
    property Format : string read FFormat write FFormat;
  End;

procedure FPC_RegisterTCP_Transport();

implementation

uses
  binary_streamer, Math, wst_types;

{ TTCPTransport }

procedure TTCPTransport.Connect();

begin
  if FConnection=Nil then
    FConnection:=TInetSocket.Create(FAddress,StrToInt(Port));
end;

constructor TTCPTransport.Create();
begin
  inherited Create();
end;

destructor TTCPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  inherited Destroy();
end;

function TTCPTransport.GetTransportName() : string;  
begin
  Result := sTRANSPORT_NAME;  
end;

procedure TTCPTransport.SendRequest(ARequest : TStream);

  Procedure SendBuffer(P : PByte; ACount : Integer);

  Var
    c : integer;

  begin
    Repeat
      C:=FConnection.Write(P^,ACount);
      if (C<0) then
         Raise ETCPException.CreateFmt('Error %d sending data to socket',[FConnection.LastError]);
      If (C>0) then
        begin
        inc(P,C);
        Dec(ACount,C);
        end;
    Until (ACount=0);
  end;


Var
  M : TMemoryStream;
  binBuff : TByteDynArray;
  wrtr : IDataStore;

begin
  SetLength(binBuff,ARequest.Size);
  ARequest.Position := 0;
  ARequest.ReadBuffer(binBuff[0],Length(binBuff));
  M := TMemoryStream.Create();
  Try
    wrtr:=CreateBinaryWriter(M);
    wrtr.WriteInt32S(0);
    wrtr.WriteAnsiStr(Target);
    wrtr.WriteAnsiStr(ContentType);
    wrtr.WriteAnsiStr(Self.Format);
    wrtr.WriteBinary(binBuff);
    M.Position := 0;
    wrtr.WriteInt32S(M.Size-4);
    M.Position := 0;
    SendBuffer(TMemoryStream(M).Memory,M.Size);
  Finally
    M.Free;
  end;
end;

procedure TTCPTransport.ReadResponse(ADest : TStream);

  Procedure ReadBuffer(Var Buf; ACount : Integer);

  Var
    P : PByte;
    C : integer;

  begin
    if (ACount=0) then exit;
    P:=PByte(@Buf);
    repeat
      C:=FConnection.Read(P^,ACount);
      If (C<=0) then
        Raise ETCPException.CreateFmt('Error %d reading data from socket',[FConnection.LastError]);
      If (C>0) then
        begin
        Inc(P,C);
        Dec(ACount,C);
        end
    Until (ACount=0);
  end;

var
  bufferLen : LongInt;
  i, j, c : PtrInt;
  locBinBuff : TByteDynArray;
begin
  bufferLen := 0;
  ReadBuffer(BufferLen,SizeOf(BufferLen));
  bufferLen := Reverse_32(bufferLen);
  ADest.Size := bufferLen;
  ADest.Position:=0;
  if (bufferLen>0) then
    begin
    c := 0;
    i := Min(1024,Bufferlen);
    SetLength(locBinBuff,i);
    repeat
      ReadBuffer(locBinBuff[0],i);
      ADest.Write(locBinBuff[0],i);
      Inc(c,i);
      i:=Min(1024,(bufferLen-c));
    until (i=0);
    end;
end;

procedure TTCPTransport.SendAndReceive(ARequest, AResponse: TStream);

Var
  buffStream : TMemoryStream;
  binBuff : TByteDynArray;
  M : TStream;
begin
  // Connect
  Connect();
  // Filter
  if HasFilter() then
    M:=TMemoryStream.Create()
  else
    M:=ARequest;
  try
    if HasFilter() then
      FilterInput(ARequest,M);
    // Actually send buffer
    SendRequest(M);
  Finally
    if (M<>ARequest) then
      FreeAndNil(M);
  end;
  // Prepare to read response
  if HasFilter() then
    M:=TmemoryStream.Create
  else
    M:=AResponse;
  try
    // Actually read response
    ReadResponse(M);
    if HasFilter() then
      FilterOutput(M,AResponse);
  Finally
    if (M<>AResponse) then
      FreeAndNil(M);
  end;
end;

procedure FPC_RegisterTCP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TTCPTransport) as IItemFactory);
end;

end.
