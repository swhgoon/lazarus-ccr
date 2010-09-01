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
unit synapse_tcp_protocol;
                                                   
interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, client_utils,
  blcksock;

//{$DEFINE WST_DBG}

Const
  sTRANSPORT_NAME = 'TCP';

Type

  ETCPException = class(EServiceException)
  End;
  
{$M+}
  { TTCPTransport }
  TTCPTransport = class(TBaseTransport,ITransport)
  Private
    FFormat : string;
    FConnection : TTCPBlockSocket;
    FContentType : string;
    FTarget: string;
    FAddress : string;
    FPort : string;
    FDefaultTimeOut: Integer;
  private
    procedure Connect();
  public
    constructor Create();override;
    destructor Destroy();override;
    procedure SendAndReceive(ARequest,AResponse:TStream); override;
  Published
    property Target : string Read FTarget Write FTarget;
    property ContentType : string Read FContentType Write FContentType;
    property Address : string Read FAddress Write FAddress;
    property Port : string Read FPort Write FPort;
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure SYNAPSE_RegisterTCP_Transport();

implementation
uses
  binary_streamer, Math, wst_types;

{ TTCPTransport }

procedure TTCPTransport.Connect();
var
  locReconnect : Boolean;
begin
  if ( FConnection.Socket = NOT(0) ) then begin
    FConnection.Connect(Address,Port);
  end else begin
    locReconnect := False;
    try
      locReconnect := not FConnection.CanRead(0);
    except
      locReconnect := True;
    end;
    if locReconnect then begin
      FConnection.CloseSocket();
      FConnection.Connect(Address,Port);
    end;
  end;
end;

constructor TTCPTransport.Create();
begin
  inherited Create();
  FConnection := TTCPBlockSocket.Create();
  FConnection.RaiseExcept := True;
  FDefaultTimeOut := 90000;
end;

destructor TTCPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  inherited Destroy();
end;

procedure TTCPTransport.SendAndReceive(ARequest, AResponse: TStream);

  procedure ReadResponse(ADest : TStream);
  var
    bufferLen : LongInt;
    i, j, c : PtrInt;
    locBinBuff : TByteDynArray;
  begin
    bufferLen := 0;
    FConnection.RecvBufferEx(@bufferLen,SizeOf(bufferLen),DefaultTimeOut);
    FConnection.ExceptCheck();
    bufferLen := Reverse_32(bufferLen);
    ADest.Size := bufferLen;
    if ( bufferLen > 0 ) then begin
      c := 0;
      i := 1024;
      if ( i > bufferLen ) then
        i := bufferLen;
      SetLength(locBinBuff,i);
      repeat
        j := FConnection.RecvBufferEx(@(locBinBuff[0]),i,DefaultTimeOut);
        FConnection.ExceptCheck();
        ADest.Write(locBinBuff[0],j);
        Inc(c,j);
        i := Min(1024,(bufferLen-c));
      until ( i =0 ) or ( j <= 0 );
    end;
    ADest.Position := 0;
  end;

Var
  wrtr : IDataStore;
  buffStream : TMemoryStream;
  binBuff : TByteDynArray;
  locTempStream, locTempRes : TMemoryStream;
begin
  locTempStream := nil;
  locTempRes := nil;
  buffStream := TMemoryStream.Create();
  Try
    wrtr := CreateBinaryWriter(buffStream);
    wrtr.WriteInt32S(0);
    wrtr.WriteAnsiStr(Target);
    wrtr.WriteAnsiStr(ContentType);
    wrtr.WriteAnsiStr(Self.Format);
    if not HasFilter() then begin
      SetLength(binBuff,ARequest.Size);
      ARequest.Position := 0;
      ARequest.Read(binBuff[0],Length(binBuff));
    end else begin
      locTempStream := TMemoryStream.Create();
      FilterInput(ARequest,locTempStream);
{$IFDEF WST_DBG}
      TMemoryStream(locTempStream).SaveToFile('request.log.wire');
{$ENDIF WST_DBG}
      SetLength(binBuff,locTempStream.Size);
      locTempStream.Position := 0;
      locTempStream.Read(binBuff[0],Length(binBuff));
      locTempStream.Size := 0;
    end;
    wrtr.WriteBinary(binBuff);
    buffStream.Position := 0;
    wrtr.WriteInt32S(buffStream.Size-4);
    buffStream.Position := 0;

    Connect();
    FConnection.SendBuffer(buffStream.Memory,buffStream.Size);

    if not HasFilter() then begin
      ReadResponse(AResponse);
    end else begin
      locTempRes := TMemoryStream.Create();
      ReadResponse(locTempRes);
  {$IFDEF WST_DBG}
      TMemoryStream(locTempRes).SaveToFile('response.log.wire');
  {$ENDIF WST_DBG}
      FilterOutput(locTempRes,AResponse);
    end;

    {$IFDEF WST_DBG}
    TMemoryStream(AResponse).SaveToFile('response.log');
    {$ENDIF WST_DBG}
  Finally
    locTempStream.Free();
    locTempRes.Free();
    buffStream.Free();
  End;
end;

procedure SYNAPSE_RegisterTCP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TTCPTransport) as IItemFactory);
end;

end.
