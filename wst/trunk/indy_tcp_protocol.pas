{
    This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).
    

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit indy_tcp_protocol;

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf,
  IdTCPClient;

//{$DEFINE WST_DBG}

Const
  sTRANSPORT_NAME = 'TCP';

Type

  ETCPException = class(EServiceException)
  End;
  
{$M+}
  { TTCPTransport }
  TTCPTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FFormat : string;
    FPropMngr : IPropertyManager;
    FConnection : TIdTCPClient;
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
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream);
  Published
    property Target : string Read FTarget Write FTarget;
    property ContentType : string Read FContentType Write FContentType;
    property Address : string Read FAddress Write FAddress;
    property Port : string read FPort write FPort;
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure INDY_RegisterTCP_Transport();

implementation
uses
  binary_streamer, wst_types;

{ TTCPTransport }

procedure TTCPTransport.Connect();
var
  locReconnect : Boolean;
begin
  if not FConnection.Connected() then begin
    FConnection.ReadTimeout := DefaultTimeOut;
    FConnection.Connect(Address,StrToInt(Port));
  end else begin
    locReconnect := False;
    try
      locReconnect := not FConnection.Socket.Binding.Readable(0);
    except
      locReconnect := True;
    end;
    if locReconnect then begin
      FConnection.Disconnect();
      FConnection.ReadTimeout := DefaultTimeOut;
      FConnection.Connect(Address,StrToInt(Port));
    end;
  end;
end;

constructor TTCPTransport.Create();
begin
  FPropMngr := TPublishedPropertyManager.Create(Self);
  FConnection := TIdTCPClient.Create(nil);
  //FConnection.ReadTimeout:=;
  FDefaultTimeOut := 90000;
end;

destructor TTCPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  FPropMngr := Nil;
  inherited Destroy();
end;

function TTCPTransport.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropMngr;
end;

procedure TTCPTransport.SendAndReceive(ARequest, AResponse: TStream);
var
  wrtr : IDataStore;
  buffStream : TMemoryStream;
  binBuff : TByteDynArray;
  bufferLen : LongInt;
begin
  buffStream := TMemoryStream.Create();
  try
    wrtr := CreateBinaryWriter(buffStream);
    wrtr.WriteInt32S(0);
    wrtr.WriteAnsiStr(Target);
    wrtr.WriteAnsiStr(ContentType);
    wrtr.WriteAnsiStr(Self.Format);
    SetLength(binBuff,ARequest.Size);
    ARequest.Position := 0;
    ARequest.Read(binBuff[1],Length(binBuff));
    wrtr.WriteBinary(binBuff);
    buffStream.Position := 0;
    wrtr.WriteInt32S(buffStream.Size-4);
    buffStream.Position := 0;

    Connect();
    FConnection.IOHandler.Write(buffStream,buffStream.Size,False);

    bufferLen := 0;
    bufferLen := FConnection.IOHandler.ReadLongInt(False);
    bufferLen := Reverse_32(bufferLen);
    AResponse.Size := bufferLen;
    if ( bufferLen > 0 ) then begin
      AResponse.Position := 0;
      FConnection.IOHandler.ReadStream(AResponse,bufferLen,False);
    end;
    AResponse.Position := 0;
    {$IFDEF WST_DBG}
    TMemoryStream(AResponse).SaveToFile('response.log');
    {$ENDIF WST_DBG}
  finally
    buffStream.Free();
  end;
end;

procedure INDY_RegisterTCP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TTCPTransport) as IItemFactory);
end;

end.
