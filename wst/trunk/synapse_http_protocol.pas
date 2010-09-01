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
unit synapse_http_protocol;

//{$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,{$IFDEF WST_DBG}Dialogs,{$ENDIF}
  wst_types, service_intf, imp_utils, base_service_intf, client_utils,
  httpsend;

Const
  sTRANSPORT_NAME = 'HTTP';

Type

  { TSynapseCookieManager }

  TSynapseCookieManager = class(TInterfacedObject,ICookieManager)
  private
    FReferencedObject : TStrings;
  protected
    property ReferencedObject : TStrings read FReferencedObject;
  protected  
    function GetCount() : Integer;
    function GetName(const AIndex : Integer) : string;
    function GetValue(const AIndex : Integer) : string; overload;
    function GetValue(const AName : string) : string; overload;
    procedure SetValue(const AIndex : Integer; const AValue : string); overload;
    procedure SetValue(const AName : string; const AValue : string); overload;
  public
    constructor Create(AReferencedObject : TStrings);
  end;
  
{$M+}
  { THTTPTransport }
  THTTPTransport = class(TBaseTransport,ITransport)
  Private
    FConnection : THTTPSend;
    FAddress : string;
    FFormat : string;
    FSoapAction: string;
    FCookieManager : ICookieManager; 
  private  
    function GetAddress: string;
    function GetContentType: string;
    function GetProxyPassword: string;
    function GetProxyPort: Integer;
    function GetProxyServer: string;
    function GetProxyUsername: string;
    procedure SetAddress(const AValue: string);
    procedure SetContentType(const AValue: string);
    procedure SetProxyPassword(const AValue: string);
    procedure SetProxyPort(const AValue: Integer);
    procedure SetProxyServer(const AValue: string);
    procedure SetProxyUsername(const AValue: string);
  Public
    constructor Create();override;
    destructor Destroy();override;
    procedure SendAndReceive(ARequest,AResponse:TStream); override;
    function GetCookieManager() : ICookieManager; override;
  Published
    property ContentType : string Read GetContentType Write SetContentType;
    property Address : string Read GetAddress Write SetAddress;
    property ProxyServer : string Read GetProxyServer Write SetProxyServer;
    property ProxyPort : Integer Read GetProxyPort Write SetProxyPort;
    property ProxyUsername : string read GetProxyUsername write SetProxyUsername;
    property ProxyPassword : string read GetProxyPassword write SetProxyPassword;
    property SoapAction : string read FSoapAction write FSoapAction;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure SYNAPSE_RegisterHTTP_Transport();

implementation
uses
  wst_consts;

{ THTTPTransport }

function THTTPTransport.GetAddress: string;
begin
  Result := FAddress;
end;

function THTTPTransport.GetContentType: string;
begin
  Result := FConnection.MimeType;
end;

function THTTPTransport.GetProxyPassword: string;
begin
  Result := FConnection.ProxyPass;
end;

function THTTPTransport.GetProxyPort: Integer;
begin
  Result := StrToInt(FConnection.ProxyPort);
end;

function THTTPTransport.GetProxyServer: string;
begin
  Result := FConnection.ProxyHost;
end;

function THTTPTransport.GetProxyUsername: string;
begin
  Result := FConnection.ProxyUser;
end;

procedure THTTPTransport.SetAddress(const AValue: string);
begin
  FAddress := AValue;
end;

procedure THTTPTransport.SetContentType(const AValue: string);
begin
  FConnection.MimeType := AValue;
end;

procedure THTTPTransport.SetProxyPassword(const AValue: string);
begin
  FConnection.ProxyPass := AValue;
end;

procedure THTTPTransport.SetProxyPort(const AValue: Integer);
begin
  FConnection.ProxyPort := IntToStr(AValue);
end;

procedure THTTPTransport.SetProxyServer(const AValue: string);
begin
  FConnection.ProxyHost := AValue;
end;

procedure THTTPTransport.SetProxyUsername(const AValue: string);
begin
  FConnection.ProxyUser := AValue;
end;

constructor THTTPTransport.Create();
begin
  inherited Create();
  FConnection := THTTPSend.Create();
  FConnection.Protocol := '1.1';
end;

destructor THTTPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  inherited Destroy();
end;

procedure THTTPTransport.SendAndReceive(ARequest, AResponse: TStream);
{$IFDEF WST_DBG}
  procedure Display(const AStr : string);
  begin
    if IsConsole then
      WriteLn(AStr)
    {else
      ShowMessage(AStr)};
  end;
{$ENDIF WST_DBG}
var
{$IFDEF WST_DBG}
  s : TBinaryString;
{$ENDIF WST_DBG}
  locTempStream, locTempRes : TMemoryStream;
begin
{$IFDEF WST_DBG}
  TMemoryStream(ARequest).SaveToFile('request-1.log');
{$ENDIF}
  FConnection.Document.Size := 0;
  FConnection.Headers.Add('soapAction:' + SoapAction);
  if not HasFilter() then begin
    FConnection.Document.CopyFrom(ARequest,0);
    if not FConnection.HTTPMethod('POST',FAddress) then
      raise ETransportExecption.CreateFmt(SERR_FailedTransportRequest,[sTRANSPORT_NAME,FAddress]);
    AResponse.CopyFrom(FConnection.Document,0);
  end else begin
    locTempRes := nil;
    locTempStream := TMemoryStream.Create();
    try
      FilterInput(ARequest,locTempStream);
{$IFDEF WST_DBG}
      TMemoryStream(locTempStream).SaveToFile('request.log.wire');
{$ENDIF WST_DBG}
      FConnection.Document.CopyFrom(locTempStream,0);
      if not FConnection.HTTPMethod('POST',FAddress) then
        raise ETransportExecption.CreateFmt(SERR_FailedTransportRequest,[sTRANSPORT_NAME,FAddress]);
      locTempRes := TMemoryStream.Create();
      locTempRes.CopyFrom(FConnection.Document,0);
  {$IFDEF WST_DBG}
      TMemoryStream(locTempRes).SaveToFile('response.log.wire');
  {$ENDIF WST_DBG}
      FilterOutput(locTempRes,AResponse);
    finally
      locTempRes.Free();
      locTempStream.Free();
    end;
  end;
  FConnection.Clear();
{$IFDEF WST_DBG}
  TMemoryStream(ARequest).SaveToFile('request.log');
  SetLength(s,ARequest.Size);
  Move(TMemoryStream(ARequest).Memory^,s[1],Length(s));
  Display(s);
  SetLength(s,AResponse.Size);
  Move(TMemoryStream(AResponse).Memory^,s[1],Length(s));
  TMemoryStream(AResponse).SaveToFile('response.log');
  Display(s);
{$ENDIF}
end;

function THTTPTransport.GetCookieManager() : ICookieManager; 
begin
  if (FCookieManager = nil) then
    FCookieManager := TSynapseCookieManager.Create(FConnection.Cookies);
  Result := FCookieManager;
end;

procedure SYNAPSE_RegisterHTTP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(THTTPTransport) as IItemFactory);
end;

{ TSynapseCookieManager }

function TSynapseCookieManager.GetCount() : Integer; 
begin
  Result := ReferencedObject.Count;
end;

function TSynapseCookieManager.GetName(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject.Names[AIndex];
end;

function TSynapseCookieManager.GetValue(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject.ValueFromIndex[AIndex];
end;

function TSynapseCookieManager.GetValue(const AName : string) : string; 
begin
  Result := ReferencedObject.Values[AName];
end;

procedure TSynapseCookieManager.SetValue(
  const AIndex : Integer;  
  const AValue : string
); 
begin
  ReferencedObject.ValueFromIndex[AIndex] := AValue;
end;

procedure TSynapseCookieManager.SetValue(
  const AName : string;  
  const AValue : string
); 
begin
  ReferencedObject.Values[AName] := AValue;
end;

constructor TSynapseCookieManager.Create(AReferencedObject : TStrings); 
begin
  if (AReferencedObject = nil) then
    raise ETransportExecption.CreateFmt(SERR_InvalidParameter,['AReferencedObject']); 
  FReferencedObject := AReferencedObject;
end;

end.
