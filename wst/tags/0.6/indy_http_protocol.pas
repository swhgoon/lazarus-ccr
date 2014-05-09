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
unit indy_http_protocol;

{ $DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, wst_types,
  client_utils, IdHTTP, IdCookie;

Const
  sTRANSPORT_NAME = 'HTTP';

Type

  { TIndyCookieManager }

  TIndyCookieManager = class(TInterfacedObject,ICookieManager)
  private
    FReferencedObject : TIdCookies;
  protected
    property ReferencedObject : TIdCookies read FReferencedObject;
  protected  
    function GetCount() : Integer;
    function GetName(const AIndex : Integer) : string;
    function GetValue(const AIndex : Integer) : string; overload;
    function GetValue(const AName : string) : string; overload;
    procedure SetValue(const AIndex : Integer; const AValue : string); overload;
    procedure SetValue(const AName : string; const AValue : string); overload;
  public
    constructor Create(AReferencedObject : TIdCookies);
  end; 
  
  { THTTPTransport }
  THTTPTransport = class(TBaseTransport,ITransport)
  Private
    FFormat : string;
    FConnection : TidHttp;
    FCookieManager : ICookieManager;
  private
    function GetAddress: string;
    function GetProtocolVersion : string;
    function GetProxyPassword: string;
    function GetProxyPort: Integer;
    function GetProxyServer: string;
    function GetProxyUsername: string;
    procedure SetAddress(const AValue: string);
    procedure SetProtocolVersion(const AValue : string);
    procedure SetProxyPassword(const AValue: string);
    procedure SetProxyPort(const AValue: Integer);
    procedure SetProxyServer(const AValue: string);
    procedure SetProxyUsername(const AValue: string);
    function GetContentType: string;
    procedure SetContentType(const Value: string);
    function GetSoapAction: string;
    procedure SetSoapAction(const Value: string);
  protected
    procedure DoSendAndReceive(ARequest,AResponse:TStream);override;
  public
    constructor Create();override;
    destructor Destroy();override;
    function GetTransportName() : string; override;
    function GetCookieManager() : ICookieManager; override;
  published
    property ContentType : string Read GetContentType Write SetContentType;
    property Address : string Read GetAddress Write SetAddress;
    property ProxyServer : string Read GetProxyServer Write SetProxyServer;
    property ProxyPort : Integer Read GetProxyPort Write SetProxyPort;
    property ProxyUsername : string read GetProxyUsername write SetProxyUsername;
    property ProxyPassword : string read GetProxyPassword write SetProxyPassword;
    property SoapAction : string read GetSoapAction write SetSoapAction;
    property Format : string read FFormat write FFormat;
    property ProtocolVersion : string read GetProtocolVersion write SetProtocolVersion;
  End;

  procedure INDY_RegisterHTTP_Transport();

implementation
uses
  wst_consts;

const
  ProtocolVersionMAP : array[TIdHTTPProtocolVersion] of string = ('1.0', '1.1');

function TryStrToProtocolVersion(
  const AStr : string;
  out   ARes : TIdHTTPProtocolVersion
) : Boolean;
var
  i : TIdHTTPProtocolVersion;
begin
  for i := Low(TIdHTTPProtocolVersion) to High(TIdHTTPProtocolVersion) do begin
    if ( AStr = ProtocolVersionMAP[i] ) then begin
      ARes := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;


{ THTTPTransport }

function THTTPTransport.GetAddress: string;
begin
  Result := FConnection.Request.URL;
end;

function THTTPTransport.GetProtocolVersion : string;
begin
  Result := ProtocolVersionMAP[FConnection.ProtocolVersion];
end;

function THTTPTransport.GetProxyPassword: string;
begin
  Result := FConnection.ProxyParams.ProxyPassword;
end;

function THTTPTransport.GetProxyPort: Integer;
begin
  Result := FConnection.ProxyParams.ProxyPort;
end;

function THTTPTransport.GetProxyServer: string;
begin
  Result := FConnection.ProxyParams.ProxyServer;
end;

function THTTPTransport.GetProxyUsername: string;
begin
  Result := FConnection.ProxyParams.ProxyUsername;
end;

function THTTPTransport.GetSoapAction: string;
begin
  Result := FConnection.Request.CustomHeaders.Values['SOAPAction'];
end;

function THTTPTransport.GetTransportName() : string;
begin
  Result := sTRANSPORT_NAME;
end;

procedure THTTPTransport.SetAddress(const AValue: string);
begin
  FConnection.Request.URL := AValue;
end;

procedure THTTPTransport.SetContentType(const Value: string);
begin
  FConnection.Request.ContentType := Value;
end;

procedure THTTPTransport.SetProtocolVersion(const AValue : string);
var
  locValue : TIdHTTPProtocolVersion;
begin
  if not TryStrToProtocolVersion(AValue,locValue) then
    raise ETransportExecption.CreateFmt(SERR_InvalidPropertyValue,['ProtocolVersion',AValue]);
  FConnection.ProtocolVersion := locValue;
  if not ( hoKeepOrigProtocol in FConnection.HTTPOptions ) then
    FConnection.HTTPOptions := FConnection.HTTPOptions + [hoKeepOrigProtocol];
end;

procedure THTTPTransport.SetProxyPassword(const AValue: string);
begin
  FConnection.ProxyParams.ProxyPassword := AValue;
end;

procedure THTTPTransport.SetProxyPort(const AValue: Integer);
begin
  FConnection.ProxyParams.ProxyPort := AValue;
end;

procedure THTTPTransport.SetProxyServer(const AValue: string);
begin
  FConnection.ProxyParams.ProxyServer := AValue;
end;

procedure THTTPTransport.SetProxyUsername(const AValue: string);
begin
  FConnection.ProxyParams.ProxyUsername := AValue;
end;

procedure THTTPTransport.SetSoapAction(const Value: string);
begin
  FConnection.Request.CustomHeaders.Values['SOAPAction'] := Value;
end;

constructor THTTPTransport.Create();
begin
  inherited;
  FConnection := TidHttp.Create(Nil);
end;

destructor THTTPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  inherited Destroy();
end;

procedure THTTPTransport.DoSendAndReceive(ARequest, AResponse: TStream);
begin
  if not ( IsStrEmpty(FConnection.ProxyParams.ProxyUsername) and
           IsStrEmpty(FConnection.ProxyParams.ProxyPassword)
         )
  then begin
    FConnection.ProxyParams.BasicAuthentication := True;
  end;
  FConnection.Post(Address,ARequest, AResponse);
end;

function THTTPTransport.GetContentType: string;
begin
  Result := FConnection.Request.ContentType;
end;

function THTTPTransport.GetCookieManager() : ICookieManager;
begin
  if (FCookieManager = nil) then
    FCookieManager := TIndyCookieManager.Create(FConnection.CookieManager.CookieCollection);
  Result := FCookieManager;    
end;

procedure INDY_RegisterHTTP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(THTTPTransport) as IItemFactory);
end;


{ TIndyCookieManager }

function TIndyCookieManager.GetCount() : Integer; 
begin
  Result := ReferencedObject.Count;
end;

function TIndyCookieManager.GetName(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject[AIndex].CookieName;
end;

function TIndyCookieManager.GetValue(const AIndex : Integer) : string; 
begin
  Result := ReferencedObject[AIndex].Value;
end;

function TIndyCookieManager.GetValue(const AName : string) : string; 
var
  i : Integer;
begin       
  i := ReferencedObject.GetCookieIndex(0,AName);
  if (i >= 0) then
    Result := ReferencedObject[i].Value
  else
    Result := '';
end;

procedure TIndyCookieManager.SetValue(
  const AIndex : Integer;  
  const AValue : string
); 
begin
  ReferencedObject[AIndex].Value := AValue; 
end;

procedure TIndyCookieManager.SetValue(
  const AName : string;  
  const AValue : string
); 
var
  i : Integer;
  locItem : TIdNetscapeCookie;
begin
  i := ReferencedObject.GetCookieIndex(0,AName);  
  if (i >= 0) then begin
    ReferencedObject[i].Value := AValue;
  end else begin
    locItem := ReferencedObject.Add();
    locItem.CookieName := AName;
    locItem.Value := AValue;
  end;
end;

constructor TIndyCookieManager.Create(AReferencedObject : TIdCookies); 
begin
  if (AReferencedObject = nil) then
    raise ETransportExecption.CreateFmt(SERR_InvalidParameter,['AReferencedObject']); 
  FReferencedObject := AReferencedObject;
end;

end.
