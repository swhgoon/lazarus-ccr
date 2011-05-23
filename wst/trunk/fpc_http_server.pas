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

unit fpc_http_server;

interface

uses
  Classes, SysUtils, httpdefs, fphttpserver, server_listener, wst_types;

type

  { TwstFPHttpListener }

  TwstFPHttpListener = class(TwstListener)
  private
    FHTTPServerObject: TFPHTTPServer;
    FRootAddress : string;
    FServerSoftware : String;
  private
    procedure ProcessWSDLRequest(
          ARequest  : TRequest;
          AResponse : TResponse;
      var APath     : string
    );
    procedure ProcessServiceRequest(
          ARequest  : TRequest;
          AResponse : TResponse;
      var APath     : string
    );
  private
    procedure RequestHandler(Sender: TObject; 
                Var ARequest: TFPHTTPConnectionRequest;
                Var AResponse : TFPHTTPConnectionResponse);
  public
    constructor Create(
      const AServerIpAddress   : string  = '127.0.0.1';
      const AListningPort      : Integer = 8000;
      const ADefaultClientPort : Integer = 25000;
      const AServerSoftware    : string  = 'Web Service Toolkit Application'
    );
    destructor Destroy(); override;
    class function GetDescription() : string;override;
    procedure Start();override;
    procedure Stop();override;
    function IsActive : Boolean; override;
  end;


implementation
uses
  base_service_intf, server_service_intf, server_service_imputils, metadata_wsdl;

{$IFDEF WST_DBG}
procedure Display(const AMsg : string);
begin
  if IsConsole then
    WriteLn(AMsg);
end;
{$ENDIF}

function ExtractNextPathElement(var AFullPath : string):string;
var
  i : SizeInt;
begin
  Result := '';
  if ( Length(AFullPath) > 0 ) then begin
    while ( Length(AFullPath) > 0 ) and ( AFullPath[1] = sSEPARATOR ) do begin
      Delete(AFullPath,1,1);
    end;
    i := Pos(sSEPARATOR,AFullPath);
    if ( i < 1 ) then begin
      Result := AFullPath;
      AFullPath := '';
    end else begin
      Result := Copy(AFullPath,1,Pred(i));
      Delete(AFullPath,1,i);
    end;
  end;
end;

{ TwstFPHttpListener }

procedure TwstFPHttpListener.ProcessWSDLRequest(
      ARequest  : TRequest;
      AResponse : TResponse;
  var APath     : string
);
var
  locRepName, strBuff : string;
  i : Integer;
begin
  locRepName := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,locRepName) then
    locRepName := ExtractNextPathElement(APath);
  strBuff := GenerateWSDL(locRepName,FRootAddress);
  i:=Length(strBuff);
  if (StrBuff<>'') then 
    begin
    AResponse.ContentType := 'text/xml';
    AResponse.Content:=strBuff;
    end
  else
    begin  
    AResponse.ContentType := 'text/html';
    AResponse.Content := GenerateWSDLHtmlTable();
    end;
  if AResponse.ContentLength=0 then  
    AResponse.ContentLength:=Length(AResponse.Content);  
end;

procedure TwstFPHttpListener.ProcessServiceRequest(
      ARequest  : TRequest;
      AResponse : TResponse;
  var APath     : string
);
var
  trgt,ctntyp, frmt : string;
  rqst : IRequestBuffer;
  inStream : TStringStream;
begin
  trgt := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,trgt) then 
    begin
    ProcessWSDLRequest(ARequest,AResponse,APath);
    Exit;
    end;
  inStream := nil;
  try
    inStream := TStringStream.Create(ARequest.Content);
    try
      AResponse.ContentStream := TMemoryStream.Create();
      ctntyp := ARequest.ContentType;
      AResponse.ContentType := ctntyp;
      frmt := Trim(ARequest.QueryFields.Values['format']);
      rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,AResponse.ContentStream,frmt);
      rqst.GetPropertyManager().SetProperty(sREMOTE_IP,ARequest.RemoteAddress);
      HandleServiceRequest(rqst);
      AResponse.ContentLength:=AResponse.ContentStream.Size;
    finally
      inStream.Free();
    end;
  except
    on e : Exception do begin
      NotifyMessage('ProcessData()>> Exception = '+e.Message);
      raise;
    end;
  end;
end;

procedure TWstFPHttpListener.RequestHandler(Sender: TObject; 
                Var ARequest: TFPHTTPConnectionRequest;
                Var AResponse : TFPHTTPConnectionResponse);

var
{$IFDEF WST_DBG}
  s : string;
  j : SizeInt;
{$ENDIF}
  locPath, locPathPart : string;
begin
  AResponse.Server:=FServerSoftware;
  locPath := ARequest.URL;
  locPathPart := ExtractNextPathElement(locPath);
  if AnsiSameText(sSERVICES_PREFIXE,locPathPart)  then 
    ProcessServiceRequest(ARequest,AResponse,locPath)
  else
    ProcessWSDLRequest(ARequest,AResponse,locPath);
  try
    AResponse.SendContent;
  finally  
    If Assigned(AResponse.ContentStream) then
      begin
      AResponse.ContentStream.Free;
      AResponse.ContentStream:=Nil;
      end;
  end;  
end;

constructor TwstFPHttpListener.Create(
      const AServerIpAddress   : string;
      const AListningPort      : Integer;
      const ADefaultClientPort : Integer;
      const AServerSoftware    : string
);

begin
  inherited Create();
  FHTTPServerObject := TFPHTTPServer.Create(nil);
//  b.IP := AServerIpAddress;
  FHTTPServerObject.port := AListningPort;
  FRootAddress := Format('http://%s:%d/',[AServerIpAddress,AListningPort]);
  FServerSoftware := AServerSoftware;
  FHTTPServerObject.OnRequest := @RequestHandler;
end;

destructor TwstFPHttpListener.Destroy();
begin
  if ( FHTTPServerObject <> nil ) then
    Stop();
  FreeAndNil(FHTTPServerObject);
  inherited Destroy();
end;

procedure TwstFPHttpListener.Start();
begin
  if not FHTTPServerObject.Active then
    FHTTPServerObject.Active := True;
end;

procedure TwstFPHttpListener.Stop();
begin
  if FHTTPServerObject.Active then
    FHTTPServerObject.Active := False;
end;

class function TwstFPHttpListener.GetDescription: string;
begin
  Result := 'WST FP HTTP Listener';
end;

function TwstFPHttpListener.IsActive: Boolean;
begin
  Result := FHTTPServerObject.Active;
end;

initialization


end.
