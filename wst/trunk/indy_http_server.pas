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
{$IFDEF FPC}
  //{$UNDEF INDY_9}
  //{$DEFINE INDY_10}
{$ELSE}
  //{$UNDEF INDY_10}
  //{$DEFINE INDY_9}
{$ENDIF}

unit indy_http_server;

interface

uses
  Classes, SysUtils,
  IdCustomHTTPServer,
  IdHTTPServer,
{$IFDEF INDY_10}
  IdContext,
{$ENDIF}
{$IFDEF INDY_9}
  IdTCPServer,
{$ENDIF}
  IdSocketHandle,
  server_listener;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}
  
type

  { TwstIndyHttpListener }

  TwstIndyHttpListener = class(TwstListener)
  private
    FHTTPServerObject: TIdHTTPServer;
    FRootAddress : string;
  private
    function GenerateWSDLTable():string;

    procedure ProcessWSDLRequest(
          {$IFDEF INDY_10}
          AContext        : TIdContext;
          {$ENDIF}
          ARequestInfo    : TIdHTTPRequestInfo;
          AResponseInfo   : TIdHTTPResponseInfo;
      var APath           : string
    );
    procedure ProcessServiceRequest(
          {$IFDEF INDY_10}
          AContext        : TIdContext;
          {$ENDIF}
          ARequestInfo    : TIdHTTPRequestInfo;
          AResponseInfo   : TIdHTTPResponseInfo;
      var APath           : string
    );
  private
    procedure Handler_CommandGet(
    {$IFDEF INDY_10}
      AContext        : TIdContext;
    {$ENDIF}
    {$IFDEF INDY_9}
      AThread: TIdPeerThread;
    {$ENDIF}
      ARequestInfo    : TIdHTTPRequestInfo;
      AResponseInfo   : TIdHTTPResponseInfo
    );
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
  end;


implementation
uses base_service_intf,
     server_service_intf, server_service_imputils,
     server_service_soap, server_binary_formatter, server_service_xmlrpc,
     metadata_repository, metadata_wsdl,
{$IFNDEF FPC}
     ActiveX, XMLDoc,XMLIntf,xmldom, wst_delphi_xml,
{$ELSE}
     DOM, XMLWrite, wst_fpc_xml,
{$ENDIF}
     metadata_service, metadata_service_binder, metadata_service_imp,

     user_service_intf, user_service_intf_binder, user_service_intf_imp;

{$IFNDEF FPC}
type
  TwstIndy9Thread = class(TIdPeerThread)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  end;

{ TwstIndy9Thread }

procedure TwstIndy9Thread.AfterExecute;
begin
  CoUninitialize();
  inherited;
end;

procedure TwstIndy9Thread.BeforeExecute;
begin
  inherited;
  CoInitialize(nil);
end;
{$ENDIF}

const
  sSEPARATOR = '/';
  sSERVICES_PREFIXE = 'services';
  sWSDL = 'WSDL';

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

function GetWSDL(const ARepName, ARootAddress: shortstring):string;
var
  strm : TMemoryStream;
  rep : PServiceRepository;
  doc :TXMLDocument;
  i : SizeInt;
  s : string;
begin
  Result := '';
  rep := nil;
  doc := Nil;
  i := GetModuleMetadataMngr().IndexOfName(ARepName);
  if ( i < 0 ) then
    Exit;
  strm := TMemoryStream.Create();
  try
    s := GetModuleMetadataMngr().GetRepositoryName(i);
    GetModuleMetadataMngr().LoadRepositoryName(s,ARootAddress,rep);
    //if ( GetModuleMetadataMngr().LoadRepositoryName(s,rep) > 0 ) then
      //rep^.namespace := 'urn:wst';
    strm.Clear();
    doc := CreateDoc();
    GenerateWSDL(rep,doc);
    WriteXMLFile(doc,strm);
    i := strm.Size;
    if ( i > 0 ) then begin
      SetLength(Result,i);
      Move(strm.memory^,Result[1],i);
    end;
  finally
    ReleaseDomNode(doc);
    strm.Free();
    GetModuleMetadataMngr().ClearRepository(rep);
  end;
end;


{ TwstIndyHttpListener }

function TwstIndyHttpListener.GenerateWSDLTable(): string;
var
  r : IModuleMetadataMngr;
  i : Integer;
begin
  r := GetModuleMetadataMngr();
  Result := '<html>' +
              '<head>'+
                '<title>'+
                  'The Web Services Toolkit generated Metadata table'+
                '</title>'+
                '<body>' +
                  '<p BGCOLOR="#DDEEFF"><FONT FACE="Arial" COLOR="#0000A0" SIZE="+2">The following repositories has available. Click on the link to view the corresponding WSDL.</FONT></p>'+
                  '<table width="100%">';

  for i := 0 to Pred(r.GetCount()) do begin
    Result := Result +
                '<tr>' +
                      '<td align="left">' +
                          Format('<a href="%s">',[sSEPARATOR+sSERVICES_PREFIXE+sSEPARATOR+sWSDL+sSEPARATOR+r.GetRepositoryName(i)])+
                          r.GetRepositoryName(i) +
                          '</a>'+
                      '</td>' +
                '</tr>';
  end;
  Result := Result +

                  '</table>'+
                '</body>'+
              '</head>'+
            '</html>';
end;

procedure TwstIndyHttpListener.ProcessWSDLRequest(
      {$IFDEF INDY_10}
      AContext        : TIdContext;
      {$ENDIF}
      ARequestInfo    : TIdHTTPRequestInfo;
      AResponseInfo   : TIdHTTPResponseInfo;
  var APath           : string
);
var
  locRepName, strBuff : string;
  i : Integer;
begin
  locRepName := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,locRepName) then
    locRepName := ExtractNextPathElement(APath);
  strBuff := GetWSDL(locRepName,FRootAddress);
  i := Length(strBuff);
  if ( i > 0 ) then begin
    AResponseInfo.ContentType := 'text/xml';
    if not Assigned(AResponseInfo.ContentStream) then
      AResponseInfo.ContentStream := TMemoryStream.Create();
    AResponseInfo.ContentStream.Write(strBuff[1],i);
    Exit;
  end;
  AResponseInfo.ContentText := GenerateWSDLTable();
  AResponseInfo.ContentType := 'text/html';
end;

procedure TwstIndyHttpListener.ProcessServiceRequest(
      {$IFDEF INDY_10}
      AContext        : TIdContext;
      {$ENDIF}
      ARequestInfo    : TIdHTTPRequestInfo;
      AResponseInfo   : TIdHTTPResponseInfo;
  var APath           : string
);
var
  trgt,ctntyp, frmt : string;
  rqst : IRequestBuffer;
  inStream : {$IFDEF FPC}TMemoryStream{$ELSE}TStringStream{$ENDIF};
begin
  trgt := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,trgt) then begin
    ProcessWSDLRequest({$IFDEF INDY_10}AContext,{$ENDIF}ARequestInfo,AResponseInfo,APath);
    Exit;
  end;
  inStream := nil;
  try
    try
      inStream := {$IFDEF FPC}TMemoryStream.Create();{$ELSE}TStringStream.Create(ARequestInfo.FormParams);{$ENDIF}
      AResponseInfo.ContentStream := TMemoryStream.Create();

      ctntyp := ARequestInfo.ContentType;
    {$IFDEF FPC}
      inStream.CopyFrom(ARequestInfo.PostStream,0);
    {$ENDIF}
      inStream.Position := 0;
      AResponseInfo.ContentType := ctntyp;
      frmt := Trim(ARequestInfo.Params.Values['format']);
      rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,AResponseInfo.ContentStream,frmt);
      HandleServiceRequest(rqst);
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

procedure TwstIndyHttpListener.Handler_CommandGet(
  {$IFDEF INDY_10}
    AContext        : TIdContext;
  {$ENDIF}
  {$IFDEF INDY_9}
    AThread: TIdPeerThread;
  {$ENDIF}
  ARequestInfo   : TIdHTTPRequestInfo;
  AResponseInfo  : TIdHTTPResponseInfo
);
var
{$IFDEF WST_DBG}
  s : string;
{$ENDIF}
  locPath, locPathPart : string;
  j : SizeInt;
begin
{$IFDEF WST_DBG}
  if Assigned(ARequestInfo.PostStream) and ( ARequestInfo.PostStream.Size > 0 ) then begin
    j := ARequestInfo.PostStream.Size;
    SetLength(s,j);
    ARequestInfo.PostStream.Read(s[1],j);
    NotifyMessage('----------- QUERY ----------------------');
    Display(s);
  end;
{$ENDIF}
  locPath := ARequestInfo.Document;
  locPathPart := ExtractNextPathElement(locPath);
  if AnsiSameText(sSERVICES_PREFIXE,locPathPart)  then begin
    ProcessServiceRequest({$IFDEF INDY_10}AContext,{$ENDIF}ARequestInfo,AResponseInfo,locPath);
  {$IFDEF WST_DBG}
    if Assigned(AResponseInfo.ContentStream) and ( AResponseInfo.ContentStream.Size > 0 ) then begin
      j := AResponseInfo.ContentStream.Size;
      SetLength(s,j);
      AResponseInfo.ContentStream.Position := 0;
      AResponseInfo.ContentStream.Read(s[1],j);
      Display('--------- RESPONSE ------------------------');
      Display(s);
    end;
  {$ENDIF}
    Exit;
  end;

  ProcessWSDLRequest({$IFDEF INDY_10}AContext,{$ENDIF}ARequestInfo,AResponseInfo,locPath);
end;

constructor TwstIndyHttpListener.Create(
      const AServerIpAddress   : string;
      const AListningPort      : Integer;
      const ADefaultClientPort : Integer;
      const AServerSoftware    : string
);
var
  b : TIdSocketHandle;
begin
  inherited Create();
  FHTTPServerObject := TIdHTTPServer.Create({$IFNDEF INDY_10}nil{$ENDIF});
{$IFNDEF FPC}
  FHTTPServerObject.ThreadClass := TwstIndy9Thread;
{$ENDIF}
  b := FHTTPServerObject.Bindings.Add();
  b.IP := AServerIpAddress;
  b.port := AListningPort;
  FRootAddress := Format('http://%s:%d/',[AServerIpAddress,AListningPort]);

  FHTTPServerObject.DefaultPort := ADefaultClientPort;
  FHTTPServerObject.ServerSoftware := AServerSoftware;
  //FHTTPServerObject.Active := True;
  FHTTPServerObject.OnCommandGet := {$IFDEF FPC}@{$ENDIF}Handler_CommandGet;
end;

destructor TwstIndyHttpListener.Destroy();
begin
  if ( FHTTPServerObject <> nil ) then
    Stop();
  FreeAndNil(FHTTPServerObject);
  inherited Destroy();
end;

procedure TwstIndyHttpListener.Start();
begin
  if not FHTTPServerObject.Active then
    FHTTPServerObject.Active := True;
end;

procedure TwstIndyHttpListener.Stop();
begin
  if FHTTPServerObject.Active then
    FHTTPServerObject.Active := False;
end;

class function TwstIndyHttpListener.GetDescription: string;
begin
  Result := 'Indy HTTP Listener';
end;

initialization
  RegisterStdTypes();
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  Server_service_RegisterXmlRpcFormat();

  RegisterUserServiceImplementationFactory();
  Server_service_RegisterUserServiceService();

  Register_user_service_intf_ServiceMetadata();

  RegisterWSTMetadataServiceImplementationFactory();
  Server_service_RegisterWSTMetadataServiceService();

end.
