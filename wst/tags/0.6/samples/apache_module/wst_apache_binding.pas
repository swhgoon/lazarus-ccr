//{$UNDEF WST_DBG}
//{$DEFINE WST_DBG}

(*
  Without the broker (WST_BROKER undefined):
    Apache must be configured to route requests to wst services

      <Location /wst>
        SetHandler wst-handler
      </Location>

    Services can then be invoked through the following addressing schema
    http://127.0.0.1:8080/wst/services/UserService

      UserService  : the target service
      wst/services : constant.

  ============================================================================

  WST_BROKER(still experimental !!!) enable the service brokering :
  if enabled, this module just forwards the request to the
  implementation libraries contained in the WstRootPath path.
  WST load these libraries in the local file system folder
  configured by the value of WstRootPath.
  WstRootPath is a configuration directive which must be
  in the wst "Location" scope.
  Example :
  
    <Location /wst>
      SetHandler wst-handler
      WstRootPath "C:/Programmes/lazarus/wst/trunk/samples/library_server/"
    </Location>

 Services can then be invoked through the following addressing schema
   http://127.0.0.1:8080/wst/services/lib_server/UserService
   
     lib_server   : the library name ( without extension )
     UserService  : the target service
     wst/services : constant.
*)
//{$DEFINE WST_BROKER}

unit wst_apache_binding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  httpd, apr, apriconv, aprutil;

const
  sWST_ROOT = 'wst';
  sSEPARATOR = '/';
  sSERVICES_PREFIXE = 'services';
  sWSDL = 'WSDL';
  sHTTP_BINARY_CONTENT_TYPE = 'application/octet-stream';
  sCONTENT_TYPE = 'Content-Type';
  
{$IFDEF WST_BROKER}  
  sWstRootPath = 'WstRootPath'; // The WST local file system path configure in apache
  sWST_LIBRARY_EXTENSION = '.dll';
  
type
  PWstConfigData = ^TWstConfigData;
  TWstConfigData = record
    Dir : PChar;
    BasePath : PChar;
  end;
{$ENDIF WST_BROKER}
  
var
  wst_module_ptr : Pmodule = nil;
{$IFDEF WST_BROKER}  
  WstConfigData : PWstConfigData = nil;
  WstCommandStructArray : array[0..1] of command_rec = (
    ( name    : sWstRootPath;
      func      : ( func_take1 : @ap_set_string_slot );
      cmd_data  : ( nil {@WstConfigData^.BasePath} );
      req_override : OR_ALL;
      args_how     : TAKE1;
      errmsg       : 'usage : WstRootPath <path>' + LineEnding + '  path is the path to the WST root path.';
    ),
    ()
  );
{$ENDIF WST_BROKER}

  function wst_RequestHandler(r: Prequest_rec): Integer;

{$IFDEF WST_BROKER}    
  function wst_create_dir_config(p: Papr_pool_t; dir: PChar) : Pointer;cdecl;
{$ENDIF WST_BROKER}
  
implementation
uses base_service_intf,
     server_service_intf, server_service_imputils,
     server_service_soap, server_binary_formatter, server_service_xmlrpc,
     metadata_repository, metadata_wsdl,
     imp_utils, binary_streamer, library_base_intf, library_imp_utils,
     DOM, XMLWrite,
     metadata_service, metadata_service_binder, metadata_service_imp;

procedure wst_initialize();
begin
  RegisterStdTypes();
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  //Server_service_RegisterXmlRpcFormat();

  RegisterWSTMetadataServiceImplementationFactory();
  Server_service_RegisterWSTMetadataServiceService();
end;

{$IFDEF WST_BROKER}  
function wst_create_dir_config(p: Papr_pool_t; dir: PChar) : Pointer; cdecl;
begin
  WstConfigData := PWstConfigData(apr_palloc(p,SizeOf(TWstConfigData)));
  FillChar(WstConfigData^,SizeOf(TWstConfigData),#0);
  WstConfigData^.Dir := apr_pstrdup(p,dir);
  Result := WstConfigData;
end;

function GetWstPath(): PChar;inline;
begin
  Result := WstConfigData^.BasePath;
end;
{$ENDIF WST_BROKER}  

type
  PRequestArgument = ^TRequestArgument;
  TRequestArgument = record
    Name  : shortstring;
    Value : shortstring;
    Next  : PRequestArgument;
  end;
  TRequestInfo = record
    InnerRequest : Pointer;
    Root         : string;
    URI          : string;
    ContentType  : string;
    Buffer       : string;
    Arguments    : string;
    ArgList      : PRequestArgument;
  end;
  
  TResponseInfo = record
    ContentText : string;
    ContentType : string;
  end;

function ParseArgs(
  const APool      : Papr_pool_t;
  const AArgs      : string;
  const ASeparator : Char = '&'
) : PRequestArgument;
var
  locBuffer, locArg : string;
  locPrev, locTmpArg : PRequestArgument;
begin
  Result := nil;
  locBuffer := Trim(AArgs);
  if not IsStrEmpty(locBuffer) then begin
    locTmpArg := nil;
    locPrev := nil;
    while True do begin
      locArg := GetToken(locBuffer,ASeparator);
      if IsStrEmpty(locArg) then
        Break;
      locPrev := locTmpArg;
      locTmpArg := PRequestArgument(apr_palloc(APool,SizeOf(TRequestArgument)));
      FillChar(locTmpArg^,SizeOf(TRequestArgument),#0);
      if ( Result = nil ) then begin
        Result := locTmpArg;
      end else begin
        locPrev^.Next := locTmpArg;
      end;
      locTmpArg^.Name := GetToken(locArg,'=');
      locTmpArg^.Value := locArg;
    end;
  end;
end;

function FindArg(const AArgs : PRequestArgument; const AName : string) : PRequestArgument;
var
  p : PRequestArgument;
begin
  Result := nil;
  p := AArgs;
  while Assigned(p) do begin
    if AnsiSameText(AName,AArgs^.Name) then begin
      Result := p;
      Break;
    end;
    p := p^.Next;
  end;
end;

{$IFDEF WST_DBG}
procedure SaveStringToFile(const AStr,AFile:string;const AKeepExisting : Boolean);
begin
  with TMemoryStream.Create() do try
    if AKeepExisting and FileExists(AFile) then begin
      LoadFromFile(AFile);
      Position := Size;
    end;
    if ( Length(AStr) > 0 ) then
      Write(AStr[1],Length(AStr));
    SaveToFile(AFile);
  finally
    Free();
  end;
end;
{$ENDIF WST_DBG}

function ReadBuffer(r : Prequest_rec; out rbuf : string ) : Integer;
var
  argsbuffer : string;
  rsize, len_read, rpos : Integer;
  loc_length : Integer;
begin
  rbuf := '';
  Result := ap_setup_client_block(r, REQUEST_CHUNKED_ERROR);
  if ( Result <> OK ) then
    Exit;

  if ( ap_should_client_block(r) <> 0 ) then begin
    SetLength(argsbuffer,HUGE_STRING_LEN);
    FillChar(argsbuffer[1],Length(argsbuffer),0);
    rsize := 0; len_read := 0; rpos := 0;
    loc_length := r^.remaining;
    SetLength(rbuf, loc_length );

     while True do begin
       len_read := ap_get_client_block(r, @(argsbuffer[1]), Length(argsbuffer));
       if ( len_read <= 0 ) then
         Exit;
        if ( ( rpos + len_read ) > loc_length ) then
          rsize := loc_length - rpos
        else
          rsize := len_read;
        Move(argsbuffer[1],rbuf[ ( 1 + rpos ) ], rsize);
        Inc(rpos,rsize);
    end;
  end;
end;

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
    strm.Clear();
    doc := TXMLDocument.Create();
    GenerateWSDL(rep,doc);
    WriteXMLFile(doc,strm);
    i := strm.Size;
    if ( i > 0 ) then begin
      SetLength(Result,i);
      Move(strm.memory^,Result[1],i);
    end;
  finally
    doc.Free();
    strm.Free();
    GetModuleMetadataMngr().ClearRepository(rep);
  end;
end;

function GenerateWSDLTable(): string;
var
  r : IModuleMetadataMngr;
  i : Integer;
begin
  r := GetModuleMetadataMngr();
  Result := '<html>' +
              '<head>'+
                '<title>'+
                  'The Web Service Toolkit generated Metadata table XXXXX'+
                '</title>'+
                '<body>' +
                  '<p BGCOLOR="#DDEEFF"><FONT FACE="Arial" COLOR="#0000A0" SIZE="+2">The following repositories has available. Click on the link to view the corresponding WSDL.</FONT></p>'+
                  '<table width="100%">';

  for i := 0 to Pred(r.GetCount()) do
    Result := Result + '<tr>' +
                        '<td>' +
                            Format('<a href="%s">',[sSEPARATOR+sWST_ROOT+sSEPARATOR+sSERVICES_PREFIXE+sSEPARATOR+sWSDL+sSEPARATOR+r.GetRepositoryName(i)])+
                            r.GetRepositoryName(i) +
                            '</a>'+
                        '</td>' +
                      '</tr>';

  Result := Result +
                    '</tr>'+
                  '</table>'+
                '</body>'+
              '</head>'+
            '</html>';
end;

procedure ProcessWSDLRequest(
  const ARequestInfo    : TRequestInfo;
  out   AResponseInfo   : TResponseInfo
);
var
  locRepName, strBuff, locPath : string;
  i : Integer;
begin
  FillChar(AResponseInfo,SizeOf(TResponseInfo),#0);
  locPath := ARequestInfo.URI;
  locRepName := ExtractNextPathElement(locPath);
  if AnsiSameText(sWSDL,locRepName) then
    locRepName := ExtractNextPathElement(locPath);
  strBuff := GetWSDL(locRepName,ARequestInfo.Root);
  i := Length(strBuff);
  if ( i > 0 ) then begin
    AResponseInfo.ContentType := 'text/xml';
    AResponseInfo.ContentText := strBuff;
    Exit;
  end;
  AResponseInfo.ContentText := GenerateWSDLTable();
  AResponseInfo.ContentType := 'text/html';
end;

function ProcessServiceRequest(
  const ARequestInfo    : TRequestInfo;
  out   AResponseInfo   : TResponseInfo
):Boolean;
var
  trgt,ctntyp, loc_path : string;
  rqst : IRequestBuffer;
  inStream, outStream: TMemoryStream;
  i : Integer;
begin
  FillChar(AResponseInfo,SizeOf(TResponseInfo),#0);
  loc_path := ARequestInfo.URI;
  trgt := ExtractNextPathElement(loc_path);
  Result := False;
  if AnsiSameText(sWSDL,trgt) then
    Exit;
  Result := True;
  inStream := nil;
  outStream := nil;
  try
    inStream := TMemoryStream.Create();
    outStream := TMemoryStream.Create();

    ctntyp := ARequestInfo.ContentType;
    i := Length(ARequestInfo.Buffer);
    if ( i > 0 ) then
      inStream.Write(ARequestInfo.Buffer[1],i);
    inStream.Position := 0;
    if AnsiSameText(sBINARY_CONTENT_TYPE,ctntyp) then
      AResponseInfo.ContentType := sHTTP_BINARY_CONTENT_TYPE
    else
      AResponseInfo.ContentType := ctntyp;
    rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,outStream,ARequestInfo.ContentType);
    HandleServiceRequest(rqst);
    i := outStream.Size;
    if ( i > 0 ) then begin
      SetLength(AResponseInfo.ContentText,i);
      Move(outStream.Memory^,AResponseInfo.ContentText[1],i);
    end;
  finally
    outStream.Free();
    inStream.Free();
    {$IFDEF WST_DBG}
      SaveStringToFile('RequestInfo.Arguments=' + ARequestInfo.Arguments + LineEnding,'c:\log.log',False);
      SaveStringToFile('ResponseInfo.ContentText=' + AResponseInfo.ContentText + LineEnding,'c:\log.log',False);
    {$ENDIF}
  end;
end;

{$IFDEF WST_BROKER}  
const MAX_ERR_LEN = 500;
function ProcessServiceRequestLibrary(
  const ARequestInfo    : TRequestInfo;
  out   AResponseInfo   : TResponseInfo
) : Boolean;
var
  loc_path, ctntyp : string;
  targetModuleName, targetFormat, targetService : string;
  wrtr : IDataStore;
  buffStream : TMemoryStream;
  strBuff : string;
  intfBuffer : IwstStream;
  bl : LongInt;
  targetModule : IwstModule;
  handlerProc : TwstLibraryHandlerFunction;
  pArg : PRequestArgument;
  i : Integer;
begin
  try
    FillChar(AResponseInfo,SizeOf(TResponseInfo),#0);
    loc_path := ARequestInfo.URI;
    targetModuleName := ExtractNextPathElement(loc_path);
    Result := False; 
    targetModule := LibraryManager.Get(GetWstPath() + targetModuleName + sWST_LIBRARY_EXTENSION);
    handlerProc := TwstLibraryHandlerFunction(targetModule.GetProc(WST_LIB_HANDLER));
    if not Assigned(handlerProc) then
      Exit;
    targetService := ExtractNextPathElement(loc_path);
    if AnsiSameText(sWSDL,targetService) then
      Exit;
    pArg := FindArg(ARequestInfo.ArgList,'format');
    if Assigned(pArg) then
      targetFormat := pArg^.Value;
    if IsStrEmpty(targetFormat) then
      targetFormat := ARequestInfo.ContentType;
    buffStream := TMemoryStream.Create();
    try
      wrtr := CreateBinaryWriter(buffStream);
      wrtr.WriteInt32S(0);
      wrtr.WriteAnsiStr(targetService);
      wrtr.WriteAnsiStr(ARequestInfo.ContentType);
      wrtr.WriteAnsiStr(targetFormat);
      wrtr.WriteAnsiStr(ARequestInfo.Buffer);
      buffStream.Position := 0;
      wrtr.WriteInt32S(buffStream.Size-4);

      buffStream.Position := 0;
      intfBuffer := TwstStream.Create(buffStream);
      bl := MAX_ERR_LEN;
      strBuff := StringOfChar(#0,bl);
      i := handlerProc(intfBuffer,Pointer(strBuff),bl);
      if ( i <> RET_OK ) then
        raise Exception.CreateFmt('Library server error :'#13'Code : %d'#13'Message : %s',[i,strBuff]);

      if AnsiSameText(sBINARY_CONTENT_TYPE,ARequestInfo.ContentType) then
        AResponseInfo.ContentType := sHTTP_BINARY_CONTENT_TYPE
      else
        AResponseInfo.ContentType := ARequestInfo.ContentType;
      buffStream.Position := 0;
      if ( buffStream.Size > 0 ) then begin
        SetLength(AResponseInfo.ContentText,buffStream.Size);
        buffStream.Read(AResponseInfo.ContentText[1],Length(AResponseInfo.ContentText));
      end else begin
        AResponseInfo.ContentText := '';
      end;
    finally
      buffStream.Free();
    end;

    Result := True;
  except
    on e : Exception do begin
      Result := False;
      ap_log_rerror(PCHAR('wst_apache_binding'),392,APLOG_ERR,0,Prequest_rec(ARequestInfo.InnerRequest),PCHAR(e.Message),[]);
    end;
  end;
end;
{$ENDIF WST_BROKER}  

function wst_RequestHandler(r: Prequest_rec): Integer;

  function FillRequestInfo(var ARequestInfo    : TRequestInfo):Integer;
  begin
    ARequestInfo.InnerRequest := r;
    ARequestInfo.ContentType := apr_table_get(r^.headers_in,sCONTENT_TYPE);
    ARequestInfo.Root := ap_get_server_name(r) + sSEPARATOR + sWST_ROOT + sSEPARATOR;
    ARequestInfo.URI := r^.uri;
    ARequestInfo.Arguments := r^.args;
    ARequestInfo.ArgList := ParseArgs(r^.pool,ARequestInfo.Arguments);
    Result := ReadBuffer(r,ARequestInfo.Buffer);
  end;

var
  loc_RequestInfo    : TRequestInfo;
  loc_ResponseInfo   : TResponseInfo;
begin
  Result := FillRequestInfo(loc_RequestInfo);
  if not AnsiSameText(sWST_ROOT,ExtractNextPathElement(loc_RequestInfo.URI)) then
    Result := DECLINED;
  if ( Result <> OK ) then
    Exit;

  try
    if AnsiSameText(sSERVICES_PREFIXE,ExtractNextPathElement(loc_RequestInfo.URI)) then begin

{$IFDEF WST_BROKER}
      if not ProcessServiceRequestLibrary(loc_RequestInfo,loc_ResponseInfo) then
        ProcessWSDLRequest(loc_RequestInfo,loc_ResponseInfo);
{$ELSE}
      if not ProcessServiceRequest(loc_RequestInfo,loc_ResponseInfo) then
        ProcessWSDLRequest(loc_RequestInfo,loc_ResponseInfo);
{$ENDIF}
    end else begin
      ProcessWSDLRequest(loc_RequestInfo,loc_ResponseInfo);
    end;

    ap_set_content_type(r, PCHAR(loc_ResponseInfo.ContentType));
//    if AnsiSameText(sHTTP_BINARY_CONTENT_TYPE,loc_ResponseInfo.ContentType) then begin
      ap_set_content_length(r,Length(loc_ResponseInfo.ContentText));
      ap_rwrite(@(loc_ResponseInfo.ContentText[1]),Length(loc_ResponseInfo.ContentText),r);
      ap_rflush(r);
  {  end else begin
      ap_rputs(PCHAR(loc_ResponseInfo.ContentText), r);
      ap_rflush(r);
    end;}
    Result := OK;
  except
    on e : Exception do begin
      ap_set_content_type(r, 'text/html');
      ap_rputs('<HTML><HEAD> <TITLE>Error</TITLE></HEAD>' + LineEnding, r);
      ap_rputs('<BODY></BODY></HTML>',r);
      ap_rprintf(r, '<BODY><H1>"%s"</H1></BODY></HTML>' + LineEnding, [PCHAR(e.Message)]);
      ap_rflush(r);
      Exit;
    end;
  end;
end;

initialization
  wst_initialize();
  
end.
