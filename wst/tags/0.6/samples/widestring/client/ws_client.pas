program ws_client;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cwstring,
  {$ENDIF}
  Classes, SysUtils,
  service_intf, soap_formatter, synapse_http_protocol, //synapse_tcp_protocol,
  echo_service, echo_service_proxy;

const
  utf16_bom : array[0..1] of Byte = ($FF, $FE);
  binary_buffer : array[0..7] of Word = ($E9, $E9, $E8, $E8, $E7, $E7, $E0, $E0);

var
  locService : IEchoService;
  ws, wsres : WideString;
  ms : TMemoryStream;
  c : Integer;
begin
  SYNAPSE_RegisterHTTP_Transport();
  //SYNAPSE_RegisterTCP_Transport();

  locService := wst_CreateInstance_IEchoService('SOAP:','HTTP:','http://127.0.0.1:8000/services/IEchoService');
  //locService := wst_CreateInstance_IEchoService('SOAP:','TCP:Port=1234;Target=IEchoService;','127.0.0.1');

  WriteLn('WST WideString Sample - Client');

  ms := TMemoryStream.Create();
  try
    SetLength(ws, Length(binary_buffer) );
    Move(binary_buffer[0], Pointer(ws)^, Length(binary_buffer) * 2);


    ms.Write(utf16_bom,SizeOf(utf16_bom));
    ms.Write(Pointer(ws)^, ( Length(ws) * SizeOf(WideChar) ) );
    ms.SaveToFile(ExpandFileName('.' + PathDelim + 'input.widestring'));

    wsres := locService.EchoWideString(ws);
    ms.Clear();
    ms.Write(utf16_bom,SizeOf(utf16_bom));
    ms.Write(Pointer(wsres)^, ( Length(wsres) * SizeOf(WideChar) ) );
    ms.SaveToFile(ExpandFileName('.' + PathDelim + 'output.widestring'));
  finally
    ms.Free();
  end;
end.

