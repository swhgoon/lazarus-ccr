program client_sample;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  base_service_intf, synapse_http_protocol,
  soap_formatter,
  base64sample, base64sample_proxy;

var
  service : SampleService;
  locBuffer, locResBuffer : TBase64StringRemotable;
  i : Integer;
  s : ansistring;
begin
  SYNAPSE_RegisterHTTP_Transport();

  locResBuffer := nil;
  locBuffer := TBase64StringRemotable.Create();
  try
    SetLength(s,255);
    for i := 1 to Length(s) do
      s[i] := Char(i);
    locBuffer.BinaryData := s;
    service := wst_CreateInstance_SampleService();
    locResBuffer := service.DuplicateContent(locBuffer,1);
    WriteLn('Input content : ',locBuffer.BinaryData,'  Encoded : ',locBuffer.EncodedString);
    WriteLn('Output content : ',locResBuffer.BinaryData,'  Encoded : ',locResBuffer.EncodedString);
    WriteLn('Check = ', ( locResBuffer.BinaryData = s ) );
    ReadLn;
  finally
    locResBuffer.Free();
    locBuffer.Free();
  end;
end.

