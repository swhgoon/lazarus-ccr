program client_calc;
{$mode objfpc}{$H+}
uses
  Classes, SysUtils,
  soap_formatter, synapse_http_protocol,
  calcservice, calcservice_proxy, base_service_intf;
var
  theProxy : ICalcService;
  callContext : ICallContext;
  loginHeader : TLoginHeader;
  sessionHeader : TSessionHeader;
  sessionToken : string;
  
  procedure PrepareAndSendSessionHeader();
  var
    k, c : Integer;
  begin
    callContext := theProxy as ICallContext;
    c := callContext.GetHeaderCount(AllHeaderDirection);
    if ( c > 0 ) then begin
      for k := 0 to Pred(c) do begin
        if callContext.GetHeader(k).InheritsFrom(TSessionHeader) then begin
          sessionToken := TSessionHeader(callContext.GetHeader(k)).SessionToken;
          Writeln; WriteLn('  Your session token is : ',sessionToken); WriteLn;
          sessionHeader := TSessionHeader.Create();
          sessionHeader.SessionToken := sessionToken;
          // Delete the Login Header
          callContext.ClearHeaders(hdOut);
          loginHeader := nil{the object no longer exists};
          //Add the session token
          callContext.AddHeader(sessionHeader,True{Let the CallContext free the object});
          Break;
        end;
      end;
    end;
  end;

var
  a, b : Integer;
  op, c : Char;
  strBuffer : string;
begin
  SYNAPSE_RegisterHTTP_Transport();
  theProxy := wst_CreateInstance_ICalcService();
  loginHeader := TLoginHeader.Create();
  WriteLn('Calculator Client demo');
  Write('  Enter your user name : ');
    ReadLn(strBuffer);
    loginHeader.UserName := strBuffer;
  Write('  Enter your password : ');
    ReadLn(strBuffer);
    loginHeader.Password := strBuffer;
  WriteLn();
  sessionHeader := nil;
  try
    repeat
      if ( sessionHeader = nil ) then begin
        // login the first time
        loginHeader.mustUnderstand := 1;
        loginHeader.Direction := hdOut;
        (theProxy as ICallContext).AddHeader(loginHeader,True{Let the CallContext free the object});
      end;
      Write('Enter first operand : '); ReadLn(a);
      Write('Enter second operand : '); ReadLn(b);
      Write('Enter operator ( + or - ) : '); ReadLn(op);
      case op of
        '+' : WriteLn(a,' + ',b,' = ',theProxy.Add(a,b));
        '-' : WriteLn(a,' - ',b,' = ',theProxy.Substract(a,b));
        else WriteLn('Unknown operator : ',op);
      end;
      if ( sessionHeader = nil ) then begin
        {Now prepare the session header}
        PrepareAndSendSessionHeader();
      end;
      WriteLn(''); Write('  Continue (y/n) ? : '); ReadLn(c);
      WriteLn('')
    until ( c <> 'y' );
  except
    on e : Exception do begin
      WriteLn;
      WriteLn('An exception occurs >> ');
      Write(e.Message);
    end;
  end;
end.

