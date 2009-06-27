{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007, 2008, 2009 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_support_client;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  TypInfo,
  wst_types, base_service_intf, imp_utils, test_suite_utils, service_intf;

type

  { TTest_TBaseProxy }

  TTest_TBaseProxy = class(TWstBaseTest)
  published
    procedure test_CreateWithInvalidParameters_AProtocol();
    procedure test_CreateWithInvalidParameters_AProtocolData();
    procedure test_CreateWithInvalidParameters_ATransportData();
  end;

implementation
uses
  //Include this so we are sure to have a valid transport protocol registered
  same_process_protocol,
  //Include this so we are sure to have a valid serialization protocol registered
  binary_formatter;

const
  s_target_service = 'SampleService';

{ TTest_TBaseProxy }

procedure TTest_TBaseProxy.test_CreateWithInvalidParameters_AProtocol();
var
  ok : Boolean;
  sp : IServiceProtocol;
begin
  ok := False;
  try
    sp := nil;
    TBaseProxy.Create(s_target_service,sp);
  except
    ok := True;
  end;
  Check(ok);
end;

procedure TTest_TBaseProxy.test_CreateWithInvalidParameters_AProtocolData();
var
  ok : Boolean;
begin
  ok := False;
  try
    TBaseProxy.Create(s_target_service,'NILPROTOCOL:nil_prop=nilvalue','SAME_PROCESS:');
  except
    ok := True;
  end;
  Check(ok);
end;

procedure TTest_TBaseProxy.test_CreateWithInvalidParameters_ATransportData();
var
  ok : Boolean;
  sp : IServiceProtocol;
begin
  ok := False;
  try
    sp := nil;
    TBaseProxy.Create(s_target_service,'binary:','NILPROTOCOL:');
  except
    ok := True;
  end;
  Check(ok);
end;

initialization
  SAME_PROCESS_Register_Local_Transport();

  RegisterTest('Support-Client',TTest_TBaseProxy.Suite);

end.

