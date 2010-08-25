unit RegisterRxTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

procedure Register;

implementation
uses RxSystemServices, RxLogin, RxVersInfo;
const
  sRxToolsPage = 'RX Tools';

procedure RegisterRxSystemServices;
begin
  RegisterComponents(sRxToolsPage, [TRxSystemServices]);
end;

procedure RegisterRxLogin;
begin
  RegisterComponents(sRxToolsPage, [TRxLoginDialog]);
end;

procedure RegisterRxVersInfo;
begin
  RegisterComponents(sRxToolsPage, [TRxVersionInfo]);
end;

procedure Register;
begin
  RegisterUnit('RxLogin', @RegisterRxLogin);
  RegisterUnit('RxVersInfo', @RegisterRxVersInfo);
  RegisterUnit('RxSystemServices', @RegisterRxSystemServices);
end;

end.

