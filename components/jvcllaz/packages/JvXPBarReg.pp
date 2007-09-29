unit JvXPBarReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation
uses JvXPBar;

procedure Register;
begin
  RegisterComponents('JvXP',[TJvXPBar]);
end;

initialization
  {$I JvXPBarLaz.lrs}
  
end.

