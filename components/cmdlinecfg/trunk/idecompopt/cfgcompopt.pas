{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit cfgcompopt;

interface

uses
  optviewform, cmdlinelclctrlsbox, cmdlinelclutils, cmdlinecfg, 
  cmdlinecfgjson, cmdlinecfgparser, cmdlinecfgui, cmdlinecfguijson, 
  cmdlinecfgutils, cmdlinefpccond, cfgcompoptreg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('cfgcompoptreg', @cfgcompoptreg.Register);
end;

initialization
  RegisterPackage('cfgcompopt', @Register);
end.
