unit register_smnetgradient;

{$mode objfpc}{$H+}

interface

uses
  Classes, LazarusPackageIntf, SMNetGradient, LResources;

procedure Register;

implementation

procedure RegisterUnitSMNetGradient;
begin
  RegisterComponents('SMACE', [TNetGradient, TDBNetGradient]);
end;

procedure Register;
begin
  RegisterUnit('SMNetGradient', @RegisterUnitSMNetGradient);
end;

initialization
  {$i register_smnetgradient.lrs}
end.

