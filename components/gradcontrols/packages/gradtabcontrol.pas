{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit gradtabcontrol; 

interface

uses
ugradtabcontrol, gradtabcontroleditor, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ugradtabcontrol', @ugradtabcontrol.Register); 
end; 

initialization
  RegisterPackage('gradtabcontrol', @Register); 
end.
