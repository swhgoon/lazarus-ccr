{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit virtualtreeslcl; 

interface

uses
  VirtualTrees, VirtualDrawTree, VirtualStringTree, VTHeaderPopup, VTRegister, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('VTRegister', @VTRegister.Register); 
end; 

initialization
  RegisterPackage('virtualtreeslcl', @Register); 
end.
