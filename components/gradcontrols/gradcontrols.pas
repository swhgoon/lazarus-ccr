{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit gradcontrols; 

interface



uses
  ugradtabcontrol, ugradbtn, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ugradtabcontrol', @ugradtabcontrol.Register); 
  RegisterUnit('ugradbtn', @ugradbtn.Register); 
end; 

initialization
  RegisterPackage('gradcontrols', @Register); 
end.
