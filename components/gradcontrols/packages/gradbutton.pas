{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit gradbutton; 

interface

uses
  ugradbtn, uRotateBitmap, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ugradbtn', @ugradbtn.Register); 
end; 

initialization
  RegisterPackage('gradbutton', @Register); 
end.
