unit registervirtualtreeview; 

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  VirtualTrees, VTHeaderPopup;
  
procedure Register;

implementation

procedure RegisterUnitVirtualTrees;
begin
  RegisterComponents('Virtual Treeview', [TVirtualDrawTree,TVirtualStringTree]);
end;  

procedure RegisterUnitVTHeaderPopup;
begin
  RegisterComponents('Virtual Treeview', [TVTHeaderPopupMenu]);
end;

procedure Register;

begin
  RegisterUnit('VirtualTrees',@RegisterUnitVirtualTrees);
  RegisterUnit('VTHeaderPopup',@RegisterUnitVTHeaderPopup);
end; 

initialization
{$i ideicons.lrs}
 
end.
