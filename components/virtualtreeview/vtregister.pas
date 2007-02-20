unit VTRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,VirtualTrees,VirtualStringTree,VirtualDrawTree,VTHeaderPopup;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Virtual Controls', [TVirtualStringTree, TVirtualDrawTree]);
  RegisterComponents('Virtual Controls', [TVTHeaderPopupMenu]);
end;


end.

