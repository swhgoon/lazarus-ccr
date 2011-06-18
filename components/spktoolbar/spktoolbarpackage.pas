{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SpkToolbarPackage; 

interface

uses
  spkt_Appearance, spkt_BaseItem, spkt_Buttons, spkt_Const, spkt_Dispatch, 
  spkt_Exceptions, spkt_Items, spkt_Pane, spkt_Tab, spkt_Tools, spkt_Types, 
  SpkToolbar, SpkMath, SpkGUITools, SpkGraphTools, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('SpkToolbarPackage', @Register); 
end.
