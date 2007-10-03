{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit JvNavigationPaneLaz; 

interface

uses
  JvNavigationPaneReg, JvNavigationPane, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('JvNavigationPaneReg', @JvNavigationPaneReg.Register); 
end; 

initialization
  RegisterPackage('JvNavigationPaneLaz', @Register); 
end.
