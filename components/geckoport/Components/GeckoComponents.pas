{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GeckoComponents; 

interface

uses
    GeckoBrowser, GeckoChromeWindow, GeckoInit, nsCID, nsConsts, 
  nsEnumerators, nsError, nsErrorUtils, nsGeckoStrings, nsInit, nsMemory, 
  nsNetUtil, nsStream, nsTypes, nsXPCOM, nsXPCOMGlue, nsXRE, nsXPCOM_std19, 
  GeckoPromptService, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GeckoBrowser', @GeckoBrowser.Register); 
  RegisterUnit('GeckoPromptService', @GeckoPromptService.Register); 
end; 

initialization
  RegisterPackage('GeckoComponents', @Register); 
end.
