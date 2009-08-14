{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit beepfp; 

interface

uses
    BeepPeer, BeepChannel, BeepChannelPool, BeepClient, BeepConnection, 
  BeepContext, BeepFrame, BeepListener, BeepObject, BeepProfile, BeepServer, 
  BeepUtils, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('beepfp', @Register); 
end.
