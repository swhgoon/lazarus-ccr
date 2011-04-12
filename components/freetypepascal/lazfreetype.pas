{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazfreetype; 

interface

uses
  FreeType, TTCache, TTCalc, TTCMap, TTDebug, TTError, TTFile, TTGLoad, 
  TTInterp, TTLoad, TTMemory, TTObjs, TTRASTER, TTTables, TTTypes, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('lazfreetype', @Register); 
end.
