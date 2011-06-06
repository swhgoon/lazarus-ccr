{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpvectorialpkg; 

interface

uses
  svgvectorialwriter, pdfvrsintatico, pdfvrsemantico, pdfvrlexico, 
  pdfvectorialreader, fpvtocanvas, fpvectorial, fpvectbuildunit, 
  dxfvectorialreader, cdrvectorialreader, avisozlib, avisocncgcodewriter, 
  avisocncgcodereader, svgvectorialreader, epsvectorialreader, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('fpvectorialpkg', @Register); 
end.
