This program is a simple IDL-parser that is used to generate header-include 
files for the Gecko-SDK. 

This application is able to parse the .idl files from the Gecko-SDK project.
It could be that it can be used or extended to parse idl-files from other 
projects, but I never tested that.

The generated Pascal-sources are as close as possible to the original idl
files. C-types are converted to their corresponding types in fpc's ctypes 
unit.

It is possible to map some types from their idl to Pascal name. You can also
specify which types have to be converted to their ctypes-unit equivalent.

All files are licensed by the modified LGPL, as used in the Lazarus LCL.

I hope this is useful for someone,

Joost van der Sluis, CNOC

March 14, 2012
