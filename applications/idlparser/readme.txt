This program is a simple IDL-parser that is used to generate header-include 
files for the Gecko-SDK. 

This application is able to parse the .idl files from the Gecko-SDK project.
It could be that it can be used or extended to parse idl-files from other 
projects, but I never tested that.

The generated Pascal-sources are as close as possible to the original idl
files. 

It is possible to specify what to do with each idl-type and how to
translate them into Pascal code. These settings are passed in a separate
configuration file. An example of such a file is idltypemap.cfg. 

All files are licensed by the modified LGPL, as used in the Lazarus LCL.

I hope this is useful for someone,

Joost van der Sluis, CNOC

March 19, 2012
