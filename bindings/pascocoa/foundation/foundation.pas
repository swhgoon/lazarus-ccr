unit foundation;

{$ifdef fpc}
  {$mode delphi}
  {$packrecords c}
{$endif}

interface

uses ctypes, objc, FPCMacOSAll;

{$define HEADER}
{$include Foundation.inc}
{$undef HEADER}

type
{$define CLASSES}
{$include Foundation.inc}
{$undef CLASSES}

implementation

{$define IMPLEMENTATION}
{$include Foundation.inc}
{$undef IMPLEMENTATION}

end.

