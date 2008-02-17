unit foundation;

{$ifdef fpc}
  {$mode delphi}
  {$packrecords c}
{$endif}

interface

uses ctypes, objc, FPCMacOSAll;

{$include Foundation.inc}

implementation

{$include Foundation_impl.inc}

end.

