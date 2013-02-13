program TableView;

{$modeswitch ObjectiveC1}

uses
  iPhoneAll, appdelegate_iphoneu, TableData;

var
  pool : NSAutoreleasePool;
begin
  pool := NSAutoreleasePool.alloc.init;
  UIApplicationMain(argc, argv, nil, nil);
  pool.release;
end.

