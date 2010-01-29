library mod_wst;

{$mode objfpc}{$H+}

{$IFDEF WIN32}
  {$DEFINE WINDOWS}
{$ENDIF}

uses
  SysUtils,
  httpd, apr, apriconv, aprutil, wst_apache_binding,
  user_service_intf, user_service_intf_binder, user_service_intf_imp;

var
 wst_module: module; {$ifdef Unix} public name 'wst_module'; {$endif}
 default_module_ptr: Pmodule;

const
  MODULE_NAME = 'mod_wst.so';

{$ifdef WINDOWS}
exports
 wst_module name 'wst_module';
{$endif}


function DefaultHandler(r: Prequest_rec): Integer;cdecl;
begin
  if not SameText(r^.handler, 'wst-handler') then
  begin
    Result := DECLINED;
    Exit;
  end;

  Result := wst_RequestHandler(r);
end;

procedure RegisterHooks(p: Papr_pool_t);cdecl;
begin
  ap_hook_handler(@DefaultHandler, nil, nil, APR_HOOK_MIDDLE);
end;

begin
  default_module_ptr := @wst_module;
  wst_apache_binding.wst_module_ptr := default_module_ptr;
  FillChar(default_module_ptr^, SizeOf(default_module_ptr^), 0);
  STANDARD20_MODULE_STUFF(default_module_ptr^);
  with wst_module do
  begin
    name := MODULE_NAME;
    magic := MODULE_MAGIC_COOKIE;
    register_hooks := @RegisterHooks;
{$IF DECLARED(wst_create_dir_config)}    
    create_dir_config := @wst_create_dir_config;
{$IFEND}
{$IF DECLARED(WstCommandStructArray)}    
    cmds := WstCommandStructArray;
{$IFEND}
  end;
{$IF DECLARED(WstCommandStructArray)}  
  WstCommandStructArray[0].cmd_data := @WstConfigData^.BasePath;
  FillChar(WstCommandStructArray[1],SizeOf(command_rec),#0);
{$IFEND}   

  RegisterUserServiceImplementationFactory();
  Server_service_RegisterUserServiceService();  
end.
