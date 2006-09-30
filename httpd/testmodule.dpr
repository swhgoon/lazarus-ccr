{*******************************************************************
*  Test library of the Apache Pascal Headers
*******************************************************************}
library testmodule;

{*******************************************************************
*  The mode must be objfpc on this unit because the unix code uses
* some extensions introduced on Free Pascal
*******************************************************************}
{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

{$IFNDEF FPC}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE WINDOWS}
{$ENDIF}

{*******************************************************************
*  Assembler code to export variables on UNIXes
*******************************************************************}
{$IFNDEF WINDOWS}
  {$l apache_module.o}
{$ENDIF}

uses
 minimain in 'minimain.pas';

var
 test_module: module; {$ifdef Unix}cvar; external; {$endif}
 default_module_ptr: Pmodule;

{*******************************************************************
*  Free Pascal only supports exporting variables on Windows
*******************************************************************}
{$ifdef WINDOWS}
exports
 test_module name 'test_module';
{$endif}

{*******************************************************************
*  Library initialization code
*******************************************************************}
begin
  default_module_ptr := @test_module;
  FillChar(default_module_ptr^, SizeOf(default_module_ptr^), 0);
  with default_module_ptr^ do begin
    version := MODULE_MAGIC_NUMBER_MAJOR;
    minor_version := MODULE_MAGIC_NUMBER_MINOR;
    module_index := -1;
    name := 'testmodule.so';
    magic := MODULE_MAGIC_COOKIE;
    register_hooks := @RegisterHooks;
  end;
end.

