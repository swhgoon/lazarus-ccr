program loadmod;

{$mode objfpc}{$H+}

{$apptype console}

uses Classes, Windows, sysutils;

var
  Lib, Lib2: HINST;
begin
  Lib := LoadLibrary('C:\Programas\Apache\lazarus-ccr\httpd\mod_hello.so');
//  Lib2 := LoadLibrary('C:\Programas\Apache\pmodules\newlib2.dll');
  WriteLn(Lib);
  WriteLn(Lib2);
  WriteLn('Last Error: ' + IntToStr(GetLastError));
//  Sleep(5000);
  FreeLibrary(Lib);
//  FreeLibrary(Lib2);
end.

