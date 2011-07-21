//******************************************************************************
//***                     Cactus Jukebox                                     ***
//***                                                                        ***
//***  (c) 2006-2009                                                         ***
//***                                                                        ***
//***  Sebastian Kraft, sebastian_kraft@gmx.de                               ***
//***  Massimo Magnano, maxm.dev@gmail.com                                   ***
//***                                                                        ***
//******************************************************************************
//  File        : global_vars.pas
//
//  Description : Common Global Vars of the project.
//
//******************************************************************************
unit global_vars;
{$mode delphi}{$H+}
interface

uses Controls, menus, ExtCtrls;

Const
     INI_PLUGINS ='plugins.ini';

Var
   AppMainMenu          :TMainMenu =Nil;
   AppTrayIcon          :TTrayIcon =Nil;
   ImageListNormal      :TImageList;
   PluginsSeparatorItem :TMenuItem=Nil;
   PATH_Home,
   PATH_Data,
   PATH_Config,
   PATH_Plugins          :String;


procedure RegisterPlugin(Name, DLLFileName :String);

implementation

uses SysUtils, Forms, inifiles;


procedure RegisterPlugin(Name, DLLFileName :String);
Var
   theINI  :TIniFile;

begin
     theINI :=TIniFile.Create(PATH_Config+INI_PLUGINS);
     theINI.WriteString(Name, 'DLL', DLLFileName);
     theINI.Free;
end;

procedure CalcPathValues;
begin
   PATH_Home :=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
{$ifdef CactusRPM}
   PATH_Data :='/usr/share/cactusjukebox/';
   PATH_Config :=IncludeTrailingPathDelimiter(PATH_Home+'.cactusjukebox');
 {$else}
   PATH_Data :=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
   SetCurrentDir(PATH_Data);
   PATH_Config :=PATH_Data;
{$endif}
   PATH_Plugins :=IncludeTrailingPathDelimiter(PATH_Config+'plugings');
end;

initialization
              CalcPathValues;

end.
