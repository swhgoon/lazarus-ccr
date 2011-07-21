//******************************************************************************
//***                     Cactus Jukebox                                     ***
//***                                                                        ***
//***  (c) 2006-2009                                                         ***
//***                                                                        ***
//***  Massimo Magnano, maxm.dev@gmail.com                                   ***
//***                                                                        ***
//******************************************************************************
//  File        : cj_pluginslist.pas
//
//  Description : List of Plugins and other plugins functions.
//
//******************************************************************************
unit cj_pluginslist;
{$mode delphi}{$H+}
interface

uses dynlibs, global_vars, MGList, cj_plugin, cj_interfaces, cj_interfaces_impl;

type
    TCJ_PluginDescr = record
       DllHandle   :TLibHandle;
       DllPath     :String;
       Enabled     :Boolean;
       pClass      :TCJ_Plugin;
       Info        :TCJ_PluginInfo;
    end;
    PCJ_PluginDescr = ^TCJ_PluginDescr;

    TCJ_PluginsList = class (TMGList)
    protected
       function allocData :Pointer; override;
       procedure deallocData(pData :Pointer); override;
    public
       destructor Destroy; override;
       procedure LoadFromINI;
       function  CanClose :Boolean;
       function ExtFind(PluginName :String): PCJ_PluginDescr; overload;
       function Find(PluginName :String) :Boolean; overload;
       procedure SettingsChanged;
    end;

Var
   PluginsList :TCJ_PluginsList=Nil;

implementation

uses Classes, Controls, SysUtils, Forms, Dialogs, inifiles;

const
     ERR_LOAD_PLUGIN  ='Error Loading %s'#13#10'%s'#13#10'Try to Load at next time?';
     ERR_LOAD_INVALID ='Invalid Plugin...';
     ERR_LOAD_CAPT    ='Plugins loading..';

destructor TCJ_PluginsList.Destroy;
var
   curPlugin :PCJ_PluginDescr;

begin
  try
     curPlugin :=Self.FindFirst;
     while (curPlugin<>Nil) do
     begin
          if (curPlugin^.pClass<>Nil)
          then try
                  if curPlugin^.Enabled
                  then curPlugin^.pClass.SetEnabled(False);
                  curPlugin^.pClass.OnDestroy;
                  curPlugin^.pClass.Free;
               except
                  On E:Exception do begin end;
               end;

          FreeLibrary(curPlugin^.DllHandle);

          curPlugin :=Self.FindNext;
     end;
     Self.FindClose;
  except
     On E:Exception do begin end;
  end;

  inherited Destroy;
end;

function TCJ_PluginsList.allocData :Pointer;
begin
     GetMem(Result, SizeOf(TCJ_PluginDescr));
     FillChar(Result^, SizeOf(TCJ_PluginDescr), 0);
     PCJ_PluginDescr(Result)^.DllPath :='';     //Avoid strange String problems
end;

procedure TCJ_PluginsList.deallocData(pData :Pointer);
begin
     PCJ_PluginDescr(pData)^.DllPath :='';
     FreeMem(pData, SizeOf(TCJ_PluginDescr));
end;

procedure TCJ_PluginsList.LoadFromINI;
var
   curPlugin   :PCJ_PluginDescr;
   theINI      :TIniFile;
   INIpackList :TStringList;
   curSect     :String;
   i           :Integer;
   xDllHandle  :HModule;
   xDllPath    :String;
   xEnabled    :Boolean;
   xGetPluginInfo :Tfunction_GetPluginInfo;
   xGetPlugin     :Tfunction_GetPlugin;

   procedure DoInClass;
   Var
      tmpInfo :TCJ_PluginInfo;

   begin
        if (curPlugin^.pClass<>Nil) then
        begin
             try
                curPlugin^.pClass.OnCreate(cj_interfaces_impl.CJ_Interface);
             except
             end;

             try
                tmpInfo :=curPlugin^.pClass.GetInfo;
                curPlugin^.Info :=tmpInfo;
             except
             end;

             try
                if xEnabled
                then curPlugin^.pClass.SetEnabled(True);
             except
             end;
        end;
   end;

begin
     theINI :=TIniFile.Create(PATH_Config+INI_PLUGINS);
     INIpackList :=TStringList.Create;
     try
        theINI.ReadSections(INIpackList);

        for i :=0 to INIpackList.Count-1 do
        begin
          curPlugin :=Nil;
          try
             CurSect :=Uppercase(INIpackList.Strings[i]);
             curPlugin := Self.ExtFind(CurSect);
             if (curPlugin=Nil) then
             begin
                  curPlugin :=self.Add;
                  //xDllPath :=ProcessVarValue(theINI.ReadString(CurSect, 'DLL', ''));
                  xDllPath :=PATH_Plugins+theINI.ReadString(CurSect, 'DLL', '');
                  xEnabled :=theINI.ReadBool(CurSect, 'ENABLED', True);
                  xDllHandle := LoadLibrary(PChar(xDllPath));
                  if (xDllHandle=0)
                  then raise Exception.Create(ERR_LOAD_INVALID);

                  curPlugin^.DllHandle :=xDllHandle;
                  curPlugin^.DllPath :=xDllPath;
                  curPlugin^.Enabled :=xEnabled;

                  xGetPluginInfo :=GetProcAddress(xDllHandle, 'GetPluginInfo');
                  if not(Assigned(xGetPluginInfo))
                  then raise Exception.Create(ERR_LOAD_INVALID);

                  //Move(xGetPluginInfo, curPlugin^.Info, SizeOf(TCJ_PluginInfo));
                  curPlugin^.Info :=xGetPluginInfo;

                  Case curPlugin^.Info.pType of
                  PLUGIN_TYPE_NONE : begin end;
                  PLUGIN_TYPE_STD  : begin
                                         xGetPlugin :=GetProcAddress(xDllHandle, 'GetPlugin');
                                         if Assigned(xGetPlugin)
                                         then curPlugin^.pClass := xGetPlugin;
                                     end;
                  else raise Exception.Create(ERR_LOAD_INVALID);
                  end;

                  DoInClass;
             end;
          except
             On E:Exception do
             begin
                  if (MessageDlg(ERR_LOAD_CAPT, Format(ERR_LOAD_PLUGIN, [xDllPath, E.Message]),
                                 mtError, [mbYes, mbNo], 0)=mrNo)
                  then theINI.EraseSection(CurSect);

                  if (curPlugin<>Nil)
                  then self.Delete(curPlugin, 0);
             end;
          end;
        end;
     finally
        theINI.Free;
        INIpackList.Free;
     end;
end;

function  TCJ_PluginsList.CanClose :Boolean;
var
   curPlugin :PCJ_PluginDescr;

begin
  Result :=True;
  try
     curPlugin :=Self.FindFirst;
     while (curPlugin<>Nil) and (Result=True) do
     begin
          if (curPlugin^.pClass<>Nil)
          then try
                  Result :=curPlugin^.pClass.CanClose;
               except
                  Result :=True;
               end
          else Result :=True;

          curPlugin :=Self.FindNext;
     end;
     Self.FindClose;
  except
     Result :=True;
  end;
end;

function TCJ_PluginsList.ExtFind(PluginName :String): PCJ_PluginDescr;

    function FindByName(Tag :Integer; ptData1, ptData2 :Pointer) :Boolean;
    begin
         Result := (String(PChar(ptData1)) = PCJ_PluginDescr(ptData2)^.Info.pPackName);
    end;

begin
     Result :=Self.ExtFind(PChar(Uppercase(PluginName)), 0, @FindByName);
end;

function TCJ_PluginsList.Find(PluginName :String) :Boolean;
begin
     Result := (ExtFind(PluginName)<>Nil);
end;

procedure TCJ_PluginsList.SettingsChanged;
begin
end;

end.
