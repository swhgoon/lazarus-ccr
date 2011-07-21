//******************************************************************************
//***                     Cactus Jukebox                                     ***
//***                                                                        ***
//***  (c) 2006-2009                                                         ***
//***                                                                        ***
//***  Massimo Magnano, maxm.dev@gmail.com                                   ***
//***                                                                        ***
//******************************************************************************
//  File        : CJ_Plugin.pas
//
//  Description : Declaration of Functions and Class that a plugin must export.
//
//******************************************************************************
//
//  functions that must be exported by a Plugin :
//     function GetPlugin :TCJ_Plugin; stdcall;
//     function GetPluginInfo :TCJ_PluginInfo; stdcall;
//
//  Class Methods Call Sequenze :
//     Program Side    |  Plugin Side
//------------------------------------------
//        Create      --> OnCreate
//                        if LastEnabledState then SetEnabled(True)
//
//        CloseQuery  --> CanClose   (if all plugins return true then Destroy)
//
//        Destroy     --> if Enabled then SetEnabled(False)
//                        OnDestroy

unit cj_plugin;
{$mode delphi}{$H+}
interface

uses cj_interfaces;

const
     PLUGIN_TYPE_NONE      = 0;
     PLUGIN_TYPE_STD       = 1;

type
    TChars50 =array[0..50] of char;

    TCJ_PluginInfo = record
       pType      :Integer;           //PLUGIN_TYPE_XXXX
       pPackName,                     //The Name of Package when auto-install-uninstall (future use)
       pTitle,
       pAuthor,
       pEMail,
       pDescript  :TChars50;
    end;
    PCJ_PluginInfo =^TCJ_PluginInfo;

    TCJ_Plugin = class
    public
       procedure OnCreate(ACJInterface :TCJ_Interface); virtual; abstract;
       procedure OnDestroy; virtual; abstract;
       function  CanClose :Boolean; virtual; abstract;
       procedure SetEnabled(Value :Boolean); virtual; abstract;
       procedure SettingsChanged; virtual; abstract;
       procedure Settings; virtual; abstract;
       function  GetInfo :TCJ_PluginInfo; virtual; abstract;
    end;

    Tfunction_GetPlugin = function :TCJ_Plugin; stdcall;
    Tfunction_GetPluginInfo = function :TCJ_PluginInfo; stdcall;

implementation

end.
