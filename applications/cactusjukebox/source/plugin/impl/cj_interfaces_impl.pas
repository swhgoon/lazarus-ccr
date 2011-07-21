//******************************************************************************
//***                     Cactus Jukebox                                     ***
//***                                                                        ***
//***  (c) 2006-2009                                                         ***
//***                                                                        ***
//***  Massimo Magnano, maxm.dev@gmail.com                                   ***
//***                                                                        ***
//******************************************************************************
//  File        : cj_interfaces_impl.pas
//
//  Description : Implementation of the Interfaces
//                that can be accessed by a plugin.
//
//******************************************************************************

unit cj_interfaces_impl;
{$mode delphi}{$H+}
interface

uses cj_interfaces, Classes, Controls, menus, MGSignals, Messages;

type

    { TCJ_Menu_Impl }

    TCJ_Menu_Impl = class(TCJ_Menu)
    private
       procedure MenuItemClick(Sender :TObject);
       function FindItem(MenuItem :TCJ_MenuItem):TMenuItem;
    public
       function Add(Parent :TCJ_MenuItem;
                    Caption :PChar; OnClick :TCJ_MenuItemClick) :TCJ_MenuItem; override;
       function AddSeparator(Parent :TCJ_MenuItem) :TCJ_MenuItem; override;
       function Remove(MenuItem :TCJ_MenuItem) :Boolean; override;

       function SetCaption(MenuItem :TCJ_MenuItem;
                           NewCaption :PChar):PChar; override;
       function SetEnabled(MenuItem :TCJ_MenuItem; Value :Boolean):Boolean; override;
       function SetChecked(MenuItem :TCJ_MenuItem; Value :Boolean):Boolean; override;
       function SetIcon(MenuItem :TCJ_MenuItem;
                        State, NewIcon :Integer):Integer; override;
       function SetOnClick(MenuItem :TCJ_MenuItem;
                           NewOnClick :TCJ_MenuItemClick):TCJ_MenuItemClick; override;

       function GetCount(MenuItem :TCJ_MenuItem) :Integer; override;
       function GetItem(MenuItem :TCJ_MenuItem; Index :Integer) :TCJ_MenuItem; override;
       function GetCaption(MenuItem :TCJ_MenuItem; Buffer :PChar) :Integer; override;
       function GetEnabled(MenuItem :TCJ_MenuItem) :Boolean; override;
       function GetChecked(MenuItem :TCJ_MenuItem) :Boolean; override;
       function GetIcon(MenuItem :TCJ_MenuItem; State :Integer):Integer; override;
       function GetOnClick(MenuItem :TCJ_MenuItem):TCJ_MenuItemClick; override;
    end;


    TCJ_TrayIcon_Impl = class(TCJ_TrayIcon)
    private
       procedure NotificationTitleMouseUp(Sender: TObject; Button: TMouseButton;
                                          Shift: TShiftState; X, Y: Integer);

    public
       procedure AddNotificationIcon(Icon :Integer;
                            Sound :PChar;
                            ShowEverySec :Integer;
                            DelAfterSec :Integer
                            ); override;
       procedure ShowNotification(AImageList :Integer; Icon :Integer; Msg :PChar; Sound :PChar); override;
       procedure PlaySound(Sound :PChar); override;
    end;

    { TCJ_Signals_Impl }

    TCJ_Signals_Impl = class(TCJ_Signals)
    private
       rSignals :TMGSignals;
    public
       procedure Connect(ClassMethod :TCJ_SignalMethod; MessageID :Integer); override;
       procedure ConnectAsync(ClassMethod :TCJ_SignalMethod; MessageID :Integer; Priority :Integer=0); override;
       procedure Disconnect(ClassMethod :TCJ_SignalMethod; MessageID :Integer); override; overload;
       procedure Disconnect(ClassPointer :TObject); override; overload;
       function Signal(MessageID :Cardinal; WParam, LParam :Integer; var Handled :Boolean) :Integer; override; overload;
       function Signal(var aMessage: TMessage) :Boolean; override; overload;

       constructor Create;
       destructor Destroy; override;
    end;

    { TCJ_Interface_Impl }

    TCJ_Interface_Impl = class(TCJ_Interface)
    private
       rMenu     :TCJ_Menu_Impl;
       rTrayIcon :TCJ_TrayIcon_Impl;
       rSignals  :TCJ_Signals;
    public
       function GetMenu : TCJ_Menu; override;
       function GetTrayIcon : TCJ_TrayIcon; override;
       function GetSignals : TCJ_Signals; override;
       function GetOption(OptionCategoryID :Integer; OptionID :Integer;
                          Buffer :Pointer):Integer; override;

       constructor Create;
       destructor Destroy; override;
    end;

Var
   MenuOwner    :TComponent=Nil;
   CJ_Interface :TCJ_Interface_Impl=Nil;


implementation

uses SysUtils, global_vars;

type
    PMethod =^TMethod;

    TMyMenuItem = class(TMenuItem)
    public
       pluginOnClick : TCJ_MenuItemClick;
    end;



//==============================================================================
//  TCJ_Menu_Impl = class(TCJ_PluginsMenu)
// Implementazione dell' Interfaccia al Menu
//==============================================================================

procedure TCJ_Menu_Impl.MenuItemClick(Sender :TObject);
Var
   PluginMethod :TCJ_MenuItemClick;

begin
  try
     if (Sender is TMyMenuItem) then
     begin
          if Assigned(TMyMenuItem(Sender).pluginOnClick)
          then TMyMenuItem(Sender).pluginOnClick(TMyMenuItem(Sender).Command);
     end;
  except
  end;
end;

function TCJ_Menu_Impl.FindItem(MenuItem: TCJ_MenuItem): TMenuItem;
begin
     Result :=TMenuItem(AppMainMenu.FindItem(MenuItem, fkCommand));
     if (Result=Nil)
     then Result :=TMenuItem(AppTrayIcon.PopUpMenu.FindItem(MenuItem, fkCommand));
end;



function TCJ_Menu_Impl.Add(Parent :TCJ_MenuItem;
                    Caption :PChar; OnClick :TCJ_MenuItemClick) :TCJ_MenuItem;
Var
   NewItem        :TMyMenuItem;
   ParentMenuItem :TMenuItem;
   xCaption       :String;
   itemMethod     :PMethod;
   iMenu          :Integer;
   addOnTop       :Boolean;

begin
  Result :=CJ_MENU_NULL;
  try
     if (Parent>0)
     then begin //an user MenuItem Command ID is specified as Parent, find
               ParentMenuItem :=Self.FindItem(Parent);
               if (ParentMenuItem=Nil)
               then Exit;
               if not(ParentMenuItem is TMyMenuItem)
               then Exit; //an internal Command Id is specified, exit
          end
     else begin //Menu IDs
               iMenu := (abs(Parent) and $FF00);
               case iMenu of
               CJ_MAINMENU_ROOT :begin
                                       iMenu :=(abs(Parent) and $FF)-1;
                                       if (iMenu>0)
                                       then ParentMenuItem :=AppMainMenu.Items[iMenu]
                                       else ParentMenuItem :=AppMainMenu.Items;

                                       addOnTop :=False;
                                  end;
               CJ_TRAYMENU_ROOT :begin
                                       iMenu :=(abs(Parent) and $FF)-1;
                                       if (iMenu>0)
                                       then ParentMenuItem :=AppTrayicon.PopUpMenu.Items[iMenu]
                                       else ParentMenuItem :=AppTrayicon.PopUpMenu.Items;
                                       addOnTop :=True;
                                  end;
               end;
          end;

     NewItem :=TMyMenuItem.Create(MenuOwner);
     xCaption :=Copy(Caption, 1, Length(Caption));
     NewItem.Caption :=xCaption;
     NewItem.OnClick :=MenuItemClick;

     NewItem.pluginOnClick :=OnClick;

     if addOnTop
     then ParentMenuItem.Insert(0, NewItem)
     else ParentMenuItem.Add(NewItem);

     ParentMenuItem.Visible :=(ParentMenuItem.Count>0);
     Result :=NewItem.Command;
  except
     On E:Exception do Result :=CJ_MENU_NULL;
  end;
end;

function TCJ_Menu_Impl.AddSeparator(Parent :TCJ_MenuItem) :TCJ_MenuItem;
begin
     Result :=Add(Parent, '-', Nil);
end;

function TCJ_Menu_Impl.Remove(MenuItem :TCJ_MenuItem) :Boolean;
Var
   ParentMenuItem,
   toDelMenuItem :TMenuItem;
   i             :Integer;

begin
  Result :=False;
  try
     toDelMenuItem :=Self.FindItem(MenuItem);
     if (toDelMenuItem=Nil)
     then Exit;

     //Avoid delete of our Menu Item....
     if not(toDelMenuItem is TMyMenuItem)
     then Exit; //an internal Command Id is specified, exit

     ParentMenuItem :=toDelMenuItem.Parent;
     if (ParentMenuItem<>Nil)
     then begin
               ParentMenuItem.Remove(toDelMenuItem);

               ParentMenuItem.Visible :=(ParentMenuItem.Count>0);
          end;

     Result :=True;
  except
     On E:Exception do Result :=False;
  end;
end;

function TCJ_Menu_Impl.SetCaption(MenuItem :TCJ_MenuItem;
                           NewCaption :PChar):PChar;
Var
   toChangeMenuItem :TMyMenuItem;
   xCaption         :String;

begin
  Result :=Nil;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
          xCaption :=Copy(NewCaption, 1, Length(NewCaption));
          toChangeMenuItem.Caption :=xCaption;
     end;
  except
     On E:Exception do Result :=Nil;
  end;
end;

function TCJ_Menu_Impl.SetEnabled(MenuItem :TCJ_MenuItem; Value :Boolean):Boolean;
Var
   toChangeMenuItem :TMyMenuItem;

begin
  Result :=False;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
          Result :=toChangeMenuItem.Enabled;
          toChangeMenuItem.Enabled :=Value;
     end;
  except
     On E:Exception do Result :=False;
  end;
end;

function TCJ_Menu_Impl.SetChecked(MenuItem :TCJ_MenuItem; Value :Boolean):Boolean;
Var
   toChangeMenuItem :TMyMenuItem;

begin
  Result :=False;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
          Result :=toChangeMenuItem.Checked;
          toChangeMenuItem.Checked :=Value;
     end;
  except
     On E:Exception do Result :=False;
  end;
end;


function TCJ_Menu_Impl.SetIcon(MenuItem :TCJ_MenuItem;
                              State, NewIcon :Integer):Integer;
Var
   toChangeMenuItem :TMyMenuItem;

begin
  Result :=-1;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
     {
          Case State of
          STATE_NORMAL      :begin
                                  Result :=toChangeMenuItem.icoUnchecked;
                                  toChangeMenuItem.icoUnchecked :=NewIcon;
                             end;
          STATE_SELECTED    :begin
                                  Result :=toChangeMenuItem.icoSelected;
                                  toChangeMenuItem.icoSelected :=NewIcon;
                             end;
          STATE_DISABLED    :begin
                                  Result :=toChangeMenuItem.icoDisabled;
                                  toChangeMenuItem.icoDisabled :=NewIcon;
                             end;
          STATE_HIGHLIGHTED :begin
                                  Result :=toChangeMenuItem.icoHighlighted;
                                  toChangeMenuItem.icoHighlighted :=NewIcon;
                             end;
          STATE_DOWN        :begin
                                  Result :=toChangeMenuItem.icoChecked;
                                  toChangeMenuItem.icoChecked :=NewIcon;
                             end;
          end;
      }
     end;
  except
     On E:Exception do Result :=-1;
  end;
end;

function TCJ_Menu_Impl.SetOnClick(MenuItem :TCJ_MenuItem;
                           NewOnClick :TCJ_MenuItemClick):TCJ_MenuItemClick;
Var
   toChangeMenuItem :TMyMenuItem;
   PluginMethod     :TCJ_MenuItemClick;

begin
  Result :=Nil;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
            Result := toChangeMenuItem.pluginOnClick;
            toChangeMenuItem.pluginOnClick := NewOnClick;
     end;
  except
     On E:Exception do Result :=Nil;
  end;
end;

function TCJ_Menu_Impl.GetCount(MenuItem :TCJ_MenuItem) :Integer;
Var
   toChangeMenuItem :TMyMenuItem;

begin
  Result :=-1;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
          Result :=toChangeMenuItem.Count;
     end;
  except
     On E:Exception do Result :=-1;
  end;
end;

function TCJ_Menu_Impl.GetItem(MenuItem :TCJ_MenuItem; Index :Integer) :TCJ_MenuItem;
Var
   theParentMenuItem,
   toGetMenuItem      :TMenuItem;

begin
  Result :=CJ_MENU_NULL;
  try
     theParentMenuItem :=Self.FindItem(MenuItem);
     if (theParentMenuItem<>Nil) then
     begin
          toGetMenuItem :=TMenuItem(theParentMenuItem.Items[Index]);
          if (toGetMenuItem<>Nil)
          then Result :=toGetMenuItem.Command;
     end;
  except
     On E:Exception do Result :=CJ_MENU_NULL;
  end;
end;

function TCJ_Menu_Impl.GetCaption(MenuItem :TCJ_MenuItem; Buffer :PChar) :Integer;
Var
   toChangeMenuItem :TMyMenuItem;

begin
  Result :=0;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
          Result :=Length(toChangeMenuItem.Caption)+1;
          if (Buffer<>Nil)
          then StrPLCopy(Buffer, toChangeMenuItem.Caption, Result-1);
     end;
  except
     On E:Exception do Result :=0;
  end;
end;

function TCJ_Menu_Impl.GetEnabled(MenuItem :TCJ_MenuItem) :Boolean;
Var
   toChangeMenuItem :TMyMenuItem;

begin
  Result :=False;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
          Result :=toChangeMenuItem.Enabled;
     end;
  except
     On E:Exception do Result :=False;
  end;
end;

function TCJ_Menu_Impl.GetChecked(MenuItem :TCJ_MenuItem) :Boolean;
Var
   toChangeMenuItem :TMyMenuItem;

begin
  Result :=False;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
          Result :=toChangeMenuItem.Checked;
     end;
  except
     On E:Exception do Result :=False;
  end;
end;


function TCJ_Menu_Impl.GetIcon(MenuItem :TCJ_MenuItem; State :Integer):Integer;
Var
   toChangeMenuItem :TMyMenuItem;

begin
  Result :=-1;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
       {   Case State of
          STATE_NORMAL      :Result :=toChangeMenuItem.icoUnchecked;
          STATE_SELECTED    :Result :=toChangeMenuItem.icoSelected;
          STATE_DISABLED    :Result :=toChangeMenuItem.icoDisabled;
          STATE_HIGHLIGHTED :Result :=toChangeMenuItem.icoHighlighted;
          STATE_DOWN        :Result :=toChangeMenuItem.icoChecked;
          end;
        }
     end;
  except
     On E:Exception do Result :=-1;
  end;
end;

function TCJ_Menu_Impl.GetOnClick(MenuItem :TCJ_MenuItem):TCJ_MenuItemClick;
Var
   toChangeMenuItem :TMyMenuItem;
   PluginMethod     :TCJ_MenuItemClick;

begin
  Result :=Nil;
  try
     toChangeMenuItem :=TMyMenuItem(Self.FindItem(MenuItem));
     if (toChangeMenuItem<>Nil) then
     begin
         Result :=toChangeMenuItem.pluginOnClick;
     end;
  except
     On E:Exception do Result :=Nil;
  end;
end;

//==============================================================================
//  TCJ_TrayIcon_Impl = class(TCJ_TrayIcon)
// Implementazione dell'interfaccia alla Tray Icon & ai Messaggi
//==============================================================================
procedure TCJ_TrayIcon_Impl.AddNotificationIcon(Icon :Integer;
                            Sound :PChar;
                            ShowEverySec :Integer;
                            DelAfterSec :Integer
                            );
begin
end;

procedure TCJ_TrayIcon_Impl.ShowNotification(AImageList :Integer; Icon :Integer; Msg :PChar; Sound :PChar);
begin
     if (AImageList=-1)
     then AImageList :=global_vars.ImageListNormal.Handle;

(*     TTrayNotifications.ShowNotification(-1, 'Cactus Jukebox',
                                         Icon, Msg, Sound,
                                         AImageList,
                                         NotificationTitleMouseUp, Nil);
*)
end;

procedure TCJ_TrayIcon_Impl.PlaySound(Sound :PChar);
begin
end;

procedure TCJ_TrayIcon_Impl.NotificationTitleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
     //AppTrayIcon.Popup;
end;



//==============================================================================
//  TCJ_Interface_Impl = class(TCJ_Interface)
// Implementazione dell'interfaccia al Programma
//==============================================================================

function TCJ_Interface_Impl.GetMenu : TCJ_Menu;
begin
     Result :=rMenu;
end;

function TCJ_Interface_Impl.GetTrayIcon : TCJ_TrayIcon;
begin
     Result :=rTrayIcon;
end;

function TCJ_Interface_Impl.GetSignals: TCJ_Signals;
begin
     Result:=rSignals;
end;

function TCJ_Interface_Impl.GetOption(OptionCategoryID :Integer; OptionID :Integer;
                                      Buffer :Pointer):Integer;

   procedure ReturnString(theString :String);
   begin
        Result :=Length(theString)+1;
        if (Buffer<>Nil)
        then begin
                  FillChar(PChar(Buffer)^, Result, 0);
                  StrPLCopy(PChar(Buffer), theString, Result-1);
             end;     
   end;

   procedure ReturnInteger(theInteger :Integer);
   begin
        Result :=theInteger;
   end;

begin
{     case OptionCategoryID of
     end;
}
end;

constructor TCJ_Interface_Impl.Create;
begin
     inherited Create;
     rMenu :=TCJ_Menu_Impl.Create;
     rTrayIcon :=TCJ_TrayIcon_Impl.Create;
     rSignals :=TCJ_Signals_Impl.Create;
end;

destructor TCJ_Interface_Impl.Destroy;
begin
     rMenu.Free;
     rMenu :=Nil;
     rTrayIcon.Free;
     rTrayIcon :=Nil;
end;


{ TCJ_Signals_Impl }

procedure TCJ_Signals_Impl.Connect(ClassMethod: TCJ_SignalMethod;
  MessageID: Integer);
begin
  rSignals.Connect(ClassMethod, MessageID);
end;

procedure TCJ_Signals_Impl.ConnectAsync(ClassMethod: TCJ_SignalMethod; MessageID: Integer; Priority :Integer=0);
begin
  rSignals.ConnectAsync(ClassMethod, MessageID, Priority);
end;

procedure TCJ_Signals_Impl.Disconnect(ClassMethod: TCJ_SignalMethod;
  MessageID: Integer);
begin
  rSignals.Disconnect(ClassMethod, MessageID);
end;

procedure TCJ_Signals_Impl.Disconnect(ClassPointer: TObject);
begin
  rSignals.Disconnect(ClassPointer);
end;

function TCJ_Signals_Impl.Signal(MessageID: Cardinal; WParam, LParam: Integer;
  var Handled: Boolean): Integer;
begin
  Result:=rSignals.Signal(MessageID, WParam, LParam, Handled);
end;

function TCJ_Signals_Impl.Signal(var aMessage: TMessage): Boolean;
begin
  Result:=rSignals.Signal(aMessage);
end;


constructor TCJ_Signals_Impl.Create;
begin
     inherited Create;
     rSignals :=TMGSignals.Create;
end;

destructor TCJ_Signals_Impl.Destroy;
begin
     rSignals.Free;
     rSignals :=Nil;
     inherited Destroy;
end;

end.
