{ iOS NOB-designer for the Lazarus IDE

  Copyright (C) 2012 Joost van der Sluis joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit iOSNIBDesigner;

{$mode objfpc}{$H+}
{$typeinfo off}

interface

uses
  LCLProc, LCLType, Classes, SysUtils, FormEditingIntf, LCLIntf, Graphics, propedits, CodeToolManager,
  ProjectIntf,
  iOS_Views,
  dom,
  IDEIntf,
  IDEWindowIntf,
  LazIDEIntf,
  Dialogs,
  Controls,
  ComponentReg,
  typinfo,
  forms;

type

  { TNSObjectDesignerMediator }

  TNSObjectDesignerMediator = class(TDesignerMediator,IMyWidgetDesigner)
  private
    FMyForm: NSObject;
  protected
    // This method is available through IMyWidgetDesigner and is used to
    // clear the MyForm variable when the form is destroyed. This is necessary
    // because it could be that this mediator is destroyed somewhat later,
    // which could lead into problems.
    procedure ClearMyForm;
  public
    function UseRTTIForMethods(aComponent: TComponent): boolean; override;
    // needed by the lazarus form editor
    class function CreateMediator(TheOwner, aForm: TComponent): TDesignerMediator; override;
    class function FormClass: TComponentClass; override;
    procedure GetBounds(AComponent: TComponent; out CurBounds: TRect); override;
    procedure SetBounds(AComponent: TComponent; NewBounds: TRect); override;
    procedure GetClientArea(AComponent: TComponent; out CurClientArea: TRect; out ScrollOffset: TPoint); override;
    procedure Paint; override;
    function ComponentIsIcon(AComponent: TComponent): boolean; override;
    function ParentAcceptsChild(Parent: TComponent; Child: TComponentClass): boolean; override;
    procedure InitComponent(AComponent, NewParent: TComponent; NewBounds: TRect); override;
    function CreateComponent(ParentComp: TComponent;
                             TypeClass: TComponentClass;
                             const AUnitName: shortstring;
                             X,Y,W,H: Integer;
                             DisableAutoSize: boolean): TComponent;
  public
    // needed by UIView
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
    property MyForm: NSObject read FMyForm;
  end;

  { TUIResponderDesignerMediator }

  TUIResponderDesignerMediator = class(TNSObjectDesignerMediator)
  public
    class function FormClass: TComponentClass; override;
  end;


  { TiOSMethodPropertyEditor }

  TiOSMethodPropertyEditor = class(TMethodPropertyEditor)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

procedure Register;

implementation

{$R iosdesigner.res}

uses
  ObjInspStrConsts;

type

  { TiOSEventHandlers }

  TiOSEventHandlers = class
  private
    FUpdateVisibleHandlerSet: boolean;
    FUpdateVisibleHandlerDesignerSet: boolean;
  public
    constructor create;
    destructor destroy;
    procedure ChangeLookupRoot;
    procedure HandlerUpdateVisible(AComponent: TRegisteredComponent; var VoteVisible: integer);
    procedure HandlerUpdateVisibleDesigner(AComponent: TRegisteredComponent; var VoteVisible: integer);
  end;

var
  GiOSEventHandlers: TiOSEventHandlers = nil;

procedure Register;

  procedure SetFakeUnitname(AClass: TClass);
  var
    ATypInfo: PTypeInfo;
    ATypData: PTypeData;
  begin
    ATypInfo:=PTypeInfo(AClass.ClassInfo);
    ATypData:=GetTypeData(ATypInfo);

    ATypData^.UnitName[1]:='i';
    ATypData^.UnitName[2]:='P';
    ATypData^.UnitName[3]:='h';
    ATypData^.UnitName[4]:='o';
    ATypData^.UnitName[5]:='n';
    ATypData^.UnitName[6]:='e';
    ATypData^.UnitName[7]:='A';
    ATypData^.UnitName[8]:='l';
    ATypData^.UnitName[9]:='l';
  end;

begin
  FormEditingHook.RegisterDesignerMediator(TNSObjectDesignerMediator);
  FormEditingHook.RegisterDesignerMediator(TUIResponderDesignerMediator);
  RegisterComponents('iOS-Windows && Bars',[UIWindow, UISearchBar, UIView, UIxcodePlaceholder]);
  RegisterComponents('iOS-Data Views',[UITableView, UITextField]);
  RegisterComponents('iOS-Controls',[UIButton, UILabel, UIProgressView, UISegmentedControl]);
  RegisterComponents('iOS-Objects & Controllers',[UINavigationController, UIViewController]);

  GiOSEventHandlers := TiOSEventHandlers.Create;

  RegisterClass(UINavigationItem);
  RegisterClass(UIViewController);
  RegisterClass(UINavigationBar);

  RegisterPropertyEditor(FindPropInfo(UIButton, 'onTouchDown')^.PropType , tiOSFakeComponent,'onTouchDown',TiOSMethodPropertyEditor);

  // This is a hack to overwrite the unitname RTTI-information of these objects.
  // This is to make sure that the Codetools add the right unit-name to the
  // source when an object is added to the NIB-file.
  SetFakeUnitname(UIButton);
  SetFakeUnitname(UILabel);
  SetFakeUnitname(UITextField);
  SetFakeUnitname(UITableView);
  SetFakeUnitname(UISearchBar);
  SetFakeUnitname(UIWindow);
  SetFakeUnitname(UIView);
  SetFakeUnitname(UINavigationController);
  SetFakeUnitname(UIViewController);
  SetFakeUnitname(UIProgressView);
  SetFakeUnitname(UISegmentedControl);
  SetFakeUnitname(UIxcodePlaceholder);
end;

{ TiOSEventHandlers }

constructor TiOSEventHandlers.create;
begin
  GlobalDesignHook.AddHandlerChangeLookupRoot(@ChangeLookupRoot);
end;

destructor TiOSEventHandlers.destroy;
begin
  GlobalDesignHook.RemoveAllHandlersForObject(self);
end;

procedure TiOSEventHandlers.ChangeLookupRoot;
begin
  if GlobalDesignHook.LookupRoot is tiOSFakeComponent then
    begin
    if not FUpdateVisibleHandlerDesignerSet then
      begin
      IDEComponentPalette.AddHandlerUpdateVisible(@GiOSEventHandlers.HandlerUpdateVisibleDesigner);
      FUpdateVisibleHandlerDesignerSet := true;
      end;
    if FUpdateVisibleHandlerSet then
      begin
      IDEComponentPalette.RemoveHandlerUpdateVisible(@GiOSEventHandlers.HandlerUpdateVisible);
      FUpdateVisibleHandlerSet := false;
      end;
    end
  else if assigned(GlobalDesignHook.LookupRoot) then
    begin
    if FUpdateVisibleHandlerDesignerSet then
      begin
      IDEComponentPalette.RemoveHandlerUpdateVisible(@GiOSEventHandlers.HandlerUpdateVisibleDesigner);
      FUpdateVisibleHandlerDesignerSet := False;
      end;
    if not FUpdateVisibleHandlerSet then
      begin
      IDEComponentPalette.AddHandlerUpdateVisible(@GiOSEventHandlers.HandlerUpdateVisible);
      FUpdateVisibleHandlerSet := true;
      end;
    end
  else
    begin
    if FUpdateVisibleHandlerDesignerSet then
      begin
      IDEComponentPalette.RemoveHandlerUpdateVisible(@GiOSEventHandlers.HandlerUpdateVisibleDesigner);
      FUpdateVisibleHandlerDesignerSet := False;
      end;
    if FUpdateVisibleHandlerSet then
      begin
      IDEComponentPalette.RemoveHandlerUpdateVisible(@GiOSEventHandlers.HandlerUpdateVisible);
      FUpdateVisibleHandlerSet := false;
      end;
    end;
end;

procedure TiOSEventHandlers.HandlerUpdateVisible(
  AComponent: TRegisteredComponent; var VoteVisible: integer);
begin
  if assigned(AComponent) and assigned(AComponent.ComponentClass) and AComponent.ComponentClass.InheritsFrom(tiOSFakeComponent) then
    dec(VoteVisible);
end;

procedure TiOSEventHandlers.HandlerUpdateVisibleDesigner(
  AComponent: TRegisteredComponent; var VoteVisible: integer);
begin
  if not AComponent.ComponentClass.InheritsFrom(tiOSFakeComponent) then
    dec(VoteVisible);
end;

{ TiOSMethodPropertyEditor }

procedure TiOSMethodPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  proc(oisNone);
end;

procedure TiOSMethodPropertyEditor.SetValue(const NewValue: ansistring);
var
  CreateNewMethod: Boolean;
  CurValue: string;
  NewMethodExists, NewMethodIsCompatible, NewMethodIsPublished,
  NewIdentIsMethod: boolean;
  IsNil: Boolean;
  NewMethod: TMethod;
begin
  CurValue := GetValue;
  if CurValue = NewValue then exit;
  //DebugLn('### TMethodPropertyEditor.SetValue A OldValue="',CurValue,'" NewValue=',NewValue);
  IsNil := (NewValue='') or (NewValue=oisNone);

  if (not IsNil) and (not IsValidIdent(NewValue)) then
  begin
    MessageDlg(oisIncompatibleIdentifier,
      Format(oisIsNotAValidMethodName,['"',NewValue,'"']), mtError,
      [mbCancel, mbIgnore], 0);
    exit;
  end;

  NewMethodExists := (not IsNil); {and
    PropertyHook.CompatibleMethodExists(NewValue, GetInstProp,
                   NewMethodIsCompatible, NewMethodIsPublished, NewIdentIsMethod);}
  //DebugLn('### TMethodPropertyEditor.SetValue B NewMethodExists=',NewMethodExists,' NewMethodIsCompatible=',NewMethodIsCompatible,' ',NewMethodIsPublished,' ',NewIdentIsMethod);
{  if NewMethodExists then
  begin
    if not NewIdentIsMethod then
    begin
      if MessageDlg(oisIncompatibleIdentifier,
        Format(oisTheIdentifierIsNotAMethodPressCancelToUndoPressIgn,
               ['"', NewValue, '"', LineEnding, LineEnding]),
        mtWarning, [mbCancel, mbIgnore], 0)<>mrIgnore
      then
        exit;
    end;
    if not NewMethodIsPublished then
    begin
      if MessageDlg(oisIncompatibleMethod,
        Format(oisTheMethodIsNotPublishedPressCancelToUndoPressIgnor,
               ['"', NewValue, '"', LineEnding, LineEnding]),
        mtWarning, [mbCancel, mbIgnore], 0)<>mrIgnore
      then
        exit;
    end;
    if not NewMethodIsCompatible then
    begin
      if MessageDlg(oisIncompatibleMethod,
        Format(oisTheMethodIsIncompatibleToThisEventPressCancelToUnd,
               ['"', NewValue, '"', GetName, LineEnding, LineEnding]),
          mtWarning, [mbCancel, mbIgnore], 0)<>mrIgnore
      then
        exit;
    end;
  end;  }
  //DebugLn('### TMethodPropertyEditor.SetValue C');
  if IsNil then
  begin
    NewMethod.Data := nil;
    NewMethod.Code := nil;
    SetMethodValue(NewMethod);
  end
  else
  if IsValidIdent(CurValue) and
     not NewMethodExists and
     not PropertyHook.MethodFromAncestor(GetMethodValue) then
  begin
    // rename the method
    // Note:
    //   All other not selected properties that use this method, contain just
    //   the TMethod record. So, changing the name in the jitform will change
    //   all other event names in all other components automatically.
    PropertyHook.RenameMethod(CurValue, NewValue)
  end else
  begin
    //DebugLn('### TMethodPropertyEditor.SetValue E');
    CreateNewMethod := not NewMethodExists;
    SetMethodValue(
       PropertyHook.CreateMethod(NewValue, GetPropType,
                                 GetComponent(0), GetPropertyPath(0)));
    //DebugLn('### TMethodPropertyEditor.SetValue F NewValue=',GetValue);
    if CreateNewMethod then
    begin
      //DebugLn('### TMethodPropertyEditor.SetValue G');
      PropertyHook.ShowMethod(NewValue);
    end;
  end;
  //DebugLn('### TMethodPropertyEditor.SetValue END  NewValue=',GetValue);
end;

{ TNSObjectDesignerMediator }

constructor TNSObjectDesignerMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TNSObjectDesignerMediator.Destroy;
begin
  if FMyForm<>nil then FMyForm.Designer:=nil;
  FMyForm:=nil;
  inherited Destroy;
end;

function TNSObjectDesignerMediator.UseRTTIForMethods(aComponent: TComponent): boolean;
begin
  if aComponent is tiOSFakeComponent then
    result := true
  else
    Result:=inherited UseRTTIForMethods(aComponent);
end;

class function TNSObjectDesignerMediator.CreateMediator(TheOwner, aForm: TComponent): TDesignerMediator;
var
  Mediator: TNSObjectDesignerMediator;
begin
  Result:=inherited CreateMediator(TheOwner,aForm);
  Mediator:=Result as TNSObjectDesignerMediator;
  if assigned(result) and assigned(aForm) and (aForm is NSObject) then
    begin
    Mediator.FMyForm:=aForm as NSObject;
    Mediator.FMyForm.Designer:=Mediator as IMyWidgetDesigner;
    end;
end;

class function TNSObjectDesignerMediator.FormClass: TComponentClass;
begin
  Result:=NSObject;
end;

procedure TNSObjectDesignerMediator.GetBounds(AComponent: TComponent; out CurBounds: TRect);
var
  w: tiOSFakeComponent;
begin
  if AComponent is tiOSFakeComponent then begin
    w:=tiOSFakeComponent(AComponent);
    CurBounds:=Bounds(w.Left,w.Top,w.Width,w.Height);
  end else
    inherited GetBounds(AComponent,CurBounds);
end;

procedure TNSObjectDesignerMediator.InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
begin
  if (LCLForm=nil) or (not LCLForm.HandleAllocated) then exit;
  LCLIntf.InvalidateRect(LCLForm.Handle,@ARect,Erase);
end;

procedure TNSObjectDesignerMediator.ClearMyForm;
begin
  FMyForm := nil;
end;

procedure TNSObjectDesignerMediator.SetBounds(AComponent: TComponent; NewBounds: TRect);
begin
  if AComponent is tiOSFakeComponent then begin
    tiOSFakeComponent(AComponent).SetBounds(NewBounds.Left,NewBounds.Top,
      NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top);
  end else
    inherited SetBounds(AComponent,NewBounds);
end;

procedure TNSObjectDesignerMediator.GetClientArea(AComponent: TComponent; out CurClientArea: TRect; out ScrollOffset: TPoint);
var
  Widget: tiOSFakeComponent;
begin
  if AComponent is tiOSFakeComponent then begin
    Widget:=tiOSFakeComponent(AComponent);
    CurClientArea:=Rect(0,0,
                        Widget.Width,
                        Widget.Height);
    ScrollOffset:=Point(0,0);
  end else
    inherited GetClientArea(AComponent, CurClientArea, ScrollOffset);
end;

procedure TNSObjectDesignerMediator.Paint;

  procedure PaintWidget(AWidget: tiOSFakeComponent);
  var
    i: Integer;
    Child: tiOSFakeComponent;
  begin
    with LCLForm.Canvas do
      begin
      SaveHandleState;
      if AWidget is NSObject then
        begin
        Brush.Style:=bsClear;
        Brush.Color:=clLtGray;
        Pen.Color:=clMaroon;
        Rectangle(0,0,AWidget.Width,AWidget.Height);
        end
      else
        begin
        AWidget.Paint(LCLForm.Canvas);
        end;
      RestoreHandleState;
      // children
      if AWidget.ChildCount>0 then
        begin
        for i:=0 to AWidget.ChildCount-1 do begin
          SaveHandleState;
          Child:=AWidget.Children[i];
          // clip child area
          MoveWindowOrgEx(Handle,Child.Left,Child.Top);
          if IntersectClipRect(Handle,0,0,Child.Width,Child.Height)<>NullRegion then
            PaintWidget(Child);
          RestoreHandleState;
          end;
        end;
      end;
  end;

begin
  PaintWidget(MyForm);
  inherited Paint;
end;

function TNSObjectDesignerMediator.ComponentIsIcon(AComponent: TComponent): boolean;
begin
  Result:=not (AComponent is tiOSFakeComponent);
end;

function TNSObjectDesignerMediator.ParentAcceptsChild(Parent: TComponent; Child: TComponentClass): boolean;
begin
  Result:=(Parent is tiOSFakeComponent) and (Child.InheritsFrom(tiOSFakeComponent))
    and (tiOSFakeComponent(Parent).AcceptChildsAtDesignTime);
end;

procedure TNSObjectDesignerMediator.InitComponent(AComponent, NewParent: TComponent; NewBounds: TRect);
begin
  inherited InitComponent(AComponent, NewParent, NewBounds);
  if AComponent is tiOSFakeComponent then
    tiOSFakeComponent(AComponent).InitializeDefaults;
end;

function TNSObjectDesignerMediator.CreateComponent(ParentComp: TComponent;
  TypeClass: TComponentClass; const AUnitName: shortstring; X, Y, W,
  H: Integer; DisableAutoSize: boolean): TComponent;
begin
  result := FormEditingHook.CreateComponent(ParentComp,TypeClass,AUnitName,x,y,w,h,DisableAutoSize);
end;

{ TUIResponderDesignerMediator }

class function TUIResponderDesignerMediator.FormClass: TComponentClass;
begin
  Result:=UIResponder;
end;

end.

