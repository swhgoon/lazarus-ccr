{ Fake components to mimic iOS's views in the Lazarus designer

  Copyright (C) 2012 Joost van der Sluis/CNOC joost@cnoc.nl

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
// The name of this unit has to be of the same length as the name of the
// iPhoneAll unit.
unit iOS_Views;

{$mode objfpc}{$H+}
{$modeswitch ObjectiveC1}

interface

uses
  Classes, SysUtils, Math, types, DOM, XMLWrite, Graphics;

type
  IMyWidgetDesigner = interface(IUnknown)
    procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
    function CreateComponent(ParentComp: TComponent;
                             TypeClass: TComponentClass;
                             const AUnitName: shortstring;
                             X,Y,W,H: Integer;
                             DisableAutoSize: boolean): TComponent;
  end;

  TiOSFakeFontType = (ftNotSet, ftSystem, ftSystemBold, ftSystemItalic);

  { TiOSFakeFontDescription }

  TiOSFakeFontDescription = class(TPersistent)
  private
    FFontType: TiOSFakeFontType;
    FpointSize: double;
  public
    constructor Create;
  published
    property FontType: TiOSFakeFontType read FFontType write FFontType;
    property pointSize: double read FpointSize write FpointSize;
  end;

  { tiOSFakeComponent }

  tiOSWriteDomMethod = function (AnObjectDomElement: TDOMElement): TDOMElement of object;

  tiOSFakeComponent = class(TComponent)
  private
    FAcceptChildsAtDesignTime: boolean;
    FChilds: TFPList; // list of tiOSFakeComponent
    FHeight: integer;
    FLeft: integer;
    FObjectID: integer;
    FParent: tiOSFakeComponent;
    FTop: integer;
    FWidth: integer;
    // iOS
    FRef: integer;
    function ElementToString(AWriteDomMethod: tiOSWriteDomMethod): string;
  protected
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetParent(const AValue: tiOSFakeComponent);
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure SetHeight(const AValue: integer);
    procedure SetLeft(const AValue: integer);
    procedure SetTop(const AValue: integer);
    procedure SetWidth(const AValue: integer);
    function GetChilds(Index: integer): tiOSFakeComponent;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure InternalInvalidateRect(ARect: TRect; Erase: boolean); virtual;
    // iOS
    procedure AddConnectionRecord(AnObjectDomElement: TDOMElement; AConnectionType, ALabel, AEventType: string);
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); virtual;
    function WriteToObject(AnObjectDomElement: TDOMElement): TDOMElement; virtual;
    function WriteToConnectionRecords(AnObjectDomElement: TDOMElement): TDOMElement; virtual;
    function WriteToObjectRecord(AnObjectDomElement: TDOMElement): TDOMElement; virtual;
    class function GetIBClassName: string; virtual;
    function GetDesigner: IMyWidgetDesigner; virtual;
    function GetHeight: integer; virtual;
    function GetLeft: integer; virtual;
    function GetTop: integer; virtual;
    function GetWidth: integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitializeDefaultChildren; virtual;
    procedure paint(ACanvas: TCanvas); virtual;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); virtual;
    procedure InvalidateRect(ARect: TRect; Erase: boolean);
    procedure Invalidate;
    function ChildCount: integer;
    property Parent: tiOSFakeComponent read FParent write SetParent;
    property Children[Index: integer]: tiOSFakeComponent read GetChilds;
    property AcceptChildsAtDesignTime: boolean read FAcceptChildsAtDesignTime;
    // iOS
    function getAsXIBObject: string; virtual;
    function getConnectionRecords: string; virtual;
    function getObjectRecord: string; virtual;
    property Ref: integer read FRef write FRef;
    property ObjectID: integer read FObjectID;
    // Dom utils
    function AddElement(ADomNode: TDOMElement; AName: string): TDOMElement;
    function AddIBInt(ADomNode: TDOMElement; APropName: string; APropValue: integer; ADefaultValue: integer = MaxInt): TDOMElement;
    function AddIBDouble(ADomNode: TDOMElement; APropName: string; APropValue: double): TDOMElement;
    function AddIBFloat(ADomNode: TDOMElement; APropName: string; APropValue: double; ADefaultValue: double = MaxFloat): TDOMElement;
    function AddIBString(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
    function AddIBReference(ADomNode: TDOMElement; APropName: string; APropValue: tiOSFakeComponent; ForceValue: boolean = false): TDOMElement;
    function AddIBReference(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
    function AddIBBoolean(ADomNode: TDOMElement; APropName: string; APropValue: Boolean): TDOMElement;
    function AddIBBoolean(ADomNode: TDOMElement; APropName: string; APropValue: Boolean; ADefaultValue: boolean): TDOMElement;
    function AddIBBytes(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
    function AddIBObject(ADomNode: TDOMElement; APropName: string; AClass: string): TDOMElement;
    function AddIBObject(ADomDocument: TXMLDocument; APropName: string; AClass: string): TDOMElement;
    function AddIBColor(ADomNode: TDOMElement; APropName: string; AColor: TColor): TDOMElement;
    function AddIBFontDescription(ADomNode: TDOMElement; APropName: string; AFontDescription: TiOSFakeFontDescription): TDOMElement;
  published
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
  end;

  { UIView }

  UIView = class(tiOSFakeComponent)
  private
    FCaption: string;
    FNSNextResponder: UIView;
    FOpaque: boolean;
    FAlpha: double;
    FBackgroundColor: TColor;
    FHidden: boolean;
    function GetNSSuperview: UIView;
    procedure SetCaption(const AValue: string);
  protected
    procedure SetName(const NewName: TComponentName); override;
    // Dom
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure paint(ACanvas: TCanvas); override;
    class function GetIBClassName: string; override;
    property NSSuperview: UIView read GetNSSuperview;
    property NSNextResponder: UIView read FNSNextResponder write FNSNextResponder;
  published
    property Caption: string read FCaption write SetCaption;
    property Opaque: boolean read FOpaque write FOpaque;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property Alpha: double read FAlpha write FAlpha;
    property Hidden: boolean read FHidden write FHidden;
  end;
  TUIViewClass = class of UIView;

  { NSObject }

  NSObject = class(tiOSFakeComponent)
  private
    FDesigner: IMyWidgetDesigner;
    FFilesOwnerClass: string;
    FHiddenObjectOutletName: string;
    FIsHiddenObject: boolean;
    procedure GetXIBSaveParam(Sender: TObject; const ParamName: String; out AValue: String);
    function IsNIBRoot: boolean;
    function GetFilesOwnerID: string;
  protected
    function GetFlattenedProperties: string;
    procedure InternalInvalidateRect(ARect: TRect; Erase: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveAsXIB(const Filename: string);
    function getObjectRecord: string; override;
    function getAsXIBObject: string; override;
    function getConnectionRecords: string; override;
    function GetDesigner: IMyWidgetDesigner; override;
    property Designer: IMyWidgetDesigner read FDesigner write FDesigner;
  published
    property FilesOwnerClass: string read FFilesOwnerClass write FFilesOwnerClass stored IsNIBRoot;
    property IsHiddenObject: boolean read FIsHiddenObject write FIsHiddenObject stored IsNIBRoot;
    property HiddenObjectOutletName: string read FHiddenObjectOutletName write FHiddenObjectOutletName stored IsNIBRoot;
  end;

  UIResponder = class(NSObject);

  { UIWindow }

  UIWindow = class(UIView)
  private
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure paint(ACanvas: TCanvas); override;
    class function GetIBClassName: string; override;
  published
  end;

  { UIViewController }

  UIViewController = class(tiOSFakeComponent)
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
    function GetHeight: integer; override;
    function GetWidth: integer; override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;
  end;

  { UINavigationBar }

  UINavigationBar = class(UIView)
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
    procedure paint(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;
  end;

  { UINavigationItem }

  UINavigationItem = class(UIView)
  protected
    function GetWidth: integer; override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;
  end;

  { UINavigationController }

  UINavigationController = class(UIViewController)
  private
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
     procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitializeDefaultChildren; override;
    class function GetIBClassName: string; override;
  published
  end;

  { UIButton }
  id = ^objc_object;
  TcocoaEvent = procedure(sender: id) of object;

  UIButton = class(UIView)
  private
    FNSNextKeyView: UIView;
    FonTouchDown: TCocoaEvent;
    FTextColor: TColor;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
    function WriteToConnectionRecords(AnObjectDomElement: TDOMElement): TDOMElement; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;

    property NSNextKeyView: UIView read FNSNextKeyView write FNSNextKeyView;
  published
    property TextColor: TColor read FTextColor write FTextColor;
    property onTouchDown: TCocoaEvent read FonTouchDown write FonTouchDown;
  end;

  { UILabel }
  TLineBreaks = (lbWordWrap,lbCharacterMode,lbClip,lbTruncateHead,lbTruncateTail,lbTruncateMiddle);
  TiOSFakeAlignment = (alLeft, alCenter, alRight);

  UILabel = class(UIView)
  private
    FEnabled: boolean;
    FFont: TiOSFakeFontDescription;
    FHighlighted: boolean;
    FLineBreaks: TLineBreaks;
    FLines: integer;
    FTextAlignment: TiOSFakeAlignment;
    FTextColor: TColor;
    procedure SetFont(AValue: TiOSFakeFontDescription);
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetIBClassName: string; override;
    procedure paint(ACanvas: TCanvas); override;
  published
    property Lines: integer read FLines write FLines;
    property TextAlignment: TiOSFakeAlignment read FTextAlignment write FTextAlignment;
    property TextColor: TColor read FTextColor write FTextColor;
    property Font: TiOSFakeFontDescription read FFont write SetFont;
    property Enabled: boolean read FEnabled write FEnabled;
    property Highlighted: boolean read FHighlighted write FHighlighted;
    property LineBreaks: TLineBreaks read FLineBreaks write FLineBreaks;
  end;

  { UITextField }

  UITextField = class(UIView)
  private
    FPlaceholder: string;
    FText: string;
    FTextAlignment: TiOSFakeAlignment;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetIBClassName: string; override;
    //procedure paint(ACanvas: TCanvas); override;
  published
    property Text: string read FText write FText;
    property Placeholder: string read FPlaceholder write FPlaceholder;
    property Alignment: TiOSFakeAlignment read FTextAlignment write FTextAlignment;
  end;

  TiOSFakeSeparatorStyle = (ssNone,ssSingleLine,ssSingleLineEtched);

  { UITableView }

  UITableView = class(UIView)
  private
    FClipSubviews: boolean;
    FRowHeight: float;
    FSectionFooterHeight: float;
    FSectionHeaderHeight: float;
    FSeparatorColor: TColor;
    FSeparatorStyle: TiOSFakeSeparatorStyle;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetIBClassName: string; override;
  published
    property RowHeight: float read FRowHeight write FRowHeight;
    property SectionHeaderHeight: float read FSectionHeaderHeight write FSectionHeaderHeight;
    property SectionFooterHeight: float read FSectionFooterHeight write FSectionFooterHeight;
    property SeparatorStyle: TiOSFakeSeparatorStyle read FSeparatorStyle write FSeparatorStyle;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor;
    property ClipSubviews: boolean read FClipSubviews write FClipSubviews;
  end;

  { UISearchBar }

  UISearchBar = class(UIView)
  private
    FPlaceholder: string;
    FPrompt: string;
    FText: string;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetIBClassName: string; override;
  published
    property Text: string read FText write FText;
    property Placeholder: string read FPlaceholder write FPlaceholder;
    property Prompt: string read FPrompt write FPrompt;
  end;

  { UIProgressView }

  TiOSFakeProgressViewStyle = (pvUIProgressViewStyleDefault,pvUIProgressViewStyleBar);

  UIProgressView = class(UIView)
  private
    FProgress: float;
    FprogressTintColor: TColor;
    FProgressViewStyle: TiOSFakeProgressViewStyle;
    FtrackTintColor: TColor;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;
  published
    property progressViewStyle: TiOSFakeProgressViewStyle read FProgressViewStyle write FprogressViewStyle;
    property progress: float read FProgress write FProgress;
    property progressTintColor: TColor read FprogressTintColor write FprogressTintColor;
    property trackTintColor: TColor read FtrackTintColor write FtrackTintColor;
  end;


implementation

uses
  fpTemplate,
  variants, varutils,
  typinfo,
{$ifndef OutsideIDE}
  MacroIntf,
{$endif OutsideIDE}
  PropEdits,
  base64;

var
  GConnectionID: integer;

{ UIProgressView }

procedure UIProgressView.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBFloat(AnObjectDomElement,'IBUIProgress',FProgress);
  AddIBInt(AnObjectDomElement,'IBUIProgressViewStyle',ord(progressViewStyle),0);
  AddIBColor(AnObjectDomElement,'IBUIProgressTintColor',progressTintColor);
  AddIBColor(AnObjectDomElement,'IBUITrackTintColor',trackTintColor);
end;

constructor UIProgressView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime := false;
  FprogressTintColor := clDefault;
  FtrackTintColor := clDefault;
end;

class function UIProgressView.GetIBClassName: string;
begin
  Result:='IBUIProgressView';
end;

{ UINavigationItem }

function UINavigationItem.GetWidth: integer;
begin
  if assigned(parent) then
    result := parent.Width
  else
    result := 320;
end;

procedure UINavigationItem.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited SetBounds(Left, Top, Width, Height);
end;

constructor UINavigationItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeight:=44;
end;

class function UINavigationItem.GetIBClassName: string;
begin
  Result:='IBUINavigationItem';
end;

{ UIViewController }

procedure UIViewController.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  ASubElement: TDOMElement;
  i: Integer;
begin
  AddIBString(AnObjectDomElement,'targetRuntimeIdentifier','IBCocoaTouchFramework');

  for i := 0 to ChildCount-1 do
    begin
    ASubElement := Children[i].WriteToObject(AnObjectDomElement);
    if Children[i] is UIView then
      ASubElement.AttribStrings['key']:='IBUIView'
    else
      ASubElement.AttribStrings['key']:=Children[i].GetIBClassName;
    end;
end;

function UIViewController.GetHeight: integer;
begin
  if FHeight<>0 then
    result := FHeight
  else if assigned(parent) then
    result := min(Parent.Height,480)
  else
    result := 480;
end;

function UIViewController.GetWidth: integer;
begin
  if FWidth <> 0 then
    result := FWidth
  else if assigned(parent) then
    result := min(Parent.Width,320)
  else
    result := 320;
end;

procedure UIViewController.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  if (FWidth<>0) or (FHeight<>0) or ((width=320) and (height=480)) then
    inherited SetBounds(NewLeft, NewTop, Width, Height)
  else
    inherited SetBounds(Left, Top, Width, Height)
end;

constructor UIViewController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=true;
  FObjectID:=GConnectionID;
  inc(GConnectionID);
  FRef:=random(999999999);
end;

class function UIViewController.GetIBClassName: string;
begin
  Result:='IBUIViewController';
end;

{ UINavigationBar }

procedure UINavigationBar.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  AnElement: TDOMElement;
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AnElement := AnObjectDomElement.FindNode('NSFrame') as TDOMElement;
  if assigned(AnElement) then
    AnElement.NodeValue:='{{0, -44}, {0, 44}}';
end;

procedure UINavigationBar.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited SetBounds(0, 0, 0, 0);
end;

procedure UINavigationBar.paint(ACanvas: TCanvas);
begin
  // Do not paint;
end;

constructor UINavigationBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

class function UINavigationBar.GetIBClassName: string;
begin
  Result:='IBUINavigationBar';
end;

{ UINavigationController }

procedure UINavigationController.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  NavigationBarElement: TDOMElement;
  ViewControllesArray: TDOMElement;
  ViewController: TDOMElement;
  NavigationItemElement: TDOMElement;
  i: Integer;
begin
  AddIBString(AnObjectDomElement,'targetRuntimeIdentifier','IBCocoaTouchFramework');

  ViewControllesArray := AddIBObject(AnObjectDomElement,'IBUIViewControllers','NSMutableArray');
  AddIBBoolean(ViewControllesArray,'EncodedWithXMLCoder',true);

  for i := 0 to ChildCount-1 do
    begin
    if Children[i] is UINavigationBar then
      begin
      NavigationBarElement := Children[i].WriteToObject(AnObjectDomElement);
      NavigationBarElement.AttribStrings['key']:=Children[i].GetIBClassName;
      end;
    if children[i] is UIViewController then
      begin
      ViewController := children[i].WriteToObject(ViewControllesArray);
      AddIBString(ViewController,'targetRuntimeIdentifier','IBCocoaTouchFramework');
      AddIBReference(ViewController,'IBUIParentViewController',self,true);
      end;
    end;

end;

procedure UINavigationController.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited SetBounds(NewLeft, NewTop, Width, Height);
end;

constructor UINavigationController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
  FWidth:=320;
  FHeight:=480;
end;

procedure UINavigationController.InitializeDefaultChildren;
var
  AChild: tiOSFakeComponent;
  AViewController: TComponent;
  Designer: IMyWidgetDesigner;
begin
  designer := GetDesigner;
  if assigned(designer) then
    begin
    Designer.CreateComponent(self,UINavigationBar,'iOS_Views',0,0,0,0,true);
    AViewController := Designer.CreateComponent(self,UIViewController,'iOS_Views',0,0,0,0,true);
    Designer.CreateComponent(AViewController,UINavigationItem,'iOS_Views',0,0,0,0,true);
    end
  else
    begin
    AChild := UINavigationBar.Create(self);
    AChild.Parent := self;

    AViewController := UIViewController.Create(self);
    tiOSFakeComponent(AViewController).Parent := self;

    AChild := UINavigationItem.Create(self);
    AChild.Parent := tiOSFakeComponent(AViewController);
    end
end;

class function UINavigationController.GetIBClassName: string;
begin
  Result:='IBUINavigationController';
end;

{ UIWindow }

procedure UIWindow.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBReference(AnObjectDomElement,'NSWindow',self);
  AddIBString(AnObjectDomElement,'targetRuntimeIdentifier','IBCocoaTouchFramework');
  AddIBBoolean(AnObjectDomElement,'IBUIResizesToFullScreen',True);
end;

procedure UIWindow.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited SetBounds(NewLeft, NewTop, Width, Height);
end;

constructor UIWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NSNextResponder:=self;
  FWidth:=320;
  FHeight:=480;
  BackgroundColor:=clWhite;
  // Value from template
  FObjectID:=2;
end;

procedure UIWindow.paint(ACanvas: TCanvas);
begin
  with ACanvas do
    begin
    Brush.Style:=bsSolid;
    Brush.Color:=BackgroundColor;
    // outer frame
    Pen.Color:=clRed;
    Rectangle(0,0,self.Width,self.Height);
    end;
end;

class function UIWindow.GetIBClassName: string;
begin
  Result:='IBUIWindow';
end;

{ NSObject }

procedure NSObject.GetXIBSaveParam(Sender: TObject; const ParamName: String; out AValue: String);
var
  i: Integer;
begin
  if ParamName='Objects' then
    begin
    AValue:=getAsXIBObject;
    for i := 0 to self.ChildCount-1 do
      begin
        avalue := AValue + self.Children[i].getAsXIBObject;
      end;
    end
  else if ParamName='flattenedProperties' then
    begin
    AValue := GetFlattenedProperties;
    end
  else if ParamName='ConnectionRecords' then
    begin
    avalue := getConnectionRecords;
    for i := 0 to self.ChildCount-1 do
      begin
        avalue := AValue + self.Children[i].getConnectionRecords;
      end;
    end
  else if ParamName='ObjectRecords' then
    begin
    AValue := getObjectRecord;
    for i := 0 to self.ChildCount-1 do
      begin
        avalue := AValue + self.Children[i].getObjectRecord;
      end;
    end;
end;

function NSObject.IsNIBRoot: boolean;
begin
  result := Parent=Nil;
end;

function NSObject.GetFilesOwnerID: string;
begin
  if IsHiddenObject then
    Result:='841351856'
  else
    Result:='664661524';
end;

function NSObject.GetFlattenedProperties: string;
var
  XMLDoc: TXMLDocument;
  AStream: TStringStream;
  FlattenedPropertiesElement: TDOMElement;
  SortedKeysElements: TDOMElement;
  ValuesElements: TDOMElement;
  AnElement: TDOMElement;
  i: integer;
begin
//  assert(self is AppDelegate);
  XMLDoc := TXMLDocument.Create;
  try
    FlattenedPropertiesElement := AddIBObject(XMLDoc,'flattenedProperties','NSMutableDictionary');
    AddIBBoolean(FlattenedPropertiesElement,'EncodedWithXMLCoder',True);

    SortedKeysElements := AddIBObject(FlattenedPropertiesElement,'dict.sortedKeys','NSArray');
    AddIBBoolean(SortedKeysElements,'EncodedWithXMLCoder',True);
    // Add props for File's Owner
    AddIBString(SortedKeysElements,'','-1.CustomClassName');
    AddIBString(SortedKeysElements,'','-1.IBPluginDependency');
    // Add props for First Responder
    AddIBString(SortedKeysElements,'','-2.CustomClassName');
    AddIBString(SortedKeysElements,'','-2.IBPluginDependency');
    AddIBString(SortedKeysElements,'','10.IBPluginDependency');
    AddIBString(SortedKeysElements,'','2.IBAttributePlaceholdersKey');
    AddIBString(SortedKeysElements,'','2.IBPluginDependency');
    if IsHiddenObject then
      begin
      AddIBString(SortedKeysElements,'','3.CustomClassName');
      AddIBString(SortedKeysElements,'','3.IBPluginDependency');
      end;
    for i := 0 to ChildCount -1 do
      AddIBString(SortedKeysElements,'',inttostr(Children[i].ObjectID)+'.IBPluginDependency');

    ValuesElements := AddIBObject(FlattenedPropertiesElement,'dict.values','NSArray');
    AddIBBoolean(ValuesElements,'EncodedWithXMLCoder',True);
    // Add props for File's Owner
    AddIBString(ValuesElements,'',FilesOwnerClass);
    AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');
    // Add props for First Responder
    AddIBString(ValuesElements,'','UIResponder');
    AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');
    AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');

    AnElement := AddIBObject(ValuesElements,'','NSMutableDictionary');
    AddIBBoolean(AnElement,'EncodedWithXMLCoder',True);
    AddIBReference(AnElement,'dict.sortedKeys','0');
    AddIBReference(AnElement,'dict.values','0');

    AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');
    if IsHiddenObject then
      begin
      AddIBString(ValuesElements,'',self.ClassName);
      AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');
      end;
    for i := 0 to ChildCount -1 do
      AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');

    AStream := TStringStream.Create('');
    try
      for i := 0 to XMLDoc.ChildNodes.Count-1 do
        WriteXML(XMLDoc.ChildNodes.Item[i],AStream);
      result := AStream.DataString;
    finally
      AStream.Free;
    end;
  finally
    XMLDoc.Free;
  end;
end;

procedure NSObject.InternalInvalidateRect(ARect: TRect; Erase: boolean);
begin
  if (Parent=nil) and (Designer<>nil) then
    Designer.InvalidateRect(Self,ARect,Erase);
end;

function NSObject.getObjectRecord: string;
begin
  if IsNIBRoot then
    begin
    // ObjectRecord's for the File's Owner and First Responder
    result :=
      '<object class="IBObjectRecord">' + LineEnding +
      '	<int key="objectID">-1</int>' +LineEnding +
      '	<reference key="object" ref="'+GetFilesOwnerID+'"/>' + LineEnding +
      '	<reference key="parent" ref="0"/>' + LineEnding +
      '	<string key="objectName">File''s Owner</string>' + LineEnding +
      '</object>' + LineEnding +
      '<object class="IBObjectRecord">' + LineEnding +
      '	<int key="objectID">-2</int>' + LineEnding +
      '	<reference key="object" ref="427554174"/>' + LineEnding +
      '	<reference key="parent" ref="0"/>' + LineEnding +
      '</object>'+ LineEnding;
    if IsHiddenObject then
      result := result +
      '<object class="IBObjectRecord">' + LineEnding +
      '	<int key="objectID">3</int>' + LineEnding +
      '	<reference key="object" ref="664661524"/>' + LineEnding +
      '	<reference key="parent" ref="0"/>' + LineEnding +
      '</object>' + LineEnding;
    end
  else
    result := inherited getObjectRecord;
end;

function NSObject.getAsXIBObject: string;
begin
  if IsNIBRoot then
    begin
    // Objects for the File's Owner and First Responder
    result :=
      // File's owner
      '<object class="IBProxyObject" id="'+GetFilesOwnerID+'">' + LineEnding +
      '	<string key="IBProxiedObjectIdentifier">IBFilesOwner</string>' + LineEnding +
      '	<string key="targetRuntimeIdentifier">IBCocoaTouchFramework</string>' + LineEnding +
      '</object>' + LineEnding +
      // First responder
      '<object class="IBProxyObject" id="427554174">' + LineEnding +
      '	<string key="IBProxiedObjectIdentifier">IBFirstResponder</string>' + LineEnding +
      '	<string key="targetRuntimeIdentifier">IBCocoaTouchFramework</string>' + LineEnding +
      '</object>' + LineEnding;
    if IsHiddenObject then
      result := result +
        '<object class="IBUICustomObject" id="664661524">' + LineEnding +
        '	<string key="targetRuntimeIdentifier">IBCocoaTouchFramework</string>' + LineEnding +
        '</object>' + LineEnding;

    end
  else
    result := inherited getObjectRecord;
end;

function NSObject.getConnectionRecords: string;
begin
  if IsNIBRoot then
    begin
    if IsHiddenObject then
      result := result +
      '<object class="IBConnectionRecord">' + LineEnding +
      '	<object class="IBCocoaTouchOutletConnection" key="connection">' + LineEnding +
      '		<string key="label">'+HiddenObjectOutletName+'</string>' + LineEnding +
      '		<reference key="source" ref="'+GetFilesOwnerID+'"/>' + LineEnding +
      '		<reference key="destination" ref="664661524"/>' + LineEnding +
      '	</object>' + LineEnding +
      '	<int key="connectionID">4</int>' + LineEnding +
      '</object>' + LineEnding;
    end
  else
    Result:=inherited getConnectionRecords;
end;

constructor NSObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsHiddenObject:=true;
  FHiddenObjectOutletName:='delegate';
  FWidth:=320;
  FHeight:=480;
end;

procedure NSObject.SaveAsXIB(const Filename: string);
var
  ATemplate: TFPTemplate;
  fs: TFileStream;
  s: string;
  dir: string;

begin
  ATemplate := TFPTemplate.Create;
  try
    ATemplate.StartDelimiter:='[{';
    ATemplate.EndDelimiter:='}]';

{$ifndef OutsideIDE}
    dir := '$PkgDir(iOSDesigner)';
    IDEMacros.SubstituteMacros(dir);
{$else}
    dir :=  '/Users/joost/svn/ccr-components/iosdesigner';
{$endif OutsideIDE}
    if dir <>'' then
      ATemplate.FileName:=dir+PathDelim+ 'MainWindow_iPhone.template'
    else
      raise exception.create('iOSDesigner package could not be found.');

    ATemplate.OnGetParam :=@GetXIBSaveParam;
    s := ATemplate.GetContent;
    fs := TFileStream.Create(Filename,fmCreate);
    try
      fs.Write(s[1],length(s));
    finally
      fs.free;
    end;
  finally
    ATemplate.Free;
  end;
end;

function NSObject.GetDesigner: IMyWidgetDesigner;
begin
  Result:=Designer;
end;

{ tiOSFakeComponent }

procedure tiOSFakeComponent.SetHeight(const AValue: integer);
begin
  SetBounds(Left,Top,Width,AValue);
end;

procedure tiOSFakeComponent.SetLeft(const AValue: integer);
begin
  SetBounds(AValue,Top,Width,Height);
end;

function tiOSFakeComponent.ElementToString(AWriteDomMethod: tiOSWriteDomMethod): string;
var
  XMLDoc: TXMLDocument;
  AStream: TStringStream;
  RootElement: TDOMElement;
  i: integer;
begin
  XMLDoc := TXMLDocument.Create;
  try
    RootElement := XMLDoc.CreateElement('root');
    XMLDoc.AppendChild(RootElement);
    AWriteDomMethod(RootElement);

    AStream := TStringStream.Create('');
    try
      for i := 0 to RootElement.ChildNodes.Count-1 do
        WriteXML(RootElement.ChildNodes.Item[i],AStream);
      result := AStream.DataString;
    finally
      AStream.Free;
    end;
  finally
    XMLDoc.Free;
  end;
end;

function tiOSFakeComponent.GetHeight: integer;
begin
  result := FHeight;
end;

function tiOSFakeComponent.GetLeft: integer;
begin
  result := FLeft;
end;

function tiOSFakeComponent.GetTop: integer;
begin
  result := FTop;
end;

function tiOSFakeComponent.GetWidth: integer;
begin
  result := FWidth;
end;

procedure tiOSFakeComponent.SetParentComponent(Value: TComponent);
begin
  if Value is tiOSFakeComponent then
    Parent:=UIView(Value);
end;

procedure tiOSFakeComponent.SetParent(const AValue: tiOSFakeComponent);
begin
  if FParent=AValue then exit;
  if FParent<>nil then begin
    Invalidate;
    FParent.FChilds.Remove(Self);
  end;
  FParent:=AValue;
  if FParent<>nil then begin
    FParent.FChilds.Add(Self);
  end;
  Invalidate;
end;

function tiOSFakeComponent.HasParent: Boolean;
begin
  Result:=Parent<>nil;
end;

function tiOSFakeComponent.GetParentComponent: TComponent;
begin
  Result:=Parent;
end;

procedure tiOSFakeComponent.SetTop(const AValue: integer);
begin
  SetBounds(Left,AValue,Width,Height);
end;

procedure tiOSFakeComponent.SetWidth(const AValue: integer);
begin
  SetBounds(Left,Top,AValue,Height);
end;

function tiOSFakeComponent.GetChilds(Index: integer): tiOSFakeComponent;
begin
  Result:=tiOSFakeComponent(FChilds[Index]);
end;

procedure tiOSFakeComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i:=0 to ChildCount-1 do
    if Children[i].Owner=Root then
      Proc(Children[i]);
end;

procedure tiOSFakeComponent.InternalInvalidateRect(ARect: TRect; Erase: boolean);
begin
  //
end;

procedure tiOSFakeComponent.AddConnectionRecord(AnObjectDomElement: TDOMElement; AConnectionType, ALabel, AEventType: string);
var
  IBConnectionRecordElement : TDOMElement;
  IBConnectionElement : TDOMElement;
begin
  if name='' then Exit;
  IBConnectionRecordElement := AddIBObject(AnObjectDomElement,'','IBConnectionRecord');

  IBConnectionElement := AddIBObject(IBConnectionRecordElement,'connection',AConnectionType);

  AddIBString(IBConnectionElement,'label',ALabel);
  if AEventType<>'' then
    begin
    AddIBReference(IBConnectionElement,'source',self,True);
    AddIBReference(IBConnectionElement,'destination','664661524');
    end
  else
    begin
    AddIBReference(IBConnectionElement,'source','664661524');
    AddIBReference(IBConnectionElement,'destination',self,True);
    end;
  AddIBInt(IBConnectionRecordElement,'connectionID',GConnectionID);
  inc(GConnectionID);
  if AEventType<>'' then
    AddIBInt(IBConnectionElement,'IBEventType',StrToInt64Def(AEventType,1));
end;

procedure tiOSFakeComponent.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  ChildsMutableArray: TDOMElement;
  i: Integer;
begin
  if ChildCount>0 then
    begin
    ChildsMutableArray := AddIBObject(AnObjectDomElement,'NSSubviews','NSMutableArray');
    AddIBBoolean(ChildsMutableArray,'EncodedWithXMLCoder',true);
    for i := 0 to ChildCount-1 do
      Children[i].WriteToObject(ChildsMutableArray);
    end;
end;

function tiOSFakeComponent.WriteToObject(AnObjectDomElement: TDOMElement) : TDOMElement;
var
  ClassDomElement: TDOMElement;
  AnAttribute: TDOMNode;

begin
  ClassDomElement := AddIBObject(AnObjectDomElement,'',GetIBClassName);
  ClassDomElement.AttribStrings['id']:=IntToStr(Ref);

  WriteToDomElement(ClassDomElement);
  result := ClassDomElement;
end;

function tiOSFakeComponent.WriteToConnectionRecords(
  AnObjectDomElement: TDOMElement): TDOMElement;
var
  i: Integer;
begin
  AddConnectionRecord(AnObjectDomElement,'IBCocoaTouchOutletConnection',Name,'');
  for i := 0 to ChildCount-1 do
    Children[i].WriteToConnectionRecords(AnObjectDomElement);
end;

function tiOSFakeComponent.WriteToObjectRecord(AnObjectDomElement: TDOMElement
  ): TDOMElement;
var
  ObjectRecordElement: TDOMElement;
  ChildArray: TDOMElement;
  i: Integer;
begin
  ObjectRecordElement := AddIBObject(AnObjectDomElement,'','IBObjectRecord');
  AddIBInt(ObjectRecordElement,'objectID',ObjectID);
  AddIBReference(ObjectRecordElement,'object',self,True);

  if ChildCount>0 then
    begin
    ChildArray := AddIBObject(ObjectRecordElement,'children','NSMutableArray');
    AddIBBoolean(ChildArray,'EncodedWithXMLCoder',True);
    for i := 0 to ChildCount-1 do
      AddIBReference(ChildArray,'',Children[i]);
    end;

  if assigned(Parent) then
    AddIBReference(ObjectRecordElement,'parent',parent)
  else
    AddIBReference(ObjectRecordElement,'parent','0');
  for i := 0 to ChildCount-1 do
    Children[i].WriteToObjectRecord(AnObjectDomElement);
  result := ObjectRecordElement;
end;

class function tiOSFakeComponent.GetIBClassName: string;
begin
  result := '';
end;

function tiOSFakeComponent.GetDesigner: IMyWidgetDesigner;
begin
  if assigned(parent) then
    result := parent.GetDesigner
  else
    result := nil;
end;

constructor tiOSFakeComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChilds:=TFPList.Create;
end;

destructor tiOSFakeComponent.Destroy;
begin
  Parent:=nil;
  while ChildCount>0 do Children[ChildCount-1].Free;
  FreeAndNil(FChilds);
  inherited Destroy;
end;

procedure tiOSFakeComponent.InitializeDefaultChildren;
begin
  // Do nothing
end;

procedure tiOSFakeComponent.paint(ACanvas: TCanvas);
begin
  with ACanvas do
    begin
    Brush.Style:=bsSolid;
    Brush.Color:=clLtGray;
    // outer frame
    Pen.Color:=clRed;
    Rectangle(0,0,self.Width,self.Height);
    end;
end;

procedure tiOSFakeComponent.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  if (Left=NewLeft) and (Top=NewTop) and (Width=NewWidth) and (Height=NewHeight) then
    exit;
  Invalidate;
  FLeft:=NewLeft;
  FTop:=NewTop;
  FWidth:=NewWidth;
  FHeight:=NewHeight;
  Invalidate;
end;

procedure tiOSFakeComponent.InvalidateRect(ARect: TRect; Erase: boolean);
begin
  //writeln('TMyWidget.InvalidateRect ',Name,' ',ARect.Left,',',ARect.Top);
  ARect.Left:=Max(0,ARect.Left);
  ARect.Top:=Max(0,ARect.Top);
  ARect.Right:=Min(Width,ARect.Right);
  ARect.Bottom:=Max(Height,ARect.Bottom);
  if Parent<>nil then begin
    Parent.InvalidateRect(ARect,Erase);
  end else begin
    InternalInvalidateRect(ARect,Erase);
  end;
end;

procedure tiOSFakeComponent.Invalidate;
begin
  InvalidateRect(Rect(0,0,Width,Height),false);
end;

function tiOSFakeComponent.ChildCount: integer;
begin
  Result:=FChilds.Count;
end;

function tiOSFakeComponent.getAsXIBObject: string;
begin
  result := ElementToString(@WriteToObject);
end;

function tiOSFakeComponent.getConnectionRecords: string;
begin
  result := ElementToString(@WriteToConnectionRecords);
  inherited;
end;

function tiOSFakeComponent.getObjectRecord: string;

begin
  result := ElementToString(@WriteToObjectRecord);
end;

function tiOSFakeComponent.AddElement(ADomNode: TDomElement; AName: string): TDOMElement;
begin
  result := ADomNode.OwnerDocument.CreateElement(AName);
  ADomNode.AppendChild(result);
end;

function tiOSFakeComponent.AddIBInt(ADomNode: TDOMElement; APropName: string;
  APropValue: integer; ADefaultValue: integer): TDOMElement;
begin
  if APropValue<>ADefaultValue then
    begin
    result := AddElement(ADomNode,'int');
    result.AttribStrings['key']:=APropName;
    result.TextContent:=inttostr(APropValue);
    end;
end;

function tiOSFakeComponent.AddIBDouble(ADomNode: TDOMElement; APropName: string; APropValue: double): TDOMElement;
begin
  result := AddElement(ADomNode,'double');
  result.AttribStrings['key']:=APropName;
  result.TextContent:=floattostr(APropValue);
end;

function tiOSFakeComponent.AddIBFloat(ADomNode: TDOMElement; APropName: string;
  APropValue: double; ADefaultValue: double): TDOMElement;
begin
  if APropValue<>ADefaultValue then
    begin
    result := AddElement(ADomNode,'double');
    result.AttribStrings['key']:=APropName;
    result.TextContent:=floattostr(APropValue);
    end;
end;

function tiOSFakeComponent.AddIBString(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
begin
  result := AddElement(ADomNode,'string');
  if APropName<>'' then
    result.AttribStrings['key']:=APropName;
  result.TextContent:=APropValue;
end;

function tiOSFakeComponent.AddIBReference(ADomNode: TDOMElement;APropName: string; APropValue: tiOSFakeComponent; ForceValue: boolean): TDOMElement;
begin
  if assigned(APropValue) then
    begin
    result := AddElement(ADomNode,'reference');
    if APropName<>'' then
      result.AttribStrings['key']:=APropName;
    if ForceValue or (APropValue<>self) then
      result.AttribStrings['ref']:=inttostr(APropValue.Ref);
    end;
end;

function tiOSFakeComponent.AddIBReference(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
begin
  result := AddElement(ADomNode,'reference');
  result.AttribStrings['key']:=APropName;
  result.AttribStrings['ref']:=APropValue;
end;

function tiOSFakeComponent.AddIBBoolean(ADomNode: TDOMElement; APropName: string; APropValue: Boolean): TDOMElement;
begin
  result := AddElement(ADomNode,'bool');
  if APropName<>'' then
    result.AttribStrings['key']:=APropName;
  if APropValue then
    result.TextContent:='YES'
  else
    result.TextContent:='NO';
end;

function tiOSFakeComponent.AddIBBoolean(ADomNode: TDOMElement; APropName: string; APropValue: Boolean; ADefaultValue: boolean): TDOMElement;
begin
  if APropValue<>ADefaultValue then
    AddIBBoolean(ADomNode,APropName,APropValue);
end;

function tiOSFakeComponent.AddIBBytes(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
begin
  if APropValue<>'' then
    begin
    APropValue:=EncodeStringBase64(APropValue);
    while APropValue[length(APropValue)]='=' do
      APropValue:=copy(APropValue,1,length(APropValue)-1);
    result := AddElement(ADomNode,'bytes');
    result.AttribStrings['key']:=APropName;
    result.TextContent:=APropValue;

    end;
end;

function tiOSFakeComponent.AddIBObject(ADomNode: TDOMElement; APropName: string; AClass: string): TDOMElement;
begin
  result := AddElement(ADomNode,'object');
  result.AttribStrings['class']:=AClass;
  if APropName<>'' then
    result.AttribStrings['key']:=APropName;
end;

function tiOSFakeComponent.AddIBObject(ADomDocument: TXMLDocument; APropName: string; AClass: string): TDOMElement;
begin
  result := ADomDocument.CreateElement('object');
  ADomDocument.AppendChild(result);
  result.AttribStrings['class']:=AClass;
  if APropName<>'' then
    result.AttribStrings['key']:=APropName;
end;

function tiOSFakeComponent.AddIBColor(ADomNode: TDOMElement; APropName: string; AColor: TColor): TDOMElement;
var
  l: longint;
  fr,fg,fb: single;
  b: byte;
  s: string;
  AnElement: TDOMElement;
begin
  if AColor=clDefault then
    Exit;

  l:=ColorToRGB(AColor);
  b := l and ($ff0000) shr 16;
  fb := b / $ff;
  b := l and ($00ff00) shr 8;
  fg := b / $ff;
  b := l and ($0000ff);
  fr := b / $ff;
  s := FloatToStr(fr)+' '+FloatToStr(fg)+' '+FloatToStr(fb);

  AnElement := AddIBObject(ADomNode,APropName,'NSColor');
  AddIBInt(AnElement,'NSColorSpace',1);
  AddIBBytes(AnElement,'NSRGB',s);
end;

function tiOSFakeComponent.AddIBFontDescription(ADomNode: TDOMElement; APropName: string; AFontDescription: TiOSFakeFontDescription): TDOMElement;
var
  l: longint;
  fr,fg,fb: single;
  b: byte;
  s: string;
  AnElement: TDOMElement;
begin
  if AFontDescription.FontType<>ftNotSet then
    begin
    AnElement := AddIBObject(ADomNode,APropName,'IBUIFontDescription');
    AddIBInt(AnElement,'type',ord(AFontDescription.FontType));
    AddIBDouble(AnElement,'pointSize',AFontDescription.pointSize);
    end;
end;



{ UISearchBar }

procedure UISearchBar.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBString(AnObjectDomElement,'IBText',Text);
  AddIBString(AnObjectDomElement,'IBPlaceholder',Placeholder);
  AddIBString(AnObjectDomElement,'IBPrompt',Prompt);
end;

constructor UISearchBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

destructor UISearchBar.Destroy;
begin
  inherited Destroy;
end;

class function UISearchBar.GetIBClassName: string;
begin
  Result:='IBUISearchBar';
end;

{ UITableView }

procedure UITableView.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBFloat(AnObjectDomElement,'IBUIRowHeight',RowHeight);
  AddIBFloat(AnObjectDomElement,'IBUISectionHeaderHeight',SectionHeaderHeight);
  AddIBFloat(AnObjectDomElement,'IBUISectionFooterHeight',SectionFooterHeight);
  AddIBInt(AnObjectDomElement,'IBUISeparatorStyle',ord(SeparatorStyle),0);
  AddIBColor(AnObjectDomElement,'IBUISeparatorColor',SeparatorColor);
  AddIBBoolean(AnObjectDomElement,'IBUIClipsSubviews',ClipSubviews);
end;

constructor UITableView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRowHeight:=44;
  FSectionFooterHeight:=22;
  FSectionHeaderHeight:=22;
  FSeparatorStyle:=ssSingleLine;
  FSeparatorColor:=clDefault;
  FClipSubviews:=true;
  FAcceptChildsAtDesignTime:=false;
  FBackgroundColor := clWhite;
end;

destructor UITableView.Destroy;
begin
  inherited Destroy;
end;

class function UITableView.GetIBClassName: string;
begin
  Result:='IBUITableView';
end;

{ UITextField }

procedure UITextField.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBString(AnObjectDomElement,'IBUIText',Text);
  AddIBString(AnObjectDomElement,'IBUIPlaceholder',Placeholder);
  AddIBInt(AnObjectDomElement,'IBUITextAlignment',ord(Alignment),0)
end;

constructor UITextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

destructor UITextField.Destroy;
begin
  inherited Destroy;
end;

class function UITextField.GetIBClassName: string;
begin
  Result:='IBUITextField';
end;

{ TiOSFakeFontDescription }

constructor TiOSFakeFontDescription.Create;
begin
  pointSize:=12;
end;

{ TMyLabelButton }

procedure UILabel.SetFont(AValue: TiOSFakeFontDescription);
begin
  if FFont=AValue then Exit;
  FFont.Assign(AValue);
end;

procedure UILabel.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  AnElement: TDOMElement;
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBString(AnObjectDomElement,'IBUIText',Caption);
  AddIBInt(AnObjectDomElement,'IBUINumberOfLines',Lines,1);
  AddIBInt(AnObjectDomElement,'IBUITextAlignment',ord(TextAlignment),0);
  AddIBColor(AnObjectDomElement,'IBUITextColor',TextColor);
  AddIBFontDescription(AnObjectDomElement,'IBUIFontDescription',Font);
  AddIBBoolean(AnObjectDomElement,'IBUIEnabled',Enabled,True);
  AddIBBoolean(AnObjectDomElement,'IBUIHighlighted',Highlighted,false);
  AddIBInt(AnObjectDomElement,'IBUILineBreakMode',ord(LineBreaks),4);
end;


constructor UILabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
  FLines := 1;
  FFont := TiOSFakeFontDescription.Create;
  FEnabled := true;
end;

destructor UILabel.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

class function UILabel.GetIBClassName: string;
begin
  result := 'IBUILabel';
end;

procedure UILabel.paint(ACanvas: TCanvas);
begin
  ACanvas.Font.Color:=TextColor;
  case Font.FontType of
    ftSystemBold: begin
                  ACanvas.font.Name:='Helvetica';
                  ACanvas.Font.Size:=round(Font.FpointSize);
                  ACanvas.Font.Bold:=true;
                  ACanvas.Font.Italic:=False;
                  end;
    ftSystem:     begin
                  ACanvas.font.Name:='Helvetica';
                  ACanvas.Font.Size:=round(Font.FpointSize);
                  ACanvas.Font.Bold:=false;
                  ACanvas.Font.Italic:=False;
                  end;
    ftSystemItalic: begin
                    ACanvas.font.Name:='Helvetica';
                    ACanvas.Font.Size:=round(Font.FpointSize);
                    ACanvas.Font.Italic:=true;
                    ACanvas.Font.Bold:=false;
                    end
  else
    begin
    ACanvas.font.Name:='Helvetica';
    ACanvas.Font.Size:=12;
    ACanvas.Font.Italic:=false;
    ACanvas.Font.Bold:=false;
    end;
  end;
  ACanvas.TextOut(5,2,Caption);
end;


{ UIView }

function UIView.GetNSSuperView: UIView;
begin
  if assigned(Parent) and (Parent is UIView) then
    result := uiview(parent).GetNSSuperView
  else
    result := self;
end;

procedure UIView.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
  Invalidate;
end;

procedure UIView.SetName(const NewName: TComponentName);
begin
  if Name=Caption then Caption:=NewName;
  inherited SetName(NewName);
end;

procedure UIView.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  b: integer;
begin
  AddIBString(AnObjectDomElement,'NSFrame','{{'+inttostr(Left)+', '+inttostr(top)+'}, {'+inttostr(width)+', '+inttostr(height)+'}}');
  AddIBReference(AnObjectDomElement,'NSSuperview',NSSuperview);
  AddIBReference(AnObjectDomElement,'NSNextResponder',NSNextResponder);
  AddIBBoolean(AnObjectDomElement,'IBUIOpaque',Opaque);
  b := 1316; //1325;
  if Hidden then
    b := b or (1 shl 31);
  AddIBInt(AnObjectDomElement,'NSvFlags',b);
  AddIBFloat(AnObjectDomElement,'IBUIAlpha',Alpha,1);
  AddIBColor(AnObjectDomElement,'IBUIBackgroundColor',BackgroundColor);
  inherited WriteToDomElement(AnObjectDomElement);
end;

constructor UIView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FObjectID:=GConnectionID;
  inc(GConnectionID);
  FRef:=random(999999999);
  FAcceptChildsAtDesignTime:=true;
  FAlpha:=1;
  FBackgroundColor:=clDefault;
end;

procedure UIView.paint(ACanvas: TCanvas);
begin
  with ACanvas do
    begin
    Brush.Style:=bsSolid;
    Brush.Color:=BackgroundColor;
    // outer frame
    Pen.Color:=clRed;
    Rectangle(0,0,self.Width,self.Height);
    // caption
    Font.Color:=clBlack;
    Font.Name:='Helvetica';
    Font.Size:=12;
    Font.Italic:=false;
    Font.Bold:=false;
    TextOut(5,2,Caption);
    end;
end;

class function UIView.GetIBClassName: string;
begin
  result := 'IBUIView';
end;

{ UIButton }

procedure UIButton.WriteToDomElement(AnObjectDomElement: TDOMElement);

begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBReference(AnObjectDomElement,'NSNextKeyView',NSNextKeyView);
  AddIBInt(AnObjectDomElement,'IBUIButtonType',1);
  AddIBString(AnObjectDomElement,'IBUINormalTitle',Caption);
  AddIBColor(AnObjectDomElement,'IBUINormalTitleColor',TextColor);
end;

function UIButton.WriteToConnectionRecords(AnObjectDomElement: TDOMElement): TDOMElement;
var
  AMethod: TMethod;
begin
  inherited WriteToConnectionRecords(AnObjectDomElement);
  //writeln('PROPVAL: ' + vartostr(GetPropValue(self,'onTouchDown')));
  AMethod := LazGetMethodProp(self,GetPropInfo(self,'onTouchDown'));

  writeln('PROPVAL: ' + hexStr(PtrInt( AMethod.Code),8) + ' - ' + hexStr(ptrint(AMethod.Data),8));
  if AMethod.Data<>nil then
    begin
    AddConnectionRecord(AnObjectDomElement,'IBCocoaTouchEventConnection',Name+'TouchDown:','1');
    writeln(TObject(AMethod.Data).MethodName(self));
    end
  else
    writeln('AA- Geen Touchdown ' + self.Name);
end;

constructor UIButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

class function UIButton.GetIBClassName: string;
begin
  result := 'IBUIButton';
end;

initialization
  GConnectionID:=50;
end.

