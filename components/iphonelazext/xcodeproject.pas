{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the iPhone Laz Extension                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit XCodeProject;

{$mode objfpc}{$H+}
// todo: re-write, using RTTI
{$TYPEINFO ON}

interface

uses
  contnrs, Classes, SysUtils, TypInfo;

const
  UTF8Mark = '// !$*UTF8*$!';

  FileType_AppWrapper = 'wrapper.application';
  FileType_Bundle     = FileType_AppWrapper;
  FileType_PList      = 'text.plist.xml';
  // sourceTree = BUILT_PRODUCTS_DIR

type
  TXCodeObjectID = string[24];

  TStringValues = class(TStringList);

  { TXCodeObject }
  TXCodeObject = class(TObject)
  private
    fOwner  : TXCodeObject;
    fChild  : TFPList;
    fValues : TStringList;
    fID     : TXCodeObjectID;
  protected
    procedure AddObject(const AName: String; AObject: TXCodeObject);
    function GetChild(i: Integer): TXCodeObject;
    function GetCount: Integer;
    procedure Clear;
    function GetValue(const AName: String): String;
    procedure SetValue(const AName: String; const AValue: String);
  public
    class function isa: ShortString; virtual; abstract;
    constructor Create;
    destructor Destroy; override;

    property Child[i: integer]: TXCodeObject read GetChild;
    property ChildCount: Integer read GetCount;
    property Owner: TXCodeObject read fOwner;
    property ID: TXCodeObjectID read fID;

    property StrValue[const AName: String]: String read GetValue write SetValue;
    function GetChildObject(const AName: String): TXCodeObject;
    function GetObjectArray(const AName: String): TFPObjectList;
    function GetStringArray(const AName: String): TStringList;
    function GetStringValues(const AName: String): TStringValues;
  end;
  TXCodeObjectClass = class of TXCodeObject;

  { TPBXFileReference }

  TPBXFileReference = class(TXCodeObject)
  private
    function GetExplicitFileType: string;
    function GetPath: String;
    function GetSourceTree: String;
    procedure SetExplicitFileType(const AValue: string);
    procedure SetPath(const AValue: String);
    procedure SetSourceTree(const AValue: String);
  public
    class function isa: ShortString; override;
    property explicitFileType: string read GetExplicitFileType write SetExplicitFileType;
    property path : String read GetPath write SetPath;
    property sourceTree: String read GetSourceTree write SetSourceTree;
  end;
// 0A7E1DA610EAD478000D7855 /* iPhoneApp.app */ =

  {isa = PBXFileReference;
   explicitFileType = wrapper.application;
   includeInIndex = 0;
   path = iPhoneApp.app;
   sourceTree = BUILT_PRODUCTS_DIR; }

{isa = PBXFileReference;
  lastKnownFileType = text.plist.xml;
  path = "iPhoneApp-Info.plist"; sourceTree = "<group>"; }



  TXCodeBuildPhase = class(TXCodeObject);

  { TPBXShellScriptBuildPhase }

  TPBXShellScriptBuildPhase = class(TXCodeBuildPhase)
  private
    function GetBuildMask: Integer;
    function GetShellPath: string;
    function GetShellScript: String;
    procedure SetBuildMask(const AValue: Integer);
    procedure SetShellPath(const AValue: string);
    procedure SetShellScript(const AValue: String);
  public
    constructor Create;
    class function isa: ShortString; override;
    property BuildActionMask : Integer read GetBuildMask write SetBuildMask;
    property ShellPath   : string read GetShellPath write SetShellPath;
    property ShellScript : String read GetShellScript write SetShellScript;
  end;

  { TPBXBuildFile }

  TPBXBuildFile = class(TXCodeObject)
  public
    fileRef   : TPBXFileReference;
    constructor Create;
    destructor Destroy; override;
    class function isa: ShortString; override;
  end;

  { TXCBuildConfiguration }

  TXCBuildConfiguration = class(TXCodeObject)
  private
    function GetName: String;
    procedure SetName(const AValue: String);
    function GetBuildSettings: TStringValues;
  public
    class function isa: ShortString; override;
    constructor Create;
    destructor Destroy; override;
    property name: String read GetName write SetName;
    property Settings: TStringValues read GetBuildSettings;
  end;

  { TXCConfigurationList }

  TXCConfigurationList = class(TXCodeObject)
  private
    fConfigs  : TFPObjectList;
  protected
    function GetConfig(i: integer): TXCBuildConfiguration;
    function GetCount: Integer;

    function GetName: string;
    procedure SetName(const AValue: string);
    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);
  public
    class function isa: ShortString; override;
    constructor Create;
    destructor Destroy; override;
    function AddList: TXCBuildConfiguration;
    property Config[i: Integer] : TXCBuildConfiguration read GetConfig;
    property ConfigCount: Integer read GetCount;
    property defaultVisible: Boolean read GetVisible write SetVisible;
    property defaulConfigName: string read GetName write SetName;
  end;


  { TPBXNativeTarget }

  TPBXNativeTarget = class(TXCodeObject)
  private
    fConfigList : TXCConfigurationList;
    fRef        : TPBXFileReference;
    function GetName: String;
    function GetProductFile: TPBXFileReference;
    function GetProductName: String;
    function GetProductType: String;
    procedure SetName(const AValue: String);
    procedure SetProductName(const AValue: String);
    procedure SetProductType(const AValue: String);

    function GetConfigList: TXCConfigurationList;
  public
    constructor Create;
    class function isa: ShortString; override;
    function AddBuildConfig: TXCBuildConfiguration;
    function AddScriptPhase: TPBXShellScriptBuildPhase;
    property name: String read GetName write SetName;
    property productName: String read GetProductName write SetProductName;
    property productType: String read GetProductType write SetProductType;
    property productFile: TPBXFileReference read GetProductFile;
  end;

  { TPBXProject }

  TPBXProject = class(TXCodeObject)
  private
    fConfigList : TXCConfigurationList;
    function GetDirPath: string;
    function GetRoot: String;
    procedure SetDirPath(const AValue: string);
    procedure SetRoot(const AValue: String);
  protected
    function GetConfigList: TXCConfigurationList;
    function GetTargets: TFPObjectList;

    function GetVersion: String;
    procedure SetVersion(const AVersion: string);
  public
    constructor Create;
    class function isa: ShortString; override;
    function AddTarget: TPBXNativeTarget;
    function AddBuildConfig: TXCBuildConfiguration;
    property DirPath: string read GetDirPath write SetDirPath;
    property Root: String read GetRoot write SetRoot;
    property compatibilityVersion: String read GetVersion write SetVersion;
  end;

  { TPBXGroup }

  TPBXGroup = class(TXCodeObject)
  protected
    function GetPath: String;
    procedure SetPath(const APath: String);
    function GetSourceTree: String;
    procedure SetSourceTree(const ATree: String);
  public
    class function isa: ShortString; override;
    property Path: String read GetPath write SetPath;
    property sourceTree: String read GetSourceTree write SetSourceTree;
  end;

procedure WritePBXProjFile(RootObject: TXCodeObject; Dst: TStream);
procedure WritePBXProjFile(RootObject: TXCodeObject; const FileName: String);

implementation

const
  BoolVal : array[Boolean] of String = ('false', 'true');

function isQuotedStr(const s: string): Boolean;
begin
  Result:=(length(s)>=2) and (s[1] = '"') and (s[length(s)]='"');
end;

function QuotedStr(const s: string): String;
begin
  if isQuotedStr(s) then Result:=s
  else Result:='"'+s+'"';
end;

function UnquoteStr(const s: string): String;
begin
  if not isQuotedStr(s) then Result:=s
  else Result:=Copy(s, 2, length(s)-2);
end;


function GetBoolValue(xobj: TXCodeObject; const AName: String): boolean;
begin
  Result:=xobj.StrValue[AName]=BoolVal[True];
end;

procedure SetBoolValue(xobj: TXCodeObject; const AName: String; AValue: Boolean);
begin
  xobj.StrValue[AName]:=BoolVal[AValue];
end;

{ TXCConfigurationList }

function TXCConfigurationList.GetName: string;
begin
  Result:=StrValue['defaultConfigurationName'];
end;

procedure TXCConfigurationList.SetName(const AValue: string);
begin
  StrValue['defaultConfigurationName']:=AValue;
end;

function TXCConfigurationList.GetVisible: Boolean;
begin
  Result:=GetBoolValue(Self, 'defaultConfigurationIsVisible');
end;

procedure TXCConfigurationList.SetVisible(AVisible: Boolean);
begin
  SetBoolValue(Self, 'defaultConfigurationIsVisible', AVisible);
end;

function TXCConfigurationList.GetConfig(i: integer): TXCBuildConfiguration;
begin
  Result:=TXCBuildConfiguration(fConfigs[i]);
end;

function TXCConfigurationList.GetCount: Integer;
begin
  Result:=fConfigs.Count;
end;

class function TXCConfigurationList.isa: ShortString;
begin
  Result:='XCConfigurationList';
end;

constructor TXCConfigurationList.Create;
begin
  inherited Create;
  fConfigs:=GetObjectArray('buildConfigurations');
end;

destructor TXCConfigurationList.Destroy;
begin
  inherited Destroy;
end;

function TXCConfigurationList.AddList: TXCBuildConfiguration;
begin
  Result:=TXCBuildConfiguration.Create;
  AddObject('', Result);
  fConfigs.Add(Result);
end;

{ TXCBuildConfiguration }

constructor TXCBuildConfiguration.Create;
begin
  inherited Create;
  GetStringValues('buildSettings');
end;

destructor TXCBuildConfiguration.Destroy;
begin
  inherited Destroy;
end;

function TXCBuildConfiguration.GetName: String;
begin
  Result:=GetValue('name');
end;

procedure TXCBuildConfiguration.SetName(const AValue: String);
begin
  SetValue('name', AValue);
end;

function TXCBuildConfiguration.getBuildSettings: TStringValues;
begin
  Result:=GetStringValues('buildSettings');
end;

class function TXCBuildConfiguration.isa: ShortString;
begin
  Result:='XCBuildConfiguration';
end;

{ TPBXGroup }

function TPBXGroup.GetPath: String;
begin
  Result:=StrValue['Path'];
end;

procedure TPBXGroup.SetPath(const APath: String);
begin
  StrValue['Path']:=APath;
end;

function TPBXGroup.GetSourceTree: String;
begin
  Result:=StrValue['sourceTree'];
end;

procedure TPBXGroup.SetSourceTree(const ATree: String);
begin
  StrValue['sourceTree']:=ATree;
end;

class function TPBXGroup.isa: ShortString;
begin
  Result:='PBXGroup';
end;

{ TPBXFileReference }

class function TPBXFileReference.isa: ShortString;
begin
  Result:='PBXFileReference';
end;

function TPBXFileReference.GetExplicitFileType: string;
begin
  Result:=GetValue('explicitFileType');
end;

function TPBXFileReference.GetPath: String;
begin
  Result:=GetValue('path');
end;

function TPBXFileReference.GetSourceTree: String;
begin
  Result:=GetValue('sourceTree');
end;

procedure TPBXFileReference.SetExplicitFileType(const AValue: string);
begin
  SetValue('explicitFileType', AValue);
end;

procedure TPBXFileReference.SetPath(const AValue: String);
begin
  SetValue('path', AValue);
end;

procedure TPBXFileReference.SetSourceTree(const AValue: String);
begin
  SetValue('sourceTree', AValue);
end;

{ TPBXBuildFile }

constructor TPBXBuildFile.Create;
begin
  fileRef:=TPBXFileReference.Create;
end;

destructor TPBXBuildFile.Destroy;
begin
  inherited Destroy;
end;

class function TPBXBuildFile.isa: ShortString;
begin
  Result:='PBXBuildFile';
end;

{ TXCodeObject }

procedure TXCodeObject.AddObject(const AName: String; AObject: TXCodeObject);
begin
  AObject.fOwner:=Self;
  fChild.Add(AObject);
  if AName<>'' then
    fValues.AddObject(AName, AObject);
end;

function TXCodeObject.GetChild(i: Integer): TXCodeObject;
begin
  Result:=TXCodeObject(fChild[i]);
end;

function TXCodeObject.GetCount: Integer;
begin
  Result:=fChild.Count;
end;

procedure TXCodeObject.Clear;
var
  i : integer;
begin
  for i:=0 to fValues.Count-1 do begin
    if Assigned(fValues.Objects[i]) and not (fValues.Objects[i] is TXCodeObject) then
      fValues.Objects[i].Free;
  end;
  fValues.Clear;

  for i:=0 to fChild.Count-1 do
    TObject(fChild[i]).Free;
  fChild.Clear;
end;

function TXCodeObject.GetValue(const AName: String): String;
begin
  Result:=fValues.Values[AName];
end;

procedure TXCodeObject.SetValue(const AName, AValue: String);
begin
  fValues.Values[AName]:=Avalue;
end;

constructor TXCodeObject.Create;
var
  q : Qword;
begin
  inherited;
  fChild:=TFPList.Create;
  fValues:=TStringList.Create;
  Q:=PtrUInt(Self);
  fID:=hexStr(q, 24);
end;

destructor TXCodeObject.Destroy;
begin
  Clear;
  fChild.Free;
  fValues.Free;
  inherited Destroy;
end;

function TXCodeObject.GetChildObject(const AName: String): TXCodeObject;
var
  i : integer;
begin
  Result:=nil;
  i:=fValues.IndexOfName(AName);
  if i>=0 then
    if fValues.Objects[i] is TXCodeObject then
      Result:=TXCodeObject(fValues.Objects[i]);
end;

function TXCodeObject.GetObjectArray(const AName: String): TFPObjectList;
var
  i : integer;
begin
  i:=fValues.IndexOfName(AName);
  if i<0 then begin
    Result:=TFPObjectList.Create(false);
    fValues.AddObject(AName, Result);
  end else begin
    if not (fValues.Objects[i] is TFPObjectList) then
      Result:=nil
    else
      Result:= TFPObjectList(fValues.Objects[i]);
  end;
end;

function TXCodeObject.GetStringArray(const AName: String): TStringList;
var
  i : integer;
begin
  i:=fValues.IndexOfName(AName);
  if i<0 then begin
    Result:=TStringList.Create;
    fValues.AddObject(AName, Result);
  end else begin
    if not (fValues.Objects[i] is TStringList) then
      Result:=nil
    else
      Result:= TStringList(fValues.Objects[i]);
  end;
end;

function TXCodeObject.GetStringValues(const AName: String): TStringValues;
var
  i : integer;
begin
  i:=fValues.IndexOfName(AName);
  if i<0 then begin
    Result:=TStringValues.Create;
    fValues.Values[AName]:='?';
    i:=fValues.IndexOfName(AName);
    fValues.Objects[i]:=Result;
    //i:=fValues.AddObject(AName + ' = ', Result);
  end else begin
    if not (fValues.Objects[i] is TStringValues) then
      Result:=nil
    else
      Result:= TStringValues(fValues.Objects[i]);
  end;
end;

type

  { TPBXWriter }

  TPBXWriter = class(TObject)
  private
    fDst    : TStream;
    fPrefix : String;
    fIndent : String;
  protected
    procedure WriteItems(data:TObject;arg:pointer);
  public
    constructor Create(Dst: TStream);
    destructor Destroy; override;
    procedure Indent;
    procedure Unindent;
    procedure WriteLn(const utf8: AnsiString);
  end;

{ TPBXWriter }

procedure TPBXWriter.WriteItems(data: TObject; arg: pointer);
var
  list : TFPList;
  i    : Integer;
  j    : Integer;
  k    : Integer;
  xobj : TXCodeObject;
  vobj : TObject;
  sublist : TFPObjectList;
  st      : TStringList;
  prefix  : String;
begin
  list:=TFPList(data);
  if not Assigned(list) or (list.Count=0) then Exit;

  for i:=0 to list.Count-1 do begin
    xobj:=TXCodeObject(list[i]);

    Self.WriteLn( xobj.ID + ' = {');
    Self.Indent;

      Self.Writeln('isa = ' + xobj.isa+';');

      for j:=0 to xobj.fValues.Count - 1 do begin
        vobj:=xobj.fValues.Objects[j];
        if not Assigned(vobj) then begin
          prefix:=xobj.fValues.Names[j] + ' = ';
          Writeln(prefix + xobj.fValues.ValueFromIndex[j]+';')
        end else begin
          if vobj is TXCodeObject then begin
            prefix:=xobj.fValues[j] + ' = ';
            Writeln(prefix + TXCodeObject(vobj).ID+';')
          end else if vobj is TStringValues then begin
            st:=TStringValues(vobj);
            prefix:=xobj.fValues.Names[j]+' = ';
            Self.Writeln(prefix + ' {');
            Self.Indent;
              for k:=0 to st.Count-1 do
                Self.WritelN(st.Names[k] + ' = ' + st.ValueFromIndex[k] + ';' );
            Self.Unindent;
            Self.Writeln('};')
          end else if vobj is TStringList then begin
            prefix:=xobj.fValues[j] + ' = ';
            st:=TStringValues(vobj);
            Self.Writeln(prefix + ' (');
            Self.Indent;
              for k:=0 to st.Count-1 do
                Self.WritelN('      ' + st[k] + ',' );
            Self.Unindent;
            Self.Writeln(');')
          end else if vobj is TFPObjectList then begin
            prefix:=xobj.fValues[j] + ' = ';
            sublist:=TFPObjectList(vobj);
            Self.Writeln(prefix + '(');
            Self.Indent;
              for k:=0 to sublist.Count-1 do
                Self.WritelN(TXCodeObject(sublist[k]).ID+',');
            Self.Unindent;
            Self.Writeln(');')
          end;
        end;
      end;

    Self.Unindent;
    Self.WritelN('};');
  end;
end;

constructor TPBXWriter.Create(Dst: TStream);
begin
  inherited Create;
  fDst:=Dst;
  fIndent:=#9;
end;

destructor TPBXWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TPBXWriter.Indent;
begin
  fPrefix:=fPrefix+fIndent;
end;

procedure TPBXWriter.Unindent;
begin
  fPrefix:=Copy(fPrefix,1, length(fPrefix)-length(fIndent));
end;

procedure TPBXWriter.WriteLn(const utf8: AnsiString);
const
  LF : string = #10;
begin
  if utf8='' then Exit;
  if fPrefix<>'' then fDst.Write(fPrefix[1], length(fPrefix));
  fDst.Write(utf8[1], length(utf8));
  fDst.Write(LF[1], length(LF));
end;

procedure WritePBXProjFile(RootObject: TXCodeObject; Dst: TStream);
var
  lists : TFPHashObjectList; // list of TFPList
  items : TFPList;
  p     : TXCodeObject;
  stack : TFPList;
  i     : Integer;
  xobjcount : Integer;
  wr    : TPBXWriter;
begin
  if RootObject=nil then Exit;

  lists:=TFPHashObjectList.Create(True);

  stack:=TFPList.Create;
  xobjcount:=1;
  stack.Add(RootObject);
  while stack.Count > 0 do begin
    p:= TXCodeObject(stack[0]);
    items:=TFPList(lists.Find(p.isa));
    if not Assigned(items) then begin
      items:=TFPList.Create;
      lists.Add(p.isa, items);
    end;
    items.Add(p);
    inc(xobjcount, p.ChildCount);
    for i:=0 to p.ChildCount-1 do stack.Add(p.Child[i]);
    stack.Delete(0);
  end;
  stack.Free;

  wr:=TPBXWriter.Create(Dst);
  wr.WriteLn(UTF8Mark);
  wr.WriteLn('{');
  wr.Indent;
    wr.WriteLn('archiveVersion = 1;');
    wr.WriteLn('classes = {};');
    wr.WriteLn('objectVersion = 45;');
    wr.WriteLn('objects = { ');
    wr.Indent;
      lists.ForEachCall(@wr.WriteItems, nil);
    wr.Unindent;
    wr.WriteLn('};');
    wr.WriteLn('rootObject = '+RootObject.ID+';');
  wr.Unindent;
  wr.WriteLn('}');
  wr.Free;

  lists.Free;
end;

procedure WritePBXProjFile(RootObject: TXCodeObject; const FileName: String);
var
  fs   : TFileStream;
begin
  fs:=TFileStream.Create(FileName, fmCreate);
  WritePBXProjFile(RootOBject, fs);
  fs.Free;
end;

{ TXCProject }

function TPBXProject.GetDirPath: string;
begin
  Result:=UnquoteStr(StrValue['projectDirPath']);
end;

function TPBXProject.GetRoot: String;
begin
  Result:=UnquoteStr(StrValue['projectRoot']);
end;

procedure TPBXProject.SetDirPath(const AValue: string);
begin
  StrValue['projectDirPath']:=QuotedStr(AValue);
end;

procedure TPBXProject.SetRoot(const AValue: String);
begin
  StrValue['projectRoot']:=QuotedStr(AValue);
end;

function TPBXProject.GetConfigList: TXCConfigurationList;
begin
  if not Assigned(fConfigList) then begin
    fConfigList:=TXCConfigurationList.Create;
    AddObject('buildConfigurationList', fConfigList);
  end;
  Result:=fConfigList;
end;

function TPBXProject.GetTargets: TFPObjectList;
begin
  Result:=GetObjectArray('targets');
end;

function TPBXProject.GetVersion: String;
begin
  Result:=GetValue('compatibilityVersion');
end;

procedure TPBXProject.SetVersion(const AVersion: string);
begin
  SetValue('compatibilityVersion', AVersion);
end;

constructor TPBXProject.Create;
begin
  inherited Create;
  Root:='';
  DirPath:='';
end;

class function TPBXProject.isa: ShortString;
begin
  Result:='PBXProject';
end;

function TPBXProject.AddTarget: TPBXNativeTarget;
var
  list  : TFPObjectList;
begin
  // adding a target forces configuration list
  //if fConfigList=nil then GetConfigList;

  list:=GetObjectArray('targets');
  Result:=TPBXNativeTarget.Create;
  AddObject('', Result);
  list.Add(Result);
end;

function TPBXProject.AddBuildConfig: TXCBuildConfiguration;
begin
  Result:=GetConfigList.AddList;
end;

{ TPBXNativeTarget }

function TPBXNativeTarget.GetName: String;
begin
  Result:=GetValue('name');
end;

function TPBXNativeTarget.GetProductFile: TPBXFileReference;
begin
  if not Assigned(fRef) then begin
    fRef:=TPBXFileReference.Create;
    AddObject('productReference', fRef);
  end;
  Result:=fRef;
end;

procedure TPBXNativeTarget.SetName(const AValue: String);
begin
  SetValue('name', AValue);
end;

function TPBXNativeTarget.GetProductName: String;
begin
  Result:=GetValue('productName');
end;

procedure TPBXNativeTarget.SetProductName(const AValue: String);
begin
  SetValue('productName', AValue);
end;

function TPBXNativeTarget.GetProductType: String;
begin
  Result:=GetValue('productType');
end;

procedure TPBXNativeTarget.SetProductType(const AValue: String);
begin
  SetValue('productType', AValue)
end;

function TPBXNativeTarget.GetConfigList: TXCConfigurationList;
begin
  if not Assigned(fConfigList) then begin
    fConfigList:=TXCConfigurationList.Create;
    AddObject('buildConfigurationList', fConfigList);
  end;
  Result:=fConfigList;
end;

constructor TPBXNativeTarget.Create;
begin
  inherited Create;
  productType := '"com.apple.product-type.application"';
  GetObjectArray('buildPhases');
  GetObjectArray('buildRules');
  GetObjectArray('dependencies');
end;

class function TPBXNativeTarget.isa: ShortString;
begin
  Result:='PBXNativeTarget';
end;

function TPBXNativeTarget.AddBuildConfig: TXCBuildConfiguration;
begin
  Result:=GetConfigList.AddList;
end;

function TPBXNativeTarget.AddScriptPhase: TPBXShellScriptBuildPhase;
begin
  Result:=TPBXShellScriptBuildPhase.Create;
  AddObject('', Result);
  GetObjectArray('buildPhases').Add(Result);
end;

{ TPBXShellScriptBuildPhase }

function TPBXShellScriptBuildPhase.GetBuildMask: Integer;
begin
  Result:=StrToIntDef( GetValue('buildActionMask'), 0);
end;

procedure TPBXShellScriptBuildPhase.SetBuildMask(const AValue: Integer);
begin
  SetValue('buildActionMask', IntToStr(AValue));
end;

function TPBXShellScriptBuildPhase.GetShellPath: string;
begin
  Result:=GetValue('shellPath');
end;

procedure TPBXShellScriptBuildPhase.SetShellPath(const AValue: string);
begin
  SetValue('shellPath', AValue);
end;

function TPBXShellScriptBuildPhase.GetShellScript: String;
begin
  Result:=UnquoteStr(GetValue('shellScript'));
end;

procedure TPBXShellScriptBuildPhase.SetShellScript(const AValue: String);
begin
  SetValue('shellScript', QuotedStr(AValue));
end;

constructor TPBXShellScriptBuildPhase.Create;
begin
  inherited Create;
  ShellPath:='/bin/sh';
  GetObjectArray('files');
  GetObjectArray('inputPaths');
  GetObjectArray('outputPaths');
end;

class function TPBXShellScriptBuildPhase.isa: ShortString;
begin
  Result:='PBXShellScriptBuildPhase';
end;

end.



