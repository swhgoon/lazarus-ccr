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
unit project_iphone_options;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,FileUtil,LResources,Forms,StdCtrls,CheckLst,Buttons, Dialogs,
  Menus,IDEOptionsIntf,ProjectIntf,LazIDEIntf,iPhoneExtStr,
  iPhoneExtOptions, Controls, LazFilesUtils, XcodeUtils, newXibDialog, xibfile;

type

  { TiPhoneProjectOptionsEditor }

  TiPhoneProjectOptionsEditor = class(TAbstractIDEOptionsEditor)
    btnShowInFinder:TButton;
    btnAddXib:TButton;
    btnRemoveXib:TButton;
    Label5:TLabel;
    mnuDump:TMenuItem;
    mnuOpenIB:TMenuItem;
    nibFilesBox:TCheckListBox;
    chkisPhone: TCheckBox;
    cmbSDKs: TComboBox;
    edtResDir: TEdit;
    edtExclude: TEdit;
    edtAppID: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4:TLabel;
    lblAppID: TLabel;
    lblAppIDHint: TLabel;
    lblSDKVer: TLabel;
    nibsPopup:TPopupMenu;
    procedure btnAddXibClick(Sender:TObject);
    procedure btnRemoveXibClick(Sender:TObject);
    procedure btnShowInFinderClick(Sender:TObject);
    procedure Button1Click(Sender:TObject);
    procedure chkisPhoneChange(Sender:TObject);
    procedure cmbSDKsChange(Sender: TObject);
    procedure edtExcludeChange(Sender: TObject);
    procedure edtResDirChange(Sender:TObject);
    procedure edtResDirExit(Sender:TObject);
    procedure FrameClick(Sender: TObject);
    procedure mnuDumpClick(Sender:TObject);
    procedure mnuOpenIBClick(Sender:TObject);
    procedure nibFilesBoxClickCheck(Sender:TObject);
    procedure nibFilesBoxItemClick(Sender:TObject;Index:integer);
    procedure nibFilesBoxMouseDown(Sender:TObject;Button:TMouseButton;Shift:
      TShiftState;X,Y:Integer);
    procedure nibFilesBoxMouseUp(Sender:TObject;Button:TMouseButton;Shift:
      TShiftState;X,Y:Integer);
    procedure nibsPopupPopup(Sender:TObject); 
  private
    { private declarations }
    SelXibFile  : String;
    ResDirChanged : Boolean;

    fOnChanged  : TNotifyEvent;
    procedure DoChanged;

    procedure RefreshXIBList;

    procedure SetControlsEnabled(AEnabled: Boolean);
    procedure DumpClasses(const XibFileName: AnsiString; var PascalFileName: AnsiString);
  public
    { public declarations }
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    property OnChanged: TNotifyEvent read fOnChanged write fOnChanged;
  end;

implementation

{ TiPhoneProjectOptionsEditor }

procedure TiPhoneProjectOptionsEditor.cmbSDKsChange(Sender: TObject);
begin

end;

procedure TiPhoneProjectOptionsEditor.btnShowInFinderClick(Sender:TObject);
var
  path: AnsiString;
  tmp : AnsiString;
begin
  path:=Trim(edtResDir.Text);
  path:=ResolveProjectPath(path);

  ForceDirectoriesUTF8(path);

  while (path<>'') and (not DirectoryExistsUTF8(path)) do begin
    tmp:=path;
    path:=ExtractFileDir(path);
    if tmp=path then Break;
  end;
  if DirectoryExistsUTF8(path) then ExecCmdLineNoWait('open "'+path +'"');
end;

procedure TiPhoneProjectOptionsEditor.btnAddXibClick(Sender:TObject);
var
  FileName  : AnsiString;
  SrcXib    : AnsiString;

  ResDir    : AnsiString;
begin
  newXibForm:=TnewXibForm.Create(Self);
  ScanForXibTemplates(
    XibTemplateDir(  IncludeTrailingPathDelimiter(EnvOptions.PlatformsBaseDir)+iPhoneOSplatform),
    @newXibForm.AddTemplate);

  if (newXibForm.Execute(FileName, SrcXib)) and (SrcXib<>'')then begin
    ResDir:=edtResDir.Text;
    ResDir := ResolveProjectPath(ResDir);

    ForceDirectoriesUTF8(ResDir);
    if not CopyFile(SrcXib, ChangeFileExt(IncludeTrailingPathDelimiter(ResDir)+FileName,'.xib')) then
      ShowMessage('Failed to create Nib file');
    RefreshXIBList;
  end;
  newXibForm.Free;
  newXibForm:=nil;
end;

procedure TiPhoneProjectOptionsEditor.btnRemoveXibClick(Sender:TObject);
var
  XibName : AnsiString;
begin
  if nibFilesBox.ItemIndex<0 then Exit;

  XibName:=edtResDir.Text;
  XibName := ResolveProjectPath(XibName);

  XibName:=ChangeFileExt(IncludeTrailingPathDelimiter(XibName)+nibFilesBox.Items[nibFilesBox.ItemIndex],'.xib');
  if FileExistsUTF8(XibName) then DeleteFileUTF8(XibName);
  RefreshXIBList;
end;

procedure TiPhoneProjectOptionsEditor.Button1Click(Sender:TObject);
begin
end;

procedure TiPhoneProjectOptionsEditor.chkisPhoneChange(Sender:TObject);
begin
  SetControlsEnabled(chkisPhone.Checked);
end;

procedure TiPhoneProjectOptionsEditor.edtExcludeChange(Sender: TObject);
begin

end;

procedure TiPhoneProjectOptionsEditor.edtResDirChange(Sender:TObject);
begin
  ResDirChanged:=True;
end;

procedure TiPhoneProjectOptionsEditor.edtResDirExit(Sender:TObject);
var
  s   : AnsiString;
  chk : AnsiString;
  i   : Integer;
begin
  if not ResDirChanged then Exit;
  ResDirChanged:=False;


  // storing checked and selected items
  for i:=0 to nibFilesBox.Count-1 do
    if nibFilesBox.Checked[i] then
      chk:=nibFilesBox.Items[i];
  if nibFilesBox.ItemIndex>=0 then
    s:=nibFilesBox.Items[nibFilesBox.ItemIndex]
  else
    s := '';

  // refreshing the list
  RefreshXIBList;

  // restore selection and checked item
  if s<>'' then nibFilesBox.ItemIndex:=nibFilesBox.Items.IndexOf(s);
  i:=nibFilesBox.Items.IndexOf(chk);
  nibFilesBox.Checked[i]:=True;
end;

procedure TiPhoneProjectOptionsEditor.FrameClick(Sender: TObject);
begin

end;

procedure TiPhoneProjectOptionsEditor.mnuDumpClick(Sender:TObject);
var
  s   : AnsiString;
  pas : AnsiString;
  p   : TControl;
begin
  if SelXibFile ='' then Exit;
  s:=ChangeFileExt(IncludeTrailingPathDelimiter (edtResDir.Text) + SelXibFile,'.xib');
  s := ResolveProjectPath(s);
  DumpClasses(s, pas);

  p:=Parent;
  while Assigned(p) and (not (p is TForm)) do
    p:=p.Parent;
  if Assigned(p) then TForm(p).ModalResult:=mrOK;

  LazarusIDE.DoOpenEditorFile(pas, -1,-1, [ofOnlyIfExists]);
end;

procedure TiPhoneProjectOptionsEditor.mnuOpenIBClick(Sender:TObject);
var
  path  : AnsiString;
begin
  path:=ChangeFileExt(IncludeTrailingPathDelimiter(edtResDir.Text)+SelXibFile,'.xib');
  path := ResolveProjectPath(path);
  ExecCmdLineNoWait('open ' + path);
end;

procedure TiPhoneProjectOptionsEditor.nibFilesBoxClickCheck(Sender:TObject);
begin
end;

procedure TiPhoneProjectOptionsEditor.nibFilesBoxItemClick(Sender:TObject;Index: integer);
var
  i, j : Integer;
begin
  j:=-1;
  for i:=0 to nibFilesBox.Count-1 do
    if nibFilesBox.Checked[i] and (Index<>i) then begin
      j:=i;
    end;
  if j>=0 then nibFilesBox.Checked[j]:=false;
end;

procedure TiPhoneProjectOptionsEditor.nibFilesBoxMouseDown(Sender:TObject;Button
  :TMouseButton;Shift:TShiftState;X,Y:Integer);
var
  i: Integer;
begin
  i:=nibFilesBox.ItemAtPos(Point(X,Y), True);
  if i>=0 then begin
    nibFilesBox.ItemIndex:=i;
    SelXibFile:=nibFilesBox.Items[i];
  end else
    SelXibFile:='';
end;

procedure TiPhoneProjectOptionsEditor.nibFilesBoxMouseUp(Sender:TObject;Button:
  TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
end;

procedure TiPhoneProjectOptionsEditor.nibsPopupPopup(Sender:TObject);
begin
  mnuOpenIB.Enabled:=SelXibFile<>'';
  if mnuOpenIB.Enabled then
    mnuOpenIB.Caption:=Format(strOpenXibAtIB, [SelXibFile])
  else
    mnuOpenIB.Caption:=strOpenAtIB;
  mnuDump.Enabled:=SelXibFile<>''
end;

procedure TiPhoneProjectOptionsEditor.DoChanged;
begin
  if Assigned(fOnChanged) then fOnChanged(Self);
end;

procedure TiPhoneProjectOptionsEditor.RefreshXIBList;
var
  path  : AnsiString;
  st    : TStringList;
  i     : Integer;
  AProjectFile: TLazProjectFile;

begin
  path := ResolveProjectPath(trim(edtResDir.Text));
  st:=TStringList.Create;
  try
    for i := 0 to LazarusIDE.ActiveProject.FileCount-1 do
      begin
      AProjectFile := LazarusIDE.ActiveProject.Files[i];
      if (AProjectFile.IsPartOfProject) and FileExistsUTF8(ChangeFileExt(AProjectFile.filename,'.xib')) then
        st.add(AProjectFile.filename);
      end;

    EnumFilesAtDir(path, '*.xib', st);
    nibFilesBox.Clear;
    for i:=0 to st.Count-1 do
      nibFilesBox.Items.Add( ChangeFileExt( ExtractFileName(st[i]), ''));
  finally
    st.Free;
  end;
end;

procedure TiPhoneProjectOptionsEditor.SetControlsEnabled(AEnabled:Boolean);
var
  i : Integer;
begin
  for i:=0 to ControlCount-1 do
    if not (Controls[i] is TLabel) and (Controls[i]<>chkisPhone) then
      Controls[i].Enabled:=AEnabled;
end;

function MethodName(const msg: AnsiString): String;
var
  i : Integer;
begin
  Result:=msg;
  for i:=0 to length(Result) do if Result[i]=':' then Result[i]:='_';
end;

function ActionParams(const ActionName: AnsiString): String;
var
  i : integer;
  c : Integer;
begin
  c:=0;
  for i:=1 to length(ActionName) do if ActionName[i]=':' then inc(c);
  case c of
    1 : Result:=('(sender: id)');
    2 : Result:=('(sender: id; keyEvent: SEL)');
  end;
end;

function SetMethodName(const propName: AnsiString): String;
begin
  if propName<>'' then
    Result:='set'+AnsiUpperCase(propName[1])+Copy(propName, 2, length(propName)-1)
  else
    Result:='';
end;

procedure XibClassToInterface(cls: TXibClassDescr; intf: TStrings);
var
  i : Integer;
begin
  if not Assigned(cls) or not Assigned(intf) then Exit;

  intf.Add('  { ' +cls.Name + ' }');
  intf.Add('');
  intf.Add('  ' +cls.Name + '=objcclass(NSObject)');
  if length(cls.Outlets)>0 then begin
    intf.Add('  private');
    for i:=0 to length(cls.Outlets) - 1 do
      intf.Add('    f'+cls.Outlets[i].Key+' : '+ cls.Outlets[i].Value+';');
  end;

  intf.Add('  public');
  for i:=0 to length(cls.Actions) - 1 do
    with cls.Actions[i] do
      intf.Add(Format('    function %s%s: %s; message ''%s'';', [MethodName(Key), ActionParams(Key), Value, Key]));
  for i:=0 to length(cls.Outlets) - 1 do
    with cls.Outlets[i] do begin
      intf.Add(Format('    function %s: %s; message ''%s'';', [Key, Value, Key]));
      intf.Add(Format('    procedure %s(a%s: %s); message ''%s'';',
        [SetMethodName(Key), Key, Value, SetMethodName(Key)+':']));
    end;
  if length(cls.Outlets) > 0 then
    intf.Add('    procedure dealloc; override;');
  intf.Add('  end;');
  intf.Add('');
end;

procedure XibClassToImplementation(cls: TXibClassDescr; st: TStrings);
var
  i : Integer;
begin
  if not Assigned(cls) or not Assigned(st) then Exit;
  if (length(cls.Actions)=0) and (length(cls.Outlets)=0) then exit;
  st.Add('  { ' +cls.Name + ' }');
  st.Add('');
  for i:=0 to length(cls.Actions)-1 do
    with cls.Actions[i] do begin
      st.Add(Format('function %s.%s%s: %s;', [cls.Name, MethodName(Key), ActionParams(Key), Value]));
      st.Add('begin');
      st.Add('  // put action''s code here');
      st.Add('  Result:=nil;');
      st.Add('end;');
      st.Add('');
    end;

  for i:=0 to length(cls.Outlets) - 1 do
    with cls.Outlets[i] do begin
      st.Add(Format('function %s.%s: %s;', [cls.Name, Key, Value]));
      st.Add(       'begin');
      st.Add(Format('  Result:=f%s;', [Key]));
      st.Add(       'end;');
      st.Add(       '');
      st.Add(Format('procedure %s.%s(a%s: %s);', [cls.Name, SetMethodName(Key), Key, Value]));
      st.Add(       'begin');
      st.Add(Format('  f%s:=a%s;', [Key, Key]));
      st.Add(Format('  f%s.retain;', [Key]));
      st.Add(       'end;');
      st.Add(       '');
    end;

  if length(cls.Outlets)>0 then begin
    st.Add(Format('procedure %s.dealloc; ', [cls.Name]));
    st.Add(       'begin');
    for i:=0 to length(cls.Outlets) - 1 do
      st.Add(Format('  f%s.release;',[cls.Outlets[i].Key]));
    st.Add(       '  inherited;');
    st.Add(       'end;');
    st.Add('');
  end;

end;

procedure TiPhoneProjectOptionsEditor.DumpClasses(const XibFileName: AnsiString;
  var PascalFileName: AnsiString);
var
  unitNm    : AnsiString;
  fs        : TFileStream;
  xibcls    : TList;
  i         : Integer;

  intfPart  : TStringList;
  implPart  : TStringList;

  cls       : TXibClassDescr;

const
  le   : Ansistring = LineEnding;

  procedure wln(const s: AnsiString);
  begin
    if s <>'' then
      fs.Write(s[1], length(s));
    fs.Write(le[1], length(le));
  end;

begin
  if not FileExists(XibFileName) then Exit;

  intfPart:=TStringList.Create;
  implPart:=TStringList.Create;

  xibcls:=TList.Create;
  ListClassesDescr(XibFileName, xibcls);

  for i:=0 to xibcls.Count-1 do begin
    cls:=TXibClassDescr(xibcls[i]);
    XibClassToInterface(cls, intfPart);
    XibClassToImplementation(cls, implPart);
    cls.Free;
  end;
  xibcls.Free;

  unitNm:='dump'+ChangeFileExt(ExtractFileName(XibFileName),'');
  PascalFileName:=ResolveProjectPath(unitNm+'.pas');

  fs:=TFileStream.Create(PascalFileName, fmCreate);

  wln('unit ' + unitNm+';');
  wln('');
  wln('{$mode objfpc}{$h+}');
  wln('{$modeswitch objectivec1}');
  wln('');
  wln('interface');
  wln('');
  if intfPart.Count>0 then begin
    wln('type');
    for i:=0 to intfPart.Count-1 do wln(intfPart[i]);
  end;
  wln('');
  wln('implementation');
  wln('');
  if implPart.Count>0 then
    for i:=0 to implPart.Count-1 do wln(implPart[i]);
  wln('');
  wln('end.');

  intfPart.Free;
  implPart.Free;
  fs.Free;
end;

function TiPhoneProjectOptionsEditor.GetTitle: String;
begin
  Result:=strPrjOptTitle;
end;

procedure TiPhoneProjectOptionsEditor.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  chkisPhone.Caption := strPrjOptIsiPhone;
  lblSDKVer.Caption := strPrjOptSDKver;
  lblAppID.Caption := strPtrOptAppID;
  lblAppIDHint.Caption := strPtrOptAppIDHint;
end;

procedure TiPhoneProjectOptionsEditor.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i : Integer;
begin
  with TiPhoneProjectOptions(AOptions) do
  begin
    Load;
    chkisPhone.Checked:=isIPhoneApp;

    EnvOptions.GetSDKVersions(cmbSDKs.Items);
    i:=cmbSDKs.Items.IndexOf(SDK);
    if (i<0) and (cmbSDKs.Items.Count>0) then
      i:=cmbSDKs.Items.IndexOf(EnvOptions.DefaultSDK);
    cmbSDKs.ItemIndex:=i;
    edtAppID.Text:=AppID;
    edtResDir.Text:=ResourceDir;
    edtExclude.Text:=ExcludeMask;
  end;

  RefreshXIBList;
  if TiPhoneProjectOptions(AOptions).MainNib<>'' then begin
    i:=nibFilesBox.Items.IndexOf(TiPhoneProjectOptions(AOptions).MainNib);
    if i>=0 then nibFilesBox.Checked[i]:=True;
  end;
  SetControlsEnabled(chkisPhone.Checked); // is iPhone project
end;

procedure TiPhoneProjectOptionsEditor.WriteSettings(AOptions: TAbstractIDEOptions);
var
  amainnib : AnsiString;
  i : integer;
begin
  amainnib:='';
  for i:=0 to nibFilesBox.Count-1 do
    if nibFilesBox.Checked[i] then begin
      amainnib:=nibFilesBox.Items[i];
      Break;
    end;

  with TiPhoneProjectOptions(AOptions) do
  begin
    isIPhoneApp:=chkisPhone.Checked;
    SDK:=cmbSDKs.Caption;
    AppID:=edtAppID.Text;
    ResourceDir:=edtResDir.Text;
    ExcludeMask:=edtExclude.Text;
    MainNib:=amainnib;
    Save;
    DoChanged;
  end;
end;

class function TiPhoneProjectOptionsEditor.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TiPhoneProjectOptions;
end;

const
  iPhoneOptions = 1;

initialization
  {$I project_iphone_options.lrs}
  RegisterIDEOptionsEditor(iPhonePrjGroup, TiPhoneProjectOptionsEditor, iPhoneOptions);

finalization

end.

