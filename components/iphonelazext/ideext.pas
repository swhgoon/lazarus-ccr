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
unit ideext;

{$mode objfpc}{$H+}

interface

uses
  Unix, BaseUnix, process,
  Classes, SysUtils,
  Graphics, Controls, Forms, Dialogs, FileUtil,
  {Lazarus Interface}
  LazIDEIntf, MenuIntf, ProjectIntf, IDEOptionsIntf, IDEMsgIntf,

  project_iphone_options, xcodetemplate,

  iPhoneExtOptions, iPhoneExtStr, iPhoneBundle, lazfilesutils;

procedure Register;

implementation

type
  { TiPhoneExtension }

  TiPhoneExtension = class(TObject)
  protected
    procedure FillBunldeInfo(forSimulator: Boolean; var info: TiPhoneBundleInfo);
    procedure InstallAppToSim;
    function FixCustomOptions(const Options: String; isRealDevice: Boolean): String;

    function WriteIconTo(const FullName: String): Boolean;

    function ProjectBuilding(Sender: TObject): TModalResult;
    function ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
    procedure OnProjOptionsChanged(Sender: TObject);
  public
    isiPhoneMenu    :TIDEMenuCommand;
    constructor Create;
    procedure UpdateXCode(Sender: TObject);
    procedure SimRun(Sender: TObject);
    procedure isProjectClicked(Sender: TObject);
  end;

var
  Extension : TiPhoneExtension = nil;

function GetProjectPlistName(Project: TLazProject): String;
var
  ext : String;
begin
  if Project.LazCompilerOptions.TargetFilename<>'' then
    Result:=ExtractFileName(Project.LazCompilerOptions.TargetFilename)
  else begin
    Result:=ExtractFileName(Project.MainFile.Filename);
    ext:=ExtractFileExt(Result);
    Result:=Copy(Result, 1, length(Result)-length( ext ) );
  end;
  Result:=Result+'.plist';
end;

function GetProjectExeName(Project: TLazProject): String;
var
  ext : String;
begin
  if Project.LazCompilerOptions.TargetFilename<>'' then
    Result:=Project.LazCompilerOptions.TargetFilename
  else begin
    Result:=Project.MainFile.Filename;
    ext:=ExtractFileExt(Result);
    Result:=Copy(Result, 1, length(Result)-length( ext ) );
  end;
  Project.LongenFilename(Result);
end;

{ TiPhoneExtension }

procedure TiPhoneExtension.FillBunldeInfo(forSimulator: Boolean; var info: TiPhoneBundleInfo);
begin
  Info.AppID:=ProjOptions.AppID;
  Info.DisplayName:=LazarusIDE.ActiveProject.Title;
  Info.iPlatform:=EnvOptions.GetSDKName(ProjOptions.SDK, forSimulator);
  Info.SDKVersion:=ProjOptions.SDK;
  Info.MainNib:=UTF8Decode(ProjOptions.MainNib);
end;

procedure TiPhoneExtension.InstallAppToSim;
var
  bundlename  : string;
  bundlepath  : WideString;
  exepath     : WideString;
  nm          : String;
  dstpath     : String;
  Space       : WideString;
  RealSpace   : WideString;
  Info        : TiPhoneBundleInfo;

  xiblist : TStringList;
  i       : Integer;
begin
  Space:=ProjOptions.SpaceName; // LazarusIDE.ActiveProject.CustomData.;

  bundleName:=ExtractFileName(LazarusIDE.ActiveProject.ProjectInfoFile);
  bundleName:=Copy(bundleName, 1, length(bundleName)-length(ExtractFileExt(bundleName)));

  nm:=GetProjectExeName(LazarusIDE.ActiveProject);

  FillBunldeInfo(true, Info);

  CreateBundle(bundleName, Space, ExtractFileName(nm), Info, RealSpace, bundlepath, exepath);

  WriteIconTo( IncludeTrailingPathDelimiter(bundlepath)+'Icon.png');

  CopySymLinks(
    ResolveProjectPath(ProjOptions.ResourceDir),
    bundlepath,
    // don't copy .xib files, they're replaced by compiled nibs
    '*.xib; '+ ProjOptions.ExcludeMask
  );

  if nm<>'' then begin
    dstpath:=UTF8Encode(exepath);
    FpUnlink(dstpath);
    fpSymlink(PChar(nm), PChar(dstpath));
  end;

  xiblist := TStringList.Create;
  try
    EnumFilesAtDir(ResolveProjectPath(ProjOptions.ResourceDir), '*.xib', xiblist);
    for i:=0 to xiblist.Count-1 do begin
      dstpath:=IncludeTrailingPathDelimiter(bundlepath)+ChangeFileExt(ExtractFileName(xiblist[i]), '.nib');
      ExecCmdLineNoWait(Format('ibtool --compile "%s" "%s"', [dstpath, xiblist[i]]));
      writeln('compile from: ', xiblist[i]);
      writeln('          to: ', dstpath);
    end;
  finally
    xiblist.free;
  end;

  //todo: compile .xib files to .nibs
end;

function FindParam(const Source, ParamKey: String; var idx: Integer; var Content: String): Boolean;
var
  i       : Integer;
  quoted  : Boolean;
begin
  Result:=false;
  idx:=0;
  for i := 1 to length(Source)-1 do
    if (Source[i]='-') and ((i=1) or (Source[i-1] in [#9,#32,#10,#13])) then
      if Copy(Source, i, 3) = ParamKey then begin
        idx:=i;
        Result:=true;
        Break;
      end;

  if not Result then Exit;

  i:=idx+3;
  quoted:=(i<=length(Source)) and (Source[i]='"');

  if not quoted then begin
    for i:=i to length(Source) do
      if (Source[i] in [#9,#32,#10,#13]) then begin
        Content:=Copy(Source, idx+3, i-idx);
        Exit;
      end;
  end else begin
    i:=i+1;
    for i:=i to length(Source) do
      if (Source[i] = '"') then begin
        Content:=Copy(Source, idx+3, i-idx+1);
        Exit;
      end;
  end;
  Content:=Copy(Source, idx+3, length(Source)-1);
end;

function TiPhoneExtension.FixCustomOptions(const Options: String; isRealDevice: Boolean): String;
var
  prm     : string;
  rawprm  : string;
  idx     : Integer;
  needfix : Boolean;
  sdkuse  : String;
  sdkver  : String;
  st : TStringList;
begin
  sdkver:=ProjOptions.SDK;
  if sdkver='' then begin
    st := TStringList.Create;
    try
      EnvOptions.GetSDKVersions(st);
      if st.Count=0 then
        IDEMessagesWindow.AddMsg(strWNoSDK, '', 0)
      else begin
        sdkver:=st[0];
        ProjOptions.SDK:=sdkver;
      end;
    finally
      st.Free;
    end;
  end;
  sdkuse:=EnvOptions.GetSDKFullPath(sdkver, not isRealDevice);

  Result:=Options;
  if FindParam(Result, '-XR', idx, rawprm) then begin
    if (rawprm='') or(rawprm[1]<>'"')
      then prm:=rawprm
      else prm:=rawprm;

    // there might be -XR option, and it might be the same as selected SDK
    needfix:=sdkuse<>prm;

    // there's -XR option, but it doesn't match the selection options
    if needfix then
      Delete(Result, idx, length(rawprm)+3);

  end else begin
    //there's no -XR string in custom options
    needfix:=true;
    idx:=1;
    sdkuse:=sdkuse+' ';
  end;

  if needfix then Insert('-XR'+sdkuse, Result, idx);
end;

constructor TiPhoneExtension.Create;
begin
  inherited Create;
  LazarusIDE.AddHandlerOnProjectOpened(@ProjectOpened);
  LazarusIDE.AddHandlerOnProjectBuilding(@ProjectBuilding);
  ProjOptions.OnAfterWrite:=@OnProjOptionsChanged;

  RegisterIDEMenuCommand(itmProjectWindowSection, 'mnuiPhoneSeparator', '-', nil, nil);

  isiPhoneMenu:=RegisterIDEMenuCommand(itmProjectWindowSection, 'mnuiPhoneProject', striPhoneProject, @isProjectClicked, nil);

  RegisterIDEMenuCommand(itmProjectWindowSection, 'mnuiPhoneToXCode', strStartAtXCode, @UpdateXCode, nil);
  RegisterIDEMenuCommand(itmProjectWindowSection, 'mnuiPhoneRunSim', strRunSimulator, @SimRun, nil);
end;

function TiPhoneExtension.ProjectBuilding(Sender: TObject): TModalResult;
begin
  Result:=mrOk;
  if not Assigned(LazarusIDE.ActiveProject) or not ProjOptions.isIPhoneApp then Exit;

  LazarusIDE.ActiveProject.LazCompilerOptions.CustomOptions :=
    FixCustomOptions( LazarusIDE.ActiveProject.LazCompilerOptions.CustomOptions, false );

  //const InfoFileName, BundleName, ExeName: WideString; const info: TiPhoneBundleInfo): Boolean;
  {MessageDlg('CustomOptions fixed = '+ LazarusIDE.ActiveProject.LazCompilerOptions.CustomOptions,
    mtInformation, [mbOK], 0);}

  InstallAppToSim;
  Result:=mrOk;
end;

function TiPhoneExtension.ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  ProjOptions.Reset;
  ProjOptions.Load;
  isiPhoneMenu.Checked:=ProjOptions.isIPhoneApp;
  Result:=mrOk;
end;

function TiPhoneExtension.WriteIconTo(const FullName: String): Boolean;
var
  icofile : string;
  ico     : TIcon;
  png     : TPortableNetworkGraphic;
  i, idx  : Integer;
const
  iPhoneIconSize=57;
begin
  Result:=false;
  //todo: find a better way of getting the file
  icofile:=ChangeFileExt(LazarusIDE.ActiveProject.MainFile.Filename, '.ico');
  LazarusIDE.ActiveProject.LongenFilename(icofile);
  if not FileExists(icofile) then begin
    // no icon. it should be deleted!
    DeleteFile(FullName);
    Exit;
  end;

  try
    ico:=TIcon.Create;
    png:=TPortableNetworkGraphic.Create;
    try
      png.Width:=iPhoneIconSize;
      png.Height:=iPhoneIconSize;
      ico.LoadFromFile(icofile);
      idx:=-1;
      for i:=0 to ico.Count- 1 do begin
        ico.Current:=i;
        if (ico.Width=iPhoneIconSize) and (ico.Height=iPhoneIconSize) then begin
          idx:=i;
          Break;
        end;
      end;
      if (idx<0) and (ico.Count>0) then idx:=0;

      ico.Current:=idx;

      if (ico.Width=iPhoneIconSize) and (ico.Height=iPhoneIconSize) then
        png.Assign(ico)
      else begin
        //resize to adjust the image
        png.Canvas.CopyRect( Rect(0,0, iPhoneIconSize, iPhoneIconSize), ico.Canvas, Rect(0,0, ico.Width, ico.Height));
      end;

      png.SaveToFile(FullName);
    finally
      ico.free;
      png.free;
    end;
    Result:=true;
  except
  end;
end;

procedure TiPhoneExtension.OnProjOptionsChanged(Sender: TObject);
begin
  isiPhoneMenu.Checked:=ProjOptions.isIPhoneApp;
end;

procedure TiPhoneExtension.UpdateXCode(Sender: TObject);
var
  templates : TStringList;
  build     : TStringList;
  dir       : string;
  projdir   : string;
  proj      : TStringList;
  projname  : string;

  ext       : string;
  tname     : string;
  plistname : string;

  Info  : TiPhoneBundleInfo;
  name  : string;

  opt   : string;
begin
  FillBunldeInfo(false, Info);
  // the create .plist would be used by XCode project
  // the simulator .plist in created with InstallAppToSim.
  // they differ with SDKs used

  build := TStringList.Create;

  tname:=ExtractFileName( LazarusIDE.ActiveProject.MainFile.Filename);
  tname:=ChangeFileExt(tname, '');
  plistname:=tname+'.plist';

  build.Values['INFOPLIST_FILE'] := '"'+plistname+'"';
  build.Values['PRODUCT_NAME'] := '"'+tname+'"';
  build.Values['SDKROOT'] := EnvOptions.GetSDKName(ProjOptions.SDK, false);
	build.Values['FPC_COMPILER_PATH'] := '"'+EnvOptions.CompilerPath+'"';
	build.Values['FPC_MAIN_FILE'] := '"'+LazarusIDE.ActiveProject.MainFile.Filename+'"';
  opt :=
    ' -XR'+EnvOptions.GetSDKFullPath(ProjOptions.SDK, false)+' ' +
    ' -FD'+IncludeTrailingPathDelimiter(EnvOptions.PlatformsBaseDir)+'iPhoneOS.platform/Developer/usr/bin ' +
    EnvOptions.CommonOpt;

  with LazarusIDE.ActiveProject.LazCompilerOptions do begin
    opt:=opt + ' ' +BreakPathsStringToOption(OtherUnitFiles, '-Fu', '\"');
    opt:=opt + ' ' +BreakPathsStringToOption(IncludePath, '-Fi', '\"');
    opt:=opt + ' ' +BreakPathsStringToOption(ObjectPath, '-Fo', '\"');
    opt:=opt + ' ' +BreakPathsStringToOption(Libraries, '-Fl', '\"');
  end;

	build.Values['FPC_CUSTOM_OPTIONS'] :='"'+opt+'"';

  dir:='xcode';
  LazarusIDE.ActiveProject.LongenFilename(dir);
  dir:=dir+'/';

  name:=ExtractFileName(GetProjectExeName(LazarusIDE.ActiveProject));
  ForceDirectories(dir);
  WriteDefInfoList( dir + GetProjectPlistName(LazarusIDE.ActiveProject),
    name, name, Info);


  projname:=ExtractFileName(LazarusIDE.ActiveProject.MainFile.Filename);
  ext:=ExtractFileExt(projname);
  projname:=Copy(projname, 1, length(projname)-length(ext));

  projdir:=dir+projname+'.xcodeproj';
  ForceDirectories(projdir);

  projname:=IncludeTrailingPathDelimiter(projdir)+'project.pbxproj';
  proj:=TStringList.Create;
  templates:=nil;
  try
    if not FileExists(projname) then begin
      templates:=TStringList.Create;

      if WriteIconTo( IncludeTrailingPathDelimiter(dir)+'Icon.png') then begin
        templates.Values['icon']:=XCodeProjectTemplateIcon;
        templates.Values['iconid']:=XCodeProjectTemplateIconID;
        templates.Values['iconfile']:=XCodeIconFile;
        templates.Values['iconfileref']:=XCodeIconFileRef;
      end else begin
        templates.Values['icon']:='';
        templates.Values['iconid']:='';
        templates.Values['iconfile']:='';
        templates.Values['iconfileref']:='';
      end;

      //todo:
      templates.Values['bundle']:=tname+'.app';
      templates.Values['plist']:=plistname;
      templates.Values['targetname']:=tname;
      templates.Values['productname']:=tname;

      proj.Text:=XCodeProjectTemplate;
    end else
      proj.LoadFromFile(projname);

    PrepareTemplateFile(proj, templates, build);
    proj.SaveToFile(projname);
  except
    on e: exception do
      ShowMessage(e.Message);
  end;

  proj.Free;
  templates.Free;
  build.Free;

  IDEMessagesWindow.AddMsg(strXcodeUpdated, '', 0);
end;

procedure TiPhoneExtension.SimRun(Sender: TObject);
var
  t     : TProcess;
  path  : String;
begin
  t :=TProcess.Create(nil);
  try
    //ProjectBuilding(nil);
    path:=IncludeTrailingPathDelimiter(EnvOptions.SimBundle)+'Contents/MacOS/iPhone Simulator';
    t.CommandLine:='"'+path+'"';
    t.CurrentDirectory:=UTF8Encode(GetSandBoxDir(ProjOptions.SpaceName));
    t.Execute;
  except
    on E: Exception do
      MessageDlg(E.Message, mtInformation, [mbOK], 0);
  end;
  t.Free;
end;

procedure TiPhoneExtension.isProjectClicked(Sender: TObject);
begin
  if not Assigned(Sender) or not Assigned(LazarusIDE.ActiveProject) then Exit;
  TIDEMenuCommand(Sender).Checked:=not TIDEMenuCommand(Sender).Checked;
  ProjOptions.isiPhoneApp:=TIDEMenuCommand(Sender).Checked;
  ProjOptions.Save;
end;

procedure Register;
begin
  // IDE integration is done in constructor
  Extension := TiPhoneExtension.Create;
  EnvOptions.Load;
  EnvOptions.RefreshVersions;
end;

initialization

finalization
  Extension.Free;

end.

