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
unit environment_iphone_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls,
  IDEOptionsIntf, ProjectIntf,
  iPhoneExtOptions;

type

  { TiPhoneSpecificOptions }

  TiPhoneSpecificOptions = class(TAbstractIDEOptionsEditor)
    edtCompilerPath: TEdit;
    edtRTLPath: TEdit;
    edtCompilerOptions: TEdit;
    edtPlatformsPath: TEdit;
    edtSimBundle: TEdit;
    edtSimApps: TEdit;
    lblRTLUtils: TLabel;
    lblSimAppPath: TLabel;
    lblCompilerPath: TLabel;
    lblXCodeProject: TLabel;
    lblCmpOptions: TLabel;
    Label5: TLabel;
    lblSimSettings: TLabel;
    lblSimBundle: TLabel;
    procedure lblCmpOptionsClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function GetTitle: String; override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
  end;

implementation

{ TiPhoneSpecificOptions }

procedure TiPhoneSpecificOptions.lblCmpOptionsClick(Sender: TObject);
begin

end;

function TiPhoneSpecificOptions.GetTitle: String;
begin
  Result:='Files';
end;

procedure TiPhoneSpecificOptions.Setup(ADialog: TAbstractOptionsEditorDialog);
begin

end;

procedure TiPhoneSpecificOptions.ReadSettings(AOptions: TAbstractIDEOptions);
var
  opt : TiPhoneEnvironmentOptions;
begin
  if not Assigned(AOptions) or not (AOptions is TiPhoneEnvironmentOptions) then Exit;
  opt:=TiPhoneEnvironmentOptions(AOptions);
  opt.Load;

  edtPlatformsPath.Text := opt.PlatformsBaseDir;
  edtCompilerPath.Text := opt.CompilerPath;
  edtRTLPath.Text := opt.BaseRTLPath;
  edtCompilerOptions.Text := opt.CommonOpt;
  edtSimBundle.Text := opt.SimBundle;
  edtSimApps.Text:= opt.SimAppsPath;
end;

procedure TiPhoneSpecificOptions.WriteSettings(AOptions: TAbstractIDEOptions);
var
  opt : TiPhoneEnvironmentOptions;
begin
  if not Assigned(AOPtions) or not (AOptions is TiPhoneEnvironmentOptions) then Exit;
  opt:=TiPhoneEnvironmentOptions(AOptions);

  opt.PlatformsBaseDir:=edtPlatformsPath.Text;
  opt.CompilerPath:=edtCompilerPath.Text;
  opt.BaseRTLPath:=edtRTLPath.Text;
  opt.CommonOpt:=edtCompilerOptions.Text;
  opt.SimBundle:=edtSimBundle.Text;
  opt.SimAppsPath:=edtSimApps.Text;

  opt.Save;
end;

class function TiPhoneSpecificOptions.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TiPhoneEnvironmentOptions;
end;

initialization
  {$I environment_iphone_options.lrs}
  RegisterIDEOptionsEditor(iPhoneEnvGroup, TiPhoneSpecificOptions, iPhoneEnvGroup+1);

end.

