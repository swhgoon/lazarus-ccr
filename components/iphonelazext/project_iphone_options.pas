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
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls,
  IDEOPtionsIntf, ProjectIntf,
  iPhoneExtStr, iPhoneExtOptions;

type

  { TiPhoneProjectOptionsEditor }

  TiPhoneProjectOptionsEditor = class(TAbstractIDEOptionsEditor)
    btnCheckSDK: TButton;
    chkisPhone: TCheckBox;
    cmbSDKs: TComboBox;
    edtAppID: TEdit;
    lblXibFiles: TLabel;
    lblAppID: TLabel;
    lblAppIDHint: TLabel;
    lblSDKVer: TLabel;
    procedure cmbSDKsChange(Sender: TObject);
    procedure FrameClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
  end;


procedure EnableiPhoneAppProject(project: TLazProject);

implementation

procedure EnableiPhoneAppProject(project: TLazProject);
begin

end;

{ TiPhoneProjectOptionsEditor }

procedure TiPhoneProjectOptionsEditor.cmbSDKsChange(Sender: TObject);
begin

end;

procedure TiPhoneProjectOptionsEditor.FrameClick(Sender: TObject);
begin

end;

function TiPhoneProjectOptionsEditor.GetTitle: String;
begin
  Result:=strPrjOptTitle;
end;

procedure TiPhoneProjectOptionsEditor.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  chkisPhone.Caption := strPrjOptIsiPhone;
  lblSDKVer.Caption := strPrjOptSDKver;
  btnCheckSDK.Caption := strPrjOptCheckSDK;
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

    i:=cmbSDKs.Items.IndexOf(SDK);
    if (i<0) and (cmbSDKs.Items.Count>0) then i:=0;
    cmbSDKs.ItemIndex:=i;

    edtAppID.Text:=AppID;
  end;

end;

procedure TiPhoneProjectOptionsEditor.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with TiPhoneProjectOptions(AOptions) do
  begin
    isIPhoneApp:=chkisPhone.Checked;
    SDK:=cmbSDKs.Caption;
    AppID:=edtAppID.Text;
    Save;
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

end.

