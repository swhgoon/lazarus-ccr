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
  Classes,SysUtils,FileUtil,LResources,Forms,StdCtrls,Masks,CheckLst,Buttons,
  Menus,IDEOptionsIntf,ProjectIntf,LazIDEIntf,iPhoneExtStr,
  iPhoneExtOptions, process, Controls;

type

  { TiPhoneProjectOptionsEditor }

  TiPhoneProjectOptionsEditor = class(TAbstractIDEOptionsEditor)
    Label5:TLabel;
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
    procedure cmbSDKsChange(Sender: TObject);
    procedure edtExcludeChange(Sender: TObject);
    procedure edtResDirChange(Sender:TObject);
    procedure edtResDirExit(Sender:TObject);
    procedure FrameClick(Sender: TObject);
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
  public
    { public declarations }
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    property OnChanged: TNotifyEvent read fOnChanged write fOnChanged;
  end;

procedure EnumFilesAtDir(const PathUtf8, AMask : AnsiString; Dst: TStrings);
procedure ExecCmdLineNoWait(const CmdLineUtf8: AnsiString);

implementation

procedure EnumFilesAtDir(const PathUtf8, AMask : AnsiString; Dst: TStrings);
var
  mask  : TMask;
  sr    : TSearchRec;
  path  : AnsiString;
begin
  if (AMask='') or (trim(AMask)='*') then mask:=nil else mask:=TMask.Create(AMask);
  try
    path:=IncludeTrailingPathDelimiter(PathUtf8);
    if FindFirstUTF8(path+AllFilesMask, faAnyFile, sr) = 0 then begin
      repeat
        if (sr.Name<>'.') and (sr.Name<>'..') then
          if not Assigned(mask) or mask.Matches(sr.Name) then
            Dst.Add(path+sr.Name);
      until FindNextUTF8(sr)<>0;
      FindCloseUTF8(sr);
    end;
  finally
    mask.Free;
  end;
end;

procedure ExecCmdLineNoWait(const CmdLineUtf8: AnsiString);
var
  proc  : TProcess;
begin
  proc:=TProcess.Create(nil);
  try
    proc.CommandLine:=CmdLineUtf8;
    //proc.WaitOnExit:=WaitExit;
    proc.Execute;
  finally
    proc.Free;
  end;
end;

{ TiPhoneProjectOptionsEditor }

procedure TiPhoneProjectOptionsEditor.cmbSDKsChange(Sender: TObject);
begin

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

procedure TiPhoneProjectOptionsEditor.mnuOpenIBClick(Sender:TObject);
var
  path  : AnsiString;
begin
  path:=ChangeFileExt(IncludeTrailingPathDelimiter(edtResDir.Text)+SelXibFile,'.xib');
  LazarusIDE.ActiveProject.LongenFilename(path);
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
begin
  path := edtResDir.Text;
  st:=TStringList.Create;
  try
    LazarusIDE.ActiveProject.LongenFilename(path);
    EnumFilesAtDir(path, '*.xib', st);
    nibFilesBox.Clear;
    for i:=0 to st.Count-1 do
      nibFilesBox.Items.Add( ChangeFileExt( ExtractFileName(st[i]), ''));
  finally
    st.Free;
  end;
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

