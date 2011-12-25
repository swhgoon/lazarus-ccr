unit extconvdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,FileUtil,Forms,Controls,Graphics,Dialogs,StdCtrls,ExtCtrls,
  converteridesettings, ctopasconvert,
  LazIDEIntf,
  ComCtrls;

type

  { TCtoPasConfig }

  TCtoPasConfig = class(TForm)
    Button1: TButton;
    btnSelect:TButton;
    btnEdit:TButton;
    Button2:TButton;
    chkUseExternal:TCheckBox;
    chkRecordsPacked:TCheckBox;
    chkFuncAreExt:TCheckBox;
    chkEnums:TCheckBox;
    cmbCallConv:TComboBox;
    PageControl1: TPageControl;
    MainPage: TTabSheet;
    ConvPage: TTabSheet;
    txtLibName:TEdit;
    edtDefines:TEdit;
    edtExtTool: TEdit;
    Label1:TLabel;
    Label2:TLabel;
    lblCallConv:TLabel;
    lblExtLibName:TLabel;
    lblDefines:TLabel;
    Memo1:TMemo;
    Notebook1:TPageControl;
    OpenDialog1: TOpenDialog;
    Panel1:TPanel;
    Panel2:TPanel;
    Splitter1:TSplitter;
    procedure btnEditClick(Sender:TObject);
    procedure btnSelectClick(Sender:TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender:TObject);
    procedure FormClose(Sender:TObject;var CloseAction:TCloseAction);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SettingsToUI;
    procedure UIToSettings;
  end;

var
  CtoPasConfig: TCtoPasConfig;

function ShowConfigDialog : TCtoPasConfig;

implementation

{$R *.lfm}

function ShowConfigDialog: TCtoPasConfig;
begin
  if not Assigned(CtoPasConfig) then begin
    CtoPasConfig := TCtoPasConfig.Create(nil);
    CtoPasConfig.SettingsToUI;
  end;
  CtoPasConfig.Show;
  CtoPasConfig.BringToFront;
  Result:=CtoPasConfig;
end;

{ TCtoPasConfig }

procedure TCtoPasConfig.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtExtTool.Text := OpenDialog1.FileName;
end;

procedure TCtoPasConfig.Button2Click(Sender:TObject);
var
  cfg : TConvertSettings;
begin
  if MessageDlg('Reset types',
    'Reset c-to-pascal types converting to defaults?',
    mtConfirmation, mbYesNo, 0)<>mrYes then Exit;
  cfg := TConvertSettings.Create;
  try
    Memo1.Lines.Assign(cfg.CtoPasTypes);
  finally
    cfg.Free;
  end;
end;

procedure TCtoPasConfig.btnSelectClick(Sender:TObject);
begin
  if OpenDialog1.Execute then
    edtDefines.Text:=OpenDialog1.FileName;
end;

procedure TCtoPasConfig.btnEditClick(Sender:TObject);
var
  fs : TFileStream;
begin
  if edtDefines.Text='' then Exit;
  if not FileExistsUTF8(edtDefines.Text) then begin
    ForceDirectoriesUTF8( ExtractFileDir(edtDefines.Text));
    try
      fs:=TFileStream.Create(edtDefines.Text, fmCreate);
      fs.Free;
    except
    end;
  end;
  LazarusIDE.DoOpenEditorFile(edtDefines.Text, 0, 0, [ofQuiet, ofRegularFile, ofDoNotLoadResource, ofDoLoadResource]);
end;

procedure TCtoPasConfig.FormClose(Sender:TObject;var CloseAction:TCloseAction);
begin
  // don't free the form on close.
  UIToSettings;
  CloseAction:=caHide;
end;

procedure TCtoPasConfig.FormResize(Sender: TObject);
begin
end;

procedure TCtoPasConfig.SettingsToUI;
begin
  chkRecordsPacked.Checked:=ConvSettings.RecordsArePacked;
  chkFuncAreExt.Checked:=ConvSettings.FuncsAreExternal;
  chkEnums.Checked:=ConvSettings.EnumsAsConst;
  txtLibName.Text:=ConvSettings.ExtLibName;
  cmbCallConv.Text:=ConvSettings.FuncConv;
  Memo1.Lines.Assign(ConvSettings.CtoPasTypes);
  edtDefines.Text:=DefineFile;
  edtExtTool.Text:=ExtTool;
  chkUseExternal.Checked:=UseExtTool;
end;

procedure TCtoPasConfig.UIToSettings;
begin
  ConvSettings.RecordsArePacked:=chkRecordsPacked.Checked;
  ConvSettings.FuncsAreExternal:=chkFuncAreExt.Checked;
  ConvSettings.EnumsAsConst:=chkEnums.Checked;
  ConvSettings.ExtLibName:=Trim(txtLibName.Text);
  if (cmbCallConv.ItemIndex=0) and (cmbCallConv.Text=cmbCallConv.Items[0]) then
    ConvSettings.FuncConv:=''
  else
    ConvSettings.FuncConv:=cmbCallConv.Text;
  ConvSettings.CtoPasTypes.Assign(Memo1.Lines);
  DefineFile:=edtDefines.Text;
  ExtTool:=edtExtTool.Text;
  UseExtTool:=chkUseExternal.Checked;
end;

end.

