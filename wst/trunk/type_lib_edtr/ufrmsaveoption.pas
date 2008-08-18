unit ufrmsaveoption;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, Buttons;

type

  { TfrmSaveOptions }

  TfrmSaveOptions = class (TForm )
    actOK : TAction;
    AL : TActionList;
    Button1 : TButton;
    Button2 : TButton;
    btnSelectDir : TButton;
    btnSelectAll: TButton;
    btnUnselectAll: TButton;
    edtDocAsComments : TCheckBox;
    edtInterface : TCheckBox;
    edtProxy : TCheckBox;
    edtImplementation : TCheckBox;
    edtBinder : TCheckBox;
    edtOutputDir : TEdit;
    edtWrappedParams : TCheckBox;
    GroupBox1 : TGroupBox;
    Label1 : TLabel;
    Panel1 : TPanel;
    Panel2 : TPanel;
    SD : TSelectDirectoryDialog;
    procedure actOKExecute (Sender : TObject );
    procedure actOKUpdate (Sender : TObject );
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectDirClick (Sender : TObject );
    procedure btnUnselectAllClick(Sender: TObject);
  private
    procedure SelectAll(const ADoSelect : Boolean);
  public
    { public declarations }
  end; 

var
  frmSaveOptions : TfrmSaveOptions;

implementation

{ TfrmSaveOptions }

procedure TfrmSaveOptions.actOKUpdate (Sender : TObject );
begin
  TAction(Sender).Enabled :=
    ( Trim(edtOutputDir.Text) <> '' ) and
    ( edtInterface.Checked or edtProxy.Checked or
      edtImplementation.Checked or edtBinder.Checked
    );
end;

procedure TfrmSaveOptions.btnSelectAllClick(Sender: TObject);
begin
  SelectAll(True);
end;

procedure TfrmSaveOptions.btnSelectDirClick (Sender : TObject );
begin
  SD.FileName := edtOutputDir.Text;
  if SD.Execute() then begin
    edtOutputDir.Text := SD.FileName;
  end;
end;

procedure TfrmSaveOptions.btnUnselectAllClick(Sender: TObject);
begin
  SelectAll(False);
end;

procedure TfrmSaveOptions.SelectAll(const ADoSelect: Boolean);
begin
  edtBinder.Checked := ADoSelect;
  edtImplementation.Checked := edtBinder.Checked;
  edtImplementation.Checked := edtBinder.Checked;
  edtInterface.Checked := edtBinder.Checked;
  edtProxy.Checked := edtBinder.Checked;
  //edtWrappedParams.Checked := edtBinder.Checked;
end;


procedure TfrmSaveOptions.actOKExecute (Sender : TObject );
begin
  ForceDirectories(edtOutputDir.Text);
  ModalResult := mrOK;
end;

initialization
  {$I ufrmsaveoption.lrs}

end.

