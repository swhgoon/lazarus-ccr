unit mainform;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  StdCtrls, ExtCtrls
  ,cmdlinecfg , cmdlinelclctrlsbox, cmdlinecfgjson //, patheditor
  ,cmdlinecfgui, cmdlinecfguijson
  ,cmdlinecfgparser;

type

  { TForm1 }

  TForm1 = class(TForm)
    toOpt: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure toOptClick(Sender: TObject);
  private
    { private declarations }
    ctrl  : TCmdLineScrollBoxControl;
    cfg   : TCmdLineCfg;
    lt    : TCmdLineLayoutInfo;
  public
    { public declarations }
    procedure OnValueChanged(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  {cmdlinelclutils.ADirsDialogs:=DummyEditPaths;
  cmdlinelclutils.AFilesDialogs:=DummyEditPaths;}

  ctrl:=TCmdLineScrollBoxControl.Create(Self);
  if ParamCount>0 then begin
    cfg:=TCmdLineCfg.Create;
    CmdLineCfgJSONReadFile(ParamStr(1), cfg);
    if ParamCOunt>1 then begin
      lt:=TCmdLineLayoutInfo.Create;
      CmdLineUIJSONReadFile(PAramStr(2), lt);
    end;
    ctrl.Init(cfg, lt);
    ctrl.OnValueChanged:=OnValueChanged;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ctrl.Free;
  lt.Free;
  cfg.Free;
end;

procedure TForm1.toOptClick(Sender: TObject);
var
  vl : TList;
  i  : Integer;
begin
  vl := TList.Create;
  try
    CmdLineMatchArgsToOpts(cfg, Memo1.Text, vl);
    ctrl.SetValues(vl);
    for i:=0 to vl.Count-1 do TObject(vl[i]).Free;
  finally
    vl.Free;
  end;
end;

procedure TForm1.OnValueChanged(Sender: TObject);
var
  l   : TList;
  i   : integer;
begin
  l :=TList.create;
  try
    ctrl.Serialize(l);
    Memo1.Text:=CmdLineMakeOptions(l);
    for i:=0 to l.Count-1 do
      TObject(l[i]).Free;
  finally
    l.Free;
  end;
end;

end.

