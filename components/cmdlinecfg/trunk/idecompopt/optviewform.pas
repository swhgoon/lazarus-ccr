unit optviewform;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf,
  StdCtrls, LazIDEIntf, IDEOptionsIntf, ProjectIntf, CompOptsIntf, cmdlinecfg,
  cmdlinecfgjson, cmdlinecfgui, cmdlinecfguijson, cmdlinelclctrlsbox, cmdlinelazcompopt;

type

  { TOptView }

  TOptView = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    cmd    : TCmdLineScrollBoxControl;
    cfg    : TCmdLineCfg;
    layout : TCmdLineLayoutInfo;
    procedure InitOpts(acfg: TCmdLineCfg; alayout: TCmdLineLayoutInfo);
  public
    { public declarations }
    function ShowForOpts(opt: TLazCompilerOptions): Integer;
  end;

var
  OptView: TOptView = nil;

function ReviewOpts(acfg: TCmdLineCfg; alayout: TCmdLineLayoutInfo; opt: TLazCompilerOptions = nil): Integer;

implementation

{$R *.lfm}

{ TOptView }

function ReviewOpts(acfg: TCmdLineCfg; alayout: TCmdLineLayoutInfo; opt: TLazCompilerOptions = nil): Integer;
begin
  if not Assigned(OptView) then OptView:=TOptView.Create(Application);
  if (OptView.cfg<>acfg) or (OptView.layout<>alayout) then
    OptView.InitOpts(acfg, alayout);
  if not Assigned(opt) and Assigned(LazarusIDE.ActiveProject) then
    opt:=LazarusIDE.ActiveProject.LazCompilerOptions;

  if Assigned(OptView.cmd) and Assigned(opt) then
    Result:=OptView.ShowForOpts(opt)
  else
    Result:=mrCancel;
end;

procedure TOptView.FormDestroy(Sender: TObject);
begin
  cmd.Free;
end;

procedure TOptView.FormShow(Sender: TObject);
begin

end;

procedure TOptView.InitOpts(acfg: TCmdLineCfg; alayout: TCmdLineLayoutInfo);
begin
  ReleaseScrollBox(cmd);
  cmd.Free;
  cfg:=acfg;
  layout:=alayout;
  cmd:=TCmdLineScrollBoxControl.Create(Panel1);
  cmd.Init(cfg, layout);
end;

function TOptView.ShowForOpts(opt: TLazCompilerOptions): Integer;
var
  list : TList;
  i    : Integer;
begin
  list := TList.Create;
  try
    LazCompOptToVals(opt, cfg, list);
    cmd.SetValues(list);
    for i:=0 to list.Count-1 do TObject(list[i]).Free;

    Result:=ShowModal;
    if Result=mrOK then begin
      list.Clear;
      cmd.Serialize(list);
      ValsToLazCompOpt(list, opt);
      for i:=0 to list.Count-1 do TObject(list[i]).Free;
    end;

  finally
    list.Free;
  end;

end;

initialization

end.

