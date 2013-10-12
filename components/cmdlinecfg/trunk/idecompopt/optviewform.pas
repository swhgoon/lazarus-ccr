unit optviewform;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf,
  StdCtrls, LazIDEIntf, IDEOptionsIntf, ProjectIntf, CompOptsIntf, cmdlinecfg
  //, cmdlinecfgjson
  , cmdlinecfgui
  //, cmdlinecfguijson
  , cmdlinelclctrlsbox, cmdlinelazcompopt;

type
  TOptionsBox = record
    box : TScrollBox;
    cmd : TCmdLineScrollBoxControl;
  end;

  { TOptView }

  TOptView = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure Panel2Resize(Sender: TObject);
  private
    { private declarations }
    sections : array of TOptionsBox;
    cfg    : TCmdLineCfg;
    layout : TCmdLineLayoutInfo;
    procedure InitOpts(acfg: TCmdLineCfg; alayout: TCmdLineLayoutInfo);
    procedure ClearSections(clearboxes: Boolean = true);
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
  OptView.Caption:='Review Compiler Options';
  if (OptView.cfg<>acfg) or (OptView.layout<>alayout) then
    OptView.InitOpts(acfg, alayout);
  if not Assigned(opt) and Assigned(LazarusIDE.ActiveProject) then
    opt:=LazarusIDE.ActiveProject.LazCompilerOptions;

  if Assigned(opt) then Result:=OptView.ShowForOpts(opt)
  else Result:=mrCancel;
end;

procedure TOptView.FormDestroy(Sender: TObject);
begin
  ClearSections(false);
end;

procedure TOptView.FormShow(Sender: TObject);
begin

end;

procedure TOptView.ListBox1Click(Sender: TObject);
begin

end;

procedure TOptView.ListBox1SelectionChange(Sender: TObject; User: boolean);
var
  i : Integer;
  idx: integer;
begin
  idx:=ListBox1.ItemIndex;
  if (idx>=0) and (idx<length(sections)) then begin
    sections[idx].box.BringToFront;
    sections[idx].box.Show;
    for i:=0 to length(sections)-1 do
      if i<>idx then sections[i].box.Hide;
  end;

end;

procedure TOptView.Panel2Resize(Sender: TObject);
var
  w: integer;
begin
  w:=Self.Width div 2;
  btnOk.Left:=w - btnOk.Width - 2;
  btnCancel.Left:=w+2;
end;

procedure TOptView.InitOpts(acfg: TCmdLineCfg; alayout: TCmdLineLayoutInfo);
var
  lt  : TList;
  i   : Integer;
  ls  : TLayoutSection;
begin
  ClearSections;
  cfg:=acfg;
  layout:=alayout;
  //cmd:=
  lt:=TList.Create;
  try
    LayoutEnumElement( layout.RootElement, lt);
    SetLength(sections, lt.Count);
    for i:=0 to lt.Count-1 do begin
      ls:=TLayoutSection(lt[i]);
      ListBox1.AddItem( ls.Display, ls);
      sections[i].box:=TScrollBox.Create(Panel3);
      sections[i].box.Visible:=false;
      sections[i].box.Parent:=Panel3;
      sections[i].box.Align:=alClient;
      sections[i].cmd:=TCmdLineScrollBoxControl.Create(sections[i].box);
      sections[i].cmd.Init(cfg, layout, ls.Name);
    end;
  finally
    lt.Free;
  end;

end;

procedure TOptView.ClearSections(clearboxes: Boolean);
var
  i : integer;
begin
  for i:=0 to length(sections)-1 do begin
    sections[i].cmd.Free;
    if clearboxes then sections[i].box.Free;
  end;
  SetLength(sections,0);
end;

function TOptView.ShowForOpts(opt: TLazCompilerOptions): Integer;
var
  list : TList;
  i    : Integer;
begin
  list := TList.Create;
  try
    LazCompOptToVals(opt, cfg, list);
    for i:=0 to length(sections)-1 do sections[i].cmd.SetValues(list);
    for i:=0 to list.Count-1 do TObject(list[i]).Free;

    Result:=ShowModal;
    if Result=mrOK then begin
      list.Clear;
      for i:=0 to length(sections)-1 do
        sections[i].cmd.Serialize(list);
      ValsToLazCompOpt(list, opt);
      for i:=0 to list.Count-1 do TObject(list[i]).Free;
    end;

  finally
    list.Free;
  end;

end;

initialization

end.

