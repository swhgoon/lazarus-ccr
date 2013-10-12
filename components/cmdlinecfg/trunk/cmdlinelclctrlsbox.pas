unit cmdlinelclctrlsbox;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, Graphics
  , cmdlinecfg, cmdlinecfgutils, cmdlinelclutils, cmdlinecfgui
  , contnrs
  , cmdlinefpccond;

type

  { TControlInfo }

  TControlInfo = class(TObject)
  public
    check : TFPCConditionCheck;
    opt   : TCmdLineCfgOption;
    ctrl  : TControl;
    constructor Create(aopt: TCmdLineCfgOption; actrl: TControl);
    destructor Destroy; override;
    function isAllowed(const cpu, os: string): Boolean;
  end;

  { TCmdLineScrollBoxControl }

  TCmdLineScrollBoxControl = class(TCmdLineUIControl)
  private
    fCfg        : TCmdLineCfg;
    fControls   : TList;
    fOptToCtrl  : TFPHashObjectList;
    fParent     : TWinControl;

    fOtherMet   : Boolean;
  protected
    flayout: TCmdLineLayoutInfo;
    fusedoptlist: TFPHashObjectList;
    procedure OnChange(Sender: TObject);
    procedure OnCndChange(Sender: TObject);
    procedure RevaluateConditions;
    function AllocControls(AParent: TWinControl; VOffset: Integer; listofopt: TList): Integer;
    function AllocHeaderLabel(AParent: TWinControl; VOffset: Integer; const Caption: String): Integer;
    function AllocForSection(AParent: TWinControl; VOffset: Integer; sct : TLayoutSection; SkipHeader: Boolean = false): Integer;
    procedure Reset;
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    procedure Init(cfg: TCmdLineCfg; layout: TCmdLineLayoutInfo; const ASection: string = ''); override;
    procedure SetValues(list: TList {of TCmdLineOptionValue}); override;
    procedure Serialize(list: TList {of TCmdLineOptionValue}); override;
  end;

implementation

{ TControlInfo }

constructor TControlInfo.Create(aopt: TCmdLineCfgOption; actrl: TControl);
begin
  inherited Create;
  ctrl:=actrl;
  opt:=aopt;
  if aopt.Condition<>'' then
    check := TFPCConditionCheck.Create(aopt.Condition)
  else
    check:=nil;
end;

destructor TControlInfo.Destroy;
begin
  check.Free;
  inherited Destroy;
end;

function TControlInfo.isAllowed(const cpu, os: string): Boolean;
begin
  Result:=(not Assigned(check)) or (check.isValid(cpu, os));
end;

{ TCmdLineScrollBoxControl }

procedure TCmdLineScrollBoxControl.OnChange(Sender: TObject);
begin
  ValueChanged;
end;

procedure TCmdLineScrollBoxControl.OnCndChange(Sender: TObject);
begin
  OnChange(Sender);
  RevaluateConditions;
end;

procedure TCmdLineScrollBoxControl.RevaluateConditions;
var
  i : Integer;
  cpu : string;
  os  : string;
  ci  : TControlInfo;
begin
  ci:=TControlInfo(fOptToCtrl.Find('-P'));
  if Assigned(ci) then SerializeAControl(ci.opt, ci.ctrl, cpu)
  else cpu:='';
  ci:=TControlInfo(fOptToCtrl.Find('-T'));
  if Assigned(ci) then SerializeAControl(ci.opt, ci.ctrl, os)
  else os:='';

  for i:=0 to fOptToCtrl.Count-1 do begin
    ci:=TControlInfo(fOptToCtrl.Items[i]);
    if (ci.opt.Name='-P') or (ci.opt.Name='-T') then Continue;
    if Assigned(ci.check) then begin
      ci.ctrl.Enabled:=ci.isAllowed(cpu, os);
    end;
    //todo: values availability
  end;
end;

function TCmdLineScrollBoxControl.AllocControls(AParent: TWinControl;
  VOffset: Integer; listofopt: TList): Integer;
var
  i   : Integer;
  y   : Integer;
  opt : TCmdLineCfgOption;
  chk : TCheckBox;
  dd  : TComboBox;
  YOffset : Integer;
  XOffset : Integer;
  lbl     : TLabel;
  mctrl   : TControl;
  edt     : TEdit;
begin
  y:=VOffset;
  YOffset:=0; //todo: get from the widgetset
  XOffset:=10;

  for i:=0 to listofopt.Count-1 do begin
    mctrl:=nil;
    opt:=TCmdLineCfgOption(listofopt[i]);
    if opt.AliasToKey <>'' then Continue;

    if opt.ValCount>0 then begin
      CreateComboBoxWithLabel(opt, APArent, dd, lbl);
      lbl.Left:=XOffset;
      lbl.Top:=y;
      dd.Style:=csDropDownList;
      dd.Top:=y;
      dd.Left:=lbl.Width;
      //todo: hardcoded key names :(
      if (opt.Name='-P') or (opt.Name='-T') then
        dd.OnSelect:=OnCndChange
      else
        dd.OnSelect:=OnChange;
      ControlSpanToRight(dd);
      AnchorControls(lbl, dd);
      mctrl:=dd;
      inc(y, dd.Height);
    end else if opt.OptType='switch' then begin
      CreateCheckBox(opt, AParent, true, chk);
      chk.Top:=y;
      chk.Left:=XOffset;
      chk.OnClick:=OnChange;
      mctrl:=chk;
      inc(y, chk.Height + YOffset);
    end else begin
      CreateEdit(opt, AParent, lbl, edt);
      edt.Top:=y;
      lbl.Top:=y;
      lbl.Left:=XOffset;
      mctrl:=edt;
      edt.OnEditingDone:=OnChange;
      AnchorControls(lbl, edt);
      ControlSpanToRight(edt);
      inc(y, edt.Height + YOffset);
    end;
    if Assigned(mctrl) then begin
      mctrl.Tag:=PtrUInt(opt);
      fControls.Add(mctrl);
      fOptToCtrl.Add(  opt.Name, TControlInfo.Create(opt, mctrl) );
    end;
  end;
  Result:=y;
end;

function TCmdLineScrollBoxControl.AllocHeaderLabel(AParent: TWinControl;
  VOffset: Integer; const Caption: String): Integer;
var
  lbl : TLabel;
begin
  inc(VOffset, 10);//todo: this information should come from a widgetset
  lbl:=TLabel.Create(APArent);
  lbl.Caption:=Caption;
  lbl.Parent:=AParent;
  lbl.Top:=VOffset;
  lbl.Left:=0;
  lbl.Width:=AParent.ClientWidth;
  lbl.Anchors:=lbl.Anchors+[akRight];
  lbl.Alignment:=taCenter;
  inc(VOffset, lbl.Height);
  Result:=VOffset;
end;

function TCmdLineScrollBoxControl.AllocForSection(AParent: TWinControl; VOffset: Integer; sct : TLayoutSection; SkipHeader: Boolean ): Integer;
var
  ls  : TLayoutSection;
  sw  : TStringList;
  y   : Integer;
  l   : TList;
  j   : Integer;
  k    : Integer;
  box  : TGroupBox;
  by   : integer;
  opt  : TCmdLineCfgOption;
begin
  if not Assigned(sct) then begin
    Result:=VOffset;
    Exit;
  end;
  y:=VOffset;
  sw:=TStringList.Create;
  l:=TList.Create;
  try
    if (sct.Name<>'') and not SkipHeader then
      y:=AllocHeaderLabel(APArent, y, sct.Display);

    for j:=0 to sct.ElemCount-1 do begin
      ls:=sct.Elements[j];
      if ls.ElementType=letSection then begin
        if l.Count>0 then begin
          y:=AllocControls(AParent, y, l);
          l.Clear;
        end;
        if ls.GUIHint='groupbox' then begin
          box := TGroupBox.Create(AParent);
          box.Parent:=AParent;
          box.Caption:=ls.Display;
          box.Width:=AParent.Width-10;
          box.Anchors:=box.Anchors+[akRight];
          by:=AllocForSection(box, 0, ls, true);
          box.Height:=by+22; //todo: define the border size by widgetset
          box.Top:=y;
          inc(y, box.Height);
        end else
          y:=AllocForSection(AParent, y, ls );
      end else begin
        if (AnsiLowerCase(ls.Name)='%%other') and not fOtherMet then begin
          LayoutGetUnused( fCfg, flayout.RootElement, l );
          fOtherMet := true;
        end else begin
          k := fusedoptlist.FindIndexOf(ls.Name);
          if k>=0 then begin
            l.Add( fusedoptlist.Items[k] );
            fusedoptlist.Delete(k);
          end;
        end;
      end;
    end;

    if l.Count>0 then y:=AllocControls(AParent, y, l);
  finally
    sw.Free;
    l.Free;
    Result:=y;
  end;
end;

procedure TCmdLineScrollBoxControl.Reset;
var
  i :Integer;
begin
  for i:=0 to fControls.Count-1 do
    ResetValue(fControls[i]);

end;

constructor TCmdLineScrollBoxControl.Create(AParent: TWinControl);
begin
  inherited Create;
  fParent:=AParent;
  fControls:=TList.Create;
  fOptToCtrl:=TFPHashObjectList.Create(true);
end;

destructor TCmdLineScrollBoxControl.Destroy;
begin
  // fill not free fScrollBox as it should be destroyed by the parent
  fOptToCtrl.Free;
  fControls.Free;
  inherited Destroy;
end;

procedure TCmdLineScrollBoxControl.Init(cfg: TCmdLineCfg; layout: TCmdLineLayoutInfo; const ASection: string = '');
var
  i     : Integer;
  opt   : TCmdLineCfgOption;
  list  : TStringList;
  l     : TList;
  nm    : string;
  y     : Integer;
  sct   : TLayoutSection;
  sctnm : string;
begin
  if not Assigned(cfg) then Exit;
  fCfg:=cfg;
  list:=TStringList.Create;
  list.CaseSensitive:=true; // must be case sensitive
  l:=TList.Create;
  fOptToCtrl.Clear;

  fusedoptlist:=TFPHashObjectList.Create(false);
  flayout:=layout;
  try
    y:=0;
    for i:=0 to cfg.Options.Count-1 do begin
      opt:=TCmdLineCfgOption(cfg.Options[i]);
      nm:=opt.Name;
      if nm='' then nm:=opt.Key;
      list.AddObject(nm, cfg.Options[i]);
      fusedoptlist.Add(nm, cfg.Options[i]);
    end;
    if not Assigned(layout) then begin
      for i:=0 to list.Count-1 do l.Add(list.Objects[i]);
      AllocControls(fParent, y, l);
    end else begin
      fOtherMet:=false;
      sctnm:=ASection;
      sct:=layout.RootElement;
      if ASection<>'' then sct:=LayoutFindElement(sct, sctnm);
      if Assigned(sct) then
        y:=AllocForSection(fParent, y, sct, sctnm<>'');
    end;
  finally
    fusedoptlist.Free;
    fusedoptlist:=nil;
    flayout:=nil;
    l.Free;
    list.Free;
  end;
end;

procedure TCmdLineScrollBoxControl.SetValues(list: TList);
var
  vl    : TCmdLineOptionValue;
  ctrl  : TControl;
  i     : Integer;
  mlt   : TFPHashList;
  isPath : Boolean;
  ci     : TControlInfo;
const
  Delims : array [Boolean] of string = (' ', ';');
begin
  if not Assigned(fCfg) or not Assigned(list) then Exit;
  Reset;
  mlt:=TFPHashList.Create;
  try
    for i:=0 to list.Count-1 do begin
      vl:=TCmdLineOptionValue(list[i]);
      if not Assigned(vl.Option) then Continue;
      ci:=TControlInfo(fOptToCtrl.Find(vl.Option.Name));
      if not Assigned(ci) then Continue;
      ctrl:=ci.ctrl;
      if not Assigned(ctrl) then Continue;
      if ctrl is TComboBox then SetValueComboBox(vl.Option, vl.Value, TComboBoX(ctrl))
      else if ctrl is TCheckBox then SetValueCheckBox(vl.Option, vl.Value, TCheckBox(ctrl))
      else if ctrl is TEdit and not vl.Option.isMultiple then SetValueEdit(vl.Option, vl.Value, TEdit(ctrl))
      else if ctrl is TEdit and vl.Option.isMultiple then begin
        if mlt.FindIndexOf(vl.Option.Name) <0 then begin
          TEdit(ctrl).Text:='';
          mlt.Add(vl.Option.Name, ctrl);
        end;
        isPath:=(vl.Option.OptType='dirpath') or (vl.Option.OptType='filepath');
        SetMultiValueEdit(vl.Option, vl.Value, Delims[isPath], TEdit(ctrl));
      end;
    end;
  finally
    mlt.Free;
  end;
  RevaluateConditions;
end;

procedure TCmdLineScrollBoxControl.Serialize(list: TList);
var
  i    : Integer;
  vl   : TCmdLineOptionValue;
  opt  : TCmdLineCfgOption;
  ctrl : TControl;
  v    : string;
  dlm  : char;
begin
  if not Assigned(fCfg) then Exit;
  for i:=0 to fControls.Count-1 do begin
    ctrl:=TControl(fControls[i]);
    opt:=TCmdLineCfgOption(ctrl.Tag);
    if not Assigned(opt) then Continue;
    if SerializeAControl(opt, ctrl, v) then begin
      if opt.isMultiple then begin
        if (opt.OptType = 'filepath') or (opt.OptType='dirpath') then dlm:=';' else dlm:=' ';
        CmdLineAllocMultiValues(opt, v, dlm, list);
      end else
        list.Add( TCmdLineOptionValue.Create(opt, v));
    end;
  end;
end;

end.

