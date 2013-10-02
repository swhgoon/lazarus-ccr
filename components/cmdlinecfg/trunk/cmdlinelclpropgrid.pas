unit cmdlinelclpropgrid;

//todo: this unit is incomplete
{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, ValEdit, StdCtrls
  , cmdlinecfg, cmdlinecfgui;

type

  { TCmdLineGridControl }

  TCmdLineGridControl = class(TCmdLineUIControl)
  private
    fPropGrid : TValueListEditor;
    fCfg    : TCmdLineCfg;
    fDropdown : TComboBox;
    fEditCol,fEditRow: Integer;
  public
    procedure OnSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure OnHeaderSizing(sender: TObject; const IsColumn: boolean;
                                    const aIndex, aSize: Integer) ;
    procedure OnEditingDone(Sender: TObject);
    procedure OnDDKeyPress(Sender: TObject; var Key: char);
    procedure OnDDSelect(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure OnCanSelect(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure OnTopLeftChanged(Sender: TObject);

    procedure UpdateEditorBounds;
    procedure UpdateValue(aCol, arow: Integer; const newval: string; NotifyChange: Boolean = true);
  public
    constructor Create(AParent: TWinControl);
    procedure Init(cfg: TCmdLineCfg; layout: TCmdLineLayoutInfo); override;
    procedure SetValues(list: TList {of TCmdLineOptionValue}); override;
    procedure Serialize(list: TList {of TCmdLineOptionValue}); override;
  end;

implementation

{ TCmdLineGridControl }

procedure TCmdLineGridControl.OnSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
var
  i   : Integer;
  opt : TCmdLineCfgOption;
  dd  : TComboBox;
  j   : integer;
  nm  : string;
begin
  if not Assigned(fCfg) then Exit;

  i:=aRow-1;
  if (i<0) or (i>=fCfg.Options.Count) then Exit;
  opt:=TCmdLineCfgOption(fPropGrid.Objects[aCol, aRow]);
  if not Assigned(opt) then Exit;

  fEditCol:=aCol;
  fEditRow:=aRow;
  if (opt.OptType='switch') or (opt.ValCount>0) then begin
    if not Assigned(fDropdown) then begin
      dd:=TComboBox.Create(fPropGrid);
      dd.OnSelect:=OnDDSelect;
      dd.OnKeyPress:=OnDDKeyPress;
      dd.OnExit:=OnExit;
      fDropdown:=dd;
    end else
      dd:=fDropdown;
    dd.Style:=csDropDownList;
    dd.Items.Clear;
    if opt.OptType='switch' then begin
      dd.Items.Add('false');
      dd.Items.Add('true');
    end else if opt.ValCount>0 then
      for j:=0 to opt.ValCount-1 do begin
        nm:=opt.Values[j].DisplayName;
        if nm='' then nm:=opt.Values[j].CmdLineValue;
        dd.Items.Add(nm);
      end;
    dd.ItemIndex:=dd.Items.IndexOf( fPropGrid.Cells[aCol, aRow]);
    dd.BoundsRect:=fPropGrid.CellRect(aCol,Arow);
    Editor:=dd;
  end;
end;

procedure TCmdLineGridControl.OnEditingDone(Sender: TObject);
begin
  if not Assigned(fCfg) then Exit;
  if Assigned(fDropdown) and (fDropdown.Visible) then begin
    fDropdown.Hide;
    UpdateValue(fEditCol, fEditRow, fDropdown.Text);
  end;
end;

procedure TCmdLineGridControl.OnDDKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then begin
    fPropGrid.EditingDone;
    fDropdown.Hide;
  end;
end;

procedure TCmdLineGridControl.OnDDSelect(Sender: TObject);
begin

  UpdateValue(fEditCol, fEditRow, fDropdown.Text);
end;

procedure TCmdLineGridControl.OnExit(Sender: TObject);
begin
   if Assigned(fDropDown) then fDropdown.Hide;
end;

procedure TCmdLineGridControl.OnCanSelect(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect:=aCol>0;
end;

procedure TCmdLineGridControl.OnTopLeftChanged(Sender: TObject);
begin
  UpdateEditorBounds;
end;

procedure TCmdLineGridControl.UpdateEditorBounds;
begin
   if Assigned(fPropGrid.Editor) then begin
     fPropGrid.Editor.BoundsRect:=fPropGrid.CellRect(fEditCol,fEditRow);
   end;
end;

procedure TCmdLineGridControl.UpdateValue(aCol, arow: Integer;
  const newval: string; NotifyChange: Boolean );
begin
  fPropGrid.Cells[aCol, aRow]:=newval;
  if NotifyChange then ValueChanged;
end;

procedure TCmdLineGridControl.OnHeaderSizing(sender: TObject;
  const IsColumn: boolean; const aIndex, aSize: Integer);
begin
  UpdateEditorBounds;
end;

constructor TCmdLineGridControl.Create(AParent: TWinControl);
begin
  inherited Create;
  fPropGrid:=TValueListEditor.Create(AParent);
  fPropGrid.Parent:=AParent;
  fPropGrid.Align:=alClient;
  fPropGrid.OnSelectEditor:=Self.OnSelectEditor;
  fPropGrid.OnHeaderSizing:=OnHeaderSizing;
  fPropGrid.OnEditingDone:=OnEditingDone;
  fPropGrid.OnExit:=OnExit;
  fPropGrid.OnSelectCell:=OnCanSelect;
  fPropGrid.OnTopLeftChanged:=OnTopLeftChanged;
  fPropGrid.OnResize:=OnTopLeftChanged;
end;

procedure TCmdLineGridControl.Init(cfg: TCmdLineCfg; layout: TCmdLineLayoutInfo);
var
  i   : integer;
  j   : integer;
  opt : TCmdLineCfgOption;
  chk : TCheckBox;
  cr  : TRect;
begin
  // todo: Clean if exists
  fCfg:=cfg;
  if Assigned(fcfg) then begin
    fPropGrid.BeginUpdate;
    try
      fPropGrid.RowCount:=cfg.Options.Count+1;
      j:=1;
      for i:=0 to cfg.Options.Count-1 do begin
        opt:=TCmdLineCfgOption(cfg.Options[i]);
        if opt.AliasToKey <>'' then Continue;
        fPropGrid.Keys[j]:=opt.Display;
        fPropGrid.Objects[1, j]:=opt;
        if opt.OptType='switch' then begin
          fPropGrid.Values[opt.Display]:='false';
        end;
        inc(j);
      end;
      fPropGrid.RowCount:=j;
    finally
      fPropGrid.EndUpdate;
    end;
  end;
end;

procedure TCmdLineGridControl.SetValues(list: TList);
begin
  if not Assigned(fCfg) then Exit;

end;

procedure TCmdLineGridControl.Serialize(list: TList);
var
  i   : Integer;
  j   : Integer;
  vl  : TCmdLineOptionValue;
  opt : TCmdLineCfgOption;
  v   : string;
begin
  if not Assigned(fCfg) then Exit;
  for i:=1 to fPropGrid.RowCount-1 do begin
    opt:=TCmdLineCfgOption(fPropGrid.Objects[1, i]);
    if not Assigned(opt) then Continue;
    vl := TCmdLineOptionValue.Create;
    if opt.ValCount>0 then begin
      v:=fPropGrid.Cells[1, i];
      for j:=0 to opt.ValCount-1 do
        if ((opt.Values[j].DisplayName <> '') and (opt.Values[j].DisplayName=v)) or (opt.Values[j].CmdLineValue=v) then begin
          vl.Value:=opt.Values[j].CmdLineValue;
          Break;
        end;
    end else
      vl.Value:=fPropGrid.Cells[1, i];
    vl.Option:=opt;
    if vl.Option.OptType='switch' then begin
      if vl.Value='false' then vl.Value:='' else vl.Value:='1';
    end;
    list.Add(vl);
  end;
end;

end.

