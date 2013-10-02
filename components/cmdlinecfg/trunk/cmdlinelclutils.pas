unit cmdlinelclutils;

interface

uses Controls, SysUtils, StdCtrls, Classes, cmdlinecfg;

var
  ADirsDialogs : function (var path: string): Boolean = nil;
  AFilesDialogs : function (var path: string): Boolean = nil;

procedure CreateComboBoxWithLabel(opt: TCmdLineCfgOption; AOwner: TWinControl; var combo: TComboBox; var lbl: TLabel);
procedure CreateComboBox(opt: TCmdLineCfgOption; AOwner: TWinControl; var combo: TComboBox);
procedure CreateCheckBox(opt: TCmdLineCfgOption; AOwner: TWinControl; SetCaptionToDisplay: Boolean; var chk: TCheckBox);
procedure AnchorControls(left, right: TControl; Spacing: Integer = 10);
procedure ControlSpanToRight(ctrl: TControl; XOffset: Integer = 10);
function SerializeComboBox(opt: TCmdLineCfgOption; combo: TComboBox): string;
function SerializeCheckBox(opt: TCmdLineCfgOption; chk: TCheckBox): string;
function SerializeEdit(opt: TCmdLineCfgOption; edt: TEdit): string;
procedure SetValueComboBox(opt: TCmdLineCfgOption; const vl: string; combo: TComboBox);
procedure SetValueCheckBox(opt: TCmdLineCfgOption; const vl: string; chk: TCheckBox);
procedure SetValueEdit(opt: TCmdLineCfgOption; const vl: string; edt: TEdit);
procedure SetMultiValueEdit(opt: TCmdLineCfgOption; const vl: string; const Delim: string; edt: TEdit);

procedure ResetValue(ctrl: TControl);
function SerializeAControl(opt: TCmdLineCfgOption; ctrl: TControl; var v: string): Boolean;

type
  TEditPathsOpt = (epoSingleFileOnly, epoSingleDirOnly, epoFilesOnly, epoDirsOnly);

function ExecuteEditPathsDialogs(var path: string; DialogOption: TEditPathsOpt ): Boolean;
procedure CreatePathsEdit(opt: TCmdLineCfgOption; AOwner: TWinControl;
    var lbl: TLabel; var edit: TEdit; var lookupbtn: TButton);
procedure CreateEdit(opt: TCmdLineCfgOption; AOwner: TWinControl; var lbl: TLabel; var edit: TEdit);

function OptKeyLabel(opt: TCmdLineCfgOption): string;

implementation

function OptKeyLabel(opt: TCmdLineCfgOption): string;
begin
  if not Assigned(opt) then Result:=''
  else Result:='('+StringReplace(opt.key, '%value%','', [rfIgnoreCase, rfReplaceAll])+')';
end;

procedure AnchorControls(left, right: TControl; Spacing: Integer);
begin
  right.AnchorSideLeft.Control:=left;
  right.AnchorSideLeft.Side:=asrRight;
  right.BorderSpacing.Left:=Spacing;
end;

function SerializeEdit(opt: TCmdLineCfgOption; edt: TEdit): string;
begin
  Result:=edt.Text;
end;

procedure SetValueComboBox(opt: TCmdLineCfgOption; const vl: string; combo: TComboBox);
var
  j  : Integer;
begin
  if vl='' then begin
    combo.ItemIndex:=-1;
    Exit;
  end;
  for j:=0 to opt.ValCount-1 do begin
    if (opt.Values[j].CmdLineValue =vl) then begin
      if j<=combo.Items.Count then begin
        combo.ItemIndex:=j;
      end;
      Exit;
    end;
  end;
end;

procedure SetValueCheckBox(opt: TCmdLineCfgOption; const vl: string; chk: TCheckBox);
begin
  chk.Checked:=vl<>'';
end;

procedure SetValueEdit(opt: TCmdLineCfgOption; const vl: string; edt: TEdit);
begin
  edt.Text:=vl;
end;

procedure SetMultiValueEdit(opt: TCmdLineCfgOption; const vl: string;
  const Delim: string; edt: TEdit);
begin
  if vl<>'' then begin
    if edt.Text<>'' then edt.Text:=edt.Text+Delim+vl
    else edt.Text:=vl;
  end;
end;

procedure ControlSpanToRight(ctrl: TControl; XOffset: Integer = 10);
begin
  if not Assigned(ctrl) or not Assigned(ctrl.Parent) then Exit;
  ctrl.Anchors:=ctrl.Anchors-[akRight];
  ctrl.Width:=ctrl.Parent.ClientWidth-ctrl.Left-XOffset;
  ctrl.Anchors:=ctrl.Anchors+[akRight];
end;

function ExecuteEditPathsDialogs(var path: string; DialogOption: TEditPathsOpt
  ): Boolean;
begin
  case DialogOption of
    epoSingleFileOnly: begin

    end;
    epoSingleDirOnly: begin

    end;
    epoDirsOnly: if not Assigned(ADirsDialogs) then Result:=false
                 else Result:=ADirsDialogs(path);
    epoFilesOnly: if not Assigned(AFilesDialogs) then Result:=false;
                  else Result:=AFilesDialogs(path);
  end;
end;

procedure CreatePathsEdit(opt: TCmdLineCfgOption; AOwner: TWinControl;
  var lbl: TLabel; var edit: TEdit; var lookupbtn: TButton);
begin
  lbl:=TLabel.Create(AOwner);
  lbl.Caption:=opt.Display+' '+OptKeyLabel(opt);
  edit:=TEdit.Create(AOwner);
  lookupbtn:=TButton.Create(AOwner);
  lookupbtn.Caption:='...';
  lookupbtn.AutoSize:=true;
  AnchorControls(lbl, edit);
  AnchorControls(edit, lookupbtn);
end;

procedure CreateEdit(opt: TCmdLineCfgOption; AOwner: TWinControl; var lbl: TLabel; var edit: TEdit);
begin
  lbl:=TLabel.Create(AOwner);
  lbl.Caption:=opt.Display+' '+OptKeyLabel(opt);
  edit:=TEdit.Create(AOwner);
  edit.Parent:=AOwner;
  lbl.Parent:=AOwner;
end;

procedure CreateComboBoxWithLabel(opt: TCmdLineCfgOption; AOwner: TWinControl;
  var combo: TComboBox; var lbl: TLabel);
begin
  lbl:=TLabel.Create(AOwner);
  lbl.Caption:=opt.Display + ' '+OptKeyLabel(opt);
  lbl.Parent:=AOwner;
  CreateComboBox(opt, AOwner, combo);
end;

procedure CreateComboBox(opt: TCmdLineCfgOption; AOwner: TWinControl; var combo: TComboBox);
var
  dd : TComboBox;
  j  : Integer;
  nm : string;
begin
  dd:=TComboBox.Create(AOwner);
  for j:=0 to opt.ValCount-1 do begin
    nm:=opt.Values[j].DisplayName;
    if nm='' then nm:=opt.Values[j].CmdLineValue;
    dd.Items.Add(nm);
  end;
  dd.Parent:=AOwner;
  combo:=dd;
end;

function SerializeComboBox(opt: TCmdLineCfgOption; combo: TComboBox): string;
var
  vl : string;
  j  : Integer;
begin
  vl:=combo.Text;
  Result:='';
  if vl='' then Exit;
  for j:=0 to opt.ValCount-1 do
    if (opt.Values[j].DisplayName='') then begin
      if (opt.Values[j].CmdLineValue =vl) then begin
        Result:=vl;
        Exit;
      end;
    end else if (opt.Values[j].DisplayName=vl) then begin
      Result:=opt.Values[j].CmdLineValue;
      Exit;
    end;
  Result:=vl;
end;

procedure CreateCheckBox(opt: TCmdLineCfgOption; AOwner: TWinControl;
  SetCaptionToDisplay: Boolean; var chk: TCheckBox);
begin
  chk := TCheckBox.Create(AOwner);
  if SetCaptionToDisplay then chk.Caption:=opt.Display+' '+ OptKeyLabel(opt);
  chk.Parent:=AOwner;
end;

function SerializeCheckBox(opt: TCmdLineCfgOption; chk: TCheckBox): string;
begin
  if chk.Checked then Result:='1' else Result:='';
end;

procedure ResetValue(ctrl: TControl);
begin
  if ctrl is TEdit then TEdit(ctrl).Text:=''
  else if ctrl is TCheckBox then TCheckBox(ctrl).Checked:=false
  else if ctrl is TComboBox then TComboBox(ctrl).ItemIndex:=-1;
end;

function SerializeAControl(opt: TCmdLineCfgOption; ctrl: TControl; var v: string): Boolean;
begin
  v:='';
  Result:=true;
  if ctrl is TComboBox then v:=SerializeComboBox(opt, TComboBox(ctrl))
  else if ctrl is TCheckBox then v:=SerializeCheckBox(opt, TCheckBox(ctrl))
  else if ctrl is TEdit then v:=SerializeEdit(opt, TEdit(ctrl))
  else Result:=false;
end;

end.
