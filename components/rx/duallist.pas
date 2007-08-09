{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{                                                       }
{*******************************************************}

unit duallist;

interface

{$I rx.inc}

uses Classes, Controls;

type

{ TDualListDialog }

  TDualListDialog = class(TComponent)
  private
    FCtl3D: Boolean;
    FSorted: Boolean;
    FTitle:string;
    FLabel1Caption: TCaption;
    FLabel2Caption: TCaption;
    FOkBtnCaption: TCaption;
    FCancelBtnCaption: TCaption;
    FHelpBtnCaption: TCaption;
    FHelpContext: THelpContext;
    FList1: TStrings;
    FList2: TStrings;
    FShowHelp: Boolean;
    procedure SetList1(Value: TStrings);
    procedure SetList2(Value: TStrings);
    function IsLabel1Custom: Boolean;
    function IsLabel2Custom: Boolean;
    function IsOkBtnCustom: Boolean;
    function IsCancelBtnCustom: Boolean;
    function IsHelpBtnCustom: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property Ctl3D: Boolean read FCtl3D write FCtl3D default True;
    property Sorted: Boolean read FSorted write FSorted;
    property Title: string read FTitle write FTitle;
    property Label1Caption: TCaption read FLabel1Caption write FLabel1Caption
      stored IsLabel1Custom;
    property Label2Caption: TCaption read FLabel2Caption write FLabel2Caption
      stored IsLabel2Custom;
    property OkBtnCaption: TCaption read FOkBtnCaption write FOkBtnCaption
      stored IsOkBtnCustom;
    property CancelBtnCaption: TCaption read FCancelBtnCaption write FCancelBtnCaption
      stored IsCancelBtnCustom;
    property HelpBtnCaption: TCaption read FHelpBtnCaption write FHelpBtnCaption
      stored IsHelpBtnCustom;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property List1: TStrings read FList1 write SetList1;
    property List2: TStrings read FList2 write SetList2;
    property ShowHelp: Boolean read FShowHelp write FShowHelp default True;
  end;

implementation

uses SysUtils, Forms, FDualLst, VCLUtils, LCLStrConsts;

{ TDualListDialog }

constructor TDualListDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCtl3D := True;
  FShowHelp := True;
  FList1 := TStringList.Create;
  FList2 := TStringList.Create;
{  FLabel1Caption := LoadStr(SDualListSrcCaption);
  FLabel2Caption := LoadStr(SDualListDestCaption);}
  OkBtnCaption := rsmbOK;
  CancelBtnCaption := rsmbCancel;
  HelpBtnCaption := rsmbHelp;
end;

destructor TDualListDialog.Destroy;
begin
  List1.Free;
  List2.Free;
  inherited Destroy;
end;

procedure TDualListDialog.SetList1(Value: TStrings);
begin
  FList1.Assign(Value);
end;

procedure TDualListDialog.SetList2(Value: TStrings);
begin
  FList2.Assign(Value);
end;

function TDualListDialog.IsLabel1Custom: Boolean;
begin
  Result := true;//CompareStr(Label1Caption, LoadStr(SDualListSrcCaption)) <> 0;
end;

function TDualListDialog.IsLabel2Custom: Boolean;
begin
  Result := true;//CompareStr(Label2Caption, LoadStr(SDualListDestCaption)) <> 0;
end;

function TDualListDialog.IsOkBtnCustom: Boolean;
begin
  Result := CompareStr(OkBtnCaption, rsmbOK) <> 0;
end;

function TDualListDialog.IsCancelBtnCustom: Boolean;
begin
  Result := CompareStr(CancelBtnCaption, rsmbCancel) <> 0;
end;

function TDualListDialog.IsHelpBtnCustom: Boolean;
begin
  Result := CompareStr(HelpBtnCaption, rsmbHelp) <> 0;
end;

function TDualListDialog.Execute: Boolean;
var
  Form: TDualListForm;
begin
  Form := TDualListForm.Create(Application);
  try
    with Form do
    begin
      Ctl3D := Self.Ctl3D;
      if NewStyleControls then Font.Style := [];
      ShowHelp := Self.ShowHelp;
      SrcList.Sorted := Sorted;
      DstList.Sorted := Sorted;
      SrcList.Items := List1;
      DstList.Items := List2;
      if Self.Title <> '' then Form.Caption := Self.Title;
      if Label1Caption <> '' then SrcLabel.Caption := Label1Caption;
      if Label2Caption <> '' then DstLabel.Caption := Label2Caption;
      OkBtn.Caption := OkBtnCaption;
      CancelBtn.Caption := CancelBtnCaption;
      HelpBtn.Caption := HelpBtnCaption;
      HelpContext := Self.HelpContext;
      HelpBtn.HelpContext := HelpContext;
    end;
    Result := (Form.ShowModal = mrOk);
    if Result then begin
      List1 := Form.SrcList.Items;
      List2 := Form.DstList.Items;
    end;
  finally
    Form.Free;
  end;
end;

end.
