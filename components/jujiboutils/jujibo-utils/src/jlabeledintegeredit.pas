unit JLabeledIntegerEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics, Dialogs;

type

  { TJLabeledIntegerEdit }

  TJLabeledIntegerEdit = class(TCustomLabeledEdit)
  private
    { Private declarations }
    theValue: integer;
    fFormat: string;
    function getFormat: string;
    function getValue: integer;
    procedure setFormat(const AValue: string);
    procedure setValue(const AValue: integer);
    function IsValidInteger(const Value: string): boolean;
    procedure FormatInput;
  protected
    { Protected declarations }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: char); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property DisplayFormat: string read getFormat write setFormat;
    property Value: integer read getValue write setValue;

    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property EditLabel;
    property Enabled;
    property Font;
    property LabelPosition;
    property LabelSpacing;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation


procedure Register;
begin
  {$I jlabeledintegeredit_icon.lrs}
  RegisterComponents('Additional', [TJLabeledIntegerEdit]);
end;

{ TJLabeledIntegerEdit }

function TJLabeledIntegerEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJLabeledIntegerEdit.getValue: integer;
begin
  Result := theValue;
end;

procedure TJLabeledIntegerEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJLabeledIntegerEdit.setValue(const AValue: integer);
begin
  theValue := AValue;
  formatInput;
end;

function TJLabeledIntegerEdit.IsValidInteger(const Value: string): boolean;
begin
  if StrToIntDef(Value, MaxInt) = MaxInt then
    Result := False
  else
    Result := True;
end;

procedure TJLabeledIntegerEdit.FormatInput;
begin
  Text := FormatFloat(fFormat, theValue);
end;

procedure TJLabeledIntegerEdit.DoEnter;
begin
  inherited DoEnter;
  Text := IntToStr(theValue);
  SelectAll;
end;

procedure TJLabeledIntegerEdit.DoExit;
begin
  inherited DoExit;
  if IsValidInteger(Text) then
    theValue := StrToInt(Text)
  else
  begin
    ShowMessage(Text + ' no es un valor válido');
    SetFocus;
  end;
  formatInput;
end;

procedure TJLabeledIntegerEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '-']) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJLabeledIntegerEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // Set initial values
  Text := '';
  DisplayFormat := '0';
  Value := 0;
end;

destructor TJLabeledIntegerEdit.Destroy;
begin
  inherited Destroy;
end;

end.

