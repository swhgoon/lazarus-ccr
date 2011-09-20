unit JLabeledDateEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics,
  Dialogs, jcontrolutils;

type

  { TJLabeledDateEdit }

  TJLabeledDateEdit = class(TCustomLabeledEdit)
  private
    theValue: TDateTime;
    fFormat: string;
    function getFormat: string;
    function getValue: TDateTime;
    procedure formatInput;
    procedure setFormat(const AValue: string);
    procedure setValue(const AValue: TDateTime);
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
    function isNull: boolean;
    property DisplayFormat: string read getFormat write setFormat;
    property Value: TDateTime read getValue write setValue;

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
  {$I jlabeleddateedit_icon.lrs}
  RegisterComponents('Additional', [TJLabeledDateEdit]);
end;

{ TJLabeledDateEdit }

function TJLabeledDateEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJLabeledDateEdit.getValue: TDateTime;
begin
  Result := theValue;
end;

procedure TJLabeledDateEdit.formatInput;
begin
  if theValue <> 0 then
    Text := FormatDateTime(DisplayFormat, theValue);
end;

procedure TJLabeledDateEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJLabeledDateEdit.setValue(const AValue: TDateTime);
begin
  theValue := AValue;
  formatInput;
end;

procedure TJLabeledDateEdit.DoEnter;
begin
  inherited DoEnter;
  if theValue <> 0 then
    Text := FormatDateTime(DisplayFormat, theValue)
  else
    Text := '';
  SelectAll;
end;

procedure TJLabeledDateEdit.DoExit;
begin
  inherited DoExit;
  Text := NormalizeDate(Text, theValue);
  if Length(Text) = 0 then
    theValue := 0
  else
  if IsValidDateString(Text) then
    theValue := StrToDate(Text)
  else
  begin
    ShowMessage(Text + ' no es una fecha válida');
    SetFocus;
  end;
  formatInput;
end;

procedure TJLabeledDateEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/']) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJLabeledDateEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Text := '';
  fFormat := ShortDateFormat;
  theValue := 0;
  formatInput;
end;

destructor TJLabeledDateEdit.Destroy;
begin
  inherited Destroy;
end;

function TJLabeledDateEdit.isNull: boolean;
begin
  Result := theValue = 0;
end;

end.

