unit JTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics,
  Dialogs, jcontrolutils, jinputconsts;

type

  { TJTimeEdit }

  TJTimeEdit = class(TCustomEdit)
  private
    { Private declarations }
    theValue: TTime;
    hasValue: boolean;
    fFormat: string;
    function getFormat: string;
    function getValue: TTime;
    procedure setFormat(const AValue: string);
    procedure setValue(const AValue: TTime);
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
    function isNull: boolean;
    property DisplayFormat: string read getFormat write setFormat;
    property Value: TTime read getValue write setValue;

    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderStyle;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I jtimeedit_icon.lrs}
  RegisterComponents('Additional', [TJTimeEdit]);
end;

{ TJTimeEdit }

function TJTimeEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJTimeEdit.getValue: TTime;
begin
  Result := theValue;
end;

procedure TJTimeEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJTimeEdit.setValue(const AValue: TTime);
begin
  theValue := AValue;
  hasValue := True;
  formatInput;
end;

procedure TJTimeEdit.FormatInput;
begin
  if hasValue then
    Text := FormatDateTime(fFormat, theValue)
  else
    Text := '';
end;

procedure TJTimeEdit.DoEnter;
begin
  inherited DoEnter;
  if not hasValue then
    Text := ''
  else
    Text := TimeToStr(theValue);
  SelectAll;
end;

procedure TJTimeEdit.DoExit;
begin
  inherited DoExit;
  Text := NormalizeTime(Text, theValue);
  if Length(Text) = 0 then
  begin
    theValue := 0;
    hasValue := False;
  end
  else
  if IsValidTimeString(Text) then
  begin
    theValue := StrToTime(Text);
    hasValue := True;
  end
  else
  begin
    ShowMessage(Format(SInvalidTime, [Text]));
    SetFocus;
  end;
  formatInput;
end;

procedure TJTimeEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, ':']) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJTimeEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Text := '';
  DisplayFormat := 'hh:mm:ss';
  Value := 0;
  hasValue := True;
end;

destructor TJTimeEdit.Destroy;
begin
  inherited Destroy;
end;

function TJTimeEdit.isNull: boolean;
begin
  Result := not hasValue;
end;

end.

