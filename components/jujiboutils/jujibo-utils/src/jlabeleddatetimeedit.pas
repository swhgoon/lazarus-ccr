{ TJDateTimeEdit

  Copyright (C) 2011 Julio Jiménez Borreguero
  Contact: jujibo at gmail dot com

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file license-jujiboutils.txt and COPYING.LGPL, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}



unit JLabeledDateTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, ExtCtrls, LCLType, Dialogs,
  SysUtils;

type
  TJLabeledDateTimeEdit = class(TCustomLabeledEdit)
  private
    { Private declarations }
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

uses
  jcontrolutils;

procedure Register;
begin
  {$I jlabeleddatetimeedit_icon.lrs}
  RegisterComponents('Additional',[TJLabeledDateTimeEdit]);
end;

function TJLabeledDateTimeEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJLabeledDateTimeEdit.getValue: TDateTime;
begin
  Result := theValue;
end;

procedure TJLabeledDateTimeEdit.formatInput;
begin
  if theValue <> 0 then
    Text := FormatDateTime(DisplayFormat, theValue);
end;

procedure TJLabeledDateTimeEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJLabeledDateTimeEdit.setValue(const AValue: TDateTime);
begin
  theValue := AValue;
  formatInput;
end;

procedure TJLabeledDateTimeEdit.DoEnter;
begin
  inherited DoEnter;
  if theValue <> 0 then
    Text := FormatDateTime(DisplayFormat, theValue)
  else
    Text := '';
  SelectAll;
end;

procedure TJLabeledDateTimeEdit.DoExit;
var
  bufText: string;
begin
  inherited DoExit;
  bufText := Text;
  Text := NormalizeDateTime(Text, theValue);
  if (Length(bufText) > 0) and (Length(Text) = 0) then
  begin
    ShowMessage(bufText + ' no es una fecha-hora válida');
    SetFocus;
  end
  else
  if Length(Text) = 0 then
    theValue := 0
  else
  if IsValidDateTimeString(Text) then
    theValue := StrToDateTime(Text)
  else
  begin
    ShowMessage(Text + ' no es una fecha-hora válida');
    SetFocus;
  end;
  formatInput;
end;

procedure TJLabeledDateTimeEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/', ',', ':', ' ']) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJLabeledDateTimeEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Text := '';
  fFormat := ShortDateFormat + ' ' + ShortTimeFormat;
  theValue := 0;
  formatInput;
end;

destructor TJLabeledDateTimeEdit.Destroy;
begin
  inherited Destroy;
end;

function TJLabeledDateTimeEdit.isNull: boolean;
begin
  Result := theValue = 0;
end;


end.
