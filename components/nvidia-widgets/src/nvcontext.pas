//
// nvWidgets.h - User Interface library
//
//
// Author: Ignacio Castano, Samuel Gateau, Evan Hart
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
////////////////////////////////////////////////////////////////////////////////
unit nvContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nvTypes, nvPainter;

type

  { UIContext }

  UIContext = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;

    // UI method for processing window size events
    //////////////////////////////////////////////////////////////////
    procedure reshape(w, h: integer);

    //
    // Check if the UI is currently on Focus
    //////////////////////////////////////////////////////////////////
    function isOnFocus: boolean;

    //
    // UI method for processing mouse events
    //////////////////////////////////////////////////////////////////
    procedure mouse(button, state, modifier, x, y: integer);
    procedure mouse(button, state, x, y: integer);

    //
    // UI method for processing mouse motion events
    //////////////////////////////////////////////////////////////////
    procedure mouseMotion(x, y: integer);


    // UI method for processing key events
    //////////////////////////////////////////////////////////////////
    procedure keyboard(k: byte; x, y: integer);

    //
    // UI method for entering UI processing mode
    //
    //  This function must be used to begin the UI processing
    //////////////////////////////////////////////////////////////////
    procedure _begin;

    // UI method for leaving UI processing mode
    //
    //  This function must be used to end the UI processing
    //////////////////////////////////////////////////////////////////
    procedure _end; virtual;

    ////////////////////////////////////////////////////////////////////////////

    // UI element processing
    //
    // The following methods provide the interface for rendering and querying
    // UI objects. These methods must be called between begin/end.
    ////////////////////////////////////////////////////////////////////////////

    //
    // UI method for drawing a static text label
    // The label display a non interactive text.
    // The text can have multiple lines
    //
    // rect - optionally provides a location and size for the label
    // text - Text to display for the label (can have several lines)
    //////////////////////////////////////////////////////////////////
    procedure doLabel(const r: Rect; const Text: string; style: integer = 0);

    //
    // UI method for rendering and processing a push button
    //
    // rect - optionally provides a location and size for the button
    // text - text to display on the button
    // state -  whether the button is depressed
    //          if state is NULL; the buttoin behave like a touch button
    //          else; the button behave like a toggle button
    // style - optional style flag to modify the look
    //
    // @return  True if the button'state changed
    //////////////////////////////////////////////////////////////////
    function doButton(const r: Rect; const Text: string; var state: boolean; style: integer = 0): boolean;
    function doButton(const r: Rect; const Text: string): boolean;

    // UI method for rendering and processing a check button
    // Check button behaves similarly as a toggle button
    // used to display and edit a bool property.
    //
    // rect - optionally provides a location and size for the button
    // text - text to display on the button
    // state -  whether the check button is checked or not
    //          if state is NULL; the button behave like if a touch button unchecked
    // style - optional style flag to modify the look
    //
    // @return  True if the check button state changed
    //////////////////////////////////////////////////////////////////
    function doCheckButton(const r: Rect; const Text: string; var state: boolean; style: integer = 0): boolean;

    //
    // UI method for rendering and processing a radio button
    // Radio buttons are typically used in groups to diplay and edit
    // the possible reference values taken by an int value.
    //
    // One radio button is representing a possible reference value taken by the current value.
    // It is displaying a boolean state true if the current value is equal to the reference value.
    //
    // reference - The reference int value represented by this radio button.
    // rect - optionally provides a location and size for the button
    // text - text to display on the button
    // value -  The value parameter compared to the reference value parameter.
    //          if value is NULL; the radio button is off
    // style - optional style flag to modify the look
    //
    // @return  True if the radio button value changed
    //////////////////////////////////////////////////////////////////
    function doRadioButton(reference: integer; const r: Rect; const Text: string; var Value: integer; style: integer = 0): boolean;

    //
    // UI method for rendering and processing a horizontal slider
    // Horizontal slider is used to edit and display a scalar value in the specified range [min; max].
    //
    // rect - optionally provides a location and size for the widget
    // min - min bound of the varying range of the value
    // max - max bound of the varying range of the value
    // value -  the value edited by the widget
    //          if value is NULL; the value is set to min
    // style - optional style flag to modify the look
    //
    // @return  True if the slider value changed
    //////////////////////////////////////////////////////////////////
    function doHorizontalSlider(const aRect: Rect; min: double; max: double; var Value: double; style: integer = 0): boolean;

    function doListItem(index: integer; const aRect: Rect; const Text: string; selected: integer; style: integer = 0): boolean;
    function doListBox(const aRect: Rect; numOptions: integer; const options: array of string; var selected: integer; style: integer = 0): boolean;
    function doComboBox(const aRect: Rect; numOptions: integer; const options: array of string; var selected: integer; style: integer = 0): boolean;

    function doLineEdit(const aRect: Rect; var Text: string; maxTextLength: integer; out nbCharsReturned: integer; style: integer = 0): boolean;

    procedure beginGroup(groupFlags: integer; const r: Rect); overload;
    procedure beginGroup(groupFlags: integer = GroupFlags_LayoutDefault); overload;
    procedure endGroup;

    procedure beginFrame(groupFlags: integer = GroupFlags_LayoutDefault; const rect: Rect = 0; style: integer = 0);
    procedure endFrame;

    function beginPanel(var r: Rect; const Text: string; var isUnfold: boolean; groupFlags: integer = GroupFlags_LayoutDefault; style: integer = 0): boolean;
    procedure endPanel;

    function getGroupWidth: integer;
    function getGroupHeight: integer;

    function getCursorX: integer;
    function getCursorY: integer;

    function getMouseState(button: integer): ButtonState;

    //
    // UI method for drawing a texture view
    // Several parameters control the equation used to display the texel
    //      texel = texture2DFetch(...);
    //      pixel = texelSwizzling( texel * texelScale + texelOffset );
    //
    // rect - provides a location and size for the texture view
    // texID - texture identifier (Graphics API specific)
    // zoomRect - rectangle area of the texture displayed
    // mipLevel - mip Level of the texture displayed
    // texelScale - scale applyed to the texel fetch
    // texelOffset - offset applyed to the texel after scale
    // texelSwizzling - swizzle applyed to the texel (after scale and offset)
    // style - optional style flag to modify the look
    //////////////////////////////////////////////////////////////////
    procedure doTextureView(const aRect: Rect; const texID: integer; zoomRect: Rect; mipLevel: integer = -1; texelScale: double = 1; texelOffset: double = 0; red: integer = 0; green: integer = 1; blue: integer = 2; alpha: integer = 3; style: integer = 0);

  protected
    function window: Rect;

  private
    FPainter: UIPainter;
    procedure setCursor(x: integer; y: integer);

    function overlap(const aRect: Rect; const p: Point): boolean;

    function hasFocus(const aRect: Rect): boolean;
    function isHover(const aRect: Rect): boolean;
    procedure SetPainter(AValue: UIPainter);
  protected
    function placeRect(const r: Rect): Rect;

  published
    property Painter: UIPainter read FPainter write SetPainter;

  protected
    m_groupIndex: integer;
    m_groupStack: array [0..63] of Group;

    m_window: Rect;

    m_currentCursor: Point;
    m_mouseButton: array [0..2] of ButtonState;
    m_keyBuffer: array [0..31] of byte;
    m_nbKeys: integer;

    m_focusCaretPos: integer;
    m_focusPoint: Point;
    m_twoStepFocus: boolean;
    m_uiOnFocus: boolean;
  end;

implementation

uses
  Math;

{ UIContext }

constructor UIContext.Create;
begin
  m_twoStepFocus := False;
  m_focusCaretPos := -1;
end;

destructor UIContext.Destroy;
begin
  if Assigned(Painter) then
    Painter.Free;

  inherited Destroy;
end;

procedure UIContext.reshape(w, h: integer);
begin
  m_window.x := 0;
  m_window.y := 0;
  m_window.w := w;
  m_window.h := h;
end;

function UIContext.isOnFocus: boolean;
begin
  Result := m_uiOnFocus;
end;

procedure UIContext.mouse(button, state, modifier, x, y: integer);
var
  idx: integer;
begin
  setCursor(x, y);

  idx := -1;
  case button of
    MouseButton_Left: idx := 0;
    MouseButton_Middle: idx := 1;
    MouseButton_Right: idx := 2;
  end;

  modifier := modifier and (ButtonFlags_Alt or ButtonFlags_Shift or ButtonFlags_Ctrl);

  if idx >= 0 then
  begin
    if state = 1 then
    begin
      m_mouseButton[idx].state := ButtonFlags_On or ButtonFlags_Begin or modifier;
      m_mouseButton[idx].time := Now;
      m_mouseButton[idx].cursor.x := x;
      m_mouseButton[idx].cursor.y := m_window.h - y;
    end;
    if state = 0 then
      m_mouseButton[idx].state := ButtonFlags_On or ButtonFlags_End or modifier;
  end;
end;

procedure UIContext.mouse(button, state, x, y: integer);
begin
  mouse(button, state, 0, x, y);
end;

procedure UIContext.mouseMotion(x, y: integer);
begin
  setCursor(x, y);
end;

procedure UIContext.keyboard(k: byte; x, y: integer);
begin
  setCursor(x, y);
  m_keyBuffer[m_nbKeys] := k;
  Inc(m_nbKeys);
end;

procedure UIContext._begin;
begin
  Painter._begin(m_window);

  m_groupIndex := 0;
  m_groupStack[m_groupIndex].flags := GroupFlags_LayoutNone;
  m_groupStack[m_groupIndex].margin := Painter.getCanvasMargin;
  m_groupStack[m_groupIndex].space := Painter.getCanvasSpace;
  m_groupStack[m_groupIndex].bounds := m_window;
end;

procedure UIContext._end;
var
  i: integer;
begin
  Painter._end;

  // Release focus.
  if (m_mouseButton[0].state and ButtonFlags_End) > 0 then
    m_uiOnFocus := False;

  // Update state for next frame.
  for i := 0 to 2 do
  begin
    if (m_mouseButton[i].state and ButtonFlags_Begin) > 0 then
      m_mouseButton[i].state := m_mouseButton[i].state xor ButtonFlags_Begin;
    //else
    if (m_mouseButton[i].state and ButtonFlags_End) > 0 then
      m_mouseButton[i].state := ButtonFlags_Off;
  end;

  // Flush key buffer
  m_nbKeys := 0;
end;

procedure UIContext.doLabel(const r: Rect; const Text: string; style: integer);
var
  rt: Rect;
  nbLines: integer;
  aRect: Rect;
begin
  aRect := placeRect(Painter.getLabelRect(r, Text, rt, nbLines));
  Painter.drawLabel(aRect, Text, rt, nbLines, isHover(aRect), style);
end;

function UIContext.doButton(const r: Rect; const Text: string; var state: boolean; style: integer): boolean;
var
  rt: Rect;
  aRect: Rect;
  focus: boolean;
  hover: boolean;
  isDown: boolean;
begin
  aRect := placeRect(Painter.getButtonRect(r, Text, rt));
  focus := hasFocus(aRect);
  hover := isHover(aRect);

  isDown := state;
  //isDown := ((m_mouseButton[0].state and ButtonFlags_On)>0) and hover and focus;

  Painter.drawButton(aRect, Text, rt, isDown, hover, focus, style);

  if not focus then
    m_uiOnFocus := True;

  if ((m_mouseButton[0].state and ButtonFlags_End) > 0) and focus and overlap(aRect, m_currentCursor) then
  begin
    state := not state;
    Result := True;
    exit;
  end;

  Result := False;
end;

function UIContext.doButton(const r: Rect; const Text: string): boolean;
var
  tmp: boolean = False;
begin
  Result := doButton(r, Text, tmp);
end;

function UIContext.doCheckButton(const r: Rect; const Text: string; var state: boolean; style: integer): boolean;
var
  rt: Rect;
  rc: Rect;
  aRect: Rect;
  focus: boolean;
  hover: boolean;
begin
  aRect := placeRect(Painter.getCheckRect(r, Text, rt, rc));
  focus := hasFocus(aRect);
  hover := isHover(aRect);
  Painter.drawCheckButton(aRect, Text, rt, rc, state, hover, focus, style);

  if hasFocus(aRect) then
    m_uiOnFocus := True;

  if ((m_mouseButton[0].state and ButtonFlags_End) > 0) and focus and overlap(aRect, m_currentCursor) then
  begin
    state := not state;
    Result := True;
    exit;
  end;

  Result := False;
end;

function UIContext.doRadioButton(reference: integer; const r: Rect; const Text: string; var Value: integer; style: integer): boolean;
var
  rr: Rect;
  rt: Rect;
  aRect: Rect;
  focus: boolean;
  hover: boolean;
begin
  aRect := placeRect(Painter.getRadioRect(r, Text, rt, rr));
  focus := hasFocus(aRect);
  hover := isHover(aRect);
  Painter.drawRadioButton(aRect, Text, rt, rr, longbool(Value and EvalBool(reference = Value)), hover, focus, style);

  if focus then
    m_uiOnFocus := True;

  if ((m_mouseButton[0].state and ButtonFlags_End) > 0) and focus and overlap(aRect, m_currentCursor) then
  begin
    Value := reference;
    Result := True;
    exit;
  end;

  Result := False;
end;

function UIContext.doHorizontalSlider(const aRect: Rect; min: double; max: double; var Value: double; style: integer): boolean;
var
  f: double;
  rs: Rect;
  rc: Rect;
  rr: Rect;
  changed: boolean = False;
  xs: integer;
  x: integer;
begin
  // Map current value to 0-1.
  f := (Value - min) / (max - min);

  if f < 0 then
    f := 0
  else
  if f > 1 then
    f := 1;

  rr := placeRect(Painter.getHorizontalSliderRect(aRect, rs, f, rc));

  if hasFocus(rr) then
  begin
    m_uiOnFocus := True;
    xs := rr.x + rs.x + rc.w div 2;
    x := m_currentCursor.x - xs;

    if x < 0 then
      x := 0
    else
    if x > rs.w then
      x := rs.w;

    rc.x := x;

    f := x / rs.w;
    f := f * (max - min) + min;

    if abs(f - Value) > (max - min) * 0.01 then
    begin
      changed := True;
      Value := f;
    end;
  end;

  Painter.drawHorizontalSlider(rr, rs, f, rc, isHover(rr), style);

  Result := changed;
end;

function UIContext.doListItem(index: integer; const aRect: Rect; const Text: string; selected: integer; style: integer): boolean;
var
  rt: Rect;
  r: Rect;
begin
  r := placeRect(Painter.getItemRect(aRect, Text, rt));
  Painter.drawListItem(r, Text, rt, longbool(selected and EvalBool(index = selected)), isHover(r), style);

  Result := isHover(r);
end;

function UIContext.doListBox(const aRect: Rect; numOptions: integer; const options: array of string; var selected: integer; style: integer): boolean;
var
  ri: Rect;
  rt: Rect;
  rr: Rect;
  focus: boolean;
  hover: boolean;
  hovered: integer = -1;
  lSelected: integer = -1;
begin
  rr := placeRect(Painter.getListRect(aRect, numOptions, options, ri, rt));
  focus := hasFocus(rr);
  hover := isHover(rr);

  if hover then
    hovered := numOptions - 1 - (m_currentCursor.y - (rr.y + ri.y)) div (ri.h);

  if selected <> 0 then
    lSelected := selected;

  Painter.drawListBox(rr, numOptions, options, ri, rt, lSelected, hovered, style);

  if focus then
    m_uiOnFocus := True;

  if ((m_mouseButton[0].state and ButtonFlags_End) > 0) and focus and overlap(rr, m_currentCursor) and (lSelected <> hovered) then
  begin
    selected := hovered;
    Result := True;
    exit;
  end;

  Result := False;
end;

function UIContext.doComboBox(const aRect: Rect; numOptions: integer; const options: array of string; var selected: integer; style: integer): boolean;
var
  rt: Rect;
  ra: Rect;
  rr: Rect;
  focus: boolean;
  hover: boolean;
  ro: Rect;
  ri: Rect;
  rit: Rect;
  hovered: integer;
  hoverOptions: boolean;
begin
  // First get the rect of the combobox itself and do some test with it
  rr := placeRect(Painter.getComboRect(aRect, numOptions, options, selected, rt, ra));
  focus := hasFocus(rr);
  hover := isHover(rr);

  if focus then
  begin
    m_uiOnFocus := True;

    // then if the combo box has focus, we can look for the geometry of the options frame
    ro := Painter.getComboOptionsRect(rr, numOptions, options, ri, rit);
    hovered := -1;
    hoverOptions := overlap(ro, m_currentCursor);

    if hoverOptions then
      hovered := numOptions - 1 - (m_currentCursor.y - (ro.y + ri.y)) div (ri.h);

    // draw combo anyway
    Painter.drawComboBox(rr, numOptions, options, rt, ra, selected, hover, focus, style);

    // draw options
    Painter.drawComboOptions(ro, numOptions, options, ri, rit, selected, hovered, hover, focus, style);

    // When the widget get the focus, cache the focus point
    if not m_twoStepFocus then
    begin
      if hover and ((m_mouseButton[0].state and ButtonFlags_End) > 0) then
      begin
        m_focusPoint := m_mouseButton[0].cursor;
        m_twoStepFocus := True;
      end;
    end
    else
    begin
      // Else Release the 2level focus when left mouse down out or up anyway
      // replace the stored left mouse down pos with the focus point to keep focus
      // on this widget for the next widget rendered in the frame
      if not (hoverOptions or hover) and
        (((m_mouseButton[0].state and ButtonFlags_Begin) > 0) or
        ((m_mouseButton[0].state and ButtonFlags_End > 0))) then
        m_twoStepFocus := False
      else
      if (hoverOptions or hover) and ((m_mouseButton[0].state and ButtonFlags_End) > 0) then
      begin
        m_mouseButton[0].cursor := m_focusPoint;
        m_twoStepFocus := False;
      end;

      if hoverOptions and ((m_mouseButton[0].state and ButtonFlags_Begin) > 0) then
        m_mouseButton[0].cursor := m_focusPoint;
    end;

    // On mouse left bouton up, then select it
    if (hovered > -1) and (hovered < numOptions) and ((m_mouseButton[0].state and ButtonFlags_End) > 0) then
    begin
      selected := hovered;
      Result := True;
      exit;
    end;
  end
  else
    Painter.drawComboBox(rr, numOptions, options, rt, ra, selected, hover, focus, style);

  Result := False;
end;

function UIContext.doLineEdit(const aRect: Rect; var Text: string; maxTextLength: integer; out nbCharsReturned: integer; style: integer): boolean;
var
  rt: Rect;
  rr: Rect;
  focus: boolean;
  hover: boolean;
  _result: boolean = False;
  carretPos: integer = -1;
  textLength: integer;
  nbKeys: integer;
  keyNb: integer;
begin
  rr := placeRect(Painter.getLineEditRect(aRect, Text, rt));
  focus := hasFocus(rr);
  hover := isHover(rr);

  if focus then
  begin
    m_uiOnFocus := True;

    // When the widget get the focus, cache the focus point
    if not m_twoStepFocus then
    begin
      m_twoStepFocus := True;
      m_focusPoint := SetPoint(rr.x, rr.y);
    end
    else
    begin
      // Else Release the 2level focus when left mouse down out or up anyway
      // replace the stored left mouse down pos with the focus point to keep focus
      // on this widget for the next widget rendered in the frame
      if not hover and
        (((m_mouseButton[0].state and ButtonFlags_Begin) > 0) or
        ((m_mouseButton[0].state and ButtonFlags_End) > 0)) then
      begin
        m_twoStepFocus := False;
        m_focusCaretPos := -1;
      end;

      if hover and ((m_mouseButton[0].state and ButtonFlags_Begin) > 0) then
        m_mouseButton[0].cursor := m_focusPoint;
    end;

    // Eval caret pos on every click hover
    if hover and ((m_mouseButton[0].state and ButtonFlags_Begin) > 0) then
      m_focusCaretPos := Painter.getPickedCharNb(Text, SetPoint(m_currentCursor.x - rt.x - rr.x, m_currentCursor.y - rt.y - rr.y));

    // If keys are buffered, apply input to the edited text
    if m_nbKeys <> 0 then
    begin
      textLength := Length(Text);

      if m_focusCaretPos = -1 then
        m_focusCaretPos := textLength;

      nbKeys := m_nbKeys;
      keyNb := 0;

      while nbKeys <> 0 do
      begin
        // filter for special keys
        // Enter, quit edition
        if m_keyBuffer[keyNb] = 13 then
        begin
          nbKeys := 1;
          m_twoStepFocus := False;
          m_focusCaretPos := -1;
        end
        else
        // Special keys
        if m_keyBuffer[keyNb] >= Key_F1 then
        begin
          case m_keyBuffer[keyNb] of
            Key_Left:
              // move cursor left one char
              if m_focusCaretPos > 0 then
                Dec(m_focusCaretPos);
            Key_Right:
              // move cursor right one char
              if m_focusCaretPos <= textLength then
                Inc(m_focusCaretPos);
            Key_Home:
              m_focusCaretPos := 0;
            Key_End:
              m_focusCaretPos := textLength + 1;
            Key_Insert:
            begin
            end
            else
              // strange key pressed...
              Dec(m_focusCaretPos);
          end;
        end
        else
        // Delete, move the chars >= carret back 1, carret stay in place
        if m_keyBuffer[keyNb] = 127 then
          Delete(Text,m_focusCaretPos, 1)
        else
        // Backspace, move the chars > carret back 1, carret move back 1
        if m_keyBuffer[keyNb] = 8 then
        begin
          if m_focusCaretPos > 0 then
          begin
            Delete(Text,m_focusCaretPos-1, 1);

            Dec(m_focusCaretPos);
            Dec(textLength);
          end;
        end
        else
        // Regular char, append it to the edited string
        if textLength < maxTextLength then
        begin
          Insert(Chr(m_keyBuffer[keyNb]), Text, m_focusCaretPos);

          Inc(m_focusCaretPos);
          Inc(textLength);
        end;

        Inc(keyNb);
        Dec(nbKeys);
      end;
      nbCharsReturned := textLength;
      _result := True;
    end;
    carretPos := m_focusCaretPos;
  end;

  Painter.drawLineEdit(rr, Text, rt, carretPos, focus, hover, style);

  Result := _result;
end;

procedure UIContext.beginGroup(groupFlags: integer; const r: Rect);
var
  parentGroup: Group;
  newGroup: PGroup;
  parentLayout: integer;
  parentAlign: integer;
  newAlign: integer;
  newStart: integer;
  //newLayout: integer;
  aRect: Rect;
begin
  // Push one more group.
  parentGroup := m_groupStack[m_groupIndex];
  Inc(m_groupIndex);
  newGroup := @m_groupStack[m_groupIndex];

  // Assign layout behavior
  parentLayout := parentGroup.flags and GroupFlags_LayoutMask;
  parentAlign := parentGroup.flags and GroupFlags_AlignMask;

  // If the groupFlags ask to force the layout then keep the newcanvas layout as is
  // otherwise, adapt it to the parent's behavior
  if ((groupFlags and GroupFlags_LayoutForce) = 0) or ((groupFlags and GroupFlags_LayoutNone) = 0) then
  begin
    // If default then use parent style except if none layout => default fallback
    if (groupFlags and GroupFlags_LayoutDefault) > 0 then
    begin
      if (parentLayout and GroupFlags_LayoutNone) > 0 then
        groupFlags := GroupFlags_LayoutDefaultFallback
      else
        groupFlags := parentGroup.flags;
    end
    else
    if ((parentLayout and (GroupFlags_LayoutVertical or GroupFlags_LayoutHorizontal)) > 0) and
      ((groupFlags and (GroupFlags_LayoutVertical or GroupFlags_LayoutHorizontal)) > 0) then
      groupFlags := (groupFlags and GroupFlags_AlignXMask) or parentAlign;
  end;

  newGroup^.margin := EvalBool((groupFlags and GroupFlags_LayoutNoMargin) = 0) * Painter.getCanvasMargin;
  newGroup^.space := EvalBool((groupFlags and GroupFlags_LayoutNoSpace) = 0) * Painter.getCanvasSpace;
  newGroup^.flags := groupFlags;

  //newLayout := groupFlags and GroupFlags_LayoutMask;
  newAlign := groupFlags and GroupFlags_AlignMask;
  newStart := groupFlags and GroupFlags_StartMask;

  // Place a regular rect in current group, this will be the new group rect start pos
  aRect := r;

  // Don't modify parent group bounds yet, done in endGroup
  // Right now place the new group rect
  if parentLayout = GroupFlags_LayoutNone then
  begin
    // Horizontal behavior.
    aRect.x := aRect.x + (parentGroup.bounds.x + newGroup^.margin + EvalBool((newStart and GroupFlags_StartRight) > 0) * parentGroup.bounds.w - EvalBool((newAlign and GroupFlags_AlignRight) > 0) * (2 * newGroup^.margin + aRect.w));
    // Vertical behavior.
    aRect.y := aRect.y + (parentGroup.bounds.y + newGroup^.margin + EvalBool((newStart and GroupFlags_StartTop) > 0) * parentGroup.bounds.h - EvalBool((newAlign and GroupFlags_AlignTop) > 0) * (2 * newGroup^.margin + aRect.h));
  end
  else
    if parentLayout = GroupFlags_LayoutVertical then
    begin
      // Horizontal behavior.
      aRect.x := aRect.x + (parentGroup.bounds.x + newGroup^.margin + EvalBool((parentAlign and GroupFlags_AlignRight) > 0) * (parentGroup.bounds.w - 2 * newGroup^.margin - aRect.w));

      // Vertical behavior.
      if (parentAlign and GroupFlags_AlignTop) > 0 then
        aRect.y := aRect.y + (parentGroup.bounds.y - (EvalBool(parentGroup.bounds.h > 0) * parentGroup.space) - newGroup^.margin - aRect.h)
      else
        aRect.y := aRect.y + (parentGroup.bounds.y + parentGroup.bounds.h + EvalBool(parentGroup.bounds.h > 0) * parentGroup.space + newGroup^.margin);
    end
    else
      if parentLayout = GroupFlags_LayoutHorizontal then
      begin
        // Horizontal behavior.
        if (parentAlign and GroupFlags_AlignRight) > 0 then
          aRect.x := aRect.x + (parentGroup.bounds.x - (EvalBool(parentGroup.bounds.w > 0) * parentGroup.space) - newGroup^.margin - aRect.w)
        else
          aRect.x := aRect.x + (parentGroup.bounds.x + parentGroup.bounds.w + EvalBool(parentGroup.bounds.w > 0) * parentGroup.space + newGroup^.margin);

        // Vertical behavior.
        aRect.y := aRect.y + (parentGroup.bounds.y + newGroup^.margin + EvalBool((parentAlign and GroupFlags_AlignTop) > 0) * (parentGroup.bounds.h - 2 * newGroup^.margin - aRect.h));
      end;

  newGroup^.bounds := aRect;
end;

procedure UIContext.beginGroup(groupFlags: integer);
var
  r: Rect;
begin
  r.Rect(0, 0, 0, 0);
  beginGroup(groupFlags, r);
end;

procedure UIContext.endGroup;
var
  newGroup: Group;
  parentGroup: PGroup;
  maxBoundX: integer;
  minBoundX: integer;
  maxBoundY: integer;
  minBoundY: integer;
begin
  // Pop the new group.
  newGroup := m_groupStack[m_groupIndex];
  Dec(m_groupIndex);
  parentGroup := @m_groupStack[m_groupIndex];

  // add any increment from the embedded group
  if (parentGroup^.flags and (GroupFlags_LayoutVertical or GroupFlags_LayoutHorizontal)) > 0 then
  begin
    maxBoundX := max(parentGroup^.bounds.x + parentGroup^.bounds.w, newGroup.bounds.x + newGroup.bounds.w + newGroup.margin);
    minBoundX := min(parentGroup^.bounds.x, newGroup.bounds.x - newGroup.margin);
    parentGroup^.bounds.x := minBoundX;
    parentGroup^.bounds.w := maxBoundX - minBoundX;
    maxBoundY := max(parentGroup^.bounds.y + parentGroup^.bounds.h, newGroup.bounds.y + newGroup.bounds.h + newGroup.margin);
    minBoundY := min(parentGroup^.bounds.y, newGroup.bounds.y - newGroup.margin);
    parentGroup^.bounds.y := minBoundY;
    parentGroup^.bounds.h := maxBoundY - minBoundY;
  end;

  {$IFDEF DEBUG} Painter.drawDebugRect(newGroup.bounds); {$ENDIF}
end;

procedure UIContext.beginFrame(groupFlags: integer; const rect: Rect; style: integer);
begin
  beginGroup(groupFlags, rect);
end;

procedure UIContext.endFrame;
begin
  endGroup;
  Painter.drawFrame(m_groupStack[m_groupIndex + 1].bounds, m_groupStack[m_groupIndex + 1].margin, 0);
end;

function UIContext.beginPanel(var r: Rect; const Text: string; var isUnfold: boolean; groupFlags: integer; style: integer): boolean;
var
  rt: Rect;
  ra: Rect;
  rpanel: Rect;
  aRect: Rect;
  focus: boolean;
  hover: boolean;
  tmp: Rect;
begin
  rpanel := Painter.getPanelRect(SetRect(r.x, r.y), Text, rt, ra);

  if (groupFlags and GroupFlags_LayoutDefault) > 0 then
    groupFlags := GroupFlags_LayoutDefaultFallback;

  beginGroup((groupFlags or GroupFlags_LayoutNoMargin or GroupFlags_LayoutNoSpace) and GroupFlags_StartXMask, rpanel);

  aRect := m_groupStack[m_groupIndex].bounds;

  focus := hasFocus(aRect);
  hover := isHover(aRect);

  if focus then
  begin
    m_uiOnFocus := True;
    r.x := r.x + (m_currentCursor.x - m_mouseButton[0].cursor.x);
    r.y := r.y + (m_currentCursor.y - m_mouseButton[0].cursor.y);
    aRect.x := aRect.x + (m_currentCursor.x - m_mouseButton[0].cursor.x);
    aRect.y := aRect.y + (m_currentCursor.y - m_mouseButton[0].cursor.y);
    m_mouseButton[0].cursor := m_currentCursor;
  end;

  if ((m_mouseButton[0].state and ButtonFlags_End) > 0) and focus and (overlap(SetRect(aRect.x + ra.x, aRect.y + ra.y, ra.w, ra.h), m_currentCursor)) then
    isUnfold := not isUnfold;

  Painter.drawPanel(aRect, Text, rt, ra, isUnfold, hover, focus, style);

  if isUnfold then
  begin
    tmp.Rect(0, 0, r.w, r.h);
    beginFrame(groupFlags, tmp);
    Result := True;
  end
  else
  begin
    endGroup;
    Result := False;
  end;
end;

procedure UIContext.endPanel;
begin
  endFrame;
  endGroup;
end;

function UIContext.getGroupWidth: integer;
begin
  Result := m_groupStack[m_groupIndex].bounds.w;
end;

function UIContext.getGroupHeight: integer;
begin
  Result := m_groupStack[m_groupIndex].bounds.h;
end;

function UIContext.getCursorX: integer;
begin
  Result := m_currentCursor.x;
end;

function UIContext.getCursorY: integer;
begin
  Result := m_currentCursor.y;
end;

function UIContext.getMouseState(button: integer): ButtonState;
begin
  Result := m_mouseButton[button];
end;

procedure UIContext.doTextureView(const aRect: Rect; const texID: integer; zoomRect: Rect; mipLevel: integer; texelScale: double; texelOffset: double; red: integer; green: integer; blue: integer; alpha: integer; style: integer);
var
  rt: Rect;
  rr: Rect;
begin
  rr := placeRect(Painter.getTextureViewRect(aRect, rt));
  if (zoomRect.w = 0) or (zoomRect.h = 0) then
    zoomRect.Rect(0, 0, rt.w, rt.h);

  Painter.drawTextureView(rr, texID, rt, zoomRect, mipLevel, texelScale, texelOffset, red, green, blue, alpha, style);
end;

function UIContext.window: Rect;
begin
  Result := m_window;
end;

procedure UIContext.setCursor(x: integer; y: integer);
begin
  m_currentCursor.x := x;
  m_currentCursor.y := m_window.h - y;
end;

function UIContext.overlap(const aRect: Rect; const p: Point): boolean;
begin
  Result := (p.x >= aRect.x) and (p.x < aRect.x + aRect.w) and
            (p.y >= aRect.y) and (p.y < aRect.y + aRect.h);
end;

function UIContext.hasFocus(const aRect: Rect): boolean;
begin
  if m_twoStepFocus then
    Result := overlap(aRect, m_focusPoint)
  else
    Result := ((m_mouseButton[0].state and ButtonFlags_On) > 0) and overlap(aRect, m_mouseButton[0].cursor);
end;

function UIContext.isHover(const aRect: Rect): boolean;
begin
  if m_uiOnFocus and not hasFocus(aRect) then
    Result := False
  else
    Result := overlap(aRect, m_currentCursor);
end;

procedure UIContext.SetPainter(AValue: UIPainter);
begin
  if FPainter=AValue then
    exit;

  FPainter:=AValue;
end;

function UIContext.placeRect(const r: Rect): Rect;
var
  aGroup: PGroup;
  aRect: Rect;
  layout: integer;
  alignment: integer;
  minBoundX: integer;
  minBoundY: integer;
begin
  aGroup := @m_groupStack[m_groupIndex];
  aRect := r;
  layout := aGroup^.flags and GroupFlags_LayoutMask;
  alignment := aGroup^.flags and GroupFlags_AlignMask;

  if layout = GroupFlags_LayoutNone then
  begin
    // Translate rect to absolute coordinates.
    aRect.x := aRect.x + (aGroup^.bounds.x);
    aRect.y := aRect.y + (aGroup^.bounds.y);
  end
  else
  if layout = GroupFlags_LayoutVertical then
  begin
    // Vertical behavior
    if (alignment and GroupFlags_AlignTop) > 0 then
    begin
      // Move down bounds.y with the space for the new rect
      aGroup^.bounds.y -= EvalBool(aGroup^.bounds.h > 0) * aGroup^.space + aRect.h;

      // Widget's y is the group bounds.y
      aRect.y := aGroup^.bounds.y;
    end
    else
      aRect.y := aGroup^.bounds.y + aGroup^.bounds.h + EvalBool(aGroup^.bounds.h > 0) * aGroup^.space;

    // Add space after first object inserted in the group
    aGroup^.bounds.h += EvalBool(aGroup^.bounds.h > 0) * aGroup^.space + aRect.h;

    // Horizontal behavior
    if (alignment and GroupFlags_AlignRight) > 0 then
    begin
      aRect.x := aRect.x + (aGroup^.bounds.x + aGroup^.bounds.w - aRect.w);
      minBoundX := min(aGroup^.bounds.x, aRect.x);
      aGroup^.bounds.w := aGroup^.bounds.x + aGroup^.bounds.w - minBoundX;
      aGroup^.bounds.x := minBoundX;
    end
    else
    begin
      aGroup^.bounds.w := max(aGroup^.bounds.w, aRect.x + aRect.w);
      aRect.x := aRect.x + (aGroup^.bounds.x);
    end;
  end
  else
  if (layout = GroupFlags_LayoutHorizontal) then
  begin
    // Horizontal behavior
    if (alignment and GroupFlags_AlignRight) > 0 then
    begin
      // Move left bounds.x with the space for the new rect
      aGroup^.bounds.x -= EvalBool(aGroup^.bounds.w > 0) * aGroup^.space + aRect.w;

      // Widget's x is the group bounds.x
      aRect.x := aGroup^.bounds.x;
    end
    else
      aRect.x := aGroup^.bounds.x + aGroup^.bounds.w + EvalBool(aGroup^.bounds.w > 0) * aGroup^.space;

    // Add space after first object inserted in the group
    aGroup^.bounds.w += EvalBool(aGroup^.bounds.w > 0) * aGroup^.space + aRect.w;

    // Vertical behavior
    if (alignment and GroupFlags_AlignTop) > 0 then
    begin
      aRect.y := aRect.y + (aGroup^.bounds.y + aGroup^.bounds.h - aRect.h);
      minBoundY := min(aGroup^.bounds.y, aRect.y);
      aGroup^.bounds.h := aGroup^.bounds.y + aGroup^.bounds.h - minBoundY;
      aGroup^.bounds.y := minBoundY;
    end
    else
    begin
      aGroup^.bounds.h := max(aGroup^.bounds.h, aRect.y + aRect.h);
      aRect.y := aRect.y + (aGroup^.bounds.y);
    end;
  end;
  Result := aRect;
end;

end.

