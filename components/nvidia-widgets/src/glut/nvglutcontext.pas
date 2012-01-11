//
// nvGlutWidgets
//
//  Adaptor classes to integrate the nvWidgets UI library with the GLUT windowing
// toolkit. The adaptors convert native GLUT UI data to native nvWidgets data. All
// adaptor classes are implemented as in-line code in this header. The adaptor
// defaults to using the standard OpenGL painter implementation.
//
// Author: Ignacio Castano, Samuel Gateau, Evan Hart
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
////////////////////////////////////////////////////////////////////////////////////////////////////
unit nvGlutContext;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, nvContext, nvTypes;

type

  { GlutUIContext }

  GlutUIContext = class(UIContext)

  public

    //
    // One time initialization
    //
    //////////////////////////////////////////////////////////////////
    function init(w, h: integer): boolean;

    //
    // UI method for processing GLUT mouse button events
    //
    //  Call this method from the glutMouseFunc callback, the
    // modifier parameter maps to glutGetModifiers.
    //////////////////////////////////////////////////////////////////
    procedure mouse(button, state, modifier, x, y: integer);
    procedure mouse(button, state, x, y: integer);

    //
    // UI method for processing key events
    //
    //  Call this method from the glutReshapeFunc callback
    //////////////////////////////////////////////////////////////////
    procedure specialKeyboard(k, x, y: integer);

  private
    //
    //  Translate non-ascii keys from GLUT to nvWidgets
    //////////////////////////////////////////////////////////////////
    function translateKey(k: integer): byte;
  end;

implementation

uses
  GLut, GLext;

{ GlutUIContext }

function GlutUIContext.init(w, h: integer): boolean;
begin
  Result := False;

  if not Load_GL_version_2_0 then
  begin
    writeln('OpenGL version 2.0 not loaded properly');
    exit;
  end;

  if not glext_ExtensionSupported('GL_ARB_vertex_program', '') or
     not glext_ExtensionSupported('GL_ARB_fragment_program', '') then
    exit;

  reshape(w, h);
  Result := True;
end;

procedure GlutUIContext.mouse(button, state, modifier, x, y: integer);
var
  modifierMask: integer = 0;
begin
  if button = GLUT_LEFT_BUTTON then
    button := MouseButton_Left
  else
  if button = GLUT_MIDDLE_BUTTON then
    button := MouseButton_Middle
  else
  if button = GLUT_RIGHT_BUTTON then
    button := MouseButton_Right;

  if (modifier and GLUT_ACTIVE_ALT) = GLUT_ACTIVE_ALT then
    modifierMask := modifierMask or (ButtonFlags_Alt);
  if (modifier and GLUT_ACTIVE_SHIFT) = GLUT_ACTIVE_SHIFT then
    modifierMask := modifierMask or (ButtonFlags_Shift);
  if (modifier and GLUT_ACTIVE_CTRL) = GLUT_ACTIVE_CTRL then
    modifierMask := modifierMask or (ButtonFlags_Ctrl);

  if state = GLUT_DOWN then
    state := 1
  else
    state := 0;

  inherited mouse(button, state, modifierMask, x, y);
end;

procedure GlutUIContext.mouse(button, state, x, y: integer);
begin
  mouse(button, state, glutGetModifiers(), x, y);
end;

procedure GlutUIContext.specialKeyboard(k, x, y: integer);
begin
  inherited keyboard(translateKey(k), x, y);
end;

function GlutUIContext.translateKey(k: integer): byte;
begin
  case k of
    GLUT_KEY_F1:
      Result := Key_F1;
    GLUT_KEY_F2:
      Result := Key_F2;
    GLUT_KEY_F3:
      Result := Key_F3;
    GLUT_KEY_F4:
      Result := Key_F4;
    GLUT_KEY_F5:
      Result := Key_F5;
    GLUT_KEY_F6:
      Result := Key_F6;
    GLUT_KEY_F7:
      Result := Key_F7;
    GLUT_KEY_F8:
      Result := Key_F8;
    GLUT_KEY_F9:
      Result := Key_F9;
    GLUT_KEY_F10:
      Result := Key_F10;
    GLUT_KEY_F11:
      Result := Key_F11;
    GLUT_KEY_F12:
      Result := Key_F12;
    GLUT_KEY_LEFT:
      Result := Key_Left;
    GLUT_KEY_UP:
      Result := Key_Up;
    GLUT_KEY_RIGHT:
      Result := Key_Right;
    GLUT_KEY_DOWN:
      Result := Key_Down;
    GLUT_KEY_PAGE_UP:
      Result := Key_PageUp;
    GLUT_KEY_PAGE_DOWN:
      Result := Key_PageDown;
    GLUT_KEY_HOME:
      Result := Key_Home;
    GLUT_KEY_END:
      Result := Key_End;
    GLUT_KEY_INSERT:
      Result := Key_Insert;
    else
      Result := 0;
  end;
end;

end.

