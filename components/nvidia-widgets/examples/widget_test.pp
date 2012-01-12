program widget_test;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, GLut, GL, GLu, nvGLPainter, nvGlutContext, GLFreeTypeFont,
  GLFreeType, {GLUTBitmapFont,} nvTypes, nvPainter, nvContext;

var
  ui: GlutUIContext;
  win_w: integer = 512;
  win_h: integer = 512;
  texture: GLuint = 0;

  procedure closeapp;
  begin
    FreeAndNil(ui);
    halt(0);
  end;

  procedure idle; cdecl;
  begin
    glutPostRedisplay;
  end;

  procedure key(k: byte; x: integer; y: integer); cdecl;
  begin
    ui.keyboard(k, x, y);

    case k of
      27, Ord('q'):
        closeapp;
    end;
  end;

  procedure special(key: integer; x: integer; y: integer); cdecl;
  begin
    ui.specialKeyboard(key, x, y);
  end;

  procedure resize(w: integer; h: integer); cdecl;
  begin
    ui.reshape(w, h);
    glViewport(0, 0, w, h);

    win_w := w;
    win_h := h;
  end;

  procedure mouse(button: integer; state: integer; x: integer; y: integer); cdecl;
  begin
    ui.mouse(button, state, x, y);
  end;

  procedure motion(x: integer; y: integer); cdecl;
  begin
    ui.mouseMotion(x, y);
  end;

  procedure doUI;
  const
    formatLabel: array [0..1] of string = ('Combobox item 1', 'Combobox item 2');
    lbOptions: array [0..2] of string = ('Listbox item 1', 'Listbox item 2', 'Listbox item 2');
  var
    none: Rect;
    formatIdx: integer = 1;
    Text: string = 'line edit widget';
    textureRect: Rect;
    state_false: boolean = false;
    state_true: boolean = true;
    val: integer = 1;
    dval: double = 75;
  begin
    none.Rect(0, 0);

    ui._begin;

    //here follows the UI widget test code
    //each of these tests, will demonstrate the visual of a widget

    ui.beginGroup(GroupFlags_GrowDownFromLeft);

      //label
      ui.doLabel(none, 'Simple label (style=0)', 0);
      ui.doLabel(none, 'Simple label (style=1)', 1);

      //button
      ui.beginGroup(GroupFlags_GrowRightFromTop);
        ui.doButton(none, 'button up', state_false);
        ui.doButton(none, 'button down', state_true);
      ui.endGroup;

      //checkbutton
      ui.beginGroup(GroupFlags_GrowRightFromTop);
        ui.doCheckButton(none, 'checkbutton up (style=0)', state_false, 0);
        ui.doCheckButton(none, 'checkbutton down (style=0)', state_true, 0);
      ui.endGroup;
      ui.beginGroup(GroupFlags_GrowRightFromTop);
        ui.doCheckButton(none, 'checkbutton up (style=1)', state_false, 1);
        ui.doCheckButton(none, 'checkbutton down (style=1)', state_true, 1);
      ui.endGroup;

      //radio button
      ui.beginGroup(GroupFlags_GrowRightFromTop);
        ui.doRadioButton(2, none, 'radiobutton up (style=0)', val, 0);
        ui.doRadioButton(1, none, 'radiobutton down (style=0)', val, 0);
      ui.endGroup;
      ui.beginGroup(GroupFlags_GrowRightFromTop);
        ui.doRadioButton(2, none, 'radiobutton up (style=1)', val, 1);
        ui.doRadioButton(1, none, 'radiobutton down (style=1)', val, 1);
      ui.endGroup;

      //horizontal slider
      ui.doHorizontalSlider(none, 0, 100, dval, 0);

      ui.beginGroup(GroupFlags_GrowRightFromTop);
        //listbox
        val := 1;
        ui.doListBox(none, 3, lbOptions, val);

        //combobox
        ui.doComboBox(none, 2, formatLabel, formatIdx);
      ui.endGroup;

      //line edit
      val := 1;
      ui.beginGroup(GroupFlags_GrowRightFromTop);
        ui.doLineEdit(none, Text, 100, val);
        ui.doLineEdit(none, Text, 100, val);
      ui.endGroup;

      //panel
      ui.beginGroup(GroupFlags_GrowRightFromTop);
        ui.beginPanel(none, 'A folding panel', state_true);
          ui.beginGroup(GroupFlags_GrowRightFromTop);
            Text := './nvidia-widgets/trunk/src';
            ui.doLineEdit(none, Text, 100, val);
            ui.doButton(none, 'Browse', state_false);
          ui.endGroup;
        ui.endPanel;

        //ui.beginPanel(none, 'Same panel but now folded', state_false);
        //  ui.beginGroup(GroupFlags_GrowRightFromTop);
        //    ui.doLabel(none, './nvidia-widgets/trunk/src', 1);
        //    ui.doButton(none, 'Browse', state_true, 0);
        //  ui.endGroup;
        //ui.endPanel;
      ui.endGroup;

      //frame
      ui.beginGroup(GroupFlags_GrowDownFromLeft);
        ui.doLabel(none, 'This is a frame');
        ui.beginFrame(GroupFlags_GrowRightFromTop, none);
          ui.beginGroup(GroupFlags_GrowRightFromTop);
            Text := './nvidia-widgets/trunk/src';
            ui.doLineEdit(none, Text, 100, val);
            ui.doButton(none, 'Browse', state_false);
          ui.endGroup;
        ui.endFrame;
      ui.endGroup;

      //listitem
      ui.doListItem(1, none, 'List item 1', 1, 0);
      ui.doListItem(2, none, 'List item 2', 1, 1);
      ui.doListItem(3, none, 'List item 3', 1, 0);
      ui.doListItem(4, none, 'List item 4', 1, 0);

      //textureview
      textureRect.Rect(0, 0, 100, 100);
      ui.doTextureView(textureRect, texture, textureRect);

    ui.endGroup;

    ui._end;
  end;

  procedure display; cdecl;
  begin
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glMatrixMode(GL_MODELVIEW);

    glLoadIdentity;
    doUI;
    glutSwapBuffers;
  end;

var
  texture_data: array [0..3] of cardinal = ($FFFF0000, $FF0000FF, $FF00FF00, $FF00FF00);

begin
  glutInit(@argc, argv);
  glutInitWindowSize(win_w, win_h);
  glutInitDisplayMode(GLUT_DOUBLE or GLUT_DEPTH or GLUT_RGB);
  glutCreateWindow('UI example');

  ui := GlutUIContext.Create;
  ui.Painter := GLUIPainter.Create;
  ui.Painter.Font := TGLFreeTypeFont.Create('Ubuntu-R.ttf', 10);
  //ui.Painter.Font := TGLUTBitmapFont.Create('fixed', 15);

  if not ui.init(win_w, win_h) then
  begin
    writeln('UI initialization failed');
    closeapp;
  end;

  glutReportErrors;

  glGenTextures(1, @texture);
  glBindTexture(GL_TEXTURE_2D, texture);
  gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, 2, 2, GL_RGBA, GL_UNSIGNED_BYTE, @texture_data);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0, 0, 0, 1);
  glutDisplayFunc(@display);

  glutMouseFunc(@mouse);
  glutMotionFunc(@motion);
  glutPassiveMotionFunc(@motion);
  glutIdleFunc(@idle);
  glutKeyboardFunc(@key);
  glutSpecialFunc(@special);
  glutReshapeFunc(@resize);
  glutMainLoop;
end.

