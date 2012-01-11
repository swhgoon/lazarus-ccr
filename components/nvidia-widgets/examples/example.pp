program example;

{$mode objfpc}
{$H+}

uses
  Classes, SysUtils,
  GLut, GL, GLu,
  nvTypes, nvGlutContext, nvGLPainter, GLFreeTypeFont;

type
  UIOption = (
    OPTION_DIFF,
    OPTION_DXT5_YCOCG,
    OPTION_COMPRESS,
    OPTION_ANIMATE,
    OPTION_THUMBNAIL,
    OPTION_COUNT);

var
  options: array [UIOption] of boolean;
  ui: GlutUIContext;
  win_w: integer = 512;
  win_h: integer = 512;
  errorScale: double = 4;
  compressionRate: double = 1;
  texture: GLuint = 0;
  unfold: boolean = true;

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
    formatLabel: array [0..1] of string = ('YCoCg-DXT5', 'DXT1');
  var
    none: Rect;
    formatIdx: integer;
    Text: string;
    textureRect: Rect;
  begin
    none.Rect(0, 0);

    ui._begin;

      ui.beginGroup(GroupFlags_GrowDownFromLeft);

        ui.doCheckButton(none, 'Enable compression', options[OPTION_COMPRESS]);

        if options[OPTION_COMPRESS] then
        begin
          ui.beginGroup(GroupFlags_GrowLeftFromTop or GroupFlags_LayoutNoMargin);
            ui.doCheckButton(none, 'Show difference', options[OPTION_DIFF]);

            ui.beginPanel(none, 'panel', unfold);
              ui.doCheckButton(none, 'Show difference', options[OPTION_DIFF]);
            ui.endPanel;

            if options[OPTION_DIFF] then
              ui.doHorizontalSlider(none, 1, 16, errorScale);
          ui.endGroup;

          ui.beginGroup(GroupFlags_GrowLeftFromTop);
            ui.doLabel(none, 'Format');

            if options[OPTION_DXT5_YCOCG] then
              formatIdx := 0
            else
              formatIdx := 1;

            ui.doComboBox(none, 2, formatLabel, formatIdx);
            options[OPTION_DXT5_YCOCG] := formatIdx = 0;
          ui.endGroup;
        end;

        ui.doCheckButton(none, 'Display dummy texture', options[OPTION_THUMBNAIL]);

        if options[OPTION_THUMBNAIL] then
        begin
          textureRect.Rect(0, 0, 100, 100);
          ui.doTextureView(textureRect, texture, textureRect);
        end;

      ui.endGroup;

      if options[OPTION_COMPRESS] then
      begin
        ui.beginGroup(GroupFlags_GrowDownFromRight);

          if ui.doButton(none, 'Benchmark') then
          begin
            // doBenchmark = true;
          end;

          if compressionRate <> 0 then
          begin
            Text := Format('%.2d Mpixels/sec', [100]);
            ui.doLabel(none, Text);
          end;

        ui.endGroup;
      end;

      // Pass non-ui mouse events to the manipulator
      //updateManipulator(ui, manipulator);

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

