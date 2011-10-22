program dialog;

{$mode objfpc}{$H+}

uses
  Classes, Math, GLib2, GObject2 ,Gtk3;


procedure show_info(widget: PGtkWidget; window: PGtkWindow); cdecl;
var
  dialog: PGtkMessageDialog;
begin
  dialog := gtk_message_dialog_new(window,
            GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_INFO,
            GTK_BUTTONS_OK,
            'Download Completed', ['title']);
  dialog^.set_title('Information');
  dialog^.run;
  dialog^.destroy_;
end;

procedure show_error(widget: PGtkWidget; window: PGtkWindow); cdecl;
var
  dialog: PGtkMessageDialog;
begin
  dialog := gtk_message_dialog_new(window,
            GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_ERROR,
            GTK_BUTTONS_OK,
            'Error loading file', []);
  dialog^.set_title('Error');
  dialog^.run;
  dialog^.destroy_;
end;

procedure show_question(widget: PGtkWidget; window: PGtkWindow); cdecl;
var
  dialog: PGtkMessageDialog;
begin
  dialog := gtk_message_dialog_new(window,
            GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_QUESTION,
            GTK_BUTTONS_YES_NO,
            'Are you sure to quit?',[]);
  dialog^.set_title('Question');
  if dialog^.run = GTK_RESPONSE_YES then
    window^.destroy_
  else
    dialog^.destroy_;
end;

procedure show_warning(widget: PGtkWidget; window: PGtkWindow); cdecl;
var
  dialog: PGtkMessageDialog;
begin
  dialog := gtk_message_dialog_new(window,
            GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_WARNING,
            GTK_BUTTONS_OK,
            'Unallowed operation', []);
  dialog^.set_title('Warning');
  dialog^.run;
  dialog^.destroy_;
end;

var
  window : PgtkWindow;
  table: PGtkTable;
  info,
  warn,
  que,
  err: PGtkButton;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  WriteLn('Init starting');

  gtk_init(@argc, @argv);  WriteLn('Init done');

  window := TGtkWindow.new(GTK_WINDOW_TOPLEVEL);
  window^.set_position(GTK_WIN_POS_CENTER);
  window^.set_default_size(260, 150);
  window^.set_title('Message dialogs');

  table := TGtkTable.new(2, 2, TRUE);
  table^.set_row_spacings(2);
  table^.set_col_spacings(2);

  info := TGtkButton.new_with_label('Info');
  warn := TGtkButton.new_with_label('Warning');
  que := TGtkButton.new_with_label('Question');
  err := TGtkButton.new_with_label('Error');

  table^.attach(info, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 3, 3);
  table^.attach(warn, 1, 2, 0, 1, GTK_FILL, GTK_FILL, 3, 3);
  table^.attach(que, 0, 1, 1, 2,  GTK_FILL, GTK_FILL, 3, 3);
  table^.attach(err, 1, 2, 1, 2,  GTK_FILL, GTK_FILL, 3, 3);

  window^.add(PGtkWidget(table));
  window^.set_border_width(15);

  g_signal_connect_data(info, 'clicked', TGCallback(@show_info), window, nil, 0);

  g_signal_connect_data(warn, 'clicked', TGCallback(@show_warning), window, nil, 0);

  g_signal_connect_data(que, 'clicked', TGCallback(@show_question), window, nil, 0);

  g_signal_connect_data(err, 'clicked', TGCallback(@show_error), window, nil, 0);

  g_signal_connect_data(window, 'destroy', TGCallback(@gtk_main_quit), window, nil, G_CONNECT_SWAPPED);

  window^.show_all;

  gtk_main;
end.

