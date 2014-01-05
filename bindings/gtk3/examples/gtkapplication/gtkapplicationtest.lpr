program gtkapplicationtest;

{$mode objfpc}{$H+}

uses
  Classes, Math, GLib2, GObject2, Gtk3, Gio2;

var
  App: PGtkApplication;
  Win: PGtkApplicationWindow;

procedure activate(app: PGtkApplication; user_data: gpointer); cdecl;
begin
  Win := TGtkApplicationWindow.new(App);
  Win^.show_all;
end;

procedure CreateApplication;
begin
  App := TGtkApplication.new('org.gtkbinding.applicationtest', G_APPLICATION_NON_UNIQUE);
  g_signal_connect_data(App, 'activate', TGCallback(@activate), nil, nil, G_CONNECT_AFTER);
  App^.register(nil);
end;
procedure RunApplication;
begin
  App^.run(argc, @argv);
end;

begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  CreateApplication;
  RunApplication;
end.

