program signaltoclassmethod;

{$mode objfpc}{$H+}

uses
  Classes, Math, GLib2, GObject2, Gtk3, Gdk3, sysutils;

type

  { TClassApp }

  TClassApp = class
  private
    Win: PGtkWindow;
    Button: PGtkButton;
    procedure CreateWidgets;
    // these procedures are signal handlers. they are called when a signal is emitted
    function MouseMove(Event: PGdkEvent; Widget: PGtkWidget): gboolean; cdecl;
    procedure WindowDestroy(AWidget: PGtkWidget); cdecl;
    procedure ButtonClick(AWidget: PGtkWidget); cdecl;
    procedure MapWindow(AWidget: PgtkWidget); cdecl;
  public
    constructor Create;
    procedure Run;
  end;

procedure ConnectWidgetSignalToMethod(AGObject: PGObject; ASignalName: String; AMethod: TMethod; Flags: TGConnectFlags);
begin
  Flags := Flags or G_CONNECT_SWAPPED;
  g_signal_connect_data(AGObject, PChar(ASignalName), TGCallback(AMethod.Code), AMethod.Data, nil, Flags);
end;

{ TClassApp }

procedure TClassApp.CreateWidgets;
begin
  // first create window
  Win := TGtkWindow.new(GTK_WINDOW_TOPLEVEL);
  Win^.set_title('Hello World!');
  Win^.set_size_request(700,200);

  //then create a button and add it to the window
  Button := TGtkButton.new_with_label('Set TMethod class name as title');
  Win^.add(PGtkWidget(Button));

  // now connect signals
  ConnectWidgetSignalToMethod(Win, 'destroy', TMethod(@WindowDestroy), 0);
  ConnectWidgetSignalToMethod(Win, 'map', TMethod(@MapWindow), 0);
  ConnectWidgetSignalToMethod(Button, 'clicked', TMethod(@ButtonClick), 0);
  ConnectWidgetSignalToMethod(Win, 'motion-notify-event', TMethod(@MouseMove),0);

  Win^.show_all;
end;

function TClassApp.MouseMove(Event: PGdkEvent; Widget: PGtkWidget): gboolean; cdecl;
var
Str: String;
begin
  Str := 'Mouse move: '+ g_type_name_from_instance(PGTypeInstance(Widget))
      + ' For Class: ' +  Self.ClassName +  ' at ' + IntTostr(Round(Event^.motion.x))
      + ':' + IntToStr(Round(Event^.motion.y));
  Win^.title:=PChar(Str);

  Result := True;
end;

constructor TClassApp.Create;
begin
  gtk_init(@argc, @argv);
  CreateWidgets;
end;

procedure TClassApp.Run;
begin
  gtk_main;
end;

procedure TClassApp.WindowDestroy(AWidget: PGtkWidget); cdecl;
begin
  if gtk_main_level > 0 then
    gtk_main_quit;
end;

procedure TClassApp.ButtonClick(AWidget: PGtkWidget); cdecl;
var
  name: String;
begin
  name := Self.ClassName;
  Win^.set_title(PChar(name));
end;

procedure TClassApp.MapWindow(AWidget: PgtkWidget); cdecl;
var
mask : TGdkEventMask;
begin
  mask := gdk_window_get_events(Win^.window);
  mask := mask or GDK_POINTER_MOTION_MASK;
  gdk_window_set_events(Win^.window, mask);
end;

begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  with TClassApp.Create do
  begin
    Run;
    Free;
  end;
end.

