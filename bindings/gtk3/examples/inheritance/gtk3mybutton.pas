unit gtk3mybutton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GLib2, GObject2, Gtk3;

type

  { TMyButton }

  TObjectProcedure = procedure of object;

  PMyButton = ^TMyButton;
  TMyButton = object(TGtkButton)
  private
    FOnClicked: TObjectProcedure;
     saved_caption: String;
     function OnTimer: GBoolean; cdecl;
     procedure clicked;
     procedure init;
  public
     function get_type: TGType; static;
     function new: PMyButton; static;

     procedure QuitProgram;

     property OnClicked: TObjectProcedure read FOnClicked write FOnClicked;


  end;

  { TMyButtonClass }
  PMyButtonClass = ^TMyButtonClass;
  TMyButtonClass = object(TGtkButtonClass)
     origclicked: procedure(button: PGtkButton); cdecl;
     procedure init;
  end;


implementation
var
  MY_BUTTON_TYPE: TGType = 0;

{ TMyButtonClass }

procedure TMyButtonClass.init;
begin
  origclicked := clicked;
  Pointer(clicked) := @TMyButton.clicked;
end;

{ TMyButton }

function TMyButton.OnTimer: GBoolean; cdecl;
var
  klass: PMyButtonClass;
begin
  Result := False;
  label_ := PChar(saved_caption);
  saved_caption:='';

  if Assigned(FOnClicked) then
    FOnClicked;

  klass := PMyButtonClass(g_type_instance.g_class);


  if Assigned(klass^.origclicked) then
    klass^.origclicked(@self);




end;

procedure TMyButton.clicked;
begin
  // we'll add a delay and update the text. the timer calls the inherited method.
  g_timeout_add(3000, TGSourceFunc(@TMyButton.OnTimer), @Self);
  saved_caption:=label_;
  label_ := 'Wait for it...';
end;

procedure TMyButton.init;
begin

end;

function TMyButton.get_type: TGType;
var
  Info: TGTypeInfo;
begin
  if MY_BUTTON_TYPE = 0 then
  begin
    Info.class_size:=SizeOf(TMyButtonClass);
    Info.base_init:= nil;
    Info.base_finalize:=nil;
    Info.class_init:= TGClassInitFunc(@TMyButtonClass.init);
    Info.class_finalize:=nil;
    Info.class_data:=nil;
    Info.instance_size:=SizeOf(TMyButton);
    Info.n_preallocs:=0;
    Info.instance_init:=TGInstanceInitFunc(@TMyButton.init);
    Info.value_table:=nil;

    MY_BUTTON_TYPE := g_type_register_static(gtk_button_get_type, 'MyButton', @Info, 0);
  end;
  Result := MY_BUTTON_TYPE;
end;

function TMyButton.new: PMyButton;
begin
  Result := PMyButton(g_object_new(TMyButton.get_type, nil, []));
end;

procedure TMyButton.QuitProgram;
begin
  gtk_main_quit;
end;

end.

