unit appdelegate_iphoneu;

{$modeswitch ObjectiveC1}

interface

uses
  iPhoneAll;

type

  { TAppDelegate_iPhone }

  TAppDelegate_iPhone = objcclass(NSObject, UIApplicationDelegateProtocol)
    UIButton1: UIButton;
    UIButton2: UIButton;
    UIWindow1: UIWindow;
    procedure UIButton1TouchDown(sender: id); message 'UIButton1TouchDown:';
    procedure UIButton2TouchDown(sender: id); message 'UIButton2TouchDown:';
  private
    { private declarations }
  public
    procedure dealloc; override;
  end;

implementation

procedure TAppDelegate_iPhone.UIButton1TouchDown(sender: id);
begin
  UIButton1.setTitle_forState(NSSTR('Thank you'),UIControlStateNormal);
end;

procedure TAppDelegate_iPhone.UIButton2TouchDown(sender: id);
var
  AnAlertView: UIAlertView;
begin
  AnAlertView := UIAlertView.alloc.initWithTitle_message_delegate_cancelButtonTitle_otherButtonTitles(nil,NSSTR('Hello World!'),nil,nsstr('Ok'),nil);
  try
    AnAlertView.show;
  finally
    AnAlertView.release;
  end;
end;

procedure TAppDelegate_iPhone.dealloc;
begin
  UIButton1.dealloc;
  UIButton2.dealloc;
  UIWindow1.dealloc;
  inherited dealloc;
end;

{$FakeResource *.xib}

end.

