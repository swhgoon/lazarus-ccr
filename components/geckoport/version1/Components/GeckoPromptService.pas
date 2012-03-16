unit GeckoPromptService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nsXPCOM, nsTypes;

const
  NS_PROMPT_SERVICE_CID: TGUID =    '{a2112d6a-0e28-421f-b46a-25c0b308cbd0}';

type
  TGeckoPromptAlert=procedure(Sender: TObject; const aDialogTitle: UTF8String; const aText: UTF8String) of object;
  TGeckoPromptAlertCheck=procedure (Sender: TObject; const aDialogTitle: UTF8String; const aText: UTF8String; const aCheckMsg: UTF8String; out aCheckState: Boolean) of object;
  TGeckoPromptConfirm=procedure(Sender: TObject; const aDialogTitle: UTF8String; const aText: UTF8String; var aCancel: Boolean) of object;
  TGeckoPromptConfirmCheck=procedure(Sender: TObject; const aDialogTitle: UTF8String; const aText: UTF8String; const aCheckMsg: UTF8String; out aCheckState: Boolean; var aCancel: Boolean) of object;
  TGeckoPromptConfirmEx=procedure(Sender: TObject; const aDialogTitle: UTF8String; const aText: UTF8String; aButtonFlags: Cardinal; const aButton0Title: UTF8String; const aButton1Title: UTF8String; const aButton2Title: UTF8String; const aCheckMsg: UTF8String; out aCheckState: Boolean; out aSelectedButton) of object;
  TGeckoPromptPrompt=procedure(Sender: TObject; const aDialogTitle: UTF8String; const aText: UTF8String; var aValue: UTF8String; const aCheckMsg: UTF8String; var aCheckState: Boolean; var aCancel: Boolean) of object;
  TGeckoPromptPromptUsernameAndPassword=procedure(Sender: TObject; const aDialogTitle: UTF8String; const aText: UTF8String; out aUsername: UTF8String; out aPassword: UTF8String; const aCheckMsg: UTF8String; out aCheckState: Boolean; var aCancel: Boolean) of object;
  TGeckoPromptPromptPassword=procedure(Sender: TObject; const aDialogTitle: UTF8String; const aText: UTF8String; out aPassword: UTF8String; const aCheckMsg: UTF8String; out aCheckState: Boolean; var aCancel: Boolean) of object;
  TGeckoPromptSelect=procedure(Sender: TObject; const aDialogTitle: UTF8String; const aText: UTF8String; aCount: Cardinal; const aSelectList_array; out aOutSelection: integer; var aCancel: Boolean) of object;
  TGeckoPromptPromptAuth=procedure(Sender: TObject;const aChannel: nsIChannel;const level: integer;const authInfo: nsIAuthInformation;const checkboxLabel: UTF8String;var checkValue: Boolean; var aRetryOnFail: Boolean) of object;
  TGeckoPromptAsyncPromptAuth=procedure(Sender: TObject;aChannel: nsIChannel;aCallback: nsIAuthPromptCallback;aContext: nsISupports;level: Cardinal;authInfo: nsIAuthInformation;checkboxLabel: UTF8String;var checkValue: Boolean; var nsICancelable) of object;
  TGeckoPromptPromptAuthSimple=procedure(Sender: TObject;var aUserName,aPassword: UTF8String;const aCheckBoxText,aRealm: UTF8String;var aCheckValue: Boolean; var aRetryOnFail: Boolean) of object;

  { IPromptService }

  IPromptService = class (TInterfacedObject,
                          nsIPromptService,
                          nsIPromptService2)
  private
  public
    //nsIPromptService
    procedure Alert(aParent: nsIDOMWindow; const aDialogTitle: PWideChar; const aText: PWideChar); safecall;
    procedure AlertCheck(aParent: nsIDOMWindow; const aDialogTitle: PWideChar; const aText: PWideChar; const aCheckMsg: PWideChar; out aCheckState: PRBool); safecall;
    function Confirm(aParent: nsIDOMWindow; const aDialogTitle: PWideChar; const aText: PWideChar): PRBool; safecall;
    function ConfirmCheck(aParent: nsIDOMWindow; const aDialogTitle: PWideChar; const aText: PWideChar; const aCheckMsg: PWideChar; out aCheckState: PRBool): PRBool; safecall;
    function ConfirmEx(aParent: nsIDOMWindow; const aDialogTitle: PWideChar; const aText: PWideChar; aButtonFlags: PRUint32; const aButton0Title: PWideChar; const aButton1Title: PWideChar; const aButton2Title: PWideChar; const aCheckMsg: PWideChar; out aCheckState: PRBool): PRInt32; safecall;
    function Prompt(aParent: nsIDOMWindow; const aDialogTitle: PWideChar; const aText: PWideChar; var aValue: PWideChar; const aCheckMsg: PWideChar; var aCheckState: PRBool): PRBool; safecall;
    function PromptUsernameAndPassword(aParent: nsIDOMWindow; const aDialogTitle: PWideChar; const aText: PWideChar; out aUsername: PWideChar; out aPassword: PWideChar; const aCheckMsg: PWideChar; out aCheckState: PRBool): PRBool; safecall;
    function PromptPassword(aParent: nsIDOMWindow; const aDialogTitle: PWideChar; const aText: PWideChar; out aPassword: PWideChar; const aCheckMsg: PWideChar; out aCheckState: PRBool): PRBool; safecall;
    function Select(aParent: nsIDOMWindow; const aDialogTitle: PWideChar; const aText: PWideChar; aCount: PRUint32; const aSelectList_array; out aOutSelection: PRInt32): PRBool; safecall;
    //nsIPromptService2
    function PromptAuth(aParent: nsIDOMWindow; aChannel: nsIChannel; level: PRUint32; authInfo: nsIAuthInformation; const checkboxLabel: PWideChar; var checkValue: LongBool): LongBool; safecall;
    function AsyncPromptAuth(aParent: nsIDOMWindow; aChannel: nsIChannel; aCallback: nsIAuthPromptCallback; aContext: nsISupports; level: PRUint32; authInfo: nsIAuthInformation; const checkboxLabel: PWideChar; var checkValue: LongBool): nsICancelable; safecall;
  end;

  { IPromptServiceFactory }

  IPromptServiceFactory = class(TInterfacedObject,nsIFactory)
    procedure CreateInstance(aOuter: nsISupports; constref iid: TGUID; out _result); safecall;
    procedure LockFactory(lock: PRBool); safecall;
  end;

  TCustomGeckoPrompt=class(TComponent)
  private
  protected
    FOnAlert:                     TGeckoPromptAlert;
    FOnAlertCheck:                TGeckoPromptAlertCheck;
    FOnConfirm:                   TGeckoPromptConfirm;
    FOnConfirmCheck:              TGeckoPromptConfirmCheck;
    FOnConfirmEx:                 TGeckoPromptConfirmEx;
    FOnPrompt:                    TGeckoPromptPrompt;
    FOnPromptUsernameAndPassword: TGeckoPromptPromptUsernameAndPassword;
    FOnPromptPassword:            TGeckoPromptPromptPassword;
    FOnSelect:                    TGeckoPromptSelect;
    FOnPromptAuth:                TGeckoPromptPromptAuth;
    FOnAsyncPromptAuth:           TGeckoPromptAsyncPromptAuth;
    FOnPromptAuthSimple:          TGeckoPromptPromptAuthSimple;
  public
    property Alert:                     TGeckoPromptAlert
      read FOnAlert                     write FOnAlert;
    property AlertCheck:                TGeckoPromptAlertCheck
      read FOnAlertCheck                write FOnAlertCheck;
    property Confirm:                   TGeckoPromptConfirm
      read FOnConfirm                   write FOnConfirm;
    property ConfirmCheck:              TGeckoPromptConfirmCheck
      read FOnConfirmCheck              write FOnConfirmCheck;
    property ConfirmEx:                 TGeckoPromptConfirmEx
      read FOnConfirmEx                 write FOnConfirmEx;
    property Prompt:                    TGeckoPromptPrompt
      read FOnPrompt                    write FOnPrompt;
    property PromptUsernameAndPassword: TGeckoPromptPromptUsernameAndPassword
      read FOnPromptUsernameAndPassword write FOnPromptUsernameAndPassword;
    property PromptPassword:            TGeckoPromptPromptPassword
      read FOnPromptPassword            write FOnPromptPassword;
    property Select:                    TGeckoPromptSelect
      read FOnSelect                    write FOnSelect;
    property PromptAuth:                TGeckoPromptPromptAuth
      read FOnPromptAuth                write FOnPromptAuth;
    property AsyncPromptAuth:           TGeckoPromptAsyncPromptAuth
      read FOnAsyncPromptAuth           write FOnAsyncPromptAuth;
    property PromptAuthSimple:          TGeckoPromptPromptAuthSimple
      read FOnPromptAuthSimple          write FOnPromptAuthSimple;
  end;

  TGeckoPrompt=class(TCustomGeckoPrompt)
  private
  protected
  public
  published
    property Alert;
    property AlertCheck;
    property Confirm;
    property ConfirmCheck;
    property ConfirmEx;
    property Prompt;
    property PromptUsernameAndPassword;
    property PromptPassword;
    property Select;
    property PromptAuth;
    property AsyncPromptAuth;
    property PromptAuthSimple;
  end;

  procedure RegisterPromptService;

procedure Register;

implementation

uses nsInit,nsGeckoStrings,GeckoBrowser,nsMemory;

var
  GeckoPromptServiceFactory: IPromptServiceFactory;
  ThisGeckoPromptService: IPromptService;

procedure Register;
begin
  RegisterComponents('Gecko', [TGeckoPrompt]);
end;

function UnImplemented: Boolean;
begin
  (*
    This warning only appears when used in the Lazarus IDE.
    Otherwise the exception is eaten by the try...except and
    default behavior is executed.
  *)
  try
    Raise exception.Create('GeckoPort unimplemented feature in TGeckoPrompt. Please report at http://sourceforge.net/projects/lazarus-ccr/ or at http://forum.lazarus.freepascal.org/');
  except
  end;
  Result:=false;
end;

procedure nsParamStringUpdate(var pwParamString: PWideChar; const aNewString: UTF8String);
var
  wNewString: WideString;
  BufferSize: SizeInt;
begin
  if Assigned(pwParamString) then begin
    nsMemory.Free(pwParamString);
    pwParamString:=nil;
  end;
  if Length(aNewString)>0 then begin
    wNewString:=UTF8Decode(aNewString);
    BufferSize:=(Length(wNewString)+1)*sizeof(wNewString[1]); //chars + 1 NULL
    pwParamString:=nsMemory.Alloc(BufferSize);
    move(wNewString[1],pwParamString^,BufferSize);
  end;
end;

function FindAssociatedPromptService(const aParent: nsIDOMWindow; out AGecko: TGeckoBrowser): TGeckoPrompt;
var
  Gecko: TGeckoBrowser;
begin
  Gecko:=TGeckoBrowser.GetGeckoBrowserWithDOMWindow(aParent);
  if Assigned(Gecko) then begin
    Result:=Gecko.Prompt;
    AGecko:=Gecko;
  end else begin
    Result:=nil;
    AGecko:=nil;
  end;
end;

procedure RegisterPromptService;
var
  PromptService: nsIPromptService;
  ComponentRegistrar: nsIComponentRegistrar;
begin
  if not Assigned(GeckoPromptServiceFactory) then begin
    NS_GetComponentRegistrar(ComponentRegistrar);
    GeckoPromptServiceFactory:=IPromptServiceFactory.Create;
    ComponentRegistrar.RegisterFactory(NS_PROMPT_SERVICE_CID,'Prompt Service',nil,GeckoPromptServiceFactory);
  end;
end;

{ IPromptService }

procedure IPromptService.Alert(aParent: nsIDOMWindow;
  const aDialogTitle: PWideChar; const aText: PWideChar); safecall;
begin
  Unimplemented;
end;

procedure IPromptService.AlertCheck(aParent: nsIDOMWindow;
  const aDialogTitle: PWideChar; const aText: PWideChar;
  const aCheckMsg: PWideChar; out aCheckState: PRBool); safecall;
begin
  Unimplemented;
end;

function IPromptService.Confirm(aParent: nsIDOMWindow;
  const aDialogTitle: PWideChar; const aText: PWideChar): PRBool; safecall;
begin
  Result:=Unimplemented;
end;

function IPromptService.ConfirmCheck(aParent: nsIDOMWindow;
  const aDialogTitle: PWideChar; const aText: PWideChar;
  const aCheckMsg: PWideChar; out aCheckState: PRBool): PRBool; safecall;
begin
  Result:=Unimplemented;
end;

function IPromptService.ConfirmEx(aParent: nsIDOMWindow;
  const aDialogTitle: PWideChar; const aText: PWideChar;
  aButtonFlags: PRUint32; const aButton0Title: PWideChar;
  const aButton1Title: PWideChar; const aButton2Title: PWideChar;
  const aCheckMsg: PWideChar; out aCheckState: PRBool): PRInt32; safecall;
begin
  UnImplemented;
  Result:=0;
end;

function IPromptService.Prompt(aParent: nsIDOMWindow;
  const aDialogTitle: PWideChar; const aText: PWideChar; var aValue: PWideChar;
  const aCheckMsg: PWideChar; var aCheckState: PRBool): PRBool; safecall;
var
  ThePrompt: TGeckoPrompt;
  Gecko: TGeckoBrowser;
  TheValue: UTF8String;
  Cancel: Boolean;
  CheckState: Boolean;
begin
  Result:=false;
  ThePrompt:=FindAssociatedPromptService(aParent,Gecko);
  if Assigned(ThePrompt) then begin
    if Assigned(ThePrompt.FOnPrompt) then begin
      TheValue:=UTF8Encode(WideString(aValue));
      CheckState:=aCheckState;
      Cancel:=false;
      ThePrompt.FOnPrompt(Gecko,
                UTF8Encode(WideString(aDialogTitle)),
                UTF8Encode(WideString(aText)),
                TheValue,
                UTF8Encode(WideString(aCheckMsg)),
                CheckState,
                Cancel);
      Result:=not Cancel;
      if Result then begin
        nsParamStringUpdate(aValue,TheValue);
        aCheckState:=CheckState;
      end;
    end;
  end;
end;

function IPromptService.PromptUsernameAndPassword(aParent: nsIDOMWindow;
  const aDialogTitle: PWideChar; const aText: PWideChar; out
  aUsername: PWideChar; out aPassword: PWideChar; const aCheckMsg: PWideChar;
  out aCheckState: PRBool): PRBool; safecall;
begin
  Result:=Unimplemented;
end;

function IPromptService.PromptPassword(aParent: nsIDOMWindow;
  const aDialogTitle: PWideChar; const aText: PWideChar; out
  aPassword: PWideChar; const aCheckMsg: PWideChar; out aCheckState: PRBool
  ): PRBool; safecall;
begin
  Result:=Unimplemented;
end;

function IPromptService.Select(aParent: nsIDOMWindow;
  const aDialogTitle: PWideChar; const aText: PWideChar; aCount: PRUint32;
  const aSelectList_array; out aOutSelection: PRInt32): PRBool; safecall;
begin
  Result:=Unimplemented;
end;

function IPromptService.PromptAuth(aParent: nsIDOMWindow; aChannel: nsIChannel;
  level: PRUint32; authInfo: nsIAuthInformation;
  const checkboxLabel: PWideChar; var checkValue: LongBool): LongBool; safecall;
var
  ThePrompt: TGeckoPrompt;
  Gecko: TGeckoBrowser;
  RetryOnFail: Boolean;
  UserName, Password,
  Realm: IInterfacedString;
  CheckBoxValue: Boolean;
  pUserName,pPassword: UTF8String;
  Flags: PRUint32;
begin
  Result:=false;
  ThePrompt:=FindAssociatedPromptService(aParent,Gecko);
  if Assigned(ThePrompt) then begin
    if Assigned(ThePrompt.FOnPromptAuth) then begin
      //Full Auth mode
      RetryOnFail:=false;
      CheckBoxValue:=checkValue;
      ThePrompt.FOnPromptAuth(Gecko,aChannel,Level,authInfo,UTF8Encode(WideString(checkboxLabel)),CheckBoxValue,RetryOnFail);
      checkValue:=CheckBoxValue;
      Result:=RetryOnFail;
    end else if Assigned(ThePrompt.FOnPromptAuthSimple) then begin
      //Simple Auth mode only for host, proxy and others must be handled by OnPromptAuth
      authInfo.GetFlags(Flags);
      if (Flags and not(NS_IAUTHINFORMATION_AUTH_HOST))=0 then begin
        UserName:=NewString;
        Password:=NewString;
        Realm:=NewString;
        authInfo.GetUsername(UserName.AString);
        authInfo.GetPassword(Password.AString);
        authInfo.GetRealm(Realm.AString);
        pUserName:=UTF8Encode(UserName.ToString);
        pPassword:=UTF8Encode(Password.ToString);
        CheckBoxValue:=checkValue;
        RetryOnFail:=false;
        ThePrompt.FOnPromptAuthSimple(Gecko,pUserName,pPassword,UTF8Encode(WideString(checkboxLabel)),UTF8Encode(Realm.ToString),CheckBoxValue,RetryOnFail);
        Result:=RetryOnFail;
        UserName.Assign(UTF8Decode(pUserName));
        Password.Assign(UTF8Decode(pPassword));
        checkValue:=CheckBoxValue;
        authInfo.SetUsername(UserName.AString);
        authInfo.SetPassword(Password.AString);
      end;
    end;
  end;
end;

function IPromptService.AsyncPromptAuth(aParent: nsIDOMWindow;
  aChannel: nsIChannel; aCallback: nsIAuthPromptCallback;
  aContext: nsISupports; level: PRUint32; authInfo: nsIAuthInformation;
  const checkboxLabel: PWideChar; var checkValue: LongBool): nsICancelable; safecall;
begin
  Result:=nil;
end;

{ IPromptServiceFactory }

procedure IPromptServiceFactory.CreateInstance(aOuter: nsISupports; constref
  iid: TGUID; out _result); safecall;
begin
  if not Assigned(ThisGeckoPromptService) then
    ThisGeckoPromptService:=IPromptService.Create;
  ThisGeckoPromptService.QueryInterface(IID,_result);
end;

procedure IPromptServiceFactory.LockFactory(lock: PRBool); safecall;
begin
  //Unused by Gecko but keep to be ABI compatible in Win32.
end;


end.

