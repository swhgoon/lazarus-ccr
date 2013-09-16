unit mod_testhttp;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls,
  browsermodules, browserconfig;

type

  { TformTestHttp }

  TformTestHttp = class(TForm)
    buttonVideoHEADTest: TButton;
    comboUserAgent: TComboBox;
    comboURL: TComboBox;
    editProxy: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    memoTestHttpDebug: TMemo;
    procedure buttonVideoHEADTestClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  { TTestHttpBrowserModule }

  TTestHttpBrowserModule = class(TBrowserModule)
  public
    constructor Create; override;
    function GetModuleUIElements(): TBrowserModuleUIElements; override;
    // For expansions
    function GetCommandCount: Integer; override;
    function GetCommandName(AID: Integer): string; override;
    procedure ExecuteCommand(AID: Integer); override;
  end;

var
  formTestHttp: TformTestHttp;

implementation

uses httpsend;

{ TformTestHttp }

procedure TformTestHttp.buttonVideoHEADTestClick(Sender: TObject);
var
  Client: THttpSend;
  ContentsList: TStringList;
  AURL: string;
begin
  AURL := comboURL.Text;

  Client := THttpSend.Create;
  ContentsList := TStringList.Create;
  try
    Client.Headers.Add('Accept:	text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8');
    Client.Headers.Add('Accept-Language:	en-gb,en;q=0.5');
    //Client.Headers.Add('Accept-Encoding:	gzip,deflate');
    Client.Headers.Add('Accept-Charset:	utf-8;q=0.7,*;q=0.7'); // ISO-8859-1,
    Client.UserAgent := comboUserAgent.Text;
    if editProxy.Text <> '' then
    begin
      Client.ProxyHost := editProxy.Text;
      Client.ProxyPort := '80';
    end;

    Client.HttpMethod('GET', AURL);

//    Client.Headers;

    Client.Document.Position := 0;
    ContentsList.Clear();
    ContentsList.LoadFromStream(Client.Document);
    memoTestHttpDebug.Clear();
    memoTestHttpDebug.Lines.Add(Format('Loading page: %s', [AURL]));
    memoTestHttpDebug.Lines.Add('');
    memoTestHttpDebug.Lines.Add('HTTP Headers:');
    memoTestHttpDebug.Lines.Add('');
    memoTestHttpDebug.Lines.AddStrings(Client.Headers);
    memoTestHttpDebug.Lines.Add('');
  finally
    ContentsList.Free;
    Client.Free;
  end;
end;

{ TTestHttpBrowserModule }

constructor TTestHttpBrowserModule.Create;
begin
  inherited Create;

  ShortDescription := 'HTTP Test';
end;

function TTestHttpBrowserModule.GetModuleUIElements: TBrowserModuleUIElements;
begin
  Result := [bmueCommandsSubmenu];
end;

function TTestHttpBrowserModule.GetCommandCount: Integer;
begin
  Result := 1;
end;

function TTestHttpBrowserModule.GetCommandName(AID: Integer): string;
begin
  Result := 'HTTP Test Dialog';
end;

procedure TTestHttpBrowserModule.ExecuteCommand(AID: Integer);
begin
  formTestHttp.ShowModal();
end;

initialization
  {$I mod_testhttp.lrs}

  RegisterBrowserModule(TTestHttpBrowserModule.Create());
end.

