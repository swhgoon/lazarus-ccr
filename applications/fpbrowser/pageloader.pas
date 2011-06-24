unit pageloader;

{$mode delphi}

interface

uses
  Classes, SysUtils; 

type

  { TPageLoader }

  TPageLoader = class
  public
    Contents: string;
    ContentsList: TStringList;
    constructor Create;
    procedure LoadFromURL(AURL: string);
  end;

var
  MyPageLoader: TPageLoader;

implementation

uses httpsend;

{ TPageLoader }

constructor TPageLoader.Create;
begin
  ContentsList := TStringList.Create;
end;

procedure TPageLoader.LoadFromURL(AURL: string);
var
  Client: THttpSend;
begin
  Client := THttpSend.Create;

//  if checkGZip.Checked then
  begin
    Client.Headers.Add('Accept:	text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8');
    Client.Headers.Add('Accept-Language:	en-gb,en;q=0.5');
//    Client.Headers.Add('Accept-Encoding:	gzip,deflate');
    Client.Headers.Add('Accept-Charset:	utf-8;q=0.7,*;q=0.7'); // ISO-8859-1,
  end;

//  Client.UserAgent := AUserAgent;
  Client.HttpMethod('GET', AURL);
  Client.Document.Position := 0;

  ContentsList.LoadFromStream(Client.Document);

  Contents := ContentsList.Text;

  Client.Free;
end;

initialization

  MyPageLoader := TPageLoader.Create;

finalization

  MyPageLoader.Free;

end.

