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
    LastPageURL: string;
    ContentsList: TStringList;
    DebugInfo: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromURL(AURL: string);
    procedure LoadBinaryResource(AURL: string; var ADest: TMemoryStream);
    function URLToAbsoluteURL(AInput: string): string;
  end;

var
  MyPageLoader: TPageLoader;

implementation

uses httpsend;

{ TPageLoader }

constructor TPageLoader.Create;
begin
  ContentsList := TStringList.Create;
  DebugInfo := TStringList.Create;
end;

destructor TPageLoader.Destroy;
begin
  ContentsList.Free;
  DebugInfo.Free;
  inherited Destroy;
end;

procedure TPageLoader.LoadFromURL(AURL: string);
var
  Client: THttpSend;
  J: Integer;
begin
  // If there is no protocol, add http
  J := Pos(':', AURL);
  if (J = 0) then LastPageURL := 'http://' + AURL
  else LastPageURL := AURL;

  Client := THttpSend.Create;
  try
    Client.Headers.Add('Accept:	text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8');
    Client.Headers.Add('Accept-Language:	en-gb,en;q=0.5');
//    Client.Headers.Add('Accept-Encoding:	gzip,deflate');
    Client.Headers.Add('Accept-Charset:	utf-8;q=0.7,*;q=0.7'); // ISO-8859-1,

//  Client.UserAgent := AUserAgent;
    Client.HttpMethod('GET', LastPageURL);

//    Client.Headers;

    Client.Document.Position := 0;
    ContentsList.Clear();
    ContentsList.LoadFromStream(Client.Document);
    DebugInfo.Clear();
    DebugInfo.Add(Format('Loading page: %s', [LastPageURL]));
    DebugInfo.Add('');
    DebugInfo.Add('HTTP Headers:');
    DebugInfo.Add('');
    DebugInfo.AddStrings(Client.Headers);
    DebugInfo.Add('');

    Contents := ContentsList.Text;
  finally
    Client.Free;
  end;
end;

procedure TPageLoader.LoadBinaryResource(AURL: string; var ADest: TMemoryStream);
var
  Client: THttpSend;
  i: Integer;
begin
  Client := THttpSend.Create;
  try
    Client.Headers.Add('Accept:	image/png, image/jpeg, image/gif');
    Client.Headers.Add('Accept-Language:	en-gb,en;q=0.5');
//    Client.Headers.Add('Accept-Encoding:	gzip,deflate');
    Client.Headers.Add('Accept-Charset:	utf-8;q=0.7,*;q=0.7'); // ISO-8859-1,

//  Client.UserAgent := AUserAgent;
    Client.HttpMethod('GET', AURL);

    Client.Document.Position := 0;
    ADest := TMemoryStream.Create;
    ADest.CopyFrom(Client.Document, Client.Document.Size);
    DebugInfo.Add(Format('Loading image: %s Size: %d', [AURL, ADest.Size]));
  finally
    Client.Free;
  end;
end;

function TPageLoader.URLToAbsoluteURL(AInput: string): string;
var
  J: Integer;
begin
  // Add the base URL if the URL is relative
  J := Pos(':', UpperCase(AInput));
  if J = 0 then
  begin
    if (Length(LastPageURL) > 0) and
     (LastPageURL[Length(LastPageURL)] = '/') then
      Result := LastPageURL + Copy(AInput, 2, Length(AInput)-1)
    else
      Result := LastPageURL + AInput;
  end
  else
    Result := AInput;
end;

initialization

  MyPageLoader := TPageLoader.Create;

finalization

  MyPageLoader.Free;

end.
