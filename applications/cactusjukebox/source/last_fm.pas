{
  written by Sebastian Kraft
  sebastian_kraft@gmx.de

  This software is free under the GNU Public License

  (c)2007
}

Unit last_fm;

{$mode objfpc}{$H+}

Interface

Uses 
 {$ifdef linux}
cthreads,
 {$endif}
Classes, SysUtils, xmlread, dom, httpsend, debug, strutils;

Type 

TCoverSize = (LargeImage, MediumImage, SmallImage, ExtralargeImage);


TAlbumInfoRecord = record
            Artist, Album, WikiSummary, WikiContent : string;
            CoverURL, CoverSavePath: string;
      end;

THTTPCallbackProc = function : boolean of object;

{ TScanThread }

   { TNetworkThread }

  TNetworkThread = Class(TThread)
    Private 
      FHTTP: THTTPSend;
      FErrorCode: string;
      FUrl: string;
    Protected 
      Procedure Execute;override;
    Public
      fStatus : byte;
      URL: string;
      Errorcode: string;
      ReceiveProc: THTTPCallbackProc;
      ReceiveData: ^TMemoryStream;
      Constructor Create(Suspd : boolean);
  End;
{ TScanThread }


{ TLastfmAPIObject }

  TLastfmAPIObject = Class
    constructor Create;
    destructor destroy;
    Private 
    { private declarations }
      // FAlbumCover: TFPImage;
      FArtist, FAlbum: string;
      FAPIKey: string;
      FUrl, FURLPrefix: string;

      XMLResult: TXMLDocument;
      HTTPRecData: TMemoryStream;

      HTTPThread: TNetworkThread;
      FSavePath: ansistring;
      FData_Ready, FImgNotFound: boolean;


      FAlbumInfo: TAlbumInfoRecord;

      function album_getInfosRCV: boolean;
      function album_downloadCoverRCV: boolean;
      procedure SendRequest(callback: THTTPCallbackProc);
    Public 
    { public declarations }
      CoverSize: TCoverSize;
      property data_ready: boolean read FData_Ready;
      property AlbumInfo: TAlbumInfoRecord read FAlbumInfo;
      procedure album_getInfos(artist, album:string);
      procedure album_downloadCover(artist, album, savepath:string);
  End;

  Implementation

  Uses functions;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  constructor TLastfmAPIObject.Create;
Begin
  CoverSize:=MediumImage;
  HTTPRecData := TMemoryStream.Create;
  FData_Ready := false;
  FImgNotFound := false;
  FAPIKey := 'a364faa0dad3b90a2ebd2fccd2bd2173';
  FURLPrefix:='http://ws.audioscrobbler.com/2.0/';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

destructor TLastfmAPIObject.destroy;
Begin
  HTTPRecData.Free;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TLastfmAPIObject.album_getInfosRCV:boolean;
Var node: TDOMNode;
    ImgString: string;
    done: boolean;
Begin
  Try
    XMLResult := TXMLDocument.Create;

    ReadXMLFile(XMLResult, HTTPRecData);

    node := XMLResult.DocumentElement.FindNode('album');

    if Assigned(node) then begin
        FAlbumInfo.Album:=node.FindNode('name').FirstChild.NodeValue;

        FAlbumInfo.Artist:=node.FindNode('artist').FirstChild.NodeValue;

        case CoverSize of
             LargeImage: ImgString:='large';
             MediumImage: ImgString:='medium';
             ExtralargeImage: ImgString:='extralarge';
        end;

        node:=node.FindNode('image');
        done:= false;

        while ((node.Attributes.Item[0].NodeValue<>ImgString) and (done=false)) do begin

            if node.NextSibling.NodeName='image' then
                  node:=node.NextSibling
                else done:= true;
        end;
        FAlbumInfo.CoverURL:=node.FirstChild.NodeValue;
       // FAlbumInfo.CoverURL:=StringReplace(FAlbumInfo.CoverURL, #10, '', [rfReplaceAll]);
       // FAlbumInfo.CoverURL:=StringReplace(FAlbumInfo.CoverURL, #13, '', [rfReplaceAll]);
        // TODO: Clean up linebreaks in strings retrieved from XML files

   end;

   XMLResult.Free;
   //fdata_ready := true;
  finally
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TLastfmAPIObject.album_downloadCoverRCV: boolean;
var  HTTPConnection: THTTPSend;

begin
  album_getInfosRCV;
  FData_Ready:=false;
  If FAlbumInfo.CoverURL<>'' Then
    Begin
      If Not DirectoryExists(ExtractFileDir(FSavePath)) Then mkdir(ExtractFileDir(FSavePath));

      HTTPConnection:=THTTPSend.Create;
      DebugOutln('loading cover... ', 5);DebugOutLn(FAlbumInfo.CoverURL, 5);
      HTTPConnection.HTTPMethod('GET', FAlbumInfo.CoverURL);
      DebugOutLn('done', 5);

      try
         HTTPConnection.Document.SaveToFile(FAlbumInfo.CoverSavePath);
      except
         writeln('EXCEPTION writing cover art to disk');
      end;
      HTTPConnection.Free;
    End else DebugOutLn('No cover found :(', 5);
  fdata_ready:=true;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TLastfmAPIObject.album_getInfos(artist, album: string);
begin
  FAlbum:=album;
  FArtist:=artist;
  furl := Furlprefix + Format('?method=album.getinfo&api_key=%s&artist=%s&album=%s', [FAPIKey, FArtist, FAlbum]);
  furl := AnsiReplaceStr(furl, ' ', '%20');
  writeln(furl);
  SendRequest(@album_getInfosRCV);
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TLastfmAPIObject.album_downloadCover(artist, album, savepath: string);
begin
  FAlbum:=album;
  FArtist:=artist;
  furl := Furlprefix + Format('?method=album.getinfo&api_key=%s&artist=%s&album=%s', [FAPIKey, FArtist, FAlbum]);
  furl := AnsiReplaceStr(furl, ' ', '%20');
  FAlbumInfo.CoverSavePath:=savepath;
  DebugOutLn('Sending album cover request to last.fm... ', 5);
  DebugOutLn(furl, 5);
  SendRequest(@album_downloadCoverRCV);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TLastfmAPIObject.SendRequest(callback: THTTPCallbackProc);
Begin
  fdata_ready := false;
  HTTPThread := TNetworkThread.Create(true);
  HTTPThread.URL := furl;
  HTTPThread.ReceiveProc := callback;
  HTTPThread.ReceiveData := @HTTPRecData;
  HTTPThread.Resume;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TNetworkThread }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TNetworkThread.Execute;
Begin
  FHTTP := THTTPSend.Create;

  FHTTP.HTTPMethod('GET', URL);
  ReceiveData^ := FHTTP.Document;
  ReceiveProc;  // calls ReceiveProc when data is ready
  FHTTP.Free;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TNetworkThread.Create(Suspd: boolean);
Begin
  inherited Create(suspd);
  FreeOnTerminate := True;
  fStatus := 255;
End;

End.
