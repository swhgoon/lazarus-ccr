unit browserviewer;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  Controls, Forms, Graphics,
  //
  pageloader;

type

  { TBrowserViewer }

  TBrowserViewer = class
  public
    MyPageLoaderThread: TPageLoaderThread;
    MyPageLoader: TPageLoader;
    CurrentTab: Integer;
    ViewerName: string;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CreateViewer(AParent, AOwner: TWinControl); virtual; abstract;
    procedure LoadFromFile(AFilename: string); virtual; abstract;
    procedure LoadFromURL(AURL: string); virtual;
    function GetDocumentTitle: string; virtual; abstract;
    procedure SetShowImages(AValue: Boolean); virtual; abstract;
    procedure HandlePageLoaderTerminated(Sender: TObject); virtual;
    procedure Reload; virtual; abstract;
  end;

  TBrowserViewerClass = class of TBrowserViewer;

procedure SetBrowserViewerClass(AViewerClass: TBrowserViewerClass);
function GetBrowserViewer(AIndex: Integer): TBrowserViewer;
function GetCurrentBrowserViewer: TBrowserViewer;
procedure SetCurrentBrowserViewer(AIndex: Integer);
function AddBrowserViewer(): TBrowserViewer;
procedure RemoveBrowserViewer(AIndex: Integer);
function GetBrowerViewerCount: Integer;

implementation

var
  gBrowserViewerClass: TBrowserViewerClass;
  gBrowserViewers: TFPList;
  gCurrentViewer: Integer;

procedure SetBrowserViewerClass(AViewerClass: TBrowserViewerClass);
begin
  gBrowserViewerClass := AViewerClass;
end;

function GetBrowserViewer(AIndex: Integer): TBrowserViewer;
begin
  Result := gBrowserViewers.Items[AIndex];
end;

function GetCurrentBrowserViewer: TBrowserViewer;
begin
  Result := GetBrowserViewer(gCurrentViewer);
end;

procedure SetCurrentBrowserViewer(AIndex: Integer);
begin
  gCurrentViewer := AIndex;
end;

function AddBrowserViewer(): TBrowserViewer;
begin
  Result := gBrowserViewerClass.Create();
  gBrowserViewers.Add(Result);
end;

procedure RemoveBrowserViewer(AIndex: Integer);
begin

end;

function GetBrowerViewerCount: Integer;
begin
  Result := gBrowserViewers.Count;
end;

{ TBrowserViewer }

constructor TBrowserViewer.Create;
begin
  inherited Create;
  MyPageLoader := TPageLoader.Create;
end;

destructor TBrowserViewer.Destroy;
begin
  MyPageLoader.Free;
  inherited Destroy;
end;

procedure TBrowserViewer.LoadFromURL(AURL: string);
begin
  MyPageLoaderThread := TPageLoaderThread.Create(True);
  MyPageLoaderThread.URL := AURL;
  MyPageLoaderThread.PageLoader := MyPageLoader;
//  MyPageLoaderThread.OnPageLoadProgress := @HandlePageLoaderProgress;
  MyPageLoaderThread.OnTerminate := HandlePageLoaderTerminated;
  MyPageLoaderThread.FreeOnTerminate := True;
  MyPageLoaderThread.Resume;
end;

procedure TBrowserViewer.HandlePageLoaderTerminated(Sender: TObject);
begin
end;

initialization
  gBrowserViewers := TFPList.Create;
finalization
  gBrowserViewers.Free;
end.

