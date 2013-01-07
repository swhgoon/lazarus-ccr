{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
 
  Author: Tom Gregorovic

  Abstract:
    Picture manager is notebook which holds picture edits.
}
unit PictureManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, ExtCtrls, ComCtrls,
  Forms, PictureCtrls, DLBitmap;
  
type

  { TPicturePage }

  TPicturePage = class(TTabSheet)
  private
    FFilename: String;
    FShowPreview: Boolean;
    procedure SetFileName(const AValue: String);
  public
    PictureEdit: TPictureEdit;
    
    constructor Create(TheOwner: TComponent; AWidth, AHeight: Integer;
      APaperColor: TColor);
    constructor Create(TheOwner: TComponent; const AFilename: String);
    constructor Create(TheOwner: TComponent; ABitmap: TRasterImage);
    procedure Save;
    procedure ExportAsLazarusResource(const AFileName, AName: String);
    
    property FileName: String read FFileName write SetFileName;
    property ShowPreview: Boolean read FShowPreview write FShowPreview;
  end;

  { TPictureManager }

  TPictureManager = class(TPageControl)
  private
    FOnColorChange: TNotifyEvent;
    FOnPageClose: TNotifyEvent;
    FOnPageCloseQuery: TCloseQueryEvent;
    FOnPictureChange: TNotifyEvent;
    FOnPictureSizeChange: TNotifyEvent;
    FOnSaveAs: TNotifyEvent;
    FOnFileNameChange: TNotifyEvent;
    FOnPictureMouseDown: TMouseEvent;
    FOnPictureMouseMove: TMouseMoveEvent;
    FOnPictureMouseUp: TMouseEvent;
    function FindNewUniqueName: String;
    function GetActivePicturePage: TPicturePage;
    procedure SetActivePicturePage(const AValue: TPicturePage);
    procedure SetPageEvents(APage: TPicturePage); virtual;
  protected
    function CreatePage(AWidth, AHeight: Integer; APaperColor: TColor): TPicturePage; dynamic;
    function CreatePage(const Filename: String): TPicturePage; dynamic;
    function CreatePage(ABitmap: TRasterImage): TPicturePage; dynamic;
    procedure SaveAs; dynamic;
    procedure FileNameChange; dynamic;
    procedure PageClose; dynamic;
    procedure PageCloseQuery(var CanClose: Boolean); dynamic;
  public
    GraphicFileName: string;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure New(AWidth, AHeight: Integer; APaperColor: TColor);
    procedure Load(const FileName: String);
    procedure Save;
    procedure Save(const FileName: String);
    procedure ExportAsLazarusResource(const AFileName, AName: String);
    procedure ExportAsWindowsIcon(const AFileName: String);
    procedure Close;
    procedure CloseAll;
    procedure Paste;
    function GetPicturePageByIndex(AIndex: Integer): TPicturePage;
    
    function CanEdit: Boolean;
  published
    property ActivePicturePage: TPicturePage read GetActivePicturePage write SetActivePicturePage;
    
    property OnPictureMouseDown: TMouseEvent read FOnPictureMouseDown write
      FOnPictureMouseDown;
    property OnPictureMouseMove: TMouseMoveEvent read FOnPictureMouseMove write
      FOnPictureMouseMove;
    property OnPictureMouseUp: TMouseEvent read FOnPictureMouseUp write FOnPictureMouseUp;
    property OnPictureChange: TNotifyEvent read FOnPictureChange write FOnPictureChange;
    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
    property OnPictureSizeChange: TNotifyEvent read FOnPictureSizeChange write FOnPictureSizeChange;
    property OnSaveAs: TNotifyEvent read FOnSaveAs write FOnSaveAs;
    property OnFileNameChange: TNotifyEvent read FOnFileNameChange write FOnFileNameChange;
    property OnPageClose: TNotifyEvent read FOnPageClose write FOnPageClose;
    property OnPageCloseQuery: TCloseQueryEvent read FOnPageCloseQuery write FOnPageCloseQuery;
  end;

implementation

uses IconStrConsts;
{ TPictureManager }

function TPictureManager.FindNewUniqueName: String;
var
  I, J: Integer;
  Exists: Boolean;
begin
  I := 1;

  while I < maxSmallint do
  begin
    Exists := False;
    for J := 0 to Pred(PageCount) do
    begin
      if Pages[J].Caption = lieNew + IntToStr(I) then
      begin
        Inc(I);
        Exists := True;
        Break;
      end;
    end;
    if not Exists then Break;
  end;
  
  Result := lieNew + IntToStr(I);
end;

function TPictureManager.GetActivePicturePage: TPicturePage;
begin
  Result := ActivePage as TPicturePage;
end;

procedure TPictureManager.SetActivePicturePage(const AValue: TPicturePage);
begin
  ActivePage := AValue;
end;

procedure TPictureManager.SetPageEvents(APage: TPicturePage);
begin
  APage.PictureEdit.OnPictureMouseDown := OnPictureMouseDown;
  APage.PictureEdit.OnPictureMouseMove := OnPictureMouseMove;
  APage.PictureEdit.OnPictureMouseUp := OnPictureMouseUp;
  APage.PictureEdit.OnColorChange := OnColorChange;
  APage.PictureEdit.OnPictureSizeChange := OnPictureSizeChange;
  APage.PictureEdit.OnChange := OnPictureChange;
end;

function TPictureManager.CreatePage(AWidth, AHeight: Integer; APaperColor: TColor): TPicturePage;
begin
  Result := TPicturePage.Create(Self, AWidth, AHeight, APaperColor);
  Result.PageControl := Self;
  SetPageEvents(Result);

  FileNameChange;
end;

function TPictureManager.CreatePage(const Filename: String): TPicturePage;
begin
  Result := TPicturePage.Create(Self, Filename);
  Result.PageControl := Self;
  SetPageEvents(Result);
  GraphicFileName := FileName;
  FileNameChange;
end;

function TPictureManager.CreatePage(ABitmap: TRasterImage): TPicturePage;
begin
  Result := TPicturePage.Create(Self, ABitmap);
  Result.PageControl := Self;
  SetPageEvents(Result);

  FileNameChange;
end;

procedure TPictureManager.SaveAs;
begin
  if Assigned(FOnSaveAs) then FOnSaveAs(Self);
end;

procedure TPictureManager.FileNameChange;
begin
  if Assigned(FOnFileNameChange) then FOnFileNameChange(Self);
  if ActivePicturePage <> nil then
  begin
    if ActivePicturePage.FileName <> '' then
    begin
      ActivePicturePage.Caption := ExtractFileName(ActivePicturePage.FileName);
    end;
  end;
end;

procedure TPictureManager.PageClose;
begin
  if Assigned(FOnPageClose) then FOnPageClose(Self);
end;

procedure TPictureManager.PageCloseQuery(var CanClose: Boolean);
begin
  if Assigned(FOnPageCloseQuery) then FOnPageCloseQuery(Self, CanClose);
end;

constructor TPictureManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  
  PageClass := TPicturePage;
end;

destructor TPictureManager.Destroy;
begin
  inherited Destroy;
end;

procedure TPictureManager.New(AWidth, AHeight: Integer; APaperColor: TColor);
var
  NewPage: TPicturePage;
begin
  NewPage := CreatePage(AWidth, AHeight, APaperColor);
  ActivePage := NewPage;
  NewPage.Caption := FindNewUniqueName;
  Change;
end;

procedure TPictureManager.Load(const FileName: String);
var
  NewPage: TPicturePage;
  Icon: TIcon;
  Bmp: TDLBitmap;
  I: Integer;
  Pic: TPicture;
begin
  //if SameText(ExtractFileExt(FileName), '.ico') then
  //begin
  if SameText(ExtractFileExt(FileName), '.ico') then
  begin
    Icon := TIcon.Create;
    try
      // First image in std bitmap
      Icon.LoadFromFile(FileName);

      // other images
      for I := 0 to Pred(Icon.Count) do
      begin
        Icon.Current := I;
        NewPage := CreatePage(Icon);
        NewPage.Parent := Self;
        ActivePage := NewPage;
        NewPage.Caption := FindNewUniqueName;
        Change;
      end;
    finally
      Icon.Free;
    end;
  end
  else
  begin
    Bmp := TDLBitmap.Create;
    try
      // First image in std bitmap
//      Icon.LoadFromFile(FileName);
      Pic := TPicture.Create;
      Pic.LoadFromFile(FileName);
      Bmp.Width:=Pic.Width;
      Bmp.Height:=Pic.Height;
      Bmp.Canvas.Draw(0,0,Pic.Graphic);
      // other images
      //for I := 0 to Pred(Icon.Count) do
      begin
      //  Icon.Current := I;
        NewPage := CreatePage(Bmp);
        NewPage.Parent := Self;
        ActivePage := NewPage;
        NewPage.Caption := FindNewUniqueName;
        Change;
      end;
    finally
      Bmp.Free;
      Pic.Free;
    end;
  end;
  {end
  else
  begin
    NewPage := CreatePage(FileName);
    NewPage.Parent := Self;
    ActivePage := NewPage;
    Change;
  end; }
end;

procedure TPictureManager.Save;
begin
  if ActivePicturePage <> nil then ActivePicturePage.Save;
end;

procedure TPictureManager.Save(const FileName: String);
begin
  if ActivePicturePage <> nil then
  begin
    ActivePicturePage.FileName := FileName;
    Save;
  end;
end;

procedure TPictureManager.ExportAsLazarusResource(const AFileName, AName: String);
begin
  if ActivePicturePage <> nil then
  begin
    ActivePicturePage.ExportAsLazarusResource(AFileName, AName);
  end;
end;

procedure TPictureManager.ExportAsWindowsIcon(const AFileName: String);
var
  lIcon: TIcon;
  lPicturePage: TPicturePage;
  i, lWidth, lHeight: Integer;
begin
  lIcon := TIcon.Create;
  try
    for i := 0 to Self.PageCount - 1 do
    begin
      lPicturePage := GetPicturePageByIndex(i);
      if lPicturePage = nil then Continue;
      lWidth := lPicturePage.PictureEdit.Picture.Width;
      lHeight := lPicturePage.PictureEdit.Picture.height;

      lIcon.Add(pf24bit, 16, 16);
      lIcon.Current:=i;
      lIcon.Canvas.Draw(0, 0, lPicturePage.PictureEdit.Picture); // Currently this crashes due to a bug in TIcon
    end;
    lIcon.SaveToFile(AFileName);
  finally
    lIcon.Free;
  end;
end;

procedure TPictureManager.Close;
var
  CanClose: Boolean;
begin
  if ActivePicturePage <> nil then
  begin
    CanClose := True;
    PageCloseQuery(CanClose);
    
    if CanClose then
    begin
      ActivePicturePage.Free;
      PageClose;
    end;
  end;
end;

procedure TPictureManager.CloseAll;
var
  I: Integer;
begin
  for I := Pred(PageCount) downto 0 do
  begin
    ActivePageIndex := I;
    Close;
  end;
end;

procedure TPictureManager.Paste;
begin
  //if CanEdit then
  ActivePicturePage.PictureEdit.Paste;
end;

function TPictureManager.GetPicturePageByIndex(AIndex: Integer): TPicturePage;
begin
  Result := TPicturePage(Self.GetPage(AIndex));
end;

function TPictureManager.CanEdit: Boolean;
begin
  Result := ActivePicturePage <> nil;
end;

{ TPicturePage }

procedure TPicturePage.SetFileName(const AValue: String);
begin
  if AValue = FFileName then Exit;
  FFilename := AValue;
  (PageControl as TPictureManager).FileNameChange;
end;

constructor TPicturePage.Create(TheOwner: TComponent; AWidth, AHeight: Integer;
      APaperColor: TColor);
begin
  inherited Create(TheOwner);
  
  PictureEdit := TPictureEdit.Create(Self);
  PictureEdit.Parent := Self;
  PictureEdit.Align := alClient;
  PictureEdit.NewPicture(AWidth, AHeight, APaperColor);
  
  FFilename := '';
  FShowPreview := True;
end;

constructor TPicturePage.Create(TheOwner: TComponent; const AFilename: String);
begin
  inherited Create(TheOwner);
  
  PictureEdit := TPictureEdit.Create(Self);
  PictureEdit.Parent := Self;
  PictureEdit.Align := alClient;
  PictureEdit.LoadPicture(AFilename);
  
  FFilename := AFilename;
  Caption := ExtractFilename(Filename);
  FShowPreview := True;
end;

constructor TPicturePage.Create(TheOwner: TComponent; ABitmap: TRasterImage);
begin
  inherited Create(TheOwner);

  PictureEdit := TPictureEdit.Create(Self);
  PictureEdit.Parent := Self;
  PictureEdit.Align := alClient;
  PictureEdit.LoadBitmap(ABitmap);

  FFilename := '';
  FShowPreview := True;
end;

procedure TPicturePage.Save;
begin
  if FileName = '' then (PageControl as TPictureManager).SaveAs;
  if FileName <> '' then PictureEdit.SavePicture(FileName);
end;

procedure TPicturePage.ExportAsLazarusResource(const AFileName, AName: String);
begin
  PictureEdit.ExportPictureAsLazarusResource(AFileName, AName);
end;

end.



