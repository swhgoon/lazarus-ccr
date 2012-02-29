{
  LazEdit: a text editor with built-in features for HTML editing and
  Syntax Highlighting for several text formats
  (html, xml, css, javascript, pascal, c/c++, perl, python, php, bat, ini, diff)

  Copyright (C) 2011, 2012 by Bart Broersma & Flying Sheep Inc. and
  Felipe Monteiro de Carvalho
  http://wiki.lazarus.freepascal.org/LazEdit

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit LazEdit_About;

{ ************ ExtAboutForm ***********************************

  Copyright (C) 2011 Bart Broersma

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 *****************************************************************}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLType,
  Buttons, ComCtrls, StdCtrls, LCLProc, ExtCtrls, LCLIntf, Menus, ClipBrd;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    CloseBtn: TBitBtn;
    Image1: TImage;
    BuildDateLabel: TLabel;
    CopyrightLabel: TLabel;
    FpcLabel: TLabel;
    CopyVersionInfo: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    WebsiteLabel: TLabel;
    LazLabel: TLabel;
    ProductLabel: TLabel;
    VersionLabel: TLabel;
    LicenseUrlLabel: TLabel;
    LicenseMemo: TMemo;
    NoteBook: TPageControl;
    MainPage: TTabSheet;
    LicensePage: TTabSheet;
    procedure CopyVersionInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure LicenseUrlLabelClick(Sender: TObject);
    procedure WebsiteLabelClick(Sender: TObject);
    procedure WebsiteLabelMouseEnter(Sender: TObject);
    procedure WebsiteLabelMouseLeave(Sender: TObject);
  private
    { private declarations }
    FWebsiteURL: String;
    FLicenseUrl: String;
  public
    { public declarations }
    procedure SetVersionInfo(ProductName,Version, BuildDate, LazVersion, FpcVersion: String);
    procedure SetCopyrightInfo(const CopyrightStatement, WebsiteName, WebsiteUrl: String);
    procedure SetLicenseInfo(const LicenseText, LicenseWebsiteName, LicenseWebsiteUrl: String);
    procedure CopyVersionInfoToClipBoard;
  end; 

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

const
  idxMain = 0;
  idxLicense = 1;

procedure TFormAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else
  if (Key = VK_C) and (Shift = [ssCtrl]) then CopyVersionInfoToClipBoard;
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  NoteBook.PageIndex := idxMain;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  //In GTK you can resize/maximize a form with BorderStyle := bsDialog, we don't want that
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;;
  NoteBook.PopupMenu := PopupMenu1;
  PopupMenu := PopupMenu1;
end;

procedure TFormAbout.CopyVersionInfoClick(Sender: TObject);
begin
  CopyVersionInfoToClipBoard;
end;

procedure TFormAbout.LicenseUrlLabelClick(Sender: TObject);
begin
  if FLicenseUrl <> '' then OpenUrl(FLicenseUrl);
end;

procedure TFormAbout.WebsiteLabelClick(Sender: TObject);
begin
  if FWebsiteUrl <> '' then OpenUrl(FWebsiteUrl);
end;

procedure TFormAbout.WebsiteLabelMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Style := (Sender as TLabel).Font.Style + [fsUnderline];
end;

procedure TFormAbout.WebsiteLabelMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Style := (Sender as TLabel).Font.Style - [fsUnderline];
end;



procedure TFormAbout.SetVersionInfo(ProductName,Version, BuildDate, LazVersion,
  FpcVersion: String);
var
  Y,M,D: String;
  p: Integer;
begin
  if ProductName = '' then ProductName := Application.Title;
  if BuildDate = '' then
  begin
    BuildDate := {$I %DATE%};  // y/m/d format
    p := Pos('/',BuildDate);
    Y := Copy(BuildDate,1,p-1);
    System.Delete(BuildDate,1,p);
    p := Pos('/',BuildDate);
    M := Copy(BuildDate,1,p-1);
    System.Delete(BuildDate,1,p);
    D := BuildDate;
    BuildDate := 'Build date: ' + Y + DefaultFormatSettings.DateSeparator + M + DefaultFormatSettings.DateSeparator + D;
  end;
  if LazVersion = '' then LazVersion := 'Lazarus '+ LCLVersion;
  if FpcVersion = '' then FpcVersion := 'FPC ' + {$I %FPCVERSION%};
  ProductLabel.Caption := ProductName;
  VersionLabel.Caption := Version;
  BuildDateLabel.Caption := BuildDate;
  LazLabel.Caption := LazVersion;
  FpcLabel.Caption := FpcVersion;
end;

procedure TFormAbout.SetCopyrightInfo(const CopyrightStatement, WebsiteName, WebsiteUrl: String);
begin
  FWebsiteURL := WebsiteURL;
  WebsiteLabel.Caption := WebsiteName;
  if WebsiteName = '' then WebsiteLabel.Caption := WebsiteUrl;
  CopyrightLabel.Caption := CopyrightStatement;
  if (WebsiteUrl <> '') and (WebsiteName <> '') and (WebsiteUrl <> WebsiteName) then
  begin
    WebsiteLabel.ShowHint := True;
    WebsiteLabel.Hint := WebsiteUrl;
  end
  else WebsiteLabel.ShowHint := False;
end;

procedure TFormAbout.SetLicenseInfo(const LicenseText,
  LicenseWebsiteName, LicenseWebsiteUrl: String);
begin
  LicenseMemo.Text := LicenseText;
  FLicenseUrl := LicenseWebsiteUrl;
  LicenseUrlLabel.Caption := LicenseWebsiteName;
  if LicenseWebsiteName = '' then LicenseUrlLabel.Caption := FLicenseUrl;
  if (LicenseWebsiteUrl <> '') and (LicenseWebsiteName <> '') and (LicenseWebsiteURL <> LicenseWebsiteName) then
  begin
    LicenseUrlLabel.ShowHint := True;
    LicenseUrlLabel.Hint := LicenseWebsiteUrl;
  end
  else LicenseUrlLabel.ShowHint := False;
end;

procedure TFormAbout.CopyVersionInfoToClipBoard;
begin
  ClipBoard.AsText := ProductLabel.Caption + LineEnding +
                      VersionLabel.Caption + LineEnding +
                      BuildDateLabel.Caption + LineEnding +
                      LazLabel.Caption + LineEnding +
                      FpcLabel.Caption ;
end;

end.

