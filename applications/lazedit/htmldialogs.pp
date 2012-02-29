{ HtmlDialogs unit

  Copyright (C) 2012 by Bart Broersma & Flying Sheep Inc.
  http://home.tiscali.nl/~knmg0017/

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
unit HtmlDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HtmlCode, Controls, Forms,
  NewHtmlDlgForm, AnchorDlgForm, PictureDlgForm, HtmlCharMap, NewTableForm, lazedit_constants;

type

  { TNewHtmlDlg }

  TNewHtmlDlg = class
  private
    FTop: Integer;
    FLeft: Integer;
    FInitialDir: String;
    FDocType: THtmlDocType;
    FBackGroundColor: String;
    FBackGroundImage: String;
    FTextColor: String;
    FLinkColor: String;
    FVisitedColor: String;
    FTitle: String;
    FFavicon: String;
    FUseStyleSheet: Boolean;
    FDefaultStyleSheet: String;
    FPrintStyleSheet: String;
    //FInlineStyle: String;
    FAuthor: String;
    FHtmlText: String;
    FDlg: TNewHtmlDlgForm;
  protected
    function GetInlineStyle: String;
    function GetHtmlText: String;
    procedure GetFormValues;
    procedure SetFormValues;
  public
    function Execute: Boolean;

    constructor Create;
    destructor Destroy; override;

    property Top: Integer read FTop write FTop;
    property Left: Integer read FLeft write FLeft;
    property InitialDir: string read FInitialDir write FInitialDir;
    property Text: String read FHtmlText;
  end;

  { TAnchorDlg }

  TAnchorDlg = class
  private
    FAnchorText: String;
    FTop: Integer;
    FLeft: Integer;
    FUrl: String;
    FDescription: String; //the visual text for the link
    FTitle: String;
    FId: String;
    FClass: String;
    FAnchorType: TAnchorType;
    FDlg: TAnchorDlgForm;
    function GetLinkRef: String; // the < a href=""> part
  protected
    procedure GetFormValues;
    procedure SetFormValues;
  public
    function Execute: Boolean;

    constructor Create;
    destructor Destroy; override;

    property Top: Integer read FTop write FTop;
    property Left: Integer read FLeft write FLeft;
    property Text: String read FAnchorText;
    property LinkRef: String read GetLinkRef;
    property Description: String read FDescription write FDescription;
    property Title: String read FTitle;
    property DomID: String read FId;
    property DomClass: String read FClass;
  end;


  { THtmlCharMapDlg }

  THtmlCharMapDlg = class(TObject)
  private
    FCharMapForm: THtmlCharMapForm;
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetCharClick: THtmlCharClickEvent;
    procedure SetCharClick(AValue: THtmlCharClickEvent);
  protected
  public
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property OnHtmlCharClick: THtmlCharClickEvent read GetCharClick write SetCharClick;

    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;

  { TPictureDlg }

  TPictureDlg = class(TObject)
  private
    FDlg: TPictureDlgForm;
    FTop: Integer;
    TFop: Integer;
    FLeft: Integer;
    FFileName: String;
    FInitialDir: String;
    FDefaultFolderPrefix: String;
    FPicWidth: String;
    FPicHeight: String;
    FTitle: String;
    FAlt: String;
    FFloatStyle: String;
    FDomId: String;
    FDomClass: String;
  protected
    procedure GetFormValues;
    procedure SetFormValues;
  public
    function Execute: Boolean;

    constructor Create;
    destructor Destroy; override;

    property Top: Integer read FTop write FTop;
    property Left: Integer read FLeft write FLeft;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DefaultFolderPrefix: String read FDefaultFolderPrefix write FDefaultFolderPrefix;
    property FileName: String read FFileName write FFileName;
    property Title: String read FTitle;
    property Alt: String read FAlt;
    property PicWidth: String read FPicWidth;
    property PicHeight: String read FPicHeight;
    property FloatStyle: String read FFloatStyle;
    property DomID: String read FDomId;
    property DomClass: String read FDomClass;
  end;


  { TTableDlg }

  TTableDlg = class(TObject)
  private
    FDlg: TNewTableForm;
    FTop: Integer;
    FLeft: Integer;
    FDomId: String;
    FDomClass: String;
    FSummary: String;
    FColCount: Integer;
    FRowCount: Integer;
  protected
    procedure GetFormValues;
    procedure SetFormValues;
  public
    function Execute: Boolean;

    constructor Create;
    destructor Destroy; override;

    property Top: Integer read FTop write FTop;
    property Left: Integer read FLeft write FLeft;
    property DomID: String read FDomId;
    property DomClass: String read FDomClass;
    property Summary: String read FSummary;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
  end;

var
  NewHtmlDlg: TNewHtmlDlg;
  AnchorDlg: TAnchorDlg;
  HtmlCharMapDlg: THtmlCharMapDlg;
  PictureDlg: TPictureDlg;
  TableDlg: TTableDlg;

implementation

{ TTableDlg }

procedure TTableDlg.GetFormValues;
begin
  FTop := FDlg.Top;
  FLeft := FDlg.Left;
  FSummary := FDlg.Summary;
  FDomID := FDlg.DomId;
  FDomClass := FDlg.DomClass;
  FRowCount := FDlg.RowCount;
  FColCount := FDlg.ColCount;
end;

procedure TTableDlg.SetFormValues;
begin
  if FTop > -1 then FDlg.Top := FTop;
  if FLeft > -1 then FDlg.Left := FLeft;
end;

function TTableDlg.Execute: Boolean;
begin
  if not Assigned(FDlg) then FDlg := TNewTableForm.Create(nil);
  SetFormValues;
  Result := (FDlg.ShowModal = mrOk);
  if Result then GetFormValues
  else
  begin //always remember Top/Left
    FTop := FDlg.Top;
    FLeft := FDlg.Left;
  end;
end;

constructor TTableDlg.Create;
begin
  FDlg := nil;
end;

destructor TTableDlg.Destroy;
begin
  if Assigned(FDlg) then FDlg.Free;
  inherited Destroy;
end;

{ TPictureDlg }

procedure TPictureDlg.GetFormValues;
begin
  FTop := FDlg.Top;
  FLeft := FDlg.Left;
  FFileName := FDlg.Src;
  FInitialDir := FDlg.InitialDir;
  FDefaultFolderPrefix := FDlg.DefaultFolderPrefix;
  FDomId := FDlg.DomId;
  FDomClass := FDlg.DomClass;
  FTitle := FDlg.Title;
  FAlt := FDlg.Alt;
  FPicWidth := FDlg.PicWidth;
  FPicHeight := FDlg.PicHeight;
  FFloatStyle := FDlg.FloatStyle;
end;

procedure TPictureDlg.SetFormValues;
begin
  if (TFop > -1) then FDlg.Top := FTop;
  if (FLeft > -1) then FDlg.Left := FLeft;
  FDlg.InitialDir := FInitialDir;
  FDlg.DefaultFolderPrefix := FDefaultFolderPrefix;
end;

function TPictureDlg.Execute: Boolean;
begin
  if not Assigned(FDlg) then FDlg := TPictureDlgForm.Create(nil);
  SetFormValues;
  Result := (FDlg.ShowModal = mrOk);
  if Result then GetFormValues
  else
  begin //always remember Top/Left
    FTop := FDlg.Top;
    FLeft := FDlg.Left;
  end;
end;

constructor TPictureDlg.Create;
begin
  FDlg := nil;
end;

destructor TPictureDlg.Destroy;
begin
  if Assigned(FDlg) then FDlg.Free;
  inherited Destroy;
end;

{ TAnchorDlg }

function TAnchorDlg.GetLinkRef: String;
var
  STitle, SId, SClass, SCombined: String;
begin
  if (FTitle <> '') then STitle := 'title="' + FTitle + '"' else STitle := '';
  if (FId <> '') then SId := 'id="' + FId + '"' else SId := '';
  if (FClass <> '') then SClass := 'class="' + FClass + '"' else SClass := '';
  SCombined := STitle;
  if (SId <> '') then SCombined := SCombined + #32 + SId;
  if (SClass <> '') then SCombined := SCombined + #32 + SClass;
  case FAnchorType of
  atUrl:
    begin
      Result := '<a href="' + FUrl  + '"';
      if (SCombined <> '') then Result := Result + #32 + SCombined;
      Result := Result + '>';
    end;
  atMakeLocalAnchor:
    begin
      Result := '<a name="' + FUrl  + '"';
      if (SCombined <> '') then Result := Result + #32 + SCombined;
      Result := Result + '>';
    end;
  atRefLocalAnchor:
    begin
      Result := '<a href="#' + FUrl  + '"';
      if (SCombined <> '') then Result := Result + #32 + SCombined;
      Result := Result + '>';
    end;
  end;
end;


procedure TAnchorDlg.GetFormValues;
begin
  FDescription := FDlg.Description;
  FUrl := FDlg.Url;
  FTitle := FDlg.Title;
  FId := FDlg.DomId;
  FClass := FDlg.DomClass;
  FAnchorType := FDlg.AnchorType;
  FTop := FDlg.Top;
  FLeft := FDlg.Left;
end;

procedure TAnchorDlg.SetFormValues;
begin
  FDlg.Description := FDescription;
  //clear Title and ID
  FDlg.Title := '';
  FDlg.DomId := '';
  if (FTop >= 0) then FDlg.Top := FTop;
  if (FLeft >= 0) then FDlg.Left := FLeft;
end;

function TAnchorDlg.Execute: Boolean;
begin
  if not Assigned(FDlg) then FDlg := TAnchorDlgForm.Create(nil);
  SetFormValues;
  Result := (FDlg.ShowModal = mrOk);
  if Result then GetFormValues
  else
  begin //always remember Top/Left
    FTop := FDlg.Top;
    FLeft := FDlg.Left;
  end;
end;

constructor TAnchorDlg.Create;
begin
  FDlg := nil;
  FTop := -1;
  FLeft := -1;
end;

destructor TAnchorDlg.Destroy;
begin
  FDlg.Free;
  inherited Destroy;
end;

{ TNewHtmlDlg }


function TNewHtmlDlg.GetInlineStyle: String;
begin
  Result := Style_Start + LineEnding +
    'body{' + LineEnding;
  if (FBackGroundImage <> '') then
    Result := Result + 'background: url("' + FBackGroundImage + '");' + LineEnding;
  Result := Result +
    'background-color: ' + FBackGroundColor + ';'+ LineEnding +
    'color: ' + FTextColor + ';'+ LineEnding +
    '}' + LineEnding +
    'a:link {color: ' + FLinkColor + ';}' + LineEnding +
    'a:visited {color: ' + FVisitedColor + ';}' + LineEnding +
    Style_End;
end;

function TNewHtmlDlg.GetHtmlText: String;
var
  AStyleSheet, AInlineStyle: String;
begin
  AStyleSheet := '';
  AInlineStyle := '';
  if FUseStyleSheet then
  begin
    if (FDefaultStyleSheet <> '') then AStyleSheet := StyleSheet(FDefaultStyleSheet);
    if (FprintStyleSheet <> '') then
    begin
      if (AStyleSheet <> '') then AStyleSheet := AStyleSheet + LineEnding;
      AStyleSheet := AStyleSheet + StyleSheet(FPrintStyleSheet, 'print');
    end;
  end
  else
  begin
    AInlineStyle := GetInlineStyle;
  end;
  Result := CreateHtml(FDocType, FAuthor, FTitle, FFavicon, AStyleSheet, AInlineStyle, MetaGeneratorName);
end;

procedure TNewHtmlDlg.GetFormValues;
begin
  FDocType := FDlg.DocType;
  FAuthor := FDlg.Author;
  FTitle := FDlg.Title;
  FDefaultStyleSheet := FDlg.DefaultStyleSheet;
  FPrintStyleSheet := FDlg.PrintStyleSheet;
  FBackGroundImage := FDlg.BackgroundImage;
  FUseStyleSheet := FDlg.UseStyleSheet;
  FLeft := FDlg.Left;
  FTop := FDlg.Top;
  FInitialDir := FDlg.InitialPictureDir;
end;

procedure TNewHtmlDlg.SetFormValues;
begin
  if (FTop >= 0) then FDlg.Top := FTop;
  if (FLeft >= 0) then FDlg.Left := FLeft;
  FDlg.InitialPictureDir := FInitialDir;
end;

function TNewHtmlDlg.Execute: Boolean;
begin
  if not Assigned(Fdlg) then FDlg := TNewHtmlDlgForm.Create(nil);
  SetFormValues;

  //Result := True;  //<------ REMOVE THIS

  Result := (FDlg.ShowModal = mrOk);
  if Result then GetFormValues
  else
  begin //always remember Top/Left
    FTop := FDlg.Top;
    FLeft := FDlg.Left;
  end;
  if Result then FHtmlText := GetHtmlText;
end;

constructor TNewHtmlDlg.Create;
begin
  FBackGroundColor := '#ffffff';
  FTextColor := '#000000';
  FLinkColor := '#0000ff';
  FVisitedColor := '#ff00ff';
  FLeft := -1;
  FTop := -1;
  FDocType := DocType_Html_401_Strict;
  FDlg := nil;
end;

destructor TNewHtmlDlg.Destroy;
begin
  if Assigned(FDlg) then FDlg.Free;
  inherited Destroy;
end;

{ THtmlCharMapDlg }

constructor THtmlCharMapDlg.Create;
begin
  FCharMapForm := THtmlCharMapForm.Create(nil);
end;

destructor THtmlCharMapDlg.Destroy;
begin
  if Assigned(FCharMapForm) then FCharMapForm.Free;
end;


procedure THtmlCharMapDlg.Execute;
begin
  FCharMapForm.Show;
end;

procedure THtmlCharMapDlg.SetLeft(AValue: Integer);
begin
  if (AValue < 0) then Exit;
  FCharMapForm.Left := AValue;
end;

function THtmlCharMapDlg.GetLeft: Integer;
begin
  Result := FCharMapForm.Left;
end;

procedure THtmlCharMapDlg.SetTop(AValue: Integer);
begin
  if (AValue < 0) then Exit;
  FCharMapForm.Top := AValue;
end;

function THtmlCharMapDlg.GetTop: Integer;
begin
  Result := FCharMapForm.Top;
end;


function THtmlCharMapDlg.GetCharClick: THtmlCharClickEvent;
begin
  Result := FCharMapForm.OnHtmlCharClick;
end;


procedure THtmlCharMapDlg.SetCharClick(AValue: THtmlCharClickEvent);
begin
  FCharMapForm.OnHtmlCharClick := AValue;
end;



end.

