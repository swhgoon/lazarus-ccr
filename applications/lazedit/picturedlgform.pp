unit PictureDlgForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ExtDlgs, LazEdit_PicsLib, HtmlCode;

type

  { TPictureDlgForm }

  TPictureDlgForm = class(TForm)
    AltEdit: TEdit;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    DefFolderEdit: TEdit;
    ClassEdit: TEdit;
    IdEdit: TEdit;
    HeightEdit: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    FloatGroup: TRadioGroup;
    OpenPictureDialog: TOpenPictureDialog;
    WidthEdit: TEdit;
    Label5: TLabel;
    TitleEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OpenBtn: TSpeedButton;
    SrcEdit: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
  private
    FInitialDir: string;
    function GetAlt: String;
    function GetDefaultFolderPrefix: String;
    function GetDomClass: String;
    function GetDomId: String;
    function GetFloatStyle: String;
    function GetPicHeight: String;
    function GetPicWidth: String;
    function GetSrc: String;
    function GetTitle: String;
    procedure SetDefaultFolderPrefix(AValue: String);
    procedure SetInitialDir(AValue: string);
    { private declarations }
  public
    { public declarations }
    property Src: String read GetSrc;
    property DomId: String read GetDomId;
    property DomClass: String read GetDomClass;
    property FloatStyle: String read GetFloatStyle;
    property PicWidth: String read GetPicWidth;
    property PicHeight: String read GetPicHeight;
    property Alt: String read GetAlt;
    property Title: String read GetTitle;
    property InitialDir: string read FInitialDir write SetInitialDir;
    property DefaultFolderPrefix: String read GetDefaultFolderPrefix write SetDefaultFolderPrefix;
  end;



implementation

{$R *.lfm}

const
  idxLeft = 0;
  idxRight = 1;

{ TPictureDlgForm }

procedure TPictureDlgForm.OpenBtnClick(Sender: TObject);
var
  Fn: String;
  W, H: dword;
begin
  if OpenPictureDialog.Execute then
  begin
    Fn := OpenPictureDialog.FileName;
    FInitialDir := ExtractFileDir(Fn);
    SrcEdit.Text := ExtractFileName(Fn);
    //GetImageSize uses system encoding
    if GetImageSize(Fn, W, H) then
    begin
      WidthEdit.Text := IntToStr(W);
      HeightEdit.Text := IntToStr(H);
    end
    else
    begin
      WidthEdit.Text := '';
      HeightEdit.Text := '';
    end;
  end;
end;

procedure TPictureDlgForm.FormShow(Sender: TObject);
begin
  SrcEdit.Text := '';
  AltEdit.Text := '';
  TitleEdit.Text := '';
  WidthEdit.Text := '';
  HeightEdit.Text := '';
  ActiveControl := SrcEdit;
end;

function TPictureDlgForm.GetAlt: String;
begin
  Result := AltEdit.Text;
end;

function TPictureDlgForm.GetDefaultFolderPrefix: String;
begin
  Result := DefFolderEdit.Text;
end;

function TPictureDlgForm.GetDomClass: String;
begin
  Result := ClassEdit.Text;
end;

function TPictureDlgForm.GetDomId: String;
begin
  Result := IdEdit.Text;
end;

function TPictureDlgForm.GetFloatStyle: String;
begin
  case FloatGroup.ItemIndex of
    idxLeft: Result := 'left';
    idxRight: Result := 'right';
    else Result := '';
  end;
end;



function TPictureDlgForm.GetPicHeight: String;
begin
  Result := HeightEdit.Text;
end;

function TPictureDlgForm.GetPicWidth: String;
begin
  Result := WidthEdit.Text;
end;

function TPictureDlgForm.GetSrc: String;
var
  S: String;
begin
  S := Trim(DefFolderEdit.Text);
  if (S <> '') then S := IncludeTrailingPathDelimiter(S);
  S := S + SrcEdit.Text;
  Result := UrlEscape(S);
end;

function TPictureDlgForm.GetTitle: String;
begin
  Result := TitleEdit.Text;
end;

procedure TPictureDlgForm.SetDefaultFolderPrefix(AValue: String);
begin
  DefFolderEdit.Text := AValue;
end;

procedure TPictureDlgForm.SetInitialDir(AValue: string);
begin
  OpenPictureDialog.InitialDir := AValue;
end;

end.

