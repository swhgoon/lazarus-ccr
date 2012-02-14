unit NewHtmlDlgForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ExtDlgs, HtmlCode;

type

  { TNewHtmlDlgForm }

  TNewHtmlDlgForm = class(TForm)
    BackColorEdit: TEdit;
    BackImageEdit: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ColorDialog: TColorDialog;
    AuthorEdit: TEdit;
    DocTypeComboBox: TComboBox;
    DefaultStylesheetEdit: TEdit;
    PictureFolderEdit: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label9: TLabel;
    OpenDialog: TOpenDialog;
    OpenPictureDialog: TOpenPictureDialog;
    PrintStyleSheetEdit: TEdit;
    ExternalStyleBox: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    DefSheetBtn: TSpeedButton;
    PrintSheetBtn: TSpeedButton;
    BackColorBtn: TSpeedButton;
    LinkColorBtn: TSpeedButton;
    BackImgBtn: TSpeedButton;
    VisitedColorBtn: TSpeedButton;
    TextColorBtn: TSpeedButton;
    StyleChoiceGroup: TRadioGroup;
    VisitedLinkColorEdit: TEdit;
    Label5: TLabel;
    LinkColorEdit: TEdit;
    Label4: TLabel;
    TextColorEdit: TEdit;
    InlineStyleBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TitleEdit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure StyleBtnClick(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
    procedure ImgBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StyleChoiceGroupClick(Sender: TObject);
  private
    FInitialDir: String;
    function GetAuthor: string;
    function GetbackgroundColor: string;
    function GetBackgroundImage: string;
    function GetDefaultStyleSheet: string;
    function GetHtmlDocType: THtmlDocType;
    function GetLinkColor: string;
    function GetPictureFolder: string;
    function GetPrintStyleSheet: string;
    function GetTextColor: string;
    function GetTitle: string;
    function GetUseStyleSheet: Boolean;
    function GetVisitedColor: string;
    procedure SetInitialDir(AValue: String);
    { private declarations }
  public
    { public declarations }
    property DocType: THtmlDocType read GetHtmlDocType;
    property UseStyleSheet: Boolean read GetUseStyleSheet;
    property DefaultStyleSheet: string read GetDefaultStyleSheet;
    property PrintStyleSheet: string read GetPrintStyleSheet;
    property TextColor: string read GetTextColor;
    property BackgroundColor: string read GetbackgroundColor;
    property LinkColor: string read GetLinkColor;
    property VisitedColor: string read GetVisitedColor;
    property BackgroundImage: string read GetBackgroundImage;
    property PictureFolder: string read GetPictureFolder;
    property Author: string read GetAuthor;
    property Title: string read GetTitle;
    property InitialPictureDir: String read FInitialDir write SetInitialDir;
  end;



implementation

const
  idxExternal = 0;
  idxInternal = 1;

  tagDefSheet = 1;
  tagPrintSheet = 2;
  tagBackColor = 3;
  tagTextColor = 4;
  tagLinkColor = 5;
  tagVisitedColor = 6;
  tagBackImg = 7;

  StyleFilter = 'Cascading Style Sheets (*.css)|*.css|Alle bestandend (*.*)|*.*';

  HtmlDocTypeNames: THtmlDocTypeStrings = ('Html 4.01 Strict', 'Html 4.01 Transitional', 'Html 5', '');


{ TNewHtmlDlgForm }

procedure TNewHtmlDlgForm.StyleChoiceGroupClick(Sender: TObject);
begin
  case StyleChoicegroup.ItemIndex of
    idxInternal:
      begin
        InlineStyleBox.Enabled := True;
        ExternalStyleBox.Enabled := False;
      end;
    idxExternal:
      begin
        InlineStyleBox.Enabled   := False;
        ExternalStyleBox.Enabled := True;
      end;
  end;
end;

function TNewHtmlDlgForm.GetAuthor: string;
begin
  Result := AuthorEdit.Text;
end;

function TNewHtmlDlgForm.GetbackgroundColor: string;
begin
  Result := BackColorEdit.Text;
end;

function TNewHtmlDlgForm.GetBackgroundImage: string;
begin
  Result := Trim(PictureFolder);
  if (Result <> '') then
  begin
    Result := IncludeTrailingPathDelimiter(Result);
    Result := UrlEscape(Result);
    DoHtmlDirSeparators(Result);
  end;
  Result := Result + BackImageEdit.Text;
end;

function TNewHtmlDlgForm.GetDefaultStyleSheet: string;
begin
  Result := UrlEscape(DefaultStyleSheetEdit.Text);
end;

function TNewHtmlDlgForm.GetHtmlDocType: THtmlDocType;
begin
  Result := THtmlDocType(DocTypeComboBox.ItemIndex);
end;

function TNewHtmlDlgForm.GetLinkColor: string;
begin
  Result := LinkColorEdit.Text;
end;

function TNewHtmlDlgForm.GetPictureFolder: string;
begin
  Result := UrlEscape(PictureFolderEdit.Text);
end;

function TNewHtmlDlgForm.GetPrintStyleSheet: string;
begin
  Result := UrlEscape(PrintStyleSheetEdit.Text);
end;

function TNewHtmlDlgForm.GetTextColor: string;
begin
  Result := TextColorEdit.Text;
end;

function TNewHtmlDlgForm.GetTitle: string;
begin
  Result := TitleEdit.Text;
end;

function TNewHtmlDlgForm.GetUseStyleSheet: Boolean;
begin
  Result := (StyleChoiceGroup.ItemIndex = idxExternal);
end;

function TNewHtmlDlgForm.GetVisitedColor: string;
begin
  Result := VisitedLinkColorEdit.Text;
end;

procedure TNewHtmlDlgForm.SetInitialDir(AValue: String);
begin
  if FInitialDir = AValue then Exit;
  OpenPictureDialog.InitialDir := AValue;
  FInitialDir := AValue;
end;

procedure TNewHtmlDlgForm.StyleBtnClick(Sender: TObject);
var
  Tg: PtrInt;
  Css: String;
begin
  Tg := (Sender as TSpeedButton).Tag;
  OpenDialog.Filter := StyleFilter;
  OpenDialog.FilterIndex := 1;
  if OpenDialog.Execute then
  begin
    Css := OpenDialog.FileName;
    Css := ExtractFileName(Css);
    case Tg of
      tagDefSheet: DefaultStyleSheetEdit.Text := Css;
      tagPrintSheet: PrintStyleSheetEdit.Text := Css;
    end;
  end;
end;

procedure TNewHtmlDlgForm.FormShow(Sender: TObject);
begin
  ActiveControl := TitleEdit;
end;

procedure TNewHtmlDlgForm.ColorBtnClick(Sender: TObject);
var
  AColor: TColor;
  Tg: PtrInt;
  AColorStr: String;
begin
  Tg := (Sender as TSpeedButton).Tag;
  if ColorDialog.Execute then
  begin
    AColor := ColorDialog.Color;
    AColorStr := ColorToHtml(AColor);
    case Tg of
      tagBackColor:
      begin
        BackColorEdit.Text := AColorStr;
      end;
      tagTextColor:
      begin
        TextColorEdit.Text := AColorStr;
      end;
      tagLinkColor:
      begin
        LinkColorEdit.Text := AColorStr;
      end;
      tagVisitedColor:
      begin
        VisitedLinkColorEdit.Text := AColorStr;
      end;
    end;
  end;
end;

procedure TNewHtmlDlgForm.ImgBtnClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    BackImageEdit.Text := ExtractFileName(OpenPictureDialog.FileName);
    FInitialDir := ExtractFilePath(OpenPictureDialog.FileName);
  end;
end;

procedure TNewHtmlDlgForm.FormCreate(Sender: TObject);
var
  h: THtmlDocType;
begin
  DefSheetBtn.Tag := tagDefSheet;
  PrintSheetBtn.Tag := tagPrintSheet;
  BackColorBtn.Tag := tagBackColor;
  TextColorBtn.Tag := tagTextColor;
  LinkColorBtn.Tag := tagLinkColor;
  VisitedColorBtn.Tag := tagVisitedColor;
  BackImgBtn.Tag := tagBackImg;
  DocTypeComboBox.Clear;
  StyleChoiceGroupClick(Self);
  for h := Low(THtmlDocType) to High(THtmlDocType) do DocTypeComboBox.Items.Add(HtmlDoctypeNames[h]);
  DocTypeComboBox.ItemIndex := ord (DocType_Html_401_Strict);
end;



{$R *.lfm}

end.

