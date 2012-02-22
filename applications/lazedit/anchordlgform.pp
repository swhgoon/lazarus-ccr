unit AnchorDlgForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, HtmlCode;

type

  TAnchorType = (atUrl, atMakeLocalAnchor, atRefLocalAnchor);

  { TAnchorDlgForm }

  TAnchorDlgForm = class(TForm)
    CancelBtn: TBitBtn;
    DescrEdit: TEdit;
    AnchorTypeGroup: TRadioGroup;
    ClassEdit: TEdit;
    IdEdit: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    TitleEdit: TEdit;
    Label3: TLabel;
    UrlEdit: TEdit;
    Label1: TLabel;
    UrlLabel: TLabel;
    OkBtn: TBitBtn;
    procedure AnchorTypeGroupClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    function GetAnchorType: TAnchorType;
    function GetDescription: String;
    function GetDomClass: String;
    function GetDomId: String;
    function GetTitle: String;
    function GetUrl: String;
    procedure SetDescription(AValue: String);
    procedure SetDomClass(AValue: String);
    procedure SetDomId(AValue: String);
    procedure SetTitle(AValue: String);
    procedure SetUrl(AValue: String);
  public
    { public declarations }
    property AnchorType: TAnchorType read GetAnchorType;
    property Description: String read GetDescription write SetDescription;
    property Url: String read GetUrl write SetUrl;
    property Title: String read GetTitle write SetTitle;
    property DomId: String read GetDomId write SetDomId;
    property DomClass: String read GetDomClass write SetDomClass;
  end; 


implementation

{$R *.lfm}

const
  idxUrl = 0;
  idxMakeLocalAnchor = 1;
  idxRefLocalAnchor = 2;

{ TAnchorDlgForm }

procedure TAnchorDlgForm.AnchorTypeGroupClick(Sender: TObject);
begin
  case AnchorTypeGroup.ItemIndex of
    idxUrl: UrlLabel.Caption := 'Url';
    idxMakeLocalAnchor: UrlLabel.Caption := 'Anker naam';
    idxRefLocalAnchor: UrlLabel.Caption := 'Anker naam';
    else UrlLabel.Caption := 'Url';
  end;
end;

procedure TAnchorDlgForm.FormShow(Sender: TObject);
begin
  ActiveControl := DescrEdit;
  UrlEdit.Text := '';
end;

function TAnchorDlgForm.GetAnchorType: TAnchorType;
begin
  case AnchorTypeGroup.ItemIndex of
    idxUrl: Result := atUrl;
    idxMakeLocalAnchor: Result := atMakeLocalAnchor;
    idxRefLocalAnchor: Result := atRefLocalAnchor;
    else Result := atUrl;
  end;
end;

function TAnchorDlgForm.GetDescription: String;
begin
  Result := DescrEdit.Text;
end;

function TAnchorDlgForm.GetDomClass: String;
begin
  Result := ClassEdit.Text;
end;

function TAnchorDlgForm.GetDomId: String;
begin
  Result := IdEdit.Text;
end;

function TAnchorDlgForm.GetTitle: String;
begin
  Result := TitleEdit.Text;
end;

function TAnchorDlgForm.GetUrl: String;
begin
  Result := UrlEscape(UrlEdit.Text);
end;

procedure TAnchorDlgForm.SetDescription(AValue: String);
begin
  DescrEdit.Text := AValue;
end;

procedure TAnchorDlgForm.SetDomClass(AValue: String);
begin
  ClassEdit.Text := AValue;
end;

procedure TAnchorDlgForm.SetDomId(AValue: String);
begin
  IdEdit.Text := AValue;
end;

procedure TAnchorDlgForm.SetTitle(AValue: String);
begin
  TitleEdit.Text := AValue;
end;

procedure TAnchorDlgForm.SetUrl(AValue: String);
begin
  UrlEdit.Text := AValue;
end;

end.

