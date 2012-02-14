unit NewTableForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Buttons;

type

  { TNewTableForm }

  TNewTableForm = class(TForm)
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    ClassEdit: TEdit;
    IdEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    RowEdit: TSpinEdit;
    ColEdit: TSpinEdit;
    SummaryEdit: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private
    function GetColCount: integer;
    function GetDomClass: String;
    function GetDomId: String;
    function GetRowCount: Integer;
    function GetSummary: string;
    { private declarations }
  public
    { public declarations }
    property Summary: string read GetSummary;
    property DomId: String read GetDomId;
    property DomClass: String read GetDomClass;
    property RowCount: Integer read GetRowCount;
    property ColCount: integer read GetColCount;
  end; 



implementation

{$R *.lfm}

{ TNewTableForm }

procedure TNewTableForm.FormShow(Sender: TObject);
begin
  SummaryEdit.Text := '';
  IdEdit.Text := '';;
  ClassEdit.Text := '';
  ActiveControl := SummaryEdit;
end;

function TNewTableForm.GetColCount: integer;
begin
  Result := ColEdit.Value;
end;

function TNewTableForm.GetDomClass: String;
begin
  Result := ClassEdit.Text;
end;

function TNewTableForm.GetDomId: String;
begin
  Result := IdEdit.Text;
end;

function TNewTableForm.GetRowCount: Integer;
begin
  Result := RowEdit.Value;
end;

function TNewTableForm.GetSummary: string;
begin
  Result := SummaryEdit.Text;
end;

end.

