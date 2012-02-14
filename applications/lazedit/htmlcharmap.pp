unit HtmlCharMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, LCLProc, Buttons;

type

  THtmlCharClickEvent = procedure(AHtmlEntity: String) of object;

  { THtmlCharmapForm }

  THtmlCharmapForm = class(TForm)
    CloseBtn: TBitBtn;
    ExampleLabel: TLabel;
    Label1: TLabel;
    SelectionBox: TComboBox;
    EntityGrid: TStringGrid;
    procedure EntityGridKeyPress(Sender: TObject; var Key: char);
    procedure EntityGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EntityGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EntityGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SelectionBoxChange(Sender: TObject);
  private
    { private declarations }
    FOnHtmlCharClick: THtmlCharClickEvent;
    procedure DoOnHtmlCharClick(AValue: String);
    procedure FillMisc;
    procedure FillGreek;
    procedure FillMath;
    procedure FillArrow;
    procedure ClearEntityGrid;
    function RowColToIndex(const Row, Col: Integer): Integer;
    function HtmlEntityFromIndex(SelectionIndex, CellIndex: Integer): String;

  public
    { public declarations }
    property OnHtmlCharClick: THtmlCharClickEvent read FOnHtmlCharClick write FOnHtmlCharClick;
  end;


implementation

{$R *.lfm}



const
  idxMisc = 0;
  idxGreek = 1;
  idxMath = 2;
  idxArrow = 3;

  SHint = 'Klik op het teken om het als HTML-entiteit in te voegen.'^m+
  'De HTML-entiteit die ingevoegd zal worden staat in het voorbeeld';

  MiscCount = 24;
  MiscCaptions: Array[0..MiscCount-1] of String = ('CopyRight','Registered','Trademark','Paragraaf','Sectie',
     'Ampersand (&)','Harde spatie','Zacht koppelteken','Euro','Pond','Yen','Gulden','Micro','AE ligatuur','ae ligatuur',
     'OE ligatuur','oe ligatuur','O slash','o slash','<<','>>','! omgekeerd','? omgekeerd','Bullet');

  MiscEntities: Array[0..MiscCount-1] of String = ('&copy;','&reg;','&#8482;','&para;','&sect;',
     '&amp;','&nbsp;','&shy;','&euro;','&pound;','&yen;','&fnof;','&micro;','&Aelig;','&aelig;',
     '&Oelig;','&oelig;','&Oslash;','&oslash;','&laquo;','&raquo','&iexlc;','&iquest;','&bull');

  GreekCount = 33;
  GreekCaptions: Array[0..GreekCount -1] of String = ('alpha','beta','gamma','delta','epsilon','zeta','eta','theta',
     'kappa','lambda','mu','nu','xi','pi','rho','sigma','sigma (als laatste)','tau','upsilon','phi',
     'chi','psi','omega','GAMMA','DELTA','THETA','THETA symbol','XI','RHO','SIGMA','PHI','PSI','OMEGA');

  GreekEntities: Array[0..GreekCount -1] of String = ('&alpha;','&beta;','&gamma;','&delta;','&epsilon;','&zeta;','&eta;','&theta;',
     '&kappa;','&lambda;','&mu;','&nu;','&xi;','&pi;','&rho;','&sigma;','&sigmaf;','&tau;','&upsilon;','&phi;',
     '&chi;','&psi;','&omega;','&Gamma;','&Delta','&Theta;','&thetatsym;','&Xi;','&Rho;','&Sigma;','&Phi','&Psi;','&Omega;');

  MathCount = 14;
  MathCaptions: Array[0..MathCount - 1] of String = ('Kleiner dan','Groter dan','Kleiner of gelijk','Groter of gelijk',
     'Plusminus','Kwart','Half','Driekwart','Kwadraat','Derde macht','Gedeeld door','Graden','Wortelteken','Oneindig');

  MathEntities: Array[0..MathCount - 1] of String = ('&lt;','&gt;','&le;','&ge;',
     '&plusmin;','&frac14;','&frac12;','&frac34;','&sup2;','&sup3;','&divide;','&deg;','&radic;','&inf;');

  ArrowCount = 10;
  ArrowCaptions: Array[0..ArrowCount -1] of String = ('Links','Rechts','Op','Neer','LinksRechts',
    'Dubbel links','Dubbel rechts','Dubbel op','Dubbel neer','Dubbel LR');

  ArrowEntities: Array[0..ArrowCount -1] of String = ('&larr;','&rarr;','&uarr;','&darr','&harr',
    '&lArr;','&rArr;','&dArr;','&uArr;','&hArr;');


{ THtmlCharmapForm }

procedure THtmlCharmapForm.SelectionBoxChange(Sender: TObject);
begin
  case SelectionBox.ItemIndex of
    idxMisc: FillMisc;
    idxGreek: FillGreek;
    idxMath: FillMath;
    idxArrow: FillArrow;
    else FillMisc;
  end;
end;

procedure THtmlCharmapForm.DoOnHtmlCharClick(AValue: String);
begin
  if (AValue <> '') and Assigned(FOnHtmlCharClick) then FOnHtmlCharClick(AValue);
end;

procedure THtmlCharmapForm.FormCreate(Sender: TObject);
begin
  SelectionBox.ItemIndex := idxMisc;
  EntityGrid.Hint := SHint;
end;


procedure THtmlCharmapForm.EntityGridKeyPress(Sender: TObject; var Key: char);
var
  Idx: Integer;
  S: String;
begin
  if (Key = #13) then
  begin
    Key := #0;
    Idx := RowColToIndex(EntityGrid.Row, EntityGrid.Col);
    S := HtmlEntityFromIndex(SelectionBox.ItemIndex, Idx);
    DoOnHtmlCharClick(S);
  end;
end;

procedure THtmlCharmapForm.EntityGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Row, Col, Idx: Integer;
  S: String;
begin
  //debugln('THtmlCharmapForm.EntityGridMouseDown');
  if (Button = mbLeft) and (EntityGrid.MouseToGridZone(X, Y) = gzNormal) then
  begin
    EntityGrid.MouseToCell(X, Y, Col, Row);
    //debugln('  Col = ',dbgs(col),' Row = ',dbgs(row));
    Idx := RowColToIndex(Row, Col);
    //debugln('  Idx = ',dbgs(idx));
    S := HtmlEntityFromIndex(SelectionBox.ItemIndex, Idx);
    //debugln('  S = ',S);
    DoOnHtmlCharClick(S);
  end;
end;

procedure THtmlCharmapForm.EntityGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Row, Col, Idx: Integer;
  S: String;
begin
  //debugln('THtmlCharmapForm.EntityGridMouseMove');
  if (EntityGrid.MouseToGridZone(X, Y) = gzNormal) then
  begin
    EntityGrid.MouseToCell(X, Y, Col, Row);
    //debugln('  Col = ',dbgs(col),' Row = ',dbgs(row));
    Idx := RowColToIndex(Row, Col);
    //debugln('  Idx = ',dbgs(idx));
    S := HtmlEntityFromIndex(SelectionBox.ItemIndex, Idx);
    //debugln('  S = ',S);
    EntityGrid.Row := Row;
    EntityGrid.Col := Col;
    ExampleLabel.Caption := S;
  end;
end;

procedure THtmlCharmapForm.EntityGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if (gdSelected in aState) then
  begin
    EntityGrid.Canvas.Brush.Color := clLime;
  end
end;

procedure THtmlCharmapForm.FormShow(Sender: TObject);
begin
  SelectionBoxChange(Self);
  ExampleLabel.Caption := '';
end;



procedure THtmlCharmapForm.FillMisc;
var
  Col, Row, i: Integer;
begin
  //debugln('FillMisc');
  ClearEntityGrid;
  i := (MiscCount div EntityGrid.ColCount);
  if (MiscCount mod EntityGrid.ColCount) <> 0 then Inc(i);
  EntityGrid.RowCount := i;
  Col := 0;
  Row := 0;
  for i := 0 to MiscCount - 1 do
  begin
    EntityGrid.Cells[Col, Row] := MiscCaptions[i];
    Inc(Col);
    if (Col > EntityGrid.ColCount - 1) then
    begin
      Col := 0;
      Inc(Row);
      if (Row > EntityGrid.RowCount - 1) then Break;
    end;
  end;

end;

procedure THtmlCharmapForm.FillGreek;
var
  Col, Row, i: Integer;
begin
  //debugln('FillGreek');
  ClearEntityGrid;
  i := (GreekCount div EntityGrid.ColCount);
  if (GreekCount mod EntityGrid.ColCount) <> 0 then Inc(i);
  EntityGrid.RowCount := i;
  Col := 0;
  Row := 0;
  for i := 0 to GreekCount - 1 do
  begin
    EntityGrid.Cells[Col, Row] := GreekCaptions[i];
    Inc(Col);
    if (Col > EntityGrid.ColCount - 1) then
    begin
      Col := 0;
      Inc(Row);
      if (Row > EntityGrid.RowCount - 1) then Break;
    end;
  end;

end;

procedure THtmlCharmapForm.FillMath;
var
  Col, Row, i: Integer;
begin
  //debugln('FillMath');
  ClearEntityGrid;
  i := (MathCount div EntityGrid.ColCount);
  if (MathCount mod EntityGrid.ColCount) <> 0 then Inc(i);
  EntityGrid.RowCount := i;
  Col := 0;
  Row := 0;
  for i := 0 to MathCount - 1 do
  begin
    EntityGrid.Cells[Col, Row] := MathCaptions[i];
    Inc(Col);
    if (Col > EntityGrid.ColCount - 1) then
    begin
      Col := 0;
      Inc(Row);
      if (Row > EntityGrid.RowCount - 1) then Break;
    end;
  end;

end;

procedure THtmlCharmapForm.FillArrow;
var
  Col, Row, i: Integer;
begin
  //debugln('FillArrow');
  ClearEntityGrid;
  i := (ArrowCount div EntityGrid.ColCount);
  if (ArrowCount mod EntityGrid.ColCount) <> 0 then Inc(i);
  EntityGrid.RowCount := i;
  Col := 0;
  Row := 0;
  for i := 0 to ArrowCount - 1 do
  begin
    EntityGrid.Cells[Col, Row] := ArrowCaptions[i];
    Inc(Col);
    if (Col > EntityGrid.ColCount - 1) then
    begin
      Col := 0;
      Inc(Row);
      if (Row > EntityGrid.RowCount - 1) then Break;
    end;
  end;

end;
procedure THtmlCharmapForm.ClearEntityGrid;
begin
  EntityGrid.Clean(0,0,EntityGrid.ColCount-1,EntityGrid.RowCount-1, [gzNormal, gzFixedCols, gzFixedRows, gzFixedCells]);
  EntityGrid.Col := 0;
  EntityGrid.Row := 0;
end;

function THtmlCharmapForm.RowColToIndex(const Row, Col: Integer): Integer;
begin
  Result := Row * EntityGrid.ColCount + Col;
end;

function THtmlCharmapForm.HtmlEntityFromIndex(SelectionIndex, CellIndex: Integer): String;
begin
  Result := '';
  if (CellIndex < 0) then exit;
  case SelectionIndex of
    idxMisc: if (CellIndex < MiscCount) then Result := MiscEntities[CellIndex];
    idxGreek: if (CellIndex < GreekCount) then Result := GreekEntities[CellIndex];
    idxMath: if (CellIndex < MathCount) then Result := MathEntities[CellIndex];
    idxArrow: if (CellIndex < ArrowCount) then result := ArrowEntities[CellIndex];
  end;
end;

end.

