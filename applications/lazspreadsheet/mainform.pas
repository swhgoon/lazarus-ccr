unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, EditBtn, ExtCtrls, ComCtrls, fpspreadsheetchart,
  fpspreadsheetgrid, TAGraph, TASeries, TypInfo,
  // FPSpreadsheet and supported formats
  fpspreadsheet, xlsbiff8, xlsbiff5, xlsbiff2, xlsxooxml, fpsopendocument
  ;

type
  
  { Tlazfpsmainform }

  Tlazfpsmainform = class(TForm)
    btnLoadSpreadsheet: TButton;
    buttonReadCellInfo: TButton;
    editSourceFile: TFileNameEdit;
    Label2: TLabel;
    memoCellData: TMemo;
    pagesSheets: TPageControl;
    Panel1: TPanel;
    procedure btnLoadSpreadsheetClick(Sender: TObject);
    procedure buttonReadCellInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Worksheets: array of TsWorksheetGrid;
    Workbook: TsWorkbook;
    procedure DeleteAllSheets();
  end; 

var
  lazfpsmainform: Tlazfpsmainform;

implementation

{$R *.lfm}

{ Tlazfpsmainform }

procedure Tlazfpsmainform.btnLoadSpreadsheetClick(Sender: TObject);
var
  lWorksheetCount: Cardinal;
  lCurPage: TTabSheet;
  lCurWorksheet: TsWorksheet;
  i: Integer;
begin
  if editSourceFile.Text = '' then Exit;

  Workbook.ReadFromFile(editSourceFile.Text);

  DeleteAllSheets();

  lWorksheetCount := Workbook.GetWorksheetCount();
  SetLength(Worksheets, lWorksheetCount);
  for i := 0 to lWorksheetCount-1 do
  begin
    pagesSheets.AddTabSheet();
    lCurPage := pagesSheets.Pages[i];
    lCurWorksheet := Workbook.GetWorksheetByIndex(i);

    Worksheets[i] := TsWorksheetGrid.Create(nil);
    Worksheets[i].Parent := lCurPage;
    Worksheets[i].Align := alClient;
    //Worksheets[i].Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goSmoothScroll]
    Worksheets[i].LoadFromWorksheet(lCurWorksheet);
    lCurPage.Caption := lCurWorksheet.Name;
  end;
end;

procedure Tlazfpsmainform.buttonReadCellInfoClick(Sender: TObject);
var
  lX, lY, lCurTab: LongInt;
  lCurWorksheet: TsWorksheet;
  lCurCell: PCell;
begin
  lCurTab := pagesSheets.TabIndex;
  lX := Worksheets[lCurTab].Selection.Left;
  lY := Worksheets[lCurTab].Selection.Top;
  lCurWorksheet := Workbook.GetWorksheetByIndex(lCurTab);
  lCurCell := lCurWorksheet.GetCell(lY, lX);
  memoCellData.Lines.Text := '';
  memoCellData.Lines.Add(Format('Row: %d Col: %d (zero-based)', [lY, lX]));
  memoCellData.Lines.Add(Format('ContentType: %s', [GetEnumName(TypeInfo(TCellContentType), integer(lCurCell^.ContentType))]));
  memoCellData.Lines.Add(Format('NumberValue: %f', [lCurCell^.NumberValue]));
  memoCellData.Lines.Add(Format('UTF8StringValue: %s', [lCurCell^.UTF8StringValue]));
  //memoCellData.Lines.Add(Format('DateTimeValue: %s', [lCurCell^.DateTimeValue]));
  //memoCellData.Lines.Add(Format('UsedFormattingFields: %f', [lCurCell^.NumberValue]));
  memoCellData.Lines.Add(Format('TextRotation: %s', [GetEnumName(TypeInfo(TsTextRotation), integer(lCurCell^.TextRotation))]));
  //memoCellData.Lines.Add(Format('Border: %f', [lCurCell^.NumberValue]));
  memoCellData.Lines.Add(Format('BackgroundColor: %s', [GetEnumName(TypeInfo(TsColor), integer(lCurCell^.BackgroundColor))]));
  memoCellData.Lines.Add('');
  memoCellData.Lines.Add(Format('ReadAsUTF8Text(): %s', [lCurWorksheet.ReadAsUTF8Text(lY, lX)]));
end;

procedure Tlazfpsmainform.DeleteAllSheets;
var
  i: Integer;
begin
  for i := 0 to Length(Worksheets)-1 do
  begin
    Worksheets[i].Free;
    pagesSheets.Pages[i].Free;
  end;
  SetLength(Worksheets, 0);
end;

procedure Tlazfpsmainform.FormCreate(Sender: TObject);
begin
  Workbook := TsWorkbook.Create;
  editSourceFile.InitialDir := ExtractFilePath(ParamStr(0));
end;

procedure Tlazfpsmainform.FormDestroy(Sender: TObject);
begin
  Workbook.Free;
end;

end.

