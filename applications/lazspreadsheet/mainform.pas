unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, EditBtn, ExtCtrls, ComCtrls, fpspreadsheetchart,
  fpspreadsheetgrid, TAGraph, TASeries;

type
  
  { Tlazfpsmainform }

  Tlazfpsmainform = class(TForm)
    btnLoadSpreadsheet: TButton;
    editSourceFile: TFileNameEdit;
    Label2: TLabel;
    pagesSheets: TPageControl;
    Panel1: TPanel;
    procedure btnLoadSpreadsheetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Worksheets: array of TsWorksheetGrid;
    procedure DeleteAllSheets();
  end; 

var
  lazfpsmainform: Tlazfpsmainform;

implementation

uses
  // FPSpreadsheet and supported formats
  fpspreadsheet, xlsbiff8, xlsbiff5, xlsbiff2, xlsxooxml, fpsopendocument;

{$R *.lfm}

{ Tlazfpsmainform }

procedure Tlazfpsmainform.btnLoadSpreadsheetClick(Sender: TObject);
var
  lWorkbook: TsWorkbook;
  lWorksheetCount: Cardinal;
  lCurPage: TTabSheet;
  lCurWorksheet: TsWorksheet;
  i: Integer;
begin
  if editSourceFile.Text = '' then Exit;

  lWorkbook := TsWorkbook.Create;
  try
    lWorkbook.ReadFromFile(editSourceFile.Text);

    DeleteAllSheets();

    lWorksheetCount := lWorkbook.GetWorksheetCount();
    SetLength(Worksheets, lWorksheetCount);
    for i := 0 to lWorksheetCount-1 do
    begin
      pagesSheets.AddTabSheet();
      lCurPage := pagesSheets.Pages[i];
      lCurWorksheet := lWorkbook.GetWorksheetByIndex(i);

      Worksheets[i] := TsWorksheetGrid.Create(nil);
      Worksheets[i].Parent := lCurPage;
      Worksheets[i].Align := alClient;
      //Worksheets[i].Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goSmoothScroll]
      Worksheets[i].LoadFromWorksheet(lCurWorksheet);
      lCurPage.Caption := lCurWorksheet.Name;
    end;
  finally
    lWorkbook.Free;
  end;
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
  editSourceFile.InitialDir := ExtractFilePath(ParamStr(0));
end;

end.

