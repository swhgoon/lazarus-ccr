unit RxDBGridExportSpreadSheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rxdbgrid, DB, fpspreadsheet, Graphics;

type
  TRxDBGridExportSpreadSheetOption = (ressExportTitle,
    ressExportColors,
    ressExportFooter,
    ressOverwriteExisting
    );

  TRxDBGridExportSpreadSheetOptions = set of TRxDBGridExportSpreadSheetOption;

type

  { TRxDBGridExportSpeadSheet }

  TRxDBGridExportSpreadSheet = class(TRxDBGridAbstractTools)
  private
    FFileName: string;
    FOptions: TRxDBGridExportSpreadSheetOptions;
    FPageName: string;
  protected
    FDataSet:TDataSet;
    FWorkbook: TsWorkbook;
    FWorksheet: TsWorksheet;
    FCurRow : integer;
    FCurCol : integer;
    scColorBlack:TsColor;

    procedure DoExportTitle;
    procedure DoExportBody;
    procedure DoExportFooter;
    procedure DoExportColWidth;
    function DoExecTools:boolean;override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FileName:string read FFileName write FFileName;
    property PageName:string read FPageName write FPageName;
    property Options:TRxDBGridExportSpreadSheetOptions read FOptions write FOptions;
  end;

procedure Register;

implementation
uses fpsallformats, LCLType, math, LazUTF8, rxdconst;

{$R rxdbgridexportspreadsheet.res}

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxDBGridExportSpreadSheet]);
end;

const
  ssAligns : array [TAlignment] of TsHorAlignment = (haLeft, haRight, haCenter);

{ TRxDBGridExportSpeadSheet }

procedure TRxDBGridExportSpreadSheet.DoExportTitle;
var
  i, k  : Integer;
  C  : TRxColumn;
  CT : TRxColumnTitle;
  CC : TColor;
  scColor : TsColor;
  CB:TsCellBorders;
  FMaxTitleHeight : integer;
begin
  FCurCol:=0;
  FMaxTitleHeight:=1;
  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i] as TRxColumn;
    CT:=C.Title as TRxColumnTitle;
    FMaxTitleHeight:=Max(FMaxTitleHeight, CT.CaptionLinesCount);
    if C.Visible then
    begin
      if CT.CaptionLinesCount > 0 then
      begin
        for k:=0 to CT.CaptionLinesCount - 1 do
        begin
          CC:=C.Title.Color;
          if (CC and SYS_COLOR_BASE) = 0  then
          begin
            scColor:=FWorkbook.AddColorToPalette(CC);
            FWorksheet.WriteBackgroundColor(FCurRow, FCurCol, scColor);
          end;

          CB:=[cbNorth, cbWest, cbEast, cbSouth];

          FWorksheet.WriteBorderColor(FCurRow + k, FCurCol, cbNorth, scColorBlack);

          if not Assigned(CT.CaptionLine(k).Next) then
            FWorksheet.WriteBorderColor(FCurRow + k, FCurCol, cbWest, scColorBlack)
          else
            CB:=CB - [cbWest];

          if not Assigned(CT.CaptionLine(k).Prior) then
            FWorksheet.WriteBorderColor(FCurRow + k, FCurCol, cbEast, scColorBlack)
          else
            CB:=CB - [cbEast];

          FWorksheet.WriteBorderColor(FCurRow + k ,FCurCol, cbSouth, scColorBlack);

          FWorksheet.WriteBorders(FCurRow + k, FCurCol, CB);

          FWorksheet.WriteHorAlignment(FCurRow + k, FCurCol, ssAligns[C.Title.Alignment]);

          FWorksheet.WriteUTF8Text(FCurRow + k, FCurCol, CT.CaptionLine(k).Caption);
        end;
      end
      else
      begin
        CC:=C.Title.Color;
        if (CC and SYS_COLOR_BASE) = 0  then
        begin
          scColor:=FWorkbook.AddColorToPalette(CC);
          FWorksheet.WriteBackgroundColor( FCurRow, FCurCol, scColor);
        end;

        FWorksheet.WriteBorders(FCurRow,FCurCol, [cbNorth, cbWest, cbEast, cbSouth]);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbNorth, scColorBlack);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbWest, scColorBlack);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbEast, scColorBlack);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbSouth, scColorBlack);

        FWorksheet.WriteHorAlignment(FCurRow, FCurCol, ssAligns[C.Title.Alignment]);

        FWorksheet.WriteUTF8Text(FCurRow, FCurCol, C.Title.Caption);

      end;

      inc(FCurCol);
    end;
  end;

  inc(FCurRow, FMaxTitleHeight);
end;

procedure TRxDBGridExportSpreadSheet.DoExportBody;
var
  i : Integer;
  C : TRxColumn;
  CT : TRxColumnTitle;
  CC : TColor;
  scColor : TsColor;
begin
  FDataSet.First;
  while not FDataSet.EOF do
  begin
    FCurCol:=0;
    for i:=0 to FRxDBGrid.Columns.Count - 1 do
    begin
      C:=FRxDBGrid.Columns[i] as TRxColumn;
      CT:=C.Title as TRxColumnTitle;
      if C.Visible then
      begin
        FWorksheet.WriteUTF8Text(FCurRow, FCurCol, C.Field.DisplayText);
        CC:=C.Color;
        if (CC and SYS_COLOR_BASE) = 0  then
        begin
//          CC:=clWhite;
          scColor:=FWorkbook.AddColorToPalette(CC);
          FWorksheet.WriteBackgroundColor(FCurRow,FCurCol, scColor);
        end;

        FWorksheet.WriteBorders(FCurRow,FCurCol, [cbNorth, cbWest, cbEast, cbSouth]);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbNorth, scColorBlack);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbWest, scColorBlack);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbEast, scColorBlack);
        FWorksheet.WriteBorderColor(FCurRow,FCurCol, cbSouth, scColorBlack);

        FWorksheet.WriteHorAlignment(FCurRow, FCurCol, ssAligns[C.Alignment]);
        inc(FCurCol);
      end;
    end;
    inc(FCurRow);
    FDataSet.Next;
  end;
end;

procedure TRxDBGridExportSpreadSheet.DoExportFooter;
begin

end;

procedure TRxDBGridExportSpreadSheet.DoExportColWidth;
var
  FW:integer;
  C:TRxColumn;
  i: Integer;
begin
  FW:=FRxDBGrid.Canvas.TextWidth('W');
  FCurCol:=0;
  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i] as TRxColumn;
    if C.Visible then
    begin
      FWorksheet.WriteColWidth(FCurCol, Max(C.Width div FW, 20));
      inc(FCurCol);
    end;
  end;
end;


constructor TRxDBGridExportSpreadSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption:=sToolsExportSpeadSheet;
end;

function TRxDBGridExportSpreadSheet.DoExecTools: boolean;
var
  P:TBookMark;
begin
  Result:=false;
  if (not Assigned(FRxDBGrid)) or (not Assigned(FRxDBGrid.DataSource)) or (not Assigned(FRxDBGrid.DataSource.DataSet)) then
    exit;
  FDataSet:=FRxDBGrid.DataSource.DataSet;
  FDataSet.DisableControls;
  P:=FDataSet.Bookmark;

  FWorkbook := TsWorkbook.Create;
  FWorksheet := FWorkbook.AddWorksheet(FPageName);
  try
    scColorBlack:=FWorkbook.AddColorToPalette(FRxDBGrid.GridLineColor);
    FCurRow:=0;

    if ressExportTitle in FOptions then
      DoExportTitle;
    DoExportBody;

    if ressExportFooter in FOptions then
      DoExportFooter;

    DoExportColWidth;

    FWorkbook.WriteToFile(UTF8ToSys(FileName), true);
    Result:=true;
  finally
    FWorkbook.Free;
    FDataSet.Bookmark:=P;
    FDataSet.EnableControls;
  end;
end;

end.
