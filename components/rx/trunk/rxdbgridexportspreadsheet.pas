{ RxDBGridExportSpreadSheet unit

  Copyright (C) 2005-2013 Lagunov Aleksey alexs@yandex.ru
  original conception from rx library for Delphi (c)

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

unit RxDBGridExportSpreadSheet;

{$I rx.inc}

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

  { TRxDBGridExportSpreadSheet }

  TRxDBGridExportSpreadSheet = class(TRxDBGridAbstractTools)
  private
    FFileName: string;
    FOpenAfterExport: boolean;
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
    function DoSetupTools:boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FileName:string read FFileName write FFileName;
    property PageName:string read FPageName write FPageName;
    property Options:TRxDBGridExportSpreadSheetOptions read FOptions write FOptions;
    property OpenAfterExport:boolean read FOpenAfterExport write FOpenAfterExport default false;
  end;

procedure Register;

implementation
uses fpsallformats, LCLType, Forms, math, LazUTF8, rxdconst, Controls, LCLIntf,
  RxDBGridExportSpreadSheet_ParamsUnit;

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
  FOpenAfterExport:=false;
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
  {$IFDEF NoAutomatedBookmark}
  P:=FDataSet.GetBookmark;
  {$ELSE}
  P:=FDataSet.Bookmark;
  {$ENDIF}

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
    {$IFDEF NoAutomatedBookmark}
    FDataSet.GotoBookmark(P);
    FDataSet.FreeBookmark(P);
    {$ELSE}
    FDataSet.Bookmark:=P;
    {$ENDIF}
    FDataSet.EnableControls;
  end;

  if Result then
    OpenDocument(FileName);
end;

function TRxDBGridExportSpreadSheet.DoSetupTools: boolean;
var
  F:TRxDBGridExportSpreadSheet_ParamsForm;
begin
  F:=TRxDBGridExportSpreadSheet_ParamsForm.Create(Application);
  F.FileNameEdit1.FileName:=FFileName;
  F.cbOpenAfterExport.Checked:=FOpenAfterExport;
  F.cbExportColumnFooter.Checked:=ressExportFooter in FOptions;
  F.cbExportColumnHeader.Checked:=ressExportTitle in FOptions;
  F.cbExportCellColors.Checked:=ressExportColors in FOptions;
  F.cbOverwriteExisting.Checked:=ressOverwriteExisting in FOptions;
  F.edtPageName.Text:=FPageName;

  Result:=F.ShowModal = mrOk;
  if Result then
  begin
    FOpenAfterExport:=F.cbOpenAfterExport.Checked;
    FFileName:=F.FileNameEdit1.FileName;
    FPageName:=F.edtPageName.Text;

    FOptions:=[];
    if F.cbExportColumnFooter.Checked then
      FOptions :=FOptions + [ressExportFooter];
    if F.cbExportColumnHeader.Checked then
      FOptions :=FOptions + [ressExportTitle];
    if F.cbExportCellColors.Checked then
      FOptions :=FOptions + [ressExportColors];
    if F.cbOverwriteExisting.Checked then
      FOptions :=FOptions + [ressOverwriteExisting];
  end;
  F.Free;
end;

end.
