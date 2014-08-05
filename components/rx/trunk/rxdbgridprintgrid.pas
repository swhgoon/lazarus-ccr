{ rxdbgrid unit

  Copyright (C) 2005-2014 Lagunov Aleksey alexs@yandex.ru and Lazarus team
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
unit RxDBGridPrintGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, rxdbgrid, LR_Class, LR_DSet, LR_DBSet, contnrs,
  Graphics, Printers;

type
  TRxDBGridPrintOption = (rxpoShowTitle, rxpoShowFooter, rxpoShowFooterColor);
  TRxDBGridPrintOptions = set of TRxDBGridPrintOption;

  { TRxColInfo }

  TRxColInfo = class
    Col:TRxColumn;
    ColWidth:integer;
    ColTitles:TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  { TRxDBGridPrint }

  TRxDBGridPrint = class(TRxDBGridAbstractTools)
  private
    FOptions: TRxDBGridPrintOptions;
    FOrientation: TPrinterOrientation;
    FReport : TfrReport;
    FReportDataSet : TfrDBDataSet;
    FColumnDataSet : TfrUserDataSet;
    FDataSet : TDataset;
    FPage : TfrPage;

    FShowProgress : Boolean;
    FTitleRowCount : integer;
    FRxColInfoList : TObjectList;

    FYPos: Integer;
    FXPos: Integer;
    procedure DoCreateReport;
    procedure DoSetupColumns;
    procedure DoShowTitle;
    procedure DoShowFooter;
    procedure OnPrintColumn(ColNo: Integer; var Width: Integer);
    procedure OnEnterRect(Memo: TStringList; View: TfrView);
  protected
    function DoExecTools:boolean;override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PreviewReport;
  published
    property Orientation: TPrinterOrientation read FOrientation write FOrientation default poPortrait;
    property Options:TRxDBGridPrintOptions read FOptions write FOptions;
    property ShowProgress : Boolean read FShowProgress write FShowProgress default false;
  end;

procedure Register;
implementation

uses math;

{$R rxdbgridprintgrid.res}

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxDBGridPrint]);
end;

{ TRxColInfo }

constructor TRxColInfo.Create;
begin
  inherited Create;
  ColTitles:=TStringList.Create;
end;

destructor TRxColInfo.Destroy;
begin
  ColTitles.Clear;
  FreeAndNil(ColTitles);
  inherited Destroy;
end;

{ TRxDBGridPrint }

procedure TRxDBGridPrint.DoCreateReport;
var
  FBand: TfrBandView;
  FView: TfrMemoView;
begin
  if FReport.Pages.Count=0 then
      FReport.Pages.add;
  FPage := FReport.Pages[FReport.Pages.Count-1];
  FPage.ChangePaper(FPage.pgSize, FPage.Width, FPage.Height, FOrientation);

  FYPos:=0;
  FXPos:=20;

  DoShowTitle;

  FBand := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBand.BandType := btMasterData;
  FBand.Dataset := FReportDataSet.Name;
  FBand.SetBounds(0, FYPos, 1000, 18);
  FBand.Flags:=FBand.Flags or flStretched;
  FPage.Objects.Add(FBand);

  FBand := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBand.BandType := btCrossData;
  FBand.Dataset := FColumnDataSet.Name;
  FBand.SetBounds(FXPos, 0, 20, 1000);
  FPage.Objects.Add(FBand);

  FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
  FView.SetBounds(FXPos, FYPos, 20, 18);
  FView.Memo.Add('[Cell]');
  FView.Flags:=FView.Flags or flStretched;
  FView.Font.Size:=10;
//  FView.Font.Assign(FFont);
  FView.Frames:=frAllFrames;
  FView.Layout:=tlTop;
  FPage.Objects.Add(FView);

  FYPos := FYPos + 22;

  if (RxDBGrid.FooterOptions.Active) and (RxDBGrid.FooterOptions.RowCount>0) then
    DoShowFooter;
end;

procedure TRxDBGridPrint.DoSetupColumns;
var
  P:TRxColInfo;
  i: Integer;
  j: Integer;
begin
  FTitleRowCount:=0;
  FRxColInfoList.Clear;
  for i:=0 to RxDBGrid.Columns.Count-1 do
  begin
    if RxDBGrid.Columns[i].Visible then
    begin
      P:=TRxColInfo.Create;
      FRxColInfoList.Add(P);
      P.Col:=RxDBGrid.Columns[i] as TRxColumn;
      P.ColWidth:=RxDBGrid.Columns[i].Width;
      for j:=0 to TRxColumnTitle(RxDBGrid.Columns[i].Title).CaptionLinesCount-1 do
        P.ColTitles.Add(TRxColumnTitle(RxDBGrid.Columns[i].Title).CaptionLine(j).Caption);
      FTitleRowCount:=Max(FTitleRowCount, P.ColTitles.Count)
    end;
  end;
end;

procedure TRxDBGridPrint.DoShowTitle;
var
  FBand: TfrBandView;
  FView: TfrMemoView;
  i: Integer;
begin
  FBand := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBand.BandType := btMasterHeader;

{!!
  if self.fShowHdOnAllPage then
    FBand.Flags:=FBand.Flags+flBandRepeatHeader;
}
  FBand.SetBounds(FXPos, FYPos, 1000, 20 * FTitleRowCount);
  FBand.Flags:=FBand.Flags or flStretched;
  FPage.Objects.Add(FBand);

  for i:=0 to FTitleRowCount-1 do
  begin
    FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
    FView.SetBounds(FXPos, FYPos, 20, 20);
    FView.Alignment:=taCenter;
    FView.FillColor := clSilver;
  //  FView.Font.Assign(FTitleFont);
    FView.Font.Size:=12;
    FView.Frames:=frAllFrames;
    FView.Layout:=tlTop;
    FView.Memo.Add(Format('Header_%d', [i]));
    FPage.Objects.Add(FView);
    FYPos:=FYPos + 20
  end;
  FYPos := FYPos + 2;
end;

procedure TRxDBGridPrint.DoShowFooter;
var
  FBand: TfrBandView;
  FView: TfrMemoView;
  i: Integer;
begin
  FBand := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBand.BandType := btMasterFooter;

{!!
  if self.fShowHdOnAllPage then
    FBand.Flags:=FBand.Flags+flBandRepeatHeader;
}
  FBand.SetBounds(FXPos, FYPos, 1000, 20);
  FBand.Flags:=FBand.Flags or flStretched;
  FPage.Objects.Add(FBand);

  FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
  FView.SetBounds(FXPos, FYPos, 20, 20);
  FView.Alignment:=taCenter;
  FView.FillColor := RxDBGrid.FooterOptions.Color;
//  FView.Font.Assign(FTitleFont);
  FView.Font.Size:=12;
  FView.Frames:=frAllFrames;
  FView.Layout:=tlTop;
  FView.Memo.Add(Format('Footer', [i]));
  FPage.Objects.Add(FView);

  FYPos := FYPos + 22;
end;

procedure TRxDBGridPrint.OnPrintColumn(ColNo: Integer; var Width: Integer);
begin
  if (ColNo > 0) and (ColNo <= FRxColInfoList.Count) then
    Width := TRxColInfo(FRxColInfoList[ColNo-1]).ColWidth;
end;

procedure TRxDBGridPrint.OnEnterRect(Memo: TStringList; View: TfrView);
var
  C: TRxColumn;
  i, k: Integer;
  F:TRxColInfo;
  S: String;
begin
  i := FColumnDataset.RecNo;

  if (i >= 0) and (i < FRxColInfoList.Count) then
  begin
    F:=TRxColInfo(FRxColInfoList[i]);
    View.dx := F.ColWidth;

    if Assigned(F.Col) and (Memo.Count>0) then
    begin
      S:=Memo[0];
      if (S='[Cell]') and Assigned(F.Col.Field) then
      begin
        Memo[0] := F.Col.Field.DisplayText;
        TfrMemoView(View).Alignment:=F.Col.Alignment;
      end
      else
      if Copy(S, 1, 7) = 'Header_' then
      begin
        TfrMemoView(View).Alignment:=F.Col.Title.Alignment;
        K:=StrToIntDef(Copy(S, 8, Length(S)), 0);
        if TRxColumnTitle(F.Col.Title).CaptionLinesCount = 0 then
        begin
          if K = 0 then
            Memo[0] := TRxColumnTitle(F.Col.Title).Caption
          else
            Memo[0] := '';
        end
        else
        if K<TRxColumnTitle(F.Col.Title).CaptionLinesCount then
        begin;
          Memo[0] :=TRxColumnTitle(F.Col.Title).CaptionLine(k).Caption; //F.Col.Title.Caption;
        end
        else
          Memo[0] := '';
      end
      else
      if S = 'Footer' then
      begin
        Memo[0] :=F.Col.Footer.DisplayText;
        TfrMemoView(View).Alignment:=F.Col.Footer.Alignment;
      end;
    end;

  end;
end;

function TRxDBGridPrint.DoExecTools: boolean;
var
  C:integer;
begin
  Result:=false;
  if (RxDBGrid = nil) or (RxDBGrid.DataSource = nil) or (RxDBGrid.DataSource.Dataset = nil) then
    Exit;

  FDataSet := RxDBGrid.Datasource.Dataset;
  FReport:=TfrReport.Create(Self);
  FReport.OnPrintColumn:=@OnPrintColumn;
  FReport.OnEnterRect:=@OnEnterRect;
  FReportDataSet := TfrDBDataSet.Create(Self);
  FColumnDataSet := TfrUserDataSet.Create(Self);

  try
    DoSetupColumns;

    FReportDataSet.Name := 'frGridDBDataSet1';
    FReportDataSet.DataSet := FDataSet;
//    FReportDataSet.DataSource := RxDBGrid.DataSource;

    FColumnDataSet.Name := 'frGridUserDataSet1';
    FColumnDataSet.RangeEnd := reCount;

    FColumnDataSet.RangeEndCount := FRxColInfoList.Count;

    FReport.ShowProgress:=FShowProgress;
    DoCreateReport;
    FReport.ShowReport;
    Result:=true;
  finally
    FreeAndNil(FColumnDataSet);
    FreeAndNil(FReportDataSet);
    FreeAndNil(FReport);
  end;
end;

constructor TRxDBGridPrint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption:='Print grid';
  FShowProgress:=false;
  FRxColInfoList:=TObjectList.Create(true);
  FOrientation:=poPortrait;
  ShowSetupForm:=false;
  FOptions:=[rxpoShowTitle, rxpoShowFooter, rxpoShowFooterColor];
end;

destructor TRxDBGridPrint.Destroy;
begin
  FreeAndNil(FRxColInfoList);
  inherited Destroy;
end;

procedure TRxDBGridPrint.PreviewReport;
begin
  Execute;
end;

end.

