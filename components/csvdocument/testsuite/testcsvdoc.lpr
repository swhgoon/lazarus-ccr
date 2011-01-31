program testcsvdoc;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, CsvDocument, DateUtils;

function ReadStringFromFile(AFileName: string): string;
var
  FileStream: TFileStream;
  Size: Integer;
begin
  Result := '';
  if not FileExists(AFileName) then
    Exit;
  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  Size := FileStream.Size;
  if Size > 0 then
  begin
    SetLength(Result, Size);
    FileStream.ReadBuffer(Result[1], Size);
  end;
  FreeAndNil(FileStream);
end;

procedure FindTestFiles(AFileList: TStringList; const ASpec: string);
var
  SearchRec: TSearchRec;
  TestFilesPath: String;
begin
  AFileList.Clear;
  TestFilesPath := IncludeTrailingPathDelimiter(GetCurrentDir)
    + 'tests' + DirectorySeparator + ASpec + DirectorySeparator;
  if FindFirst(TestFilesPath + '*.csv', faAnyFile, SearchRec) = 0 then
    repeat
      AFileList.Add(TestFilesPath + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

procedure TestCsvFile(const AFilename: String; ADocument: TCSVDocument);
var
  InBuffer, OutBuffer: String;
  SampleBuffer: String;
  CsvDoc: TCSVDocument;
  Start: TDateTime;
  MSec: Int64;
begin
  InBuffer := ReadStringFromFile(AFilename);
  SampleBuffer := ReadStringFromFile(ChangeFileExt(AFilename,
    '.sample' + ExtractFileExt(AFilename)));
  if SampleBuffer = '' then
    SampleBuffer := InBuffer;

  ADocument.CSVText := '';
  Start := Now;
  ADocument.CSVText := InBuffer;
  MSec := MilliSecondsBetween(Start, Now);
  OutBuffer := ADocument.CSVText;

  Write(ExtractFileName(AFilename));
  if OutBuffer = InBuffer then
  begin
    Write(': ok');
    WriteLn('   (parsed in ', MSec, ' ms)');
  end else
  begin
    WriteLn(': FAILED');
    WriteLn('--- Expected: ---');
    WriteLn(SampleBuffer);
    WriteLn('--- Got: --------');
    WriteLn(OutBuffer);
    WriteLn('-----------------');
  end;
end;

procedure PerformTests(ADocument: TCSVDocument; const ASpec: String);
var
  I: Integer;
  TestFiles: TStringList;
begin
  WriteLn('== Format: ', ASpec, ' ==');
  TestFiles := TStringList.Create;
  FindTestFiles(TestFiles, ASpec);
  for I := 0 to TestFiles.Count - 1 do
    TestCsvFile(TestFiles[I], ADocument);
  FreeAndNil(TestFiles);
  WriteLn();
end;

var
  CsvDoc: TCSVDocument;
begin
  WriteLn('Testing CSVDocument');
  WriteLn('-------------------');
  CsvDoc := TCSVDocument.Create;

  // no setup needed, rfc4180 supported out-of-the-box
  PerformTests(CsvDoc, 'rfc4180');

  // setup for unofficial Creativyst spec
  PerformTests(CsvDoc, 'unofficial');

  // setup for MS Excel files
  PerformTests(CsvDoc, 'msexcel');

  // setup for OOo Calc files
  PerformTests(CsvDoc, 'oocalc');

  FreeAndNil(CsvDoc);
  WriteLn('------------------');
  WriteLn('All tests complete');
end.

