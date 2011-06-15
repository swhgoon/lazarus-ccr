unit android_sdk_bindings_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TAndroidSDKBindingsGen }

  TAndroidSDKBindingsGen = class
  private
    FSourceFile, FPasOutputClasses, FPasOutputImpl: TStringList;
    FClassName: string; // Class name of the class currently being parsed
    procedure ProcessModelFile(ASourceFile, APasOutputFile, AJavaOutputDir: string);
    procedure ProcessModelLine(ASourceLine: string);
    function GetNextWord(ALine: string; var AStartPos: Integer): string;
    function GetPascalTypeName(ABaseName: string): string;
  public
    procedure GenerateAllBindings(AInputDir, APasOutputDir, AJavaOutputDir: string);
  end;

var
  AndroidSDKBindingsGen: TAndroidSDKBindingsGen;

implementation

{ TAndroidSDKBindingsGen }

procedure TAndroidSDKBindingsGen.ProcessModelFile(ASourceFile, APasOutputFile,
  AJavaOutputDir: string);
var
  i: Integer;
  lPasOutputFile: TStringList;
begin
  FSourceFile := TStringList.Create;
  FPasOutputClasses := TStringList.Create;
  FPasOutputImpl := TStringList.Create;
  lPasOutputFile := TStringList.Create;
  try
    FSourceFile.LoadFromFile(ASourceFile);

    // Preparations
    FClassName := '';

    for i := 0 to FSourceFile.Count - 1 do
    begin
      ProcessModelLine(FSourceFile.Strings[i]);
    end;

    // Now save the output
    lPasOutputFile.Add('unit ;');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('interface');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('type');
    lPasOutputFile.AddStrings(FPasOutputClasses);
    lPasOutputFile.Add('  end;');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('implementation');
    lPasOutputFile.Add('');
    lPasOutputFile.AddStrings(FPasOutputImpl);
    lPasOutputFile.Add('');
    lPasOutputFile.Add('end.');

    lPasOutputFile.SaveToFile(APasOutputFile);
  finally
    FSourceFile.Free;
    FPasOutputClasses.Free;
    FPasOutputImpl.Free;
    lPasOutputFile.Free;
  end;
end;

procedure TAndroidSDKBindingsGen.ProcessModelLine(ASourceLine: string);
var
  lReaderPos: Integer = 1;
  lCurWord, lParentClassName: string;
  lMethodReturn, lMethodName, lParamType, lParamName: string;
  TmpStr: string;
begin
  if ASourceLine = '' then Exit;

  lCurWord := GetNextWord(ASourceLine, lReaderPos);

  // Starting a new class
  if ASourceLine[1] = '[' then
  begin
    if FClassName <> '' then
    begin
      FPasOutputClasses.Add('  end;');
      FPasOutputClasses.Add('');
    end;

    FClassName := GetPascalTypeName(lCurWord);
    lParentClassName := GetNextWord(ASourceLine, lReaderPos);
    lParentClassName := GetPascalTypeName(lParentClassName);
    FPasOutputClasses.Add(Format('  %s = class(%s)', [FClassName, lParentClassName]));
    FPasOutputClasses.Add('  public');
  end;

  // Adding methods to a class
  if lCurWord = 'method' then
  begin
    lMethodReturn := GetNextWord(ASourceLine, lReaderPos);
    lMethodReturn := GetPascalTypeName(lMethodReturn);
    lMethodName := GetNextWord(ASourceLine, lReaderPos);

    if lMethodReturn = 'void' then TmpStr := '    procedure '
    else TmpStr := '    function ';

    // Add all parameters
    TmpStr := TmpStr + lMethodName + '(';

    repeat
      lParamType := GetNextWord(ASourceLine, lReaderPos);
      lParamType := GetPascalTypeName(lParamType);
      lParamName := GetNextWord(ASourceLine, lReaderPos);

      if lParamName = '' then Break;

      TmpStr := TmpStr + lParamName + ': ' + lParamType + '; ';
    until lParamName = '';

    // Remove the last ; for the parameters, if necessary
    if TmpStr[Length(TmpStr)-1] = ';' then TmpStr := System.Copy(TmpStr, 0, Length(TmpStr)-2);

    // Add the return
    if lMethodReturn = 'void' then TmpStr := TmpStr + ');'
    else TmpStr := TmpStr + '): ' + lMethodReturn + ';';

    FPasOutputClasses.Add(TmpStr);
  end;
end;

{ Reads one word in a string, starting at AStartPos (1-based index)
  and going up to a space or comma or ( or ) or another separator }
function TAndroidSDKBindingsGen.GetNextWord(ALine: string;
  var AStartPos: Integer): string;
const
  WordSeparators = [' ','(',')','[',']',','];
var
  lState: Integer = 0;
begin
  Result := '';

  while AStartPos <= Length(ALine) do
  begin
    // Searching the word start
    if lState = 0 then
    begin
      if ALine[AStartPos] in WordSeparators then Inc(AStartPos)
      else
      begin
        Result := ALine[AStartPos];
        Inc(AStartPos);
        lState := 1;
      end;
    end
    // Reading until the word finishes
    else
    begin
      if ALine[AStartPos] in WordSeparators then Exit
      else
      begin
        Result := Result + ALine[AStartPos];
        Inc(AStartPos);
      end;
    end;
  end;
end;

function TAndroidSDKBindingsGen.GetPascalTypeName(ABaseName: string): string;
begin
  if ABaseName = '' then Exit('');

  if ABaseName = 'int' then Result := 'Integer'
  else if ABaseName = 'void' then Result := ABaseName
  else if ABaseName = 'CharSequence' then Result := 'string'
  else if ABaseName = 'TJavaObject' then Result := ABaseName
  else Result := 'T' + ABaseName;
end;

procedure TAndroidSDKBindingsGen.GenerateAllBindings(AInputDir, APasOutputDir,
  AJavaOutputDir: string);
begin
  ProcessModelFile(IncludeTrailingPathDelimiter(AInputDir) + 'android_view.txt',
    IncludeTrailingPathDelimiter(APasOutputDir) + 'android_view.pas',
    IncludeTrailingPathDelimiter(AJavaOutputDir));
end;

initialization

  AndroidSDKBindingsGen := TAndroidSDKBindingsGen.Create;

finalization

  AndroidSDKBindingsGen.Free;

end.

