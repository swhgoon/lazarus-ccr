unit android_sdk_bindings_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TAndroidSDKBindingsGen }

  TAndroidSDKBindingsGen = class
  private
    FSourceFile: TStringList;
    FPasOutputTypes, FPasOutputClassesForward, FPasOutputClasses,
      FPasOutputConsts, FPasOutputIDs, FPasOutputImpl, FPasOutputMessages: TStringList;
    FJavaOutputIDs, FJavaOutputMethods: TStringList;
    FClassName, FClassNamePas: string; // Class name of the class currently being parsed
    FClassNum, FMethodNum: Integer;
    procedure ProcessModelFile(ASourceFile, APasOutputFile, AJavaOutputFile: string);
    procedure ProcessModelLine(ASourceLine: string);
    procedure ProcessModelClass(ASourceLine: string);
    procedure ProcessModelMethod(ASourceLine: string);
    procedure ProcessModelConstructor(ASourceLine: string);
    procedure ProcessModelConst(ASourceLine: string);
    procedure ProcessModelCallbackSetterCaller(ASourceLine: string);
    function GetNextWord(ALine: string; var AStartPos: Integer): string;
    function GetPascalTypeName(ABaseName: string): string;
    function PassByReference(ABaseName: string): Boolean;
    function GetJavaResultFunction(AReturnType: string): string;
    function GetJavaTypeReader(AType: string): string;
    function GetIDString(AMethodName: string): string;
    procedure AddOutputIDs(AIDString: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GenerateAllBindings(AInputDir, APasOutputDir, AJavaOutputDir: string);
  end;

var
  AndroidSDKBindingsGen: TAndroidSDKBindingsGen;

implementation

{ TAndroidSDKBindingsGen }

procedure TAndroidSDKBindingsGen.ProcessModelFile(ASourceFile, APasOutputFile,
  AJavaOutputFile: string);
var
  i: Integer;
  lPasOutputFile: TStringList;
  lJavaOutputFile: TStringList;
begin
  lPasOutputFile := TStringList.Create;
  lJavaOutputFile := TStringList.Create;
  try
    FSourceFile.LoadFromFile(ASourceFile);

    // Preparations
    FClassName := '';

    for i := 0 to FSourceFile.Count - 1 do
    begin
      ProcessModelLine(FSourceFile.Strings[i]);
    end;

    // Now save the Pascal file
    lPasOutputFile.Add(Format('unit %s;', [ChangeFileExt(ExtractFileName(ASourceFile), '')]));
    lPasOutputFile.Add('');
    lPasOutputFile.Add('interface');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('uses javalang, androidpipescomm;');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('type');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('  { Forward declaration of classes }');
    lPasOutputFile.Add('');
    lPasOutputFile.AddStrings(FPasOutputClassesForward);
    lPasOutputFile.Add('');
    lPasOutputFile.Add('  { Types }');
    lPasOutputFile.Add('');
    lPasOutputFile.AddStrings(FPasOutputTypes);
    lPasOutputFile.Add('');
    lPasOutputFile.Add('  { Classes }');
    lPasOutputFile.Add('');
    lPasOutputFile.AddStrings(FPasOutputClasses);
    lPasOutputFile.Add('  end;');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('function HandleMessage(AFirstInt: Integer): Boolean;');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('implementation');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('const');
    lPasOutputFile.Add('  { Constants }');
    lPasOutputFile.Add('');
    lPasOutputFile.AddStrings(FPasOutputConsts);
    lPasOutputFile.Add('');
    lPasOutputFile.Add('  { IDs }');
    lPasOutputFile.Add('');
    lPasOutputFile.AddStrings(FPasOutputIDs);
    lPasOutputFile.Add('');
    lPasOutputFile.Add('{ Implementation of Classes }');
    lPasOutputFile.Add('');
    lPasOutputFile.AddStrings(FPasOutputImpl);
    lPasOutputFile.Add('');
    lPasOutputFile.Add('{ Message Handling }');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('function HandleMessage(AFirstInt: Integer): Boolean;');
    lPasOutputFile.Add('var');
    lPasOutputFile.Add('  lInt: Integer;');
    lPasOutputFile.Add('  lPascalPointer: PtrInt = -1;');
    lPasOutputFile.Add('begin');
    lPasOutputFile.Add('  case AFirstInt of');
    lPasOutputFile.AddStrings(FPasOutputMessages);
    lPasOutputFile.Add('  end;');
    lPasOutputFile.Add('end;');
    lPasOutputFile.Add('');
    lPasOutputFile.Add('end.');

    lPasOutputFile.SaveToFile(APasOutputFile);

    // Now save the Java file
    lJavaOutputFile.Add('');
    lJavaOutputFile.AddStrings(FJavaOutputIDs);
    lJavaOutputFile.Add('');
    lJavaOutputFile.AddStrings(FJavaOutputMethods);
    lJavaOutputFile.Add('');

    lJavaOutputFile.SaveToFile(AJavaOutputFile);
  finally
    lJavaOutputFile.Free;
    lPasOutputFile.Free;
  end;
end;

procedure TAndroidSDKBindingsGen.ProcessModelLine(ASourceLine: string);
var
  lReaderPos: Integer = 1;
  lCurWord: string;
begin
  if ASourceLine = '' then Exit;

  lCurWord := GetNextWord(ASourceLine, lReaderPos);

  // Comments
  if ASourceLine[1] = '#' then Exit;

  // Starting a new class
  if ASourceLine[1] = '[' then
  begin
    ProcessModelClass(ASourceLine);
    Exit;
  end;

  // Adding methods to a class
  if lCurWord = 'method' then
  begin
    ProcessModelMethod(ASourceLine);
    Exit;
  end;

  if lCurWord = 'constructor' then
  begin
    ProcessModelConstructor(ASourceLine);
    Exit;
  end;

  // Constants
  if lCurWord = 'const' then
  begin
    ProcessModelConst(ASourceLine);
    Exit;
  end;

  // Callbacks
  if lCurWord = 'callbacksettercaller' then
  begin
    ProcessModelCallbackSetterCaller(ASourceLine);
    Exit;
  end;
end;

procedure TAndroidSDKBindingsGen.ProcessModelClass(ASourceLine: string);
var
  lReaderPos: Integer = 1;
  lCurWord, lParentClassName: string;
begin
  if ASourceLine = '' then Exit;

  lCurWord := GetNextWord(ASourceLine, lReaderPos);

  if FClassName <> '' then
  begin
    FPasOutputClasses.Add('  end;');
    FPasOutputClasses.Add('');
  end;

  FClassNamePas := GetPascalTypeName(lCurWord);
  FClassName := lCurWord;
  lParentClassName := GetNextWord(ASourceLine, lReaderPos);
  lParentClassName := GetPascalTypeName(lParentClassName);
  FPasOutputClasses.Add(Format('  %s = class(%s)', [FClassNamePas, lParentClassName]));
  FPasOutputClasses.Add('  public');
  lCurWord := GetNextWord(ASourceLine, lReaderPos);
  Inc(FClassNum);
  FMethodNum := 0;

  FPasOutputIDs.Add('  // ' + FClassNamePas);
  FJavaOutputIDs.Add('  // ' + FClassName);
  FPasOutputConsts.Add('  { ' + FClassNamePas + ' }');
  FPasOutputClassesForward.Add('  ' + FClassNamePas + ' = class;')
end;

procedure TAndroidSDKBindingsGen.ProcessModelMethod(
  ASourceLine: string);
var
  lReaderPos: Integer = 1;
  lParamNum: Integer = 1;
  lCurWord, lParentClassName: string;
  lMethodReturn, lMethodReturnPas, lMethodName, lParamType, lParamTypePas, lParamName, lParamPrefix: string;
  DeclarationBase, TmpStr, lIDString: string;
  FPasOutputImplCurLine: Integer;
  lJavaParamVar, lJavaParams, lJavaParamSelf: string;
begin
  if ASourceLine = '' then Exit;

  lCurWord := GetNextWord(ASourceLine, lReaderPos);

  // Method type and name
  lMethodReturn := GetNextWord(ASourceLine, lReaderPos);
  lMethodReturnPas := GetPascalTypeName(lMethodReturn);
  lMethodName := GetNextWord(ASourceLine, lReaderPos);

  if lMethodReturn = 'void' then DeclarationBase := 'procedure '
  else DeclarationBase := 'function ';

  // Beginning of the implementation part
  FPasOutputImplCurLine := FPasOutputImpl.Count;
  lIDString := GetIDString(lMethodName);

  FPasOutputImpl.Add('begin');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(' + lIDString + ');');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer');

  lJavaParamSelf := 'param_self_' + FClassName;
  FJavaOutputMethods.Add('    // ' + ASourceLine);
  FJavaOutputMethods.Add('    case ' + lIDString + ':');
  FJavaOutputMethods.Add('      DebugOut("' + lIDString + '");');
  FJavaOutputMethods.Add('      // Self');
  FJavaOutputMethods.Add('      lInt = MyAndroidPipesComm.GetInt();');
  FJavaOutputMethods.Add('      ' + lJavaParamSelf + ' = (' + FClassName + ') ViewElements.get(lInt);');
  FJavaOutputMethods.Add('      // params');

  // Lists of constants for the IDs
  AddOutputIDs(lIDString);

  // Add all parameters
  TmpStr := lMethodName + '(';

  repeat
    lParamType := GetNextWord(ASourceLine, lReaderPos);

    // Method modifiers
    if (lParamType = 'virtual') or (lParamType = 'override') then Continue;

    lParamTypePas := GetPascalTypeName(lParamType);
    lParamName := GetNextWord(ASourceLine, lReaderPos);
    if PassByReference(lParamType) then lParamPrefix := 'var '
    else lParamPrefix := '';

    if lParamName = '' then Break;

    TmpStr := TmpStr + lParamPrefix + lParamName + ': ' + lParamTypePas + '; ';

    // Pascal parameter sending
    FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(Integer(' + lParamName + '));');

    // Java parameter reading
    lJavaParamVar := 'l' + lParamType + '_' + IntToStr(lParamNum);
    FJavaOutputMethods.Add('      ' + lJavaParamVar + ' = MyAndroidPipesComm.' + GetJavaTypeReader(lParamType) + '();');
    lJavaParams := lJavaParams + lJavaParamVar + ', ';

    Inc(lParamNum);
  until lParamName = '';

  // Remove the last ; for the parameters, if necessary
  if TmpStr[Length(TmpStr)-1] = ';' then TmpStr := System.Copy(TmpStr, 0, Length(TmpStr)-2);
  // And for Java params too
  lJavaParams := System.Copy(lJavaParams, 0, Length(lJavaParams)-2);

  // Add the return
  if lMethodReturn = 'void' then
  begin
    TmpStr := TmpStr + ');';
    FPasOutputImpl.Add('  vAndroidPipesComm.WaitForReturn();');
  end
  else
  begin
    TmpStr := TmpStr + '): ' + lMethodReturnPas + ';';
    FPasOutputImpl.Add('  Result := ' + lMethodReturnPas + '(vAndroidPipesComm.WaitForIntReturn());');
  end;

  FPasOutputClasses.Add('    ' + DeclarationBase + TmpStr);
  FPasOutputImpl.Insert(FPasOutputImplCurLine, DeclarationBase + FClassNamePas + '.' + TmpStr);
  FPasOutputImpl.Add('end;');
  FPasOutputImpl.Add('');

  FJavaOutputMethods.Add('      //');
  if lMethodReturn = 'void' then
  begin
    FJavaOutputMethods.Add('      ' + lJavaParamSelf + '.' + lMethodName + '(' + lJavaParams + ');');
    FJavaOutputMethods.Add('      MyAndroidPipesComm.SendResult();');
  end
  else
  begin
    FJavaOutputMethods.Add('      lResult_' + lMethodReturn + ' = ' + lJavaParamSelf + '.' + lMethodName + '(' + lJavaParams + ');');
    FJavaOutputMethods.Add('      MyAndroidPipesComm.' + GetJavaResultFunction(lMethodReturn) + '(lResult_' + lMethodReturn + ');');
  end;
  FJavaOutputMethods.Add('      break;');

  Inc(FMethodNum);
end;

procedure TAndroidSDKBindingsGen.ProcessModelConstructor(ASourceLine: string);
var
  lReaderPos: Integer = 1;
  lCurWord: string;
  lParamNum: Integer = 1;
  lMethodName, lParamType, lParamTypePas, lParamName: string;
  DeclarationBase, TmpStr, lIDString: string;
  FPasOutputImplCurLine: Integer;
begin
  if ASourceLine = '' then Exit;

  lCurWord := GetNextWord(ASourceLine, lReaderPos);

  // Method type and name
  lMethodName := GetNextWord(ASourceLine, lReaderPos);

  lIDString := GetIDString(lMethodName);

  FPasOutputClasses.Add('    constructor ' + lMethodName + '();');

  FPasOutputImpl.Add('constructor ' + FClassNamePas + '.' + lMethodName + '();');
  FPasOutputImpl.Add('begin');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(' + lIDString + ');');
  FPasOutputImpl.Add('  Index := vAndroidPipesComm.WaitForIntReturn();');
  FPasOutputImpl.Add('end;');

  AddOutputIDs(lIDString);

  FJavaOutputMethods.Add('    case ' + lIDString + ':');
  FJavaOutputMethods.Add('      DebugOut("' + lIDString + '");');
  FJavaOutputMethods.Add('      ViewElements.add(new ' + FClassName + '(activity));');
  FJavaOutputMethods.Add('      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);');
  FJavaOutputMethods.Add('      break;');

  Inc(FMethodNum);
end;

procedure TAndroidSDKBindingsGen.ProcessModelConst(ASourceLine: string);
var
  lReaderPos: Integer = 1;
  lCurWord: string;
  lConstName, lConstValue: string;
begin
  if ASourceLine = '' then Exit;

  lConstName := GetNextWord(ASourceLine, lReaderPos);
  lConstName := GetNextWord(ASourceLine, lReaderPos);
  lConstValue := GetNextWord(ASourceLine, lReaderPos);
  lConstValue := GetNextWord(ASourceLine, lReaderPos);

  // Method type and name
  FPasOutputConsts.Add(Format('  %s = %s;', [lConstName, lConstValue]));
end;

// callbacksettercaller setOnClickListener callOnClickListener OnClickCallback = procedure (v: TView) of object;
procedure TAndroidSDKBindingsGen.ProcessModelCallbackSetterCaller(ASourceLine: string);
var
  lReaderPos: Integer = 1;
  lCurWord: string;
  lSetterName, lCallerName, lCallbackName, lCallbackDeclaration: string;
  lIDSetter, lIDStart, lIDFinished: String;
begin
  if ASourceLine = '' then Exit;

  lSetterName := GetNextWord(ASourceLine, lReaderPos);
  lSetterName := GetNextWord(ASourceLine, lReaderPos);
  lCallerName := GetNextWord(ASourceLine, lReaderPos);
  lCallbackDeclaration := Copy(ASourceLine, lReaderPos, Length(ASourceLine));
  lCallbackDeclaration := Trim(lCallbackDeclaration);
  lCallbackName := GetNextWord(ASourceLine, lReaderPos);

  lIDSetter := GetIDString(lSetterName);
  AddOutputIDs(lIDSetter);
  lIDStart := GetIDString(lCallbackName + '_Start');
  AddOutputIDs(lIDStart);
  lIDFinished := GetIDString(lCallbackName + '_Finished');
  AddOutputIDs(lIDFinished);

  FPasOutputTypes.Add('  T' + lCallbackDeclaration);

  FPasOutputClasses.Add('  public');
  FPasOutputClasses.Add('    ' + lCallbackName + ': T' + lCallbackName + ';');
  FPasOutputClasses.Add('    procedure ' + lSetterName + '(ACallback: T' + lCallbackName + ');');
  FPasOutputClasses.Add('    procedure ' + lCallerName + '();');
  FPasOutputClasses.Add('  public');

  FPasOutputImpl.Add('procedure ' + FClassNamePas + '.' + lSetterName + '(ACallback: T' + lCallbackName + ');');
  FPasOutputImpl.Add('begin');
  FPasOutputImpl.Add('  OnClickListener := ACallback;');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(' + lIDSetter + ');');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(Index); // Self, Java Index');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(PtrInt(Self)); // Self, Pascal pointer');
  FPasOutputImpl.Add('  vAndroidPipesComm.WaitForReturn();');
  FPasOutputImpl.Add('end;');
  FPasOutputImpl.Add('');
  FPasOutputImpl.Add('procedure ' + FClassNamePas + '.' + lCallerName + '();');
  FPasOutputImpl.Add('begin');
  FPasOutputImpl.Add('  if Assigned(OnClickListener) then OnClickListener(Self);');
  FPasOutputImpl.Add('end;');

  // Method type and name
  FPasOutputMessages.Add('  ' + lIDStart + ':');
  FPasOutputMessages.Add('  begin');
  FPasOutputMessages.Add('    lPascalPointer := vAndroidPipesComm.ReadInt();');
  FPasOutputMessages.Add('    TTextView(lPascalPointer).callOnClickListener();');
  FPasOutputMessages.Add('    vAndroidPipesComm.SendMessage(amkUICommand, ' + lIDFinished + ');');
  FPasOutputMessages.Add('  end;');
{    amkUI_MenuItem_setOnMenuItemClickListener_Start:
    begin
      lInt := ReadInt();
      lMenuItem := TMenuItem(FindItemIdInList(MenuItems, lInt));
      if lMenuItem <> nil then
        lInt := lMenuItem.callOnMenuItemClickListener();
      vAndroidPipesComm.SendMessage(amkUICommand, amkUI_MenuItem_setOnMenuItemClickListener_Finished);
      vAndroidPipesComm.SendInt(lInt);
    end;
  end;}
end;

{ Reads one word in a string, starting at AStartPos (1-based index)
  and going up to a space or comma or ( or ) or another separator }
function TAndroidSDKBindingsGen.GetNextWord(ALine: string;
  var AStartPos: Integer): string;
const
  WordSeparators = [' ','(',')','[',']',',',';',':',#9{TAB}];
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
  else if ABaseName = 'boolean' then Result := 'Boolean'
  else if ABaseName = 'float' then Result := 'Single'
  else if ABaseName = 'void' then Result := ABaseName
  else if ABaseName = 'CharSequence' then Result := 'string'
  else if ABaseName = 'TJavaObject' then Result := ABaseName
  else Result := 'T' + ABaseName;
end;

function TAndroidSDKBindingsGen.PassByReference(ABaseName: string): Boolean;
begin
  if ABaseName = '' then Exit(False);

  if ABaseName = 'int' then Result := False
  else if ABaseName = 'boolean' then Result := False
  else if ABaseName = 'float' then Result := False
  else Result := True;
end;

function TAndroidSDKBindingsGen.GetJavaResultFunction(AReturnType: string
  ): string;
begin
  if AReturnType = 'boolean' then Result :=  'SendBoolResult'
  else Result := 'SendIntResult';
end;

function TAndroidSDKBindingsGen.GetJavaTypeReader(AType: string): string;
begin
  if AType = 'boolean' then Exit('GetBool')
  else Exit('GetInt');
end;

function TAndroidSDKBindingsGen.GetIDString(AMethodName: string): string;
begin
  Result := 'amkUI_' + FClassNamePas + '_' + AMethodName;;
end;

procedure TAndroidSDKBindingsGen.AddOutputIDs(AIDString: string);
begin
  FPasOutputIDs.Add('  ' + AIDString + ' = $' + IntToHex(FClassNum*$1000+FMethodNum, 8) +  ';');
  FJavaOutputIDs.Add('  static final int ' + AIDString + ' = 0x' + IntToHex(FClassNum*$1000+FMethodNum, 8) +  ';');
end;

constructor TAndroidSDKBindingsGen.Create;
begin
  FSourceFile := TStringList.Create;

  FPasOutputTypes := TStringList.Create;
  FPasOutputClassesForward := TStringList.Create;
  FPasOutputClasses := TStringList.Create;
  FPasOutputImpl := TStringList.Create;
  FPasOutputIDs := TStringList.Create;
  FPasOutputConsts := TStringList.Create;
  FPasOutputMessages := TStringList.Create;

  FJavaOutputIDs := TStringList.Create;
  FJavaOutputMethods := TStringList.Create;

  FClassNum := $100;
end;

destructor TAndroidSDKBindingsGen.Destroy;
begin
  FSourceFile.Free;

  FPasOutputTypes.Free;
  FPasOutputClassesForward.Free;
  FPasOutputClasses.Free;
  FPasOutputImpl.Free;
  FPasOutputIDs.Free;
  FPasOutputConsts.Free;
  FPasOutputMessages.Free;

  FJavaOutputIDs.Free;
  FJavaOutputMethods.Free;

  inherited Destroy;
end;

procedure TAndroidSDKBindingsGen.GenerateAllBindings(AInputDir, APasOutputDir,
  AJavaOutputDir: string);
begin
  ProcessModelFile(IncludeTrailingPathDelimiter(AInputDir) + 'android_all.txt',
    IncludeTrailingPathDelimiter(APasOutputDir) + 'android_all.pas',
    IncludeTrailingPathDelimiter(AJavaOutputDir) + 'android_all.java');
end;

initialization

  AndroidSDKBindingsGen := TAndroidSDKBindingsGen.Create;

finalization

  AndroidSDKBindingsGen.Free;

end.

