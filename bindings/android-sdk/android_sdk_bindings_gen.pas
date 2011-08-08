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
    procedure GeneratePascalFile(ASourceFile: string; ADest: TStringList);
    procedure GenerateJavaFile(ASourceFile: string; ADest: TStringList);
    procedure ProcessModelFile(ASourceFile, APasOutputFile, AJavaOutputFile: string);
    procedure ProcessModelLine(ASourceLine: string);
    procedure ProcessModelClass(ASourceLine: string);
    procedure ProcessModelMethod(ASourceLine: string; AIsField: Boolean);
    procedure ProcessModelConstructor(ASourceLine: string);
    procedure ProcessModelConst(ASourceLine: string);
    procedure ProcessModelCallbackSetterCaller(ASourceLine: string);
    function GetNextWord(ALine: string; var AStartPos: Integer): string;
    function GetPascalTypeName(ABaseName: string): string;
    function PassByReference(ABaseName: string): Boolean;
    function GetJavaResultFunction(AReturnType: string): string;
    function GetJavaTypeReader(AType: string): string;
    function GetJavaTypeLocalVar(AType: string): string;
    function GetJavaTypeConverter(AType: string): string;
    function GetIDString(AMethodName: string): string;
    procedure AddOutputIDs(AIDString: string);
    function ConvertPointToUnderline(AStr: string): string;
    function IsBasicJavaType(AStr: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GenerateAllBindings(AInputDir, APasOutputDir, AJavaOutputDir: string);
  end;

var
  AndroidSDKBindingsGen: TAndroidSDKBindingsGen;

implementation

{ TAndroidSDKBindingsGen }

procedure TAndroidSDKBindingsGen.GeneratePascalFile(ASourceFile: string; ADest: TStringList);
begin
  ADest.Add(Format('unit %s;', [ChangeFileExt(ExtractFileName(ASourceFile), '')]));
  ADest.Add('');
  ADest.Add('interface');
  ADest.Add('');
  ADest.Add('uses javalang, androidpipescomm;');
  ADest.Add('');
  ADest.Add('type');
  ADest.Add('');
  ADest.Add('  { Forward declaration of classes }');
  ADest.Add('');
  ADest.AddStrings(FPasOutputClassesForward);
  ADest.Add('');
  ADest.Add('  { Types }');
  ADest.Add('');
  ADest.AddStrings(FPasOutputTypes);
  ADest.Add('');
  ADest.Add('  { Classes }');
  ADest.Add('');
  ADest.AddStrings(FPasOutputClasses);
  ADest.Add('  end;');
  ADest.Add('');
  ADest.Add('function HandleMessage(AFirstInt: Integer): Boolean;');
  ADest.Add('');
  ADest.Add('implementation');
  ADest.Add('');
  ADest.Add('const');
  ADest.Add('  { Constants }');
  ADest.Add('');
  ADest.AddStrings(FPasOutputConsts);
  ADest.Add('');
  ADest.Add('  { IDs }');
  ADest.Add('');
  ADest.AddStrings(FPasOutputIDs);
  ADest.Add('');
  ADest.Add('{ Implementation of Classes }');
  ADest.Add('');
  ADest.AddStrings(FPasOutputImpl);
  ADest.Add('');
  ADest.Add('{ Message Handling }');
  ADest.Add('');
  ADest.Add('function HandleMessage(AFirstInt: Integer): Boolean;');
  ADest.Add('var');
  ADest.Add('  lInt: Integer;');
  ADest.Add('  lPascalPointer: PtrInt = -1;');
  ADest.Add('begin');
  ADest.Add('  case AFirstInt of');
  ADest.AddStrings(FPasOutputMessages);
  ADest.Add('  end;');
  ADest.Add('end;');
  ADest.Add('');
  ADest.Add('end.');
end;

procedure TAndroidSDKBindingsGen.GenerateJavaFile(ASourceFile: string; ADest: TStringList);
begin
  ADest.Add('package com.pascal.androidlcl;');
  ADest.Add('');
  ADest.Add('import android.app.*;');
  ADest.Add('import android.view.*;');
  ADest.Add('import android.os.*;');
  ADest.Add('import android.util.*;');
  ADest.Add('import android.content.*;');
  ADest.Add('import android.view.*;');
  ADest.Add('import android.widget.*;');
  ADest.Add('import java.util.*;');
  ADest.Add('import java.lang.*;');
  ADest.Add('');
  ADest.Add('public class AndroidAll');
  ADest.Add('{');
  ADest.Add('  // info from other classes');
  ADest.Add('  Activity activity;');
  ADest.Add('  AndroidPipesComm MyAndroidPipesComm;');
  ADest.Add('  JavaLang MyJavaLang;');
  ADest.Add('  // lists of variables');
  ADest.Add('  ArrayList ViewElements;');
  ADest.Add('');
  ADest.Add('  public AndroidAll(AndroidPipesComm AAndroidPipesComm, Activity AActivity, JavaLang AJavaLang)');
  ADest.Add('  {');
  ADest.Add('    activity = AActivity;');
  ADest.Add('    MyAndroidPipesComm = AAndroidPipesComm;');
  ADest.Add('    MyJavaLang = AJavaLang;');
  ADest.Add('    ViewElements = new ArrayList();');
  ADest.Add('  }');
  ADest.Add('');
  ADest.Add('  public void DebugOut(String Str)');
  ADest.Add('  {');
  ADest.Add('    MyAndroidPipesComm.DebugOut(Str);');
  ADest.Add('  }');
  ADest.Add('');
  ADest.AddStrings(FJavaOutputIDs);
  ADest.Add('');
  ADest.Add('  public boolean ProcessCommand(int Buffer)');
  ADest.Add('  {');
  ADest.Add('    //DebugOut("AndroidUI.ProcessCommand Command=" + java.lang.Integer.toHexString(Buffer));');
  ADest.Add('    // basic types');
  ADest.Add('    int lInt, lIndex, lPascalPointer;');
  ADest.Add('    boolean lBool;');
  ADest.Add('    float lFloat;');
  ADest.Add('    // Self params');
  ADest.Add('    View param_self_View;');
  ADest.Add('    ViewGroup param_self_ViewGroup;');
  ADest.Add('    TextView param_self_TextView;');
  ADest.Add('    Button param_self_Button;');
  ADest.Add('    EditText param_self_EditText;');
  ADest.Add('    LinearLayout param_self_LinearLayout;');
  ADest.Add('    TimePicker param_self_TimePicker;');
  ADest.Add('    Display param_self_Display;');
  ADest.Add('    DisplayMetrics param_self_DisplayMetrics;');
  ADest.Add('    CompoundButton param_self_CompoundButton;');
  ADest.Add('    WindowManager param_self_WindowManager;');
  ADest.Add('    // Params');
  ADest.Add('    ViewGroup.LayoutParams lViewGroup_LayoutParams_1;');
  ADest.Add('    DisplayMetrics lDisplayMetrics_1;');
  ADest.Add('    CharSequence lCharSequence_1;');
  ADest.Add('    int lint_1, lint_2, lint_3, lint_4;');
  ADest.Add('    float lfloat_1, lfloat_2;');
  ADest.Add('    boolean lboolean_1;');
  ADest.Add('    // Results');
  ADest.Add('    float lResult_float;');
  ADest.Add('    int lResult_int;');
  ADest.Add('    boolean lResult_boolean;');
  ADest.Add('    Display lResult_Display;');
  ADest.Add('');
  ADest.Add('    switch (Buffer)');
  ADest.Add('    {');
  ADest.Add('');
  ADest.AddStrings(FJavaOutputMethods);
  ADest.Add('');
  ADest.Add('    default:');
  ADest.Add('      return false;');
  ADest.Add('    }');
  ADest.Add('    return true;');
  ADest.Add('  }');
  ADest.Add('}');
end;

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
    GeneratePascalFile(ASourceFile, lPasOutputFile);

    lPasOutputFile.SaveToFile(APasOutputFile);

    GenerateJavaFile(ASourceFile, lJavaOutputFile);

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
    ProcessModelMethod(ASourceLine, False);
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

  // Fields
  if lCurWord = 'field' then
  begin
    ProcessModelMethod(ASourceLine, True);
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
  ASourceLine: string; AIsField: Boolean);
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
    lJavaParamVar := Format('l%s_%d', [ConvertPointToUnderline(lParamType), lParamNum]);
    FJavaOutputMethods.Add(Format('      %s = MyAndroidPipesComm.%s();',
      [GetJavaTypeLocalVar(lParamType), GetJavaTypeReader(lParamType)]));
    FJavaOutputMethods.Add(Format('      %s = %s;',
      [lJavaParamVar, GetJavaTypeConverter(lParamType)]));
    lJavaParams := lJavaParams + lJavaParamVar + ', ';

    Inc(lParamNum);
  until lParamName = '';

  // Remove the last ; for the parameters, if necessary
  if (Length(TmpStr) > 0) and (TmpStr[Length(TmpStr)-1] = ';') then
    TmpStr := System.Copy(TmpStr, 0, Length(TmpStr)-2);
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
  if AIsField then
  begin
    FJavaOutputMethods.Add('      lResult_' + lMethodReturn + ' = ' + lJavaParamSelf + '.' + lMethodName + ';');
    FJavaOutputMethods.Add('      MyAndroidPipesComm.' + GetJavaResultFunction(lMethodReturn) + '(lResult_' + lMethodReturn + ');');
  end
  else if lMethodReturn = 'void' then
  begin
    FJavaOutputMethods.Add('      ' + lJavaParamSelf + '.' + lMethodName + '(' + lJavaParams + ');');
    FJavaOutputMethods.Add('      MyAndroidPipesComm.SendResult();');
  end
  else
  begin
    FJavaOutputMethods.Add('      lResult_' + lMethodReturn + ' = ' + lJavaParamSelf + '.' + lMethodName + '(' + lJavaParams + ');');
    if IsBasicJavaType(lMethodReturn) then
      FJavaOutputMethods.Add('      MyAndroidPipesComm.' + GetJavaResultFunction(lMethodReturn) + '(lResult_' + lMethodReturn + ');')
    else
    begin
      FJavaOutputMethods.Add(Format('      ViewElements.add(lResult_%s);', [lMethodReturn]));
      FJavaOutputMethods.Add(Format('      MyAndroidPipesComm.%s(ViewElements.size() - 1);', [GetJavaResultFunction(lMethodReturn)]))
    end;
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
  lConstructorPasParams, lConstructorJavaParams, lParamPrefix, lJavaParamVar: string;
  DeclarationBase, lIDString: string;
  FPasOutputImplCurLine: Integer;
  HasActivityParam: Boolean = False;
begin
  if ASourceLine = '' then Exit;

  lCurWord := GetNextWord(ASourceLine, lReaderPos);

  // Method type and name
  lMethodName := GetNextWord(ASourceLine, lReaderPos);

  lIDString := GetIDString(lMethodName);

  AddOutputIDs(lIDString);

  FPasOutputImplCurLine := FPasOutputImpl.Count;
  FPasOutputImpl.Add('begin');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));');
  FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(' + lIDString + ');');

  FJavaOutputMethods.Add('    case ' + lIDString + ':');
  FJavaOutputMethods.Add('      DebugOut("' + lIDString + '");');

  // Constructor parameters
  lConstructorJavaParams := '';
  lConstructorPasParams := '';

  repeat
    lParamType := GetNextWord(ASourceLine, lReaderPos);

    // Method modifiers
    if (lParamType = 'virtual') or (lParamType = 'override') then Continue;

    // The Activity global object doesn't appear in the Pascal side and
    //comes as a single word in the txt file
    if (lParamType = 'Activity') then
    begin
      HasActivityParam := True;
      Continue;
    end;

    lParamTypePas := GetPascalTypeName(lParamType);
    lParamName := GetNextWord(ASourceLine, lReaderPos);
    if PassByReference(lParamType) then lParamPrefix := 'var '
    else lParamPrefix := '';

    if lParamName = '' then Break;

    lConstructorPasParams := lConstructorPasParams + lParamPrefix + lParamName + ': ' + lParamTypePas + '; ';

    // Pascal parameter sending
    FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(Integer(' + lParamName + '));');

    // Java parameter reading
    lJavaParamVar := 'l' + lParamType + '_' + IntToStr(lParamNum);
    FJavaOutputMethods.Add('      ' + lJavaParamVar + ' = MyAndroidPipesComm.' + GetJavaTypeReader(lParamType) + '();');
    lConstructorJavaParams := lConstructorJavaParams + lJavaParamVar + ', ';

    Inc(lParamNum);
  until lParamName = '';

  if HasActivityParam then lConstructorJavaParams := 'activity, ' + lConstructorJavaParams;

  // Remove the last ; for the parameters, if necessary
  if (Length(lConstructorPasParams) > 0) and (lConstructorPasParams[Length(lConstructorPasParams)-1] = ';') then
    lConstructorPasParams := System.Copy(lConstructorPasParams, 0, Length(lConstructorPasParams)-2);
  // And for Java params too
  lConstructorJavaParams := System.Copy(lConstructorJavaParams, 0, Length(lConstructorJavaParams)-2);

  // Finalization of the constructor

  FPasOutputClasses.Add(Format('    constructor %s(%s);', [lMethodName, lConstructorPasParams]));
  //
  FPasOutputImpl.Insert(FPasOutputImplCurLine,
    Format('constructor %s.%s(%s);', [FClassNamePas, lMethodName, lConstructorPasParams]));
  FPasOutputImpl.Add('  Index := vAndroidPipesComm.WaitForIntReturn();');
  FPasOutputImpl.Add('end;');

  FJavaOutputMethods.Add('      ViewElements.add(new ' + FClassName + '(' + lConstructorJavaParams + '));');
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
  else
  begin
    Result := 'T' + ABaseName;
    Result := StringReplace(Result, '.', '_', [rfReplaceAll]);
  end;
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
  else if AReturnType = 'float' then Result :=  'SendFloatResult'
  else Result := 'SendIntResult';
end;

function TAndroidSDKBindingsGen.GetJavaTypeReader(AType: string): string;
begin
  if AType = 'boolean' then Exit('GetBool')
  else if AType = 'float' then Exit('GetFloat')
  else Exit('GetInt');
end;

function TAndroidSDKBindingsGen.GetJavaTypeLocalVar(AType: string): string;
begin
  if AType = 'boolean' then Exit('lBool')
  else if AType = 'float' then Exit('lFloat')
  else Exit('lInt');
end;

function TAndroidSDKBindingsGen.GetJavaTypeConverter(AType: string): string;
begin
  if AType = 'boolean' then Exit('lBool')
  else if AType = 'int' then Exit('lInt')
  else if AType = 'float' then Exit('lFloat')
  else Result := Format('(%s) ViewElements.get(lInt)', [AType]);
end;

function TAndroidSDKBindingsGen.GetIDString(AMethodName: string): string;
begin
  Result := 'amkUI_' + FClassNamePas + '_' + AMethodName;;
  Result := StringReplace(Result, '.', '_', [rfReplaceAll]);
end;

procedure TAndroidSDKBindingsGen.AddOutputIDs(AIDString: string);
begin
  FPasOutputIDs.Add('  ' + AIDString + ' = $' + IntToHex(FClassNum*$1000+FMethodNum, 8) +  ';');
  FJavaOutputIDs.Add('  static final int ' + AIDString + ' = 0x' + IntToHex(FClassNum*$1000+FMethodNum, 8) +  ';');
end;

function TAndroidSDKBindingsGen.ConvertPointToUnderline(AStr: string): string;
begin
  Result := SysUtils.StringReplace(AStr, '.', '_',  [rfReplaceAll, rfIgnoreCase]);
end;

function TAndroidSDKBindingsGen.IsBasicJavaType(AStr: string): Boolean;
begin
  Result := (AStr = 'boolean') or (AStr = 'int') or (AStr = 'float');
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
    IncludeTrailingPathDelimiter(AJavaOutputDir) + 'AndroidAll.java');
end;

initialization

  AndroidSDKBindingsGen := TAndroidSDKBindingsGen.Create;

finalization

  AndroidSDKBindingsGen.Free;

end.

