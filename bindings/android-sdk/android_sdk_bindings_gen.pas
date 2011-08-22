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
      FPasOutputConsts, FPasOutputIDs, FPasOutputImpl, FPasOutputMessages,
      FPasGlobalVars, FPasInitialization: TStringList;
    FJavaOutputIDs, FJavaOutputMethods: TStringList;
    FClassName, FClassNamePas: string; // Class name of the class currently being parsed
    FClassNum, FMethodNum: Integer;
    FIsGlobalObject: Boolean;
    FGlobalObject: string;
    procedure GeneratePascalFile(ASourceFile: string; ADest: TStringList);
    procedure GenerateJavaFile(ASourceFile: string; ADest: TStringList);
    procedure ProcessModelFile(ASourceFile, APasOutputFile, AJavaOutputFile: string);
    procedure ProcessModelLine(ASourceLine: string);
    procedure ProcessModelClass(ASourceLine: string; AIsInterface: Boolean);
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
    function ConvertJavaSpecialCharsToUnderline(AStr: string): string;
    function ConvertPointToUnderline(AStr: string): string;
    function JavaRemoveGeneric(AStr: string): string;
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
  ADest.Add('uses SysUtils, javalang, androidpipescomm;');
  ADest.Add('');
  ADest.Add('{$INTERFACES CORBA}');
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
  ADest.Add('const');
  ADest.Add('  { Constants }');
  ADest.AddStrings(FPasOutputConsts);
  ADest.Add('');
  ADest.Add('function HandleMessage(AFirstInt: Integer): Boolean;');
  ADest.Add('');
  ADest.Add('var');
  ADest.AddStrings(FPasGlobalVars);
  ADest.Add('');
  ADest.Add('implementation');
  ADest.Add('');
  ADest.Add('const');
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
  ADest.Add('initialization');
  ADest.Add('');
  ADest.AddStrings(FPasInitialization);
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
  ADest.Add('import android.R.*;');
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
  ADest.Add('    //');
  ADest.Add('    // basic types');
  ADest.Add('    //');
  ADest.Add('    int lInt, lIndex, lPascalPointer;');
  ADest.Add('    boolean lBool;');
  ADest.Add('    float lFloat;');
  ADest.Add('    //');
  ADest.Add('    // Self params');
  ADest.Add('    //');
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
  ADest.Add('    AbsSpinner param_self_AbsSpinner;');
  ADest.Add('    ArrayAdapter<String> param_self_ArrayAdapter_String_;');
  ADest.Add('    AdapterView param_self_AdapterView;');
  ADest.Add('    AlertDialog.Builder param_self_AlertDialog_Builder;');
  ADest.Add('    //');
  ADest.Add('    // Params');
  ADest.Add('    //');
  ADest.Add('    ViewGroup.LayoutParams lViewGroup_LayoutParams_1, lViewGroup_LayoutParams_2, lViewGroup_LayoutParams_3;');
  ADest.Add('    SpinnerAdapter lSpinnerAdapter_1;');
  ADest.Add('    DisplayMetrics lDisplayMetrics_1;');
  ADest.Add('    CharSequence lCharSequence_1;');
  ADest.Add('    String lString_1;');
  ADest.Add('    View lView_1;');
  ADest.Add('    int lint_1, lint_2, lint_3, lint_4;');
  ADest.Add('    float lfloat_1, lfloat_2;');
  ADest.Add('    boolean lboolean_1;');
  ADest.Add('    //');
  ADest.Add('    // Results');
  ADest.Add('    //');
  ADest.Add('    float lResult_float;');
  ADest.Add('    int lResult_int;');
  ADest.Add('    boolean lResult_boolean;');
  ADest.Add('    CharSequence lResult_CharSequence;');
  ADest.Add('    Display lResult_Display;');
  ADest.Add('    AlertDialog.Builder lResult_AlertDialog_Builder;');
  ADest.Add('    AlertDialog lResult_AlertDialog;');
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
    FIsGlobalObject := False;
    ProcessModelClass(ASourceLine, False);
    Exit;
  end;

  // Starting a new global object class
  if ASourceLine[1] = '%' then
  begin
    FIsGlobalObject := True;
    ProcessModelClass(ASourceLine, False);
    Exit;
  end;

  // Starting a new interface
  if ASourceLine[1] = '{' then
  begin
    FIsGlobalObject := False;
    ProcessModelClass(ASourceLine, True);
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

procedure TAndroidSDKBindingsGen.ProcessModelClass(ASourceLine: string; AIsInterface: Boolean);
var
  lReaderPos: Integer = 1;
  lCurWord, lParentClassName, lTmpParent: string;
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

  // Read the global object, if it is one
  if FIsGlobalObject then
  begin
    FGlobalObject := GetNextWord(ASourceLine, lReaderPos);
    FPasGlobalVars.Add(Format('  %s: %s;', [FGlobalObject, FClassNamePas]));
    FPasInitialization.Add(Format('  %s := %s.Create;', [FGlobalObject, FClassNamePas]));
  end;

  lTmpParent := GetNextWord(ASourceLine, lReaderPos);
  lParentClassName := GetPascalTypeName(lTmpParent);

  // Read all parents
  while lTmpParent <> '' do
  begin
    lTmpParent := GetNextWord(ASourceLine, lReaderPos);
    if lTmpParent <> '' then
      lParentClassName := lParentClassName + ', ' + GetPascalTypeName(lTmpParent);
  end;

  // Java classes can implement interfaces, so based on a safe ground
  if lParentClassName = '' then
    if AIsInterface then
      lParentClassName := 'IJavaInterface'
    else
      lParentClassName := 'TJavaObject';

  if AIsInterface then
    FPasOutputClasses.Add(Format('  %s = interface(%s)', [FClassNamePas, lParentClassName]))
  else
  begin
    FPasOutputClasses.Add(Format('  %s = class(%s)', [FClassNamePas, lParentClassName]));
    FPasOutputClasses.Add('  public');
  end;
  lCurWord := GetNextWord(ASourceLine, lReaderPos);
  Inc(FClassNum);
  FMethodNum := 0;

  FPasOutputIDs.Add('  // ' + FClassNamePas);
  FJavaOutputIDs.Add('  // ' + FClassName);
  FPasOutputConsts.Add('  { ' + FClassNamePas + ' }');
  if AIsInterface then
    FPasOutputClassesForward.Add('  ' + FClassNamePas + ' = interface;')
  else
    FPasOutputClassesForward.Add('  ' + FClassNamePas + ' = class;')
end;

procedure TAndroidSDKBindingsGen.ProcessModelMethod(
  ASourceLine: string; AIsField: Boolean);
var
  lReaderPos: Integer = 1;
  lParamNum: Integer = 1;
  lCurWord, lParentClassName: string;
  lMethodReturn, lMethodReturnPas, lMethodName, lParamType, lParamTypePas, lParamName, lParamPrefix: string;
  lMethodReturnJavaIdentifier: string;
  DeclarationBase, TmpStr, lIDString: string;
  FPasOutputImplCurLine: Integer;
  lJavaParamVar, lJavaParams, lJavaParamSelf: string;
  // For adding the var for string params
  HasStringParam: Boolean = False;
  StringParamCount: Integer = 0;
  i: Integer;
  //
  lPascalMethodModifiers: string = '';
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
  if not FIsGlobalObject then
    FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer');

  lJavaParamSelf := 'param_self_' + JavaRemoveGeneric(ConvertPointToUnderline(FClassName));
  FJavaOutputMethods.Add('    // ' + ASourceLine);
  FJavaOutputMethods.Add('    case ' + lIDString + ':');
  FJavaOutputMethods.Add('      DebugOut("' + lIDString + '");');
  if not FIsGlobalObject then
  begin
    FJavaOutputMethods.Add('      // Self');
    FJavaOutputMethods.Add('      lInt = MyAndroidPipesComm.GetInt();');
    FJavaOutputMethods.Add('      ' + lJavaParamSelf + ' = (' + FClassName + ') ViewElements.get(lInt);');
  end
  else
    lJavaParamSelf := FGlobalObject;
  FJavaOutputMethods.Add('      // params');

  // Lists of constants for the IDs
  AddOutputIDs(lIDString);

  // Add all parameters
  TmpStr := lMethodName + '(';

  repeat
    lParamType := GetNextWord(ASourceLine, lReaderPos);

    // Method modifiers
    if (lParamType = 'virtual') or (lParamType = 'override') then Continue;
    if (lParamType = 'overload') then
    begin
      lPascalMethodModifiers := ' overload;';
      Continue;
    end;

    lParamTypePas := GetPascalTypeName(lParamType);
    lParamName := GetNextWord(ASourceLine, lReaderPos);
    if PassByReference(lParamType) then lParamPrefix := 'var '
    else lParamPrefix := '';

    if lParamName = '' then Break;

    TmpStr := TmpStr + lParamPrefix + lParamName + ': ' + lParamTypePas + '; ';

    // Pascal parameter sending
    if lParamTypePas = 'string' then
    begin
      HasStringParam := True;
      Inc(StringParamCount);
      FPasOutputImpl.Insert(FPasOutputImplCurLine+1, Format('  lString_%d := TString.Create(%s);', [StringParamCount, lParamName]));
      FPasOutputImpl.Add(Format('  vAndroidPipesComm.SendInt(lString_%d.Index); // text', [StringParamCount]));
    end
    else if IsBasicJavaType(lParamType) then
      FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(Integer(' + lParamName + '));')
    else // for objects
      FPasOutputImpl.Add('  vAndroidPipesComm.SendInt(Integer(' + lParamName + '.Index));');

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

  // Add the Pascal return
  if lMethodReturn = 'void' then
  begin
    TmpStr := TmpStr + ');';
    FPasOutputImpl.Add('  vAndroidPipesComm.WaitForReturn();');
  end
  else if (lMethodReturn = 'CharSequence') or (lMethodReturn = 'String') then
  begin
    TmpStr := TmpStr + '): ' + lMethodReturnPas + ';';
    FPasOutputImpl.Add('  Result := vAndroidPipesComm.WaitForStringReturn();');
  end
  else
  begin
    TmpStr := TmpStr + '): ' + lMethodReturnPas + ';';
    FPasOutputImpl.Add('  Result := ' + lMethodReturnPas + '(vAndroidPipesComm.WaitForIntReturn());');
  end;

  // Insert the start
  FPasOutputClasses.Add('    ' + DeclarationBase + TmpStr + lPascalMethodModifiers);
  FPasOutputImpl.Insert(FPasOutputImplCurLine, DeclarationBase + FClassNamePas + '.' + TmpStr);
  if HasStringParam then
  begin
    FPasOutputImpl.Insert(FPasOutputImplCurLine+1, 'var');
    for i := 1 to StringParamCount do
    begin
      FPasOutputImpl.Insert(FPasOutputImplCurLine+1+i, Format('  lString_%d: TString;', [i]));
      FPasOutputImpl.Add(Format('  lString_%d.Free;', [i]));
    end;
  end;
  FPasOutputImpl.Add('end;');
  FPasOutputImpl.Add('');

  // Java Return
  FJavaOutputMethods.Add('      //');
  lMethodReturnJavaIdentifier := 'lResult_' + ConvertPointToUnderline(lMethodReturn);
  if AIsField then
  begin
    FJavaOutputMethods.Add(Format('      %s = %s.%s;', [lMethodReturnJavaIdentifier, lJavaParamSelf, lMethodName]));
    FJavaOutputMethods.Add(Format('      MyAndroidPipesComm.%s(%s);', [GetJavaResultFunction(lMethodReturn), lMethodReturnJavaIdentifier]));
  end
  else if lMethodReturn = 'void' then
  begin
    FJavaOutputMethods.Add(Format('      %s.%s(%s);', [lJavaParamSelf, lMethodName, lJavaParams]));
    FJavaOutputMethods.Add(Format('      MyAndroidPipesComm.SendResult();', []));
  end
  else
  begin
    FJavaOutputMethods.Add(Format('      %s = %s.%s(%s);', [lMethodReturnJavaIdentifier, lJavaParamSelf, lMethodName, lJavaParams]));
    if IsBasicJavaType(lMethodReturn) or (lMethodReturn = 'CharSequence') or (lMethodReturn = 'String') then
      FJavaOutputMethods.Add(Format('      MyAndroidPipesComm.%s(%s);', [GetJavaResultFunction(lMethodReturn), lMethodReturnJavaIdentifier]))
    else
    begin
      FJavaOutputMethods.Add(Format('      ViewElements.add(%s);', [lMethodReturnJavaIdentifier]));
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
      lParamName := 'ReturnToRepeat';
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
  lJavaParamSelf: string;
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
  Inc(FMethodNum);
  lIDStart := GetIDString(lCallbackName + '_Start');
  AddOutputIDs(lIDStart);
  Inc(FMethodNum);
  lIDFinished := GetIDString(lCallbackName + '_Finished');
  AddOutputIDs(lIDFinished);
  Inc(FMethodNum);

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


  // -----------------------------------------------
  // Now Java:
  // -----------------------------------------------

  lJavaParamSelf := 'param_self_' + FClassName;
  FJavaOutputMethods.Add(       '    // ' + ASourceLine);
  FJavaOutputMethods.Add(       '    case ' + lIDSetter + ':');
  FJavaOutputMethods.Add(       '      DebugOut("' + lIDSetter + '");');
  FJavaOutputMethods.Add(       '      // Self');
  FJavaOutputMethods.Add(       '      lInt = MyAndroidPipesComm.GetInt();');
  FJavaOutputMethods.Add(       '      ' + lJavaParamSelf + ' = (' + FClassName + ') ViewElements.get(lInt);');
  FJavaOutputMethods.Add(       '      lPascalPointer = MyAndroidPipesComm.GetInt();');
  FJavaOutputMethods.Add(Format('      %s.setTag(Integer.valueOf(lPascalPointer));', [lJavaParamSelf]));
  FJavaOutputMethods.Add(       '');
  FJavaOutputMethods.Add(       '      // Run the code');
  FJavaOutputMethods.Add(Format('      %s.setOnClickListener(', [lJavaParamSelf]));
  FJavaOutputMethods.Add(Format('      new View.OnClickListener()', []));
  FJavaOutputMethods.Add(       '      {');
  FJavaOutputMethods.Add(       '        public void onClick(View v)');
  FJavaOutputMethods.Add(       '        {');
  FJavaOutputMethods.Add(       '          // Perform action');
  FJavaOutputMethods.Add(       '          DebugOut("START TextView OnClickListener");');
  FJavaOutputMethods.Add(Format('          MyAndroidPipesComm.SendMessage(AndroidPipesComm.amkUICommand, %s);', [lIDStart]));
  FJavaOutputMethods.Add(       '          Integer lTag = (Integer) v.getTag();');
  FJavaOutputMethods.Add(       '          MyAndroidPipesComm.SendInt(lTag.intValue());');
  FJavaOutputMethods.Add(Format('          MyAndroidPipesComm.WaitForPascalMessage(AndroidPipesComm.amkUICommand, %s);', [lIDFinished]));
  FJavaOutputMethods.Add(       '          DebugOut("END TextView OnClickListener");');
  FJavaOutputMethods.Add(       '        }');
  FJavaOutputMethods.Add(       '      });');
  FJavaOutputMethods.Add(       '      MyAndroidPipesComm.SendResult();');
  FJavaOutputMethods.Add(       '      break;');
(*  case amkUI_TextView_setOnClickListener:
    DebugOut("amkUI_TextView_setOnClickListener");
    // Get Self
    lInt = MyAndroidPipesComm.GetInt();
    lTextView = (TextView) ViewElements.get(lInt);
    lPascalPointer = MyAndroidPipesComm.GetInt();
    lTextView.setTag(Integer.valueOf(lPascalPointer));

    // Run the code
    lTextView.setOnClickListener(
      new View.OnClickListener()
      {
        public void onClick(View v)
        {
          // Perform action on click
          DebugOut("START TextView OnClickListener");
          MyAndroidPipesComm.SendMessage(AndroidPipesComm.amkUICommand, amkUI_TextView_OnClickCallback_Start);
          //lIndex := UIElements.indexOf(v);
          //MyAndroidPipesComm.SendInt(lIndex);
          Integer lTag = (Integer) v.getTag();
          MyAndroidPipesComm.SendInt(lTag.intValue());
          MyAndroidPipesComm.WaitForPascalMessage(AndroidPipesComm.amkUICommand, amkUI_TextView_OnClickCallback_Finished);
          DebugOut("END TextView OnClickListener");
        }
      });
    MyAndroidPipesComm.SendResult();
    break;*)
end;

{ Reads one word in a string, starting at AStartPos (1-based index)
  and going up to a space or comma or ( or ) or another separator }
function TAndroidSDKBindingsGen.GetNextWord(ALine: string;
  var AStartPos: Integer): string;
const
  WordSeparators = [' ','(',')','[',']','{','}','%',',',';',':',#9{TAB}];
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
  else if (ABaseName = 'CharSequence') or (ABaseName = 'String') then Result := 'string'
  else if ABaseName = 'TJavaObject' then Result := ABaseName
  else
  begin
    Result := 'T' + ABaseName;
    Result := ConvertJavaSpecialCharsToUnderline(Result);
  end;
end;

function TAndroidSDKBindingsGen.PassByReference(ABaseName: string): Boolean;
begin
  if ABaseName = '' then Exit(False);

  if ABaseName = 'int' then Result := False
  else if ABaseName = 'boolean' then Result := False
  else if ABaseName = 'float' then Result := False
//  else if (ABaseName = 'CharSequence') or (ABaseName = 'String') then Result := False
  else Result := False;
end;

function TAndroidSDKBindingsGen.GetJavaResultFunction(AReturnType: string
  ): string;
begin
  if AReturnType = 'boolean' then Result := 'SendBoolResult'
  else if AReturnType = 'float' then Result := 'SendFloatResult'
  else if (AReturnType = 'CharSequence') or (AReturnType = 'String') then Result := 'SendStringResult'
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
  else if (AType = 'CharSequence') or (AType = 'String') then
    Result := Format('(%s) MyJavaLang.LangElements.get(lInt)', [AType])
  else Result := Format('(%s) ViewElements.get(lInt)', [AType]);
end;

// We add the method num too for overloaded routines
function TAndroidSDKBindingsGen.GetIDString(AMethodName: string): string;
begin
  Result := Format('amkUI_%s_%s_%d', [FClassNamePas, AMethodName, FMethodNum]);
  Result := ConvertJavaSpecialCharsToUnderline(Result);
end;

procedure TAndroidSDKBindingsGen.AddOutputIDs(AIDString: string);
begin
  FPasOutputIDs.Add('  ' + AIDString + ' = $' + IntToHex(FClassNum*$1000+FMethodNum, 8) +  ';');
  FJavaOutputIDs.Add('  static final int ' + AIDString + ' = 0x' + IntToHex(FClassNum*$1000+FMethodNum, 8) +  ';');
end;

function TAndroidSDKBindingsGen.ConvertJavaSpecialCharsToUnderline(AStr: string): string;
begin
  Result := StringReplace(AStr, '.', '_',  [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '_', [rfReplaceAll]);
end;

function TAndroidSDKBindingsGen.ConvertPointToUnderline(AStr: string): string;
begin
  Result := StringReplace(AStr, '.', '_',  [rfReplaceAll, rfIgnoreCase]);
end;

function TAndroidSDKBindingsGen.JavaRemoveGeneric(AStr: string): string;
begin
  Result := StringReplace(AStr, '<', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '_', [rfReplaceAll]);
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
  FPasGlobalVars := TStringList.Create;
  FPasInitialization := TStringList.Create;

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
  FPasGlobalVars.Free;
  FPasInitialization.Free;

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

