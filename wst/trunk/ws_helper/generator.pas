{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$INCLUDE wst_global.inc}
unit generator;

interface

uses
  Classes, SysUtils,
  PasTree,
  pascal_parser_intf, source_utils, wst_types;
  
const
  sWST_EXTENSION = 'wst';
  
type

  TGeneratorOption = (
    goDocumentWrappedParameter { .Net style wrapped parameters },
    goGenerateDocAsComments    { Documentation include in the XSD/WSDL schema will be generated as comments }
  );
  TGeneratorOptions = set of TGeneratorOption;
  
  { TBaseGenerator }

  TBaseGenerator = class
    FOptions : TGeneratorOptions;
  Private
    FSrcMngr  : ISourceManager;
    FCurrentStream : ISourceStream;
    FSymbolTable: TwstPasTreeContainer;
  Protected
    procedure SetCurrentStream(AStream : ISourceStream);
    procedure Indent();
    function IncIndent():Integer;
    function DecIndent():Integer;
    procedure BeginAutoIndent();
    procedure EndAutoIndent();
    procedure Write(AText : String);overload;
    procedure Write(AText : String; Const AArgs : array of const);overload;
    procedure WriteLn(AText : String);overload;
    procedure WriteLn(AText : String; Const AArgs : array of const);overload;
    procedure NewLine();
    
    function ExtractserviceName(AIntf : TPasElement):String;
  Public
    constructor Create(
      ASymTable : TwstPasTreeContainer;
      ASrcMngr  : ISourceManager
    );
    procedure Execute();virtual;abstract;
    property SymbolTable : TwstPasTreeContainer Read FSymbolTable;
    property SrcMngr : ISourceManager Read FSrcMngr;
    property Options : TGeneratorOptions read FOptions write FOptions;
  End;

  { TProxyGenerator }

  TProxyGenerator = class(TBaseGenerator)
  Private
    FDecStream : ISourceStream;
    FDecProcStream : ISourceStream;
    FImpStream : ISourceStream;

    function GenerateClassName(AIntf : TPasElement):String;
    
    procedure GenerateUnitHeader();
    procedure GenerateUnitImplementationHeader();
    procedure GenerateUnitImplementationFooter();

    procedure GenerateProxyIntf(AIntf, AEasyIntf : TPasClassType; ABinding : TwstBinding);
    procedure GenerateProxyImp(AIntf, AEasyIntf : TPasClassType; ABinding : TwstBinding);
    
    function GetDestUnitName():string;
  Public
    constructor Create(
      ASymTable : TwstPasTreeContainer;
      ASrcMngr  : ISourceManager
    );
    procedure Execute();override;
  End;

  { TStubGenerator }

  TBinderGenerator = class(TBaseGenerator)
  Private
    FDecStream : ISourceStream;
    FImpStream : ISourceStream;

    function GenerateClassName(AIntf : TPasElement):String;

    procedure GenerateUnitHeader();
    procedure GenerateUnitImplementationHeader();
    procedure GenerateUnitImplementationFooter();

    procedure GenerateIntf(AIntf : TPasClassType);
    procedure GenerateImp(AIntf : TPasClassType);

    function GetDestUnitName():string;
  Public
    constructor Create(
      ASymTable : TwstPasTreeContainer;
      ASrcMngr  : ISourceManager
    );
    procedure Execute();override;
  End;

  { TImplementationGenerator }

  TImplementationGenerator = class(TBaseGenerator)
  Private
    FDecStream : ISourceStream;
    FImpStream : ISourceStream;

    function GenerateClassName(AIntf : TPasElement):String;

    procedure GenerateUnitHeader();
    procedure GenerateUnitImplementationHeader();
    procedure GenerateUnitImplementationFooter();

    procedure GenerateIntf(AIntf : TPasClassType);
    procedure GenerateImp(AIntf : TPasClassType);

    function GetDestUnitName():string;
  Public
    constructor Create(
      ASymTable : TwstPasTreeContainer;
      ASrcMngr  : ISourceManager
    );
    procedure Execute();override;
  End;

  { TInftGenerator }

  TInftGenerator = class(TBaseGenerator)
  private
    FDecStream : ISourceStream;
    FImpStream : ISourceStream;
    FImpTempStream : ISourceStream;
    FImpLastStream : ISourceStream;
    FRttiFunc : ISourceStream;
  private
    procedure WriteDocumetation(AElement : TPasElement);
    procedure WriteDocIfEnabled(AElement : TPasElement);{$IFDEF USE_INLINE}inline;{$ENDIF}
    // Array handling helper routines
    procedure WriteObjectArray(ASymbol : TPasArrayType);
    procedure WriteSimpleTypeArray(ASymbol : TPasArrayType);
    procedure WriteObjectCollection(ASymbol : TPasArrayType);
  private
    function GenerateIntfName(AIntf : TPasElement):string;

    procedure GenerateUnitHeader();
    procedure GenerateUnitImplementationHeader();
    procedure GenerateUnitImplementationFooter();

    procedure GenerateIntf(AIntf : TPasClassType);
    procedure GenerateTypeAlias(ASymbol : TPasAliasType);
    procedure GenerateClass(ASymbol : TPasClassType);
    procedure GenerateEnum(ASymbol : TPasEnumType);
    procedure GenerateArray(ASymbol : TPasArrayType);
    procedure GenerateRecord(ASymbol : TPasRecordType);

    procedure GenerateCustomMetadatas();
    function GetDestUnitName():string;

    procedure PrepareModule();
    procedure InternalExecute();
  public
    procedure Execute();override;
  end;
  
  
  
implementation
uses parserutils, Contnrs, logger_intf;

Const sPROXY_BASE_CLASS = 'TBaseProxy';
      sBINDER_BASE_CLASS = 'TBaseServiceBinder';
      sIMP_BASE_CLASS = 'TBaseServiceImplementation';
      sSERIALIZER_CLASS  = 'IFormatterClient';
      //RETURN_PARAM_NAME = 'return';
      RETURN_VAL_NAME = 'returnVal';
      sNAME_SPACE = 'sNAME_SPACE';
      sUNIT_NAME = 'sUNIT_NAME';
      sRECORD_RTTI_DEFINE = 'WST_RECORD_RTTI';
      sEASY_ACCESS_INTERFACE_PREFIX = 'Easy';

      sPRM_NAME = 'strPrmName';
      sLOC_SERIALIZER = 'locSerializer';
      sINPUT_PARAM = 'inputParam';
      sOUTPUT_PARAM = 'outputParam';
      sTEMP_OBJ = 'tmpObj';
      sDOCUMENTATION = 'documentation';


function DeduceEasyInterfaceForDocStyle(
  const ARawInt : TPasClassType;
  const AContainer : TwstPasTreeContainer
): TPasClassType;

  procedure HandleProc(const AIntf : TPasClassType; const AMethod : TPasProcedure);
  var
    locMethod : TPasProcedure;
    locProcType : TPasProcedureType;
    locElt : TPasElement;
    locRawInParam, locRawOutParam : TPasClassType;
    k, q : PtrInt;
    locProp, locResProp : TPasProperty;
    locArg : TPasArgument;
    locIsFunction : Boolean;
  begin
    if ( AMethod.ProcType.Args.Count < 1 ) then
      raise Exception.CreateFmt('Invalid "Document style" method, one parameter expected : %s.%s.',[AIntf.Name,AMethod.Name]);
    locElt := TPasArgument(AMethod.ProcType.Args[0]).ArgType;
    if locElt.InheritsFrom(TPasUnresolvedTypeRef) then
      locElt := AContainer.FindElement(locElt.Name);
    if ( locElt = nil ) then
      raise Exception.CreateFmt('Invalid "Document style" method, class type parameter expected, nil founded : %s.%s.',[AIntf.Name,AMethod.Name]);
    if ( not locElt.InheritsFrom(TPasClassType) ) then
      raise Exception.CreateFmt('Invalid "Document style" method, class type parameter expected : %s.%s => %s',[AIntf.Name,AMethod.Name,locElt.ElementTypeName]);
    locRawInParam := TPasClassType(locElt);
    locIsFunction := False;
    if AMethod.InheritsFrom(TPasFunction) then begin
      locElt := TPasFunctionType(AMethod.ProcType).ResultEl.ResultType;
      if locElt.InheritsFrom(TPasUnresolvedTypeRef) then
        locElt := AContainer.FindElement(locElt.Name);
      if ( locElt = nil ) or ( not locElt.InheritsFrom(TPasClassType) ) then
        raise Exception.CreateFmt('Invalid "Document style" method, class type result expected : %s.%s.',[AIntf.Name,AMethod.Name]);
      locRawOutParam := TPasClassType(locElt);
      q := locRawOutParam.Members.Count;
      if ( q > 0 ) then begin
        for k := 0 to ( q - 1 ) do begin
          if TPasElement(locRawOutParam.Members[k]).InheritsFrom(TPasProperty) then begin
            locProp := TPasProperty(locRawOutParam.Members[k]);
            if ( locProp.Visibility = visPublished ) then begin
              locResProp := locProp;
              locIsFunction := True;
              Break;
            end;
          end;
        end;
      end;
    end;
    if locIsFunction then begin
      locMethod := TPasFunction(AContainer.CreateElement(TPasFunction,AMethod.Name,AIntf,'',0));
      locMethod.ProcType := TPasFunctionType(AContainer.CreateElement(TPasFunctionType,AMethod.ProcType.Name,locMethod,'',0));
    end else begin
      locMethod := TPasProcedure(AContainer.CreateElement(TPasProcedure,AMethod.Name,AIntf,'',0));
      locMethod.ProcType := TPasProcedureType(AContainer.CreateElement(TPasProcedureType,AMethod.ProcType.Name,locMethod,'',0));
    end;
    AIntf.Members.Add(locMethod);
    q := locRawInParam.Members.Count;
    locProcType := locMethod.ProcType;
    if ( q > 0 ) then begin
      for k := 0 to ( q - 1 ) do begin
        locElt := TPasElement(locRawInParam.Members[k]);
        if locElt.InheritsFrom(TPasProperty) then begin
          locProp := TPasProperty(locElt);
          if ( locProp.Visibility = visPublished ) then begin
            locArg := TPasArgument(AContainer.CreateElement(TPasArgument,locProp.Name,locProcType,'',0));
            locArg.ArgType := locProp.VarType;
            locArg.ArgType.AddRef();
            locArg.Access := argConst;
            locProcType.Args.Add(locArg);
          end;
        end;
      end;
    end;
    if locIsFunction then begin
      TPasFunctionType(locProcType).ResultEl := TPasResultElement(AContainer.CreateElement(TPasResultElement,'Result',locProcType,'',0));
      TPasFunctionType(locProcType).ResultEl.ResultType := locResProp.VarType; locResProp.VarType.AddRef();
    end;
  end;
  
var
  locRes : TPasClassType;
  i, c : PtrInt;
  g : TGuid;
  e : TPasElement;
begin
  if ( ARawInt.ObjKind <> okInterface ) then
    raise Exception.CreateFmt('Interface expected : "%s".',[ARawInt.Name]);
  locRes := TPasClassType(AContainer.CreateElement(TPasClassType,Format('%s%s',[ARawInt.Name,sEASY_ACCESS_INTERFACE_PREFIX]),nil,'',0));
  try
    locRes.ObjKind := okInterface;
    if ( CreateGUID(g) = 0 ) then
      locRes.InterfaceGUID := GUIDToString(g);
    c := ARawInt.Members.Count;
    if ( c > 0 ) then begin
      for i := 0 to ( c - 1 ) do begin
        e := TPasElement(ARawInt.Members[i]);
        if e.InheritsFrom(TPasProcedure) then
          HandleProc(locRes,TPasProcedure(e));
      end;
    end;
  except
    FreeAndNil(locRes);
    raise;
  end;
  Result := locRes;
end;

{ TProxyGenerator }

function TProxyGenerator.GenerateClassName(AIntf: TPasElement): String;
begin
  Result := ExtractserviceName(AIntf);
  Result := Format('T%s_Proxy',[Result]);
end;

procedure TProxyGenerator.GenerateUnitHeader();
begin
  SetCurrentStream(FDecStream);
  WriteLn('{');
  WriteLn('This unit has been produced by ws_helper.');
  WriteLn('  Input unit name : "%s".',[SymbolTable.CurrentModule.Name]);
  WriteLn('  This unit name  : "%s".',[GetDestUnitName()]);
  WriteLn('  Date            : "%s".',[DateTimeToStr(Now())]);
  WriteLn('}');
  WriteLn('');
  WriteLn('Unit %s;',[GetDestUnitName()]);
  WriteLn('{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}');
  WriteLn('Interface');
  WriteLn('');
  WriteLn('Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, %s;',[SymbolTable.CurrentModule.Name]);
  WriteLn('');
  WriteLn('Type');
  WriteLn('');
end;

procedure TProxyGenerator.GenerateUnitImplementationHeader();
begin
  SetCurrentStream(FImpStream);
  WriteLn('');
  WriteLn('Implementation');
  WriteLn('uses wst_resources_imp, metadata_repository;');
end;

procedure TProxyGenerator.GenerateUnitImplementationFooter();
var
  s :string;
begin
  SetCurrentStream(FImpStream);
  NewLine();
  WriteLn('initialization');
  WriteLn('  {$i %s.%s}',[SymbolTable.CurrentModule.Name,sWST_EXTENSION]);
  NewLine();
  s := Format('Register_%s_ServiceMetadata',[SymbolTable.CurrentModule.Name]);
  WriteLn('  {$IF DECLARED(%s)}',[s]);
  WriteLn('  %s();',[s]);
  WriteLn('  {$IFEND}');
  WriteLn('End.');
end;

constructor TProxyGenerator.Create(
  ASymTable : TwstPasTreeContainer;
  ASrcMngr  : ISourceManager
);
begin
  Inherited Create(ASymTable,ASrcMngr);
  FDecStream := SrcMngr.CreateItem(GetDestUnitName() + '.dec');
  FDecProcStream := SrcMngr.CreateItem(GetDestUnitName() + '.dec_proc');
  FImpStream := SrcMngr.CreateItem(GetDestUnitName() + '.imp');
end;

procedure TProxyGenerator.Execute();
Var
  i,c : Integer;
  intf : TPasClassType;
  elt : TPasElement;
  ls : TList;
  binding : TwstBinding;
  intfEasy : TPasClassType;
  HandleEasyIntf : Boolean;
begin
  HandleEasyIntf := ( goDocumentWrappedParameter in Self.Options );
  GenerateUnitHeader();
  GenerateUnitImplementationHeader();
  ls := SymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := Pred(ls.Count);
  if HandleEasyIntf then begin
    for i := 0 to c do begin
      elt := TPasElement(ls[i]);
      if ( elt is TPasClassType ) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
        intf := elt as TPasClassType;
        binding := SymbolTable.FindBinding(intf);
        intfEasy := nil;
        if ( binding.BindingStyle = bsDocument ) then begin
          intfEasy := DeduceEasyInterfaceForDocStyle(intf,SymbolTable);
        end;
        GenerateProxyIntf(intf,intfEasy,binding);
        GenerateProxyImp(intf,intfEasy,binding);
      end;
    end;
  end else begin
    for i := 0 to c do begin
      elt := TPasElement(ls[i]);
      if ( elt is TPasClassType ) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
        intf := elt as TPasClassType;
        GenerateProxyIntf(intf,nil,binding);
        GenerateProxyImp(intf,nil,binding);
      end;
    end;
  end;
  GenerateUnitImplementationFooter();
  FSrcMngr.Merge(GetDestUnitName() + '.pas',[FDecStream,FDecProcStream,FImpStream]);
  FDecStream := nil;
  FImpStream := nil;
end;

function TProxyGenerator.GetDestUnitName(): string;
begin
  Result := Format('%s_proxy',[SymbolTable.CurrentModule.Name]);
end;

procedure TProxyGenerator.GenerateProxyIntf(AIntf, AEasyIntf : TPasClassType; ABinding : TwstBinding);
var
  HandleEasyIntf : boolean;
  
  procedure WriteDec();
  begin
    Indent();
    Write('%s=class(%s,%s',[GenerateClassName(AIntf),sPROXY_BASE_CLASS,AIntf.Name]);
    if HandleEasyIntf then
      Write(',%s',[AEasyIntf.Name]);
    WriteLn(')');
    FDecProcStream.IncIndent();
    try
      FDecProcStream.NewLine();
      FDecProcStream.Indent();
      FDecProcStream.WriteLn('Function wst_CreateInstance_%s(const AFormat : string = %s; const ATransport : string = %s):%s;',[AIntf.Name,QuotedStr('SOAP:'),QuotedStr('HTTP:'),AIntf.Name]);
      if HandleEasyIntf then begin
        FDecProcStream.Indent();
        FDecProcStream.WriteLn(
          'Function wst_CreateInstance_%s%s(const AFormat : string = %s; const ATransport : string = %s):%s%s;',
          [AIntf.Name,sEASY_ACCESS_INTERFACE_PREFIX,QuotedStr('SOAP:'),QuotedStr('HTTP:'),AIntf.Name,sEASY_ACCESS_INTERFACE_PREFIX]
        );
      end;
    finally
      FDecProcStream.DecIndent();
    end;
  end;
  
  procedure WriteMethod(AMthd : TPasProcedure);
  Var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList;
  Begin
    Indent();
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write('function ')
    end else begin
      Write('procedure ')
    end;
    Write('%s(',[AMthd.Name]);

    If ( prmCnt > 0 ) Then Begin
      IncIndent();
      For k := 0 To Pred(prmCnt) Do Begin
        prm := TPasArgument(prms[k]);
        If (k > 0 ) Then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[AccessNames[prm.Access],prm.Name,prm.ArgType.Name]);
      End;
      DecIndent();
      NewLine();
      Indent();
    End;

    Write(')');
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write(':%s',[TPasFunctionType(AMthd.ProcType).ResultEl.ResultType.Name]);
    end;
    Write(';');
    if HandleEasyIntf then
      Write('overload;');
    WriteLn('');
  End;
  
  procedure WriteMethods();
  var
    k : Integer;
    mthds : TList;
    elt : TPasElement;
  begin
    if ( GetElementCount(AIntf.Members,TPasProcedure) = 0 ) then
      Exit;
    Indent();
    WriteLn('Protected');
    IncIndent();
      Indent();WriteLn('class function GetServiceType() : PTypeInfo;override;');
      mthds := AIntf.Members;
      for k := 0 to Pred(mthds.Count) do begin
        elt := TPasElement(mthds[k]);
        if elt.InheritsFrom(TPasProcedure) then begin
          WriteMethod(TPasProcedure(elt));
        end;
      end;
      if HandleEasyIntf then begin
        Indent(); WriteLn('// Easy acces methods');
        mthds := AEasyIntf.Members;
        for k := 0 to Pred(mthds.Count) do begin
          elt := TPasElement(mthds[k]);
          if elt.InheritsFrom(TPasProcedure) then begin
            WriteMethod(TPasProcedure(elt));
          end;
        end;
      end;
    DecIndent();
  end;
  
begin
  HandleEasyIntf := ( goDocumentWrappedParameter in Self.Options ) and ( AEasyIntf <> nil );
  SetCurrentStream(FDecStream);
  NewLine();
  IncIndent();
    WriteDec();
    WriteMethods();
    Indent(); WriteLn('End;');
  DecIndent();
end;

procedure TProxyGenerator.GenerateProxyImp(AIntf, AEasyIntf : TPasClassType; ABinding : TwstBinding);
Var
  strClassName : String;
  HandleEasyIntf : Boolean;
  
  procedure WriteDec();
  begin
    NewLine();
    WriteLn('Function wst_CreateInstance_%s(const AFormat : string; const ATransport : string):%s;',[AIntf.Name,AIntf.Name]);
    WriteLn('Begin');
      IncIndent();
        Indent();
        WriteLn(
          'Result := %s.Create(%s,AFormat+%s,ATransport + %s);',
          [ strClassName,QuotedStr(AIntf.Name),
            Format('GetServiceDefaultFormatProperties(TypeInfo(%s))',[AIntf.Name]),
            QuotedStr('address=') + Format(' + GetServiceDefaultAddress(TypeInfo(%s))',[AIntf.Name])
          ]
        );
      DecIndent();
    WriteLn('End;');
    NewLine();

    if HandleEasyIntf then begin
      WriteLn(
        'Function wst_CreateInstance_%s%s(const AFormat : string; const ATransport : string):%s%s;',
        [AIntf.Name,sEASY_ACCESS_INTERFACE_PREFIX,AIntf.Name,sEASY_ACCESS_INTERFACE_PREFIX]
      );
      WriteLn('Begin');
        IncIndent();
          Indent();
          WriteLn(
            'Result := wst_CreateInstance_%s(AFormat,ATransport) as %s%s;',
            [AIntf.Name,AIntf.Name,sEASY_ACCESS_INTERFACE_PREFIX]
          );
        DecIndent();
      WriteLn('End;');
      NewLine();
    end;
    
    if ( GetElementCount(AIntf.Members,TPasProcedure) > 0 ) then
      WriteLn('{ %s implementation }',[strClassName]);
  end;

  procedure WriteMethodDec(AMthd : TPasProcedure);
  Var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList;
  Begin
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write('function ')
    end else begin
      Write('procedure ');
    end;
    Write('%s.%s(',[strClassName,AMthd.Name]);

    If ( prmCnt > 0 ) Then Begin
      IncIndent();
      For k := 0 To Pred(prmCnt) Do Begin
        prm := TPasArgument(prms[k]);
        If (k > 0 ) Then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[AccessNames[prm.Access],prm.Name,prm.ArgType.Name]);
      End;
      DecIndent();
      NewLine();
      Indent();
    End;

    Write(')');
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write(':%s',[TPasFunctionType(AMthd.ProcType).ResultEl.ResultType.Name]);
    end;
    WriteLn(';');
  End;

  procedure WriteEasyMethodImp(AMthd : TPasProcedure);
  var
    prms : TList;
    origineRes : TPasResultElement;
    origineResProp : TPasProperty;

    function HasObjectsArgs() : Boolean;
    var
      k : PtrInt;
      prm : TPasArgument;
      elt : TPasElement;
    begin
      Result := False;
      for k := 0 to ( prms.Count - 1 ) do begin
        prm := TPasArgument(prms[k]);
        elt := prm.ArgType;
        if elt.InheritsFrom(TPasUnresolvedTypeRef) then
          elt := SymbolTable.FindElement(SymbolTable.GetExternalName(elt));
        if elt.InheritsFrom(TPasUnresolvedTypeRef) or SymbolTable.IsOfType(TPasType(elt),TPasClassType) then begin
          Result := True;
          Break;
        end;
      end;
    end;
    
    procedure AssignArguments();
    var
      k : PtrInt;
      prm : TPasArgument;
      elt : TPasElement;
    begin
      for k := 0 to ( prms.Count - 1 ) do begin
        prm := TPasArgument(prms[k]);
        elt := prm.ArgType;
        if elt.InheritsFrom(TPasUnresolvedTypeRef) then
          elt := SymbolTable.FindElement(SymbolTable.GetExternalName(elt));
        if elt.InheritsFrom(TPasUnresolvedTypeRef) then begin
          Indent(); WriteLn('if ( PTypeInfo(TypeInfo(%s))^.Kind = tkClass ) then begin',[elt.Name]);
            IncIndent();
              Indent(); WriteLn('%s := TObject(%s.%s);',[sTEMP_OBJ,sINPUT_PARAM,prm.Name]);
              Indent(); WriteLn('%s.Free();',[sTEMP_OBJ]);
              Indent(); WriteLn('TObject(%s.%s) := nil;',[sINPUT_PARAM,prm.Name]);
            DecIndent();
            Indent(); WriteLn('end;');
        end else begin
          if SymbolTable.IsOfType(TPasType(elt),TPasClassType) then begin
            Indent(); WriteLn('%s := %s.%s;',[sTEMP_OBJ,sINPUT_PARAM,prm.Name]);
            Indent(); WriteLn('%s.Free();',[sTEMP_OBJ]);
          end;
        end;
        Indent(); WriteLn('%s.%s := %s;',[sINPUT_PARAM,prm.Name,prm.Name]);
      end;
    end;

    procedure ClearArguments();
    var
      k : PtrInt;
      prm : TPasArgument;
      elt : TPasElement;
    begin
      for k := 0 to ( prms.Count - 1 ) do begin
        prm := TPasArgument(prms[k]);
        elt := prm.ArgType;
        if elt.InheritsFrom(TPasUnresolvedTypeRef) then
          elt := SymbolTable.FindElement(SymbolTable.GetExternalName(elt));
        if elt.InheritsFrom(TPasUnresolvedTypeRef) then begin
          Indent(); WriteLn('if ( PTypeInfo(TypeInfo(%s))^.Kind = tkClass ) then',[elt.Name]);
            IncIndent();
              Indent(); WriteLn('TObject(%s.%s) := nil;',[sINPUT_PARAM,prm.Name]);
            DecIndent();
        end else begin
          if SymbolTable.IsOfType(TPasType(elt),TPasClassType) then begin
            Indent(); WriteLn('%s.%s := nil;',[sINPUT_PARAM,prm.Name]);
          end;
        end;
      end;
      if AMthd.ProcType.InheritsFrom(TPasFunctionType) then begin
        elt := origineResProp.VarType;
        if elt.InheritsFrom(TPasUnresolvedTypeRef) then
          elt := SymbolTable.FindElement(SymbolTable.GetExternalName(elt));
        if elt.InheritsFrom(TPasUnresolvedTypeRef) then begin
          Indent(); WriteLn('if ( PTypeInfo(TypeInfo(%s))^.Kind = tkClass ) then',[elt.Name]);
            IncIndent();
              Indent(); WriteLn('if ( %s <> nil ) then',[sOUTPUT_PARAM]);
              IncIndent();
                Indent(); WriteLn('TObject(%s.%s) := nil;',[sOUTPUT_PARAM,origineResProp.Name]);
              DecIndent();
            DecIndent();
        end else begin
          if SymbolTable.IsOfType(TPasType(elt),TPasClassType) then begin
            Indent(); WriteLn('if ( %s <> nil ) then',[sOUTPUT_PARAM]);
            IncIndent();
              Indent(); WriteLn('%s.%s := nil;',[sOUTPUT_PARAM,origineResProp.Name]);
            DecIndent();
          end;
        end;
      end;
    end;

  var
    origineMthd : TPasProcedure;
    origineIsFunc : Boolean;
    origineArgIN : TPasArgument;
    prmCnt,k : Integer;
    prm : TPasArgument;
    resPrm : TPasResultElement;
    elt : TPasElement;
    objArgs : Boolean;
    localIsFunc : boolean;
  begin
    origineMthd := FindMember(AIntf,AMthd.Name) as TPasProcedure;
    Assert ( origineMthd <> nil );
    origineArgIN := TPasArgument(origineMthd.ProcType.Args[0]);
    origineIsFunc := origineMthd.InheritsFrom(TPasFunction);
    origineResProp := nil;
    localIsFunc := AMthd.InheritsFrom(TPasFunction);
    if origineIsFunc then begin
      origineRes := TPasFunctionType(origineMthd.ProcType).ResultEl;
      for k := 0 to ( TPasClassType(origineRes.ResultType).Members.Count - 1 ) do begin
        elt := TPasElement(TPasClassType(origineRes.ResultType).Members[k]);
        if elt.InheritsFrom(TPasProperty) and ( TPasProperty(elt).Visibility = visPublished ) then begin
          origineResProp := TPasProperty(elt);
          Break;
        end;
      end;
      Assert( localIsFunc or ( origineResProp = nil ) );
    end else begin
      origineRes := nil;
    end;
    prms := AMthd.ProcType.Args;
    objArgs := HasObjectsArgs();
    IncIndent();
      WriteLn('var');
        Indent(); WriteLn('%s : TObject;',[sTEMP_OBJ]);
        Indent(); WriteLn('%s : %s;',[sINPUT_PARAM,origineArgIN.ArgType.Name]);
        if origineIsFunc then begin
          Indent(); WriteLn('%s : %s;',[sOUTPUT_PARAM,origineRes.ResultType.Name]);
        end;
      WriteLn('begin');
        Indent(); WriteLn('%s := nil;',[sOUTPUT_PARAM]);
        Indent(); WriteLn('%s := %s.Create();',[sINPUT_PARAM,origineArgIN.ArgType.Name]);
        Indent(); WriteLn('try');
          IncIndent();
            prmCnt := prms.Count;
            if ( prmCnt > 0 ) then
              AssignArguments();
            if objArgs then begin
              Indent(); WriteLn('try');
                 IncIndent();
            end;
            if origineIsFunc then begin
              Indent(); WriteLn('%s := %s(%s);',[sOUTPUT_PARAM,origineMthd.Name,sINPUT_PARAM]);
              if localIsFunc then begin
                Indent(); WriteLn('Result := %s.%s;',[sOUTPUT_PARAM,origineResProp.Name]);
              end;
            end else begin
              Indent(); WriteLn('%s(%s);',[origineMthd.Name,sINPUT_PARAM]);
            end;
            if objArgs then begin
              DecIndent();
              Indent(); WriteLn('finally');
                IncIndent();
                  ClearArguments();
                DecIndent();
              Indent(); WriteLn('end;');
            end;
          DecIndent();
        Indent(); WriteLn('finally');
          IncIndent();
            Indent(); WriteLn('FreeAndNil(%s);',[sINPUT_PARAM]);
            Indent(); WriteLn('FreeAndNil(%s);',[sOUTPUT_PARAM]);
          DecIndent();
        Indent(); WriteLn('end;');
    DecIndent();
    WriteLn('end;');
  end;
  
  procedure WriteMethodImp(AMthd : TPasProcedure);
  Var
    prmCnt,k : Integer;
    prm : TPasArgument;
    resPrm : TPasResultElement;
    prms : TList;
  Begin
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    IncIndent();
    WriteLn('Var');

      Indent();WriteLn('%s : %s;',[sLOC_SERIALIZER,sSERIALIZER_CLASS]);
      if ( prmCnt > 0 ) or AMthd.InheritsFrom(TPasFunction) then begin
        Indent();WriteLn('%s : %s;',[sPRM_NAME,'string']);
      end;

    WriteLn('Begin');
    
      Indent();WriteLn('%s := GetSerializer();',[sLOC_SERIALIZER]);
      Indent();WriteLn('Try');IncIndent();

      Indent();WriteLn('%s.BeginCall(''%s'', GetTarget(),(Self as ICallContext));',[sLOC_SERIALIZER,SymbolTable.GetExternalName(AMthd)]);
      IncIndent();
        for k := 0 To Pred(prmCnt) do begin
          prm := TPasArgument(prms[k]);
          If ( prm.Access <> argOut ) Then Begin
            Indent();WriteLn('%s.Put(%s, TypeInfo(%s), %s);',[sLOC_SERIALIZER,QuotedStr(SymbolTable.GetExternalName(prm)),prm.ArgType.Name,prm.Name]);
          End;
        End;
      DecIndent();
      Indent();WriteLn('%s.EndCall();',[sLOC_SERIALIZER]);
      
      WriteLn('');
      Indent();WriteLn('MakeCall();');
      WriteLn('');
      
      Indent();WriteLn('%s.BeginCallRead((Self as ICallContext));',[sLOC_SERIALIZER]);
      IncIndent();
        if AMthd.InheritsFrom(TPasFunction) then begin
          resPrm := TPasFunctionType(AMthd.ProcType).ResultEl;
          if SymbolTable.IsInitNeed(resPrm.ResultType) then begin
            if SymbolTable.IsOfType(resPrm.ResultType,TPasClassType) or
               SymbolTable.IsOfType(resPrm.ResultType,TPasArrayType)
            then begin
              Indent();WriteLn('TObject(Result) := Nil;');
            end else begin
              Indent();WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkInterface] ) Then',[resPrm.ResultType.Name]);
              IncIndent();
                Indent();WriteLn('Pointer(Result) := Nil;');
              DecIndent();
            end;
          end;
          Indent();WriteLn('%s := %s;',[sPRM_NAME,QuotedStr(FSymbolTable.GetExternalName(resPrm))]);
          Indent();WriteLn('%s.Get(TypeInfo(%s), %s, %s);',[sLOC_SERIALIZER,resPrm.ResultType.Name,sPRM_NAME,'Result']);
        end;
        //--------------------------------
        for k := 0 to Pred(prmCnt) do begin
          prm := TPasArgument(prms[k]);
          if ( prm.Access = argOut ) then begin
            if SymbolTable.IsInitNeed(prm.ArgType) then begin
              if SymbolTable.IsOfType(prm.ArgType,TPasClassType) or
                 SymbolTable.IsOfType(prm.ArgType,TPasArrayType)
              then begin
                Indent();WriteLn('TObject(%s) := Nil;',[prm.Name]);
              end else begin
                Indent();WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkInterface] ) Then',[prm.ArgType.Name]);
                IncIndent();
                  Indent();WriteLn('Pointer(%s) := Nil;',[prm.Name]);
                DecIndent();
              end;
            end;
          end;
        end;
        //--------------------------------

        for k := 0 to Pred(prmCnt) do begin
          prm := TPasArgument(prms[k]);
          if ( prm.Access in [argVar, argOut] ) then begin
            Indent();WriteLn('%s := %s;',[sPRM_NAME,QuotedStr(SymbolTable.GetExternalName(prm))]);
            Indent();WriteLn('%s.Get(TypeInfo(%s), %s, %s);',[sLOC_SERIALIZER,prm.ArgType.Name,sPRM_NAME,prm.Name]);
          end;
        end;
      DecIndent();

      
      WriteLn('');
      DecIndent();
      Indent();WriteLn('Finally');
        IncIndent();
          Indent();WriteLn('%s.Clear();',[sLOC_SERIALIZER]);
        DecIndent();
      Indent();WriteLn('End;');DecIndent();
      
    WriteLn('End;');
  end;

  procedure WriteTypeInfoMethod();
  begin
    NewLine();
    WriteLn('class function %s.GetServiceType() : PTypeInfo;',[strClassName]);
    WriteLn('begin');
      IncIndent();
        Indent(); WriteLn('result := TypeInfo(%s);',[AIntf.Name]);
      DecIndent();
    WriteLn('end;');
    NewLine();
  end;
  
  procedure WriteMethods();
  var
    k : Integer;
    mthds : TList;
    elt : TPasElement;
  begin
    WriteTypeInfoMethod();
    mthds := AIntf.Members;
    for k := 0 to Pred(mthds.Count) do begin
      elt := TPasElement(mthds[k]);
      if elt.InheritsFrom(TPasProcedure) then begin
        WriteMethodDec(TPasProcedure(elt));
        WriteMethodImp(TPasProcedure(elt));
        WriteLn('');
      end;
    end;
    if HandleEasyIntf then begin
      mthds := AEasyIntf.Members;
      if ( mthds.Count > 0 ) then begin
        for k := 0 to Pred(mthds.Count) do begin
          elt := TPasElement(mthds[k]);
          if elt.InheritsFrom(TPasProcedure) then begin
            WriteMethodDec(TPasProcedure(elt));
            WriteEasyMethodImp(TPasProcedure(elt));
            WriteLn('');
          end;
        end;
      end;
    end;
  end;
  
begin
  HandleEasyIntf := ( goDocumentWrappedParameter in Self.Options ) and ( AEasyIntf <> nil );
  SetCurrentStream(FImpStream);
  IncIndent();
  While ( DecIndent() > 0 ) Do
    ;
  strClassName := GenerateClassName(AIntf);
  NewLine();
  WriteDec();
  WriteMethods();
end;


{ TBaseGenerator }

procedure TBaseGenerator.SetCurrentStream(AStream: ISourceStream);
begin
  FCurrentStream := AStream;
end;

procedure TBaseGenerator.Indent();
begin
  FCurrentStream.Indent();
end;

function TBaseGenerator.IncIndent():Integer;
begin
  Result := FCurrentStream.IncIndent();
end;

function TBaseGenerator.DecIndent():Integer;
begin
  Result := FCurrentStream.DecIndent();
end;

procedure TBaseGenerator.BeginAutoIndent();
begin
  FCurrentStream.BeginAutoIndent();
end;

procedure TBaseGenerator.EndAutoIndent();
begin
  FCurrentStream.EndAutoIndent();
end;

procedure TBaseGenerator.Write(AText: String);
begin
  FCurrentStream.Write(AText);
end;

procedure TBaseGenerator.Write(AText: String; const AArgs: array of const);
begin
  Write(Format(AText,AArgs));
end;

procedure TBaseGenerator.WriteLn(AText: String);
begin
  Write(AText+sNEW_LINE);
end;

procedure TBaseGenerator.WriteLn(AText: String; const AArgs: array of const);
begin
  Write(AText+sNEW_LINE,AArgs);
end;

procedure TBaseGenerator.NewLine();
begin
  WriteLn('');
end;

function TBaseGenerator.ExtractserviceName(AIntf: TPasElement): String;
begin
  Result := AIntf.Name;
  If upCase(Result[1]) = 'I' Then
    Delete(Result,1,1);
end;

constructor TBaseGenerator.Create(ASymTable: TwstPasTreeContainer; ASrcMngr: ISourceManager);
begin
  Assert(Assigned(ASymTable));
  Assert(Assigned(ASrcMngr));
  FSrcMngr :=ASrcMngr;
  FCurrentStream := Nil;
  FSymbolTable := ASymTable;
end;

{ TBinderGenerator }

function TBinderGenerator.GenerateClassName(AIntf: TPasElement): String;
begin
  Result := ExtractserviceName(AIntf);
  Result := Format('T%s_ServiceBinder',[Result]);
end;

procedure TBinderGenerator.GenerateUnitHeader();
begin
  SetCurrentStream(FDecStream);
  WriteLn('{');
  WriteLn('This unit has been produced by ws_helper.');
  WriteLn('  Input unit name : "%s".',[SymbolTable.CurrentModule.Name]);
  WriteLn('  This unit name  : "%s".',[GetDestUnitName()]);
  WriteLn('  Date            : "%s".',[DateTimeToStr(Now())]);
  WriteLn('}');

  WriteLn('unit %s;',[GetDestUnitName()]);
  WriteLn('{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}');
  WriteLn('interface');
  WriteLn('');
  WriteLn('uses SysUtils, Classes, base_service_intf, server_service_intf, %s;',[SymbolTable.CurrentModule.Name]);
  WriteLn('');
  WriteLn('type');
  WriteLn('');
end;

procedure TBinderGenerator.GenerateUnitImplementationHeader();
begin
  SetCurrentStream(FImpStream);
  WriteLn('');
  WriteLn('Implementation');
  WriteLn('uses TypInfo, wst_resources_imp,metadata_repository;');
end;

procedure TBinderGenerator.GenerateUnitImplementationFooter();
var
  s :string;
begin
  NewLine();
  WriteLn('initialization');
  NewLine();
  WriteLn('  {$i %s.%s}',[SymbolTable.CurrentModule.Name,sWST_EXTENSION]);
  NewLine();
  s := Format('Register_%s_ServiceMetadata',[SymbolTable.CurrentModule.Name]);
  WriteLn('  {$IF DECLARED(%s)}',[s]);
  WriteLn('  %s();',[s]);
  WriteLn('  {$IFEND}');
  NewLine();
  WriteLn('End.');
end;

procedure TBinderGenerator.GenerateIntf(AIntf: TPasClassType);
  procedure WriteDec();
  begin
    Indent();
    WriteLn('%s = class(%s)',[GenerateClassName(AIntf),sBINDER_BASE_CLASS]);
  end;

  procedure WriteConstructor();
  Begin
    Indent();
      WriteLn('constructor Create();')
  End;

  procedure WriteMethod(AMthd : TPasProcedure);
  Begin
    Indent();
      WriteLn('procedure %sHandler(AFormatter : IFormatterResponse; AContext : ICallContext);',[AMthd.Name])
  End;

  procedure WriteMethods();
  var
    k : Integer;
    mbrs : TList;
    elt : TPasElement;
  begin
    if ( GetElementCount(AIntf.Members,TPasProcedure) > 0 ) then begin
      Indent();WriteLn('protected');
      IncIndent();
        mbrs := AIntf.Members;
        for k := 0 to Pred(mbrs.Count) do begin
          elt := TPasElement(mbrs[k]);
          if elt.InheritsFrom(TPasProcedure) then begin
            WriteMethod(TPasProcedure(elt));
          end;
        end;
      DecIndent();

      Indent();WriteLn('public');
        Indent();WriteConstructor();
    end;
  end;
  
  procedure GenerateFactoryClass();
  Begin
    NewLine();
    IncIndent();BeginAutoIndent();
      WriteLn('T%s_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)',[ExtractserviceName(AIntf)]);
      WriteLn('private');
      IncIndent();
        WriteLn('FInstance : IInterface;');
      DecIndent();
      
      WriteLn('protected');
      IncIndent();
        WriteLn('function CreateInstance():IInterface;');
      DecIndent();

      WriteLn('public');
      IncIndent();
        WriteLn('constructor Create();');
        WriteLn('destructor Destroy();override;');
      DecIndent();
      WriteLn('end;');
    DecIndent();EndAutoIndent();
  End;
  
  procedure GenerateRegistrationProc();
  Begin
    NewLine();
    BeginAutoIndent();
      IncIndent();
      WriteLn('procedure Server_service_Register%sService();',[ExtractserviceName(AIntf)]);
      DecIndent();
    EndAutoIndent();
  End;

begin
  SetCurrentStream(FDecStream);
  NewLine();
  IncIndent();
    WriteDec();
    WriteMethods();
    Indent();WriteLn('end;');
  DecIndent();
  
  GenerateFactoryClass();
  GenerateRegistrationProc();
end;

procedure TBinderGenerator.GenerateImp(AIntf: TPasClassType);
Var
  strClassName : String;

  procedure WriteDec();
  begin
    if ( GetElementCount(AIntf.Members,TPasProcedure) > 0 ) then
      WriteLn('{ %s implementation }',[strClassName]);
  end;

  procedure WriteMethodDec(AMthd : TPasProcedure);
  Begin
    WriteLn('procedure %s.%sHandler(AFormatter : IFormatterResponse; AContext : ICallContext);',[strClassName,AMthd.Name]);
  End;
  
  procedure WriteMethodImp(AMthd : TPasProcedure);
  Var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList;
    resElt : TPasResultElement;
    strBuff : string;
  Begin
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    WriteLn('var');
    IncIndent();BeginAutoIndent();
      WriteLn('cllCntrl : ICallControl;');
      WriteLn('objCntrl : IObjectControl;');
      WriteLn('hasObjCntrl : Boolean;');
      WriteLn('tmpObj : %s;',[AIntf.Name]);
      WriteLn('callCtx : ICallContext;');
      if ( prmCnt > 0 ) or AMthd.InheritsFrom(TPasFunction) then begin
        WriteLn('%s : string;',[sPRM_NAME]);
      end;
      WriteLn('procName,trgName : string;');
      if ( prmCnt > 0 ) then begin
        for k := 0 to Pred(prmCnt) do begin
          prm := TPasArgument(prms[k]);
          WriteLn('%s : %s;',[prm.Name,prm.ArgType.Name]);
        end;
      end;
      if AMthd.InheritsFrom(TPasFunction) then begin
        WriteLn('%s : %s;',[RETURN_VAL_NAME,TPasFunctionType(AMthd.ProcType).ResultEl.ResultType.Name]);
      end;
    DecIndent();EndAutoIndent();
    
    WriteLn('begin');
    IncIndent();BeginAutoIndent();

      WriteLn('callCtx := AContext;');
      if AMthd.InheritsFrom(TPasFunction) then begin
        resElt := TPasFunctionType(AMthd.ProcType).ResultEl;
        if SymbolTable.IsInitNeed(resElt.ResultType) then begin
          WriteLn('Fillchar(%s,SizeOf(%s),#0);',[RETURN_VAL_NAME,resElt.ResultType.Name]);
          {if ( SymbolTable.IsOfType(resElt.ResultType,TPasClassType) and
               ( TPasClassType(GetUltimeType(resElt.ResultType)).ObjKind = okClass )
             ) or
             SymbolTable.IsOfType(resElt.ResultType,TPasArrayType)
          then begin
            WriteLn('TObject(%s) := nil;',[RETURN_VAL_NAME]);
          end else if SymbolTable.IsOfType(resElt.ResultType,TPasRecordType) then begin
            WriteLn('Fillchar(%s,SizeOf(%s),#0);',[RETURN_VAL_NAME,resElt.ResultType.Name]);
          end else begin
            WriteLn('if ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkInterface] ) then',[resElt.ResultType.Name]);
            IncIndent();
              WriteLn('Pointer(%s) := nil;',[RETURN_VAL_NAME]);
            DecIndent();
          end;}
        end;
      end;

      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        if SymbolTable.IsInitNeed(prm.ArgType) then begin
          WriteLn('Fillchar(%s,SizeOf(%s),#0);',[prm.Name,prm.ArgType.Name]);
          {if SymbolTable.IsOfType(prm.ArgType,TPasClassType) or
             SymbolTable.IsOfType(prm.ArgType,TPasArrayType)
          then begin
            WriteLn('TObject(%s) := nil;',[prm.Name]);
          end else begin
            WriteLn('if ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkObject,tkInterface] ) then',[prm.ArgType.Name]);
            IncIndent();
              WriteLn('Pointer(%s) := nil;',[prm.Name]);
            DecIndent();
          end;}
        end;
      end;

      NewLine();
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        Write('%s := %s;',[sPRM_NAME,QuotedStr(SymbolTable.GetExternalName(prm))]);
          WriteLn('AFormatter.Get(TypeInfo(%s),%s,%s);',[prm.ArgType.Name,sPRM_NAME,prm.Name]);
        if SymbolTable.IsInitNeed(prm.ArgType) then begin
          if SymbolTable.IsOfType(prm.ArgType,TPasClassType) or SymbolTable.IsOfType(prm.ArgType,TPasArrayType) then begin
            WriteLn('if Assigned(Pointer(%s)) then',[prm.Name]);
            IncIndent();
              WriteLn('callCtx.AddObjectToFree(TObject(%s));',[prm.Name]);
            DecIndent();
          end else begin
            WriteLn('if ( PTypeInfo(TypeInfo(%s))^.Kind = tkClass ) and Assigned(Pointer(%s)) then',[prm.ArgType.Name,prm.Name]);
            IncIndent();
              WriteLn('callCtx.AddObjectToFree(TObject(%s));',[prm.Name]);
            DecIndent();
          end;
        end;
      end;

      NewLine();
      WriteLn('tmpObj := Self.GetFactory().CreateInstance() as %s;',[AIntf.Name]);
      WriteLn('if Supports(tmpObj,ICallControl,cllCntrl) then');
      Indent();WriteLn('cllCntrl.SetCallContext(callCtx);');
      WriteLn('hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);');
      WriteLn('if hasObjCntrl then');
      Indent();WriteLn('objCntrl.Activate();');

      WriteLn('try');IncIndent();

        if AMthd.InheritsFrom(TPasFunction) then
          Write('%s := tmpObj.%s(',[RETURN_VAL_NAME,AMthd.Name])
        else
          Write('tmpObj.%s(',[AMthd.Name]);
        strBuff := '';
        for k := 0 to Pred(prmCnt) do begin
          prm := TPasArgument(prms[k]);
          strBuff := strBuff + Format('%s,',[prm.Name]);
        end;
        if ( prmCnt > 0 ) then
          Delete(strBuff,Length(strBuff),1);
        strBuff := strBuff + ');';
        EndAutoIndent();
          WriteLn(strBuff);
        BeginAutoIndent();

        if AMthd.InheritsFrom(TPasFunction) then begin
          if SymbolTable.IsInitNeed(resElt.ResultType) then begin
            if SymbolTable.IsOfType(resElt.ResultType,TPasClassType) or SymbolTable.IsOfType(resElt.ResultType,TPasArrayType) then
              WriteLn('if Assigned(TObject(%s)) then',[RETURN_VAL_NAME])
            else
              WriteLn('if ( PTypeInfo(TypeInfo(%s))^.Kind = tkClass ) and Assigned(Pointer(%s)) then',[resElt.ResultType.Name,RETURN_VAL_NAME]);
            IncIndent();
              WriteLn('callCtx.AddObjectToFree(TObject(%s));',[RETURN_VAL_NAME]);
            DecIndent();
          end;
        end;
        NewLine();

        WriteLn('procName := AFormatter.GetCallProcedureName();');
        WriteLn('trgName := AFormatter.GetCallTarget();');
        WriteLn('AFormatter.Clear();');

        WriteLn('AFormatter.BeginCallResponse(procName,trgName);');
          IncIndent();
          if AMthd.InheritsFrom(TPasFunction) then begin
            WriteLn('AFormatter.Put(%s,TypeInfo(%s),%s);',[QuotedStr(SymbolTable.GetExternalName(resElt)),resElt.ResultType.Name,RETURN_VAL_NAME]);
          end;
          for k := 0 to Pred(prmCnt) do begin
            prm := TPasArgument(prms[k]);
            if ( prm.Access in [argOut,argVar] ) then
              WriteLn('AFormatter.Put(%s,TypeInfo(%s),%s);',[QuotedStr(SymbolTable.GetExternalName(prm)),prm.ArgType.Name,prm.Name]);
          end;
          DecIndent();
        WriteLn('AFormatter.EndCallResponse();');
        NewLine();
        WriteLn('callCtx := nil;');

      DecIndent();
      WriteLn('finally');
      WriteLn('  if hasObjCntrl then');
      WriteLn('    objCntrl.Deactivate();');
      WriteLn('  Self.GetFactory().ReleaseInstance(tmpObj);');
      WriteLn('end;');

    DecIndent();EndAutoIndent();
    WriteLn('end;');
  End;

  procedure WriteConstructor();
  Var
    k : Integer;
    mtd : TPasProcedure;
    mtds : TList;
  Begin
    NewLine();
    WriteLn('constructor %s.Create();',[strClassName]);
    WriteLn('begin');
    IncIndent();
    BeginAutoIndent();
      WriteLn('inherited Create(GetServiceImplementationRegistry().FindFactory(%s));',[QuotedStr(AIntf.Name)]);
      mtds := AIntf.Members;
      for k := 0 to Pred(mtds.Count) do begin
        if TPasElement(mtds[k]).InheritsFrom(TPasProcedure) then begin
          mtd := TPasProcedure(mtds[k]);
          WriteLn('RegisterVerbHandler(%s,{$IFDEF FPC}@{$ENDIF}%sHandler);',[QuotedStr(mtd.Name),mtd.Name]);
        end;
      end;
    EndAutoIndent();
    DecIndent();
    WriteLn('end;');
    NewLine();
  End;

  procedure WriteMethods();
  var
    k : Integer;
    mtds : TList;
    mtd : TPasProcedure;
  begin
    mtds := AIntf.Members;
    for k := 0 to Pred(mtds.Count) do begin
      if TPasElement(mtds[k]).InheritsFrom(TPasProcedure) then begin
        mtd := TPasProcedure(mtds[k]);
        WriteMethodDec(mtd);
        WriteMethodImp(mtd);
        WriteLn('');
      end;
    end;
    WriteConstructor();
  end;

  procedure GenerateFactoryClass();
  Var
    strBuff : string;
  Begin
    NewLine();
    BeginAutoIndent();
      strBuff := Format('T%s_ServiceBinderFactory',[ExtractserviceName(AIntf)]);
      WriteLn('{ %s }',[strBuff]);
      NewLine();
      WriteLn('function %s.CreateInstance():IInterface;',[strBuff]);
      WriteLn('begin');
        IncIndent();
          WriteLn('Result := FInstance;',[strClassName]);
        DecIndent();
      WriteLn('end;');

      NewLine();
      WriteLn('constructor %s.Create();',[strBuff]);
      WriteLn('begin');
        IncIndent();
          WriteLn('FInstance := %s.Create() as IInterface;',[strClassName]);
        DecIndent();
      WriteLn('end;');

      NewLine();
      WriteLn('destructor %s.Destroy();',[strBuff]);
      WriteLn('begin');
        IncIndent();
          WriteLn('FInstance := nil;');
          WriteLn('inherited Destroy();');
        DecIndent();
      WriteLn('end;');
      
    EndAutoIndent();
  End;
  
  procedure GenerateRegistrationProc();
  Var
    strBuff : string;
  Begin
    NewLine();
    BeginAutoIndent();
      strBuff := ExtractserviceName(AIntf);
      NewLine();
      WriteLn('procedure Server_service_Register%sService();',[strBuff]);
      WriteLn('Begin');
        IncIndent();
          WriteLn('GetServerServiceRegistry().Register(%s,T%s_ServiceBinderFactory.Create() as IItemFactory);',[QuotedStr(AIntf.Name),strBuff]);
        DecIndent();
      WriteLn('End;');
    EndAutoIndent();
  End;

begin
  SetCurrentStream(FImpStream);
  IncIndent();
  While ( DecIndent() > 0 ) Do
    ;
  strClassName := GenerateClassName(AIntf);
  NewLine();
  WriteDec();
  WriteMethods();

  GenerateFactoryClass();
  GenerateRegistrationProc();
end;

function TBinderGenerator.GetDestUnitName(): string;
begin
  Result := Format('%s_binder',[SymbolTable.CurrentModule.Name]);
end;

constructor TBinderGenerator.Create(ASymTable: TwstPasTreeContainer;ASrcMngr: ISourceManager);
begin
  Inherited Create(ASymTable,ASrcMngr);
  FDecStream := SrcMngr.CreateItem(GetDestUnitName() + '.dec');
  FImpStream := SrcMngr.CreateItem(GetDestUnitName() + '.imp');
end;

procedure TBinderGenerator.Execute();
Var
  i,c : Integer;
  intf : TPasClassType;
  typeList : TList;
  elt : TPasElement;
begin
  GenerateUnitHeader();
  GenerateUnitImplementationHeader();
  typeList := SymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := Pred(typeList.Count);
  for i := 0 to c do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
      intf := TPasClassType(elt);
      GenerateIntf(intf);
      GenerateImp(intf);
    end;
  end;
  GenerateUnitImplementationFooter();
  FSrcMngr.Merge(GetDestUnitName() + '.pas',[FDecStream,FImpStream]);
  FDecStream := nil;
  FImpStream := nil;
end;

{ TImplementationGenerator }

function TImplementationGenerator.GenerateClassName(AIntf: TPasElement): String;
begin
  Result := ExtractserviceName(AIntf);
  Result := Format('T%s_ServiceImp',[Result]);
end;

procedure TImplementationGenerator.GenerateUnitHeader();
begin
  SetCurrentStream(FDecStream);
  WriteLn('{');
  WriteLn('This unit has been produced by ws_helper.');
  WriteLn('  Input unit name : "%s".',[SymbolTable.CurrentModule.Name]);
  WriteLn('  This unit name  : "%s".',[GetDestUnitName()]);
  WriteLn('  Date            : "%s".',[DateTimeToStr(Now())]);
  WriteLn('}');

  WriteLn('Unit %s;',[GetDestUnitName()]);
  WriteLn('{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}');
  WriteLn('Interface');
  WriteLn('');
  WriteLn('Uses SysUtils, Classes, ');
  WriteLn('     base_service_intf, server_service_intf, server_service_imputils, %s;',[SymbolTable.CurrentModule.Name]);
  WriteLn('');
  WriteLn('Type');
  WriteLn('');
end;

procedure TImplementationGenerator.GenerateUnitImplementationHeader();
begin
  SetCurrentStream(FImpStream);
  WriteLn('');
  WriteLn('Implementation');
  WriteLn('uses config_objects;');
end;

procedure TImplementationGenerator.GenerateUnitImplementationFooter();
begin
  NewLine();
  WriteLn('End.');
end;

procedure TImplementationGenerator.GenerateIntf(AIntf: TPasClassType);
  procedure WriteDec();
  begin
    Indent();
    WriteLn('%s=class(%s,%s)',[GenerateClassName(AIntf),sIMP_BASE_CLASS,AIntf.Name]);
  end;

  procedure WriteMethod(AMthd : TPasProcedure);
  var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList;
  begin
    Indent();
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write('function ')
    end else begin
      Write('procedure ');
    end;
    Write('%s(',[AMthd.Name]);

    if ( prmCnt > 0 ) then begin
      IncIndent();
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        if (k > 0 ) then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[AccessNames[prm.Access],prm.Name,prm.ArgType.Name]);
      end;
      DecIndent();
      NewLine();
      Indent();
    end;

    Write(')');
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write(':%s',[TPasFunctionType(AMthd.ProcType).ResultEl.ResultType.Name]);
    end;
    WriteLn(';');
  end;

  procedure WriteMethods();
  var
    k : Integer;
    mtds : TList;
    elt : TPasElement;
  begin
    if ( GetElementCount(AIntf.Members,TPasProcedure) > 0 ) then begin
      Indent();WriteLn('Protected');
      IncIndent();
        mtds := AIntf.Members;
        for k := 0 to Pred(mtds.Count) do begin
          elt := TPasElement(mtds[k]);
          if elt.InheritsFrom(TPasProcedure) then begin
            WriteMethod(TPasProcedure(elt));
          end;
        end;
      DecIndent();
    end;
  end;

  procedure GenerateRegistrationProc();
  Begin
    NewLine();
    BeginAutoIndent();
      IncIndent();
      WriteLn('procedure Register%sImplementationFactory();',[ExtractserviceName(AIntf)]);
      DecIndent();
    EndAutoIndent();
  End;

begin
  SetCurrentStream(FDecStream);
  NewLine();
  IncIndent();
    WriteDec();
    WriteMethods();
    Indent(); WriteLn('End;');
    NewLine();
  DecIndent();
  
  GenerateRegistrationProc();
end;

procedure TImplementationGenerator.GenerateImp(AIntf: TPasClassType);
var
  strClassName : String;

  procedure WriteDec();
  begin
    if ( GetElementCount(AIntf.Members,TPasProcedure) > 0 ) then begin
      WriteLn('{ %s implementation }',[strClassName]);
    end;
  end;

  procedure WriteMethodDec(AMthd : TPasProcedure);
  var
    prmCnt,k : Integer;
    prms : TList;
    prm : TPasArgument;
  begin
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write('function ');
    end else begin
      Write('procedure ');
    end;
    Write('%s.%s(',[strClassName,AMthd.Name]);

    if ( prmCnt > 0 ) then begin
      IncIndent();
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        if (k > 0 ) then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[AccessNames[prm.Access],prm.Name,prm.ArgType.Name]);
      end;
      DecIndent();
      NewLine();
      Indent();
    end;

    Write(')');
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write(':%s',[TPasFunctionType(AMthd.ProcType).ResultEl.ResultType.Name]);
    end;
    WriteLn(';');
  end;

  procedure WriteMethodImp(AMthd : TPasProcedure);
  begin
    WriteLn('Begin');
    WriteLn('// your code here');
    WriteLn('End;');
  end;

  procedure WriteMethods();
  var
    k : Integer;
    mbrs : TList;
    elt : TPasElement;
    mtd : TPasProcedure;
  begin
    mbrs := AIntf.Members;
    for k := 0 to Pred(mbrs.Count) do begin
      elt := TPasElement(mbrs[k]);
      if elt.InheritsFrom(TPasProcedure) then begin
        mtd := TPasProcedure(elt);
        WriteMethodDec(mtd);
        WriteMethodImp(mtd);
        WriteLn('');
      end;
    end;
  end;

  procedure GenerateRegistrationProc();
  Var
    strBuff : string;
  Begin
    NewLine();
    BeginAutoIndent();
      strBuff := ExtractserviceName(AIntf);
      NewLine();
      WriteLn('procedure Register%sImplementationFactory();',[strBuff]);
      WriteLn('Begin');
        IncIndent();
          WriteLn('GetServiceImplementationRegistry().Register(%s,TImplementationFactory.Create(%s,wst_GetServiceConfigText(%s)) as IServiceImplementationFactory);',[QuotedStr(AIntf.Name),strClassName,QuotedStr(AIntf.Name)]);
        DecIndent();
      WriteLn('End;');
    EndAutoIndent();
  End;

begin
  SetCurrentStream(FImpStream);
  IncIndent();
  While ( DecIndent() > 0 ) Do
    ;
  strClassName := GenerateClassName(AIntf);
  NewLine();
  WriteDec();
  WriteMethods();
  
  GenerateRegistrationProc();
end;

function TImplementationGenerator.GetDestUnitName(): string;
begin
  Result := Format('%s_imp',[SymbolTable.CurrentModule.Name]);
end;

constructor TImplementationGenerator.Create(ASymTable: TwstPasTreeContainer;ASrcMngr: ISourceManager);
begin
  Inherited Create(ASymTable,ASrcMngr);
  FDecStream := SrcMngr.CreateItem(GetDestUnitName() + '.dec');
  FImpStream := SrcMngr.CreateItem(GetDestUnitName() + '.imp');
end;

procedure TImplementationGenerator.Execute();
Var
  i,c : Integer;
  intf : TPasClassType;
  elt : TPasElement;
  typeList : TList;
begin
  GenerateUnitHeader();
  GenerateUnitImplementationHeader();
  typeList := SymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := Pred(typeList.Count);
  for i := 0 to c do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
      intf := TPasClassType(elt);
      GenerateIntf(intf);
      GenerateImp(intf);
    end;
  end;
  GenerateUnitImplementationFooter();
  FSrcMngr.Merge(GetDestUnitName() + '.pas',[FDecStream,FImpStream]);
  FDecStream := nil;
  FImpStream := nil;
end;

{ TInftGenerator }

procedure TInftGenerator.WriteDocumetation(AElement : TPasElement);
var
  pl : TStrings;
  docString : string;
  i : PtrInt;
begin
  pl := FSymbolTable.Properties.FindList(AElement);
  if ( pl <> nil ) then begin
    i := pl.IndexOfName(sDOCUMENTATION);
    if ( i >= 0 ) then begin
      docString:= StringReplace(DecodeLineBreak(pl.ValueFromIndex[i]),#10,sLineBreak,[rfReplaceAll]);
      if not IsStrEmpty(docString) then begin
        WriteLn('{ %s',[AElement.Name]);
          WriteLn(docString);
        WriteLn('}');
      end;
    end;
  end;
end;

procedure TInftGenerator.WriteDocIfEnabled(AElement : TPasElement);
begin
  if ( goGenerateDocAsComments in Options ) then
    WriteDocumetation(AElement);
end;

procedure TInftGenerator.WriteObjectArray(ASymbol : TPasArrayType);
begin
  SetCurrentStream(FDecStream);
  NewLine();
  WriteDocIfEnabled(ASymbol);
  IncIndent();
  BeginAutoIndent();
  try
    WriteLn('%s = class(TBaseObjectArrayRemotable)',[ASymbol.Name]);
    WriteLn('private');
      Indent();WriteLn('function GetItem(AIndex: Integer): %s;',[ASymbol.ElType.Name]);
    WriteLn('public');
      Indent();WriteLn('class function GetItemClass():TBaseRemotableClass;override;');
      Indent();WriteLn('property Item[AIndex:Integer] : %s Read GetItem;Default;',[ASymbol.ElType.Name]);
    WriteLn('end;');
  finally
    EndAutoIndent();
    DecIndent();
  end;

  SetCurrentStream(FImpStream);
  NewLine();
  WriteLn('{ %s }',[ASymbol.Name]);

  NewLine();
  WriteLn('function %s.GetItem(AIndex: Integer): %s;',[ASymbol.Name,ASymbol.ElType.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('Result := %s(Inherited GetItem(AIndex));',[ASymbol.ElType.Name]);
  DecIndent();
  WriteLn('end;');

  NewLine();
  WriteLn('class function %s.GetItemClass(): TBaseRemotableClass;',[ASymbol.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('Result:= %s;',[ASymbol.ElType.Name]);
  DecIndent();
  WriteLn('end;');
end;

procedure TInftGenerator.WriteSimpleTypeArray(ASymbol : TPasArrayType);
begin
  SetCurrentStream(FDecStream);
  NewLine();
  WriteDocIfEnabled(ASymbol);
  IncIndent();
  BeginAutoIndent();
  try
    WriteLn('%s = class(TBaseSimpleTypeArrayRemotable)',[ASymbol.Name]);
    WriteLn('private');
      Indent();WriteLn('FData : array of %s;',[ASymbol.ElType.Name]);
    WriteLn('private');
      Indent();WriteLn('function GetItem(AIndex: Integer): %s;',[ASymbol.ElType.Name]);
      Indent();WriteLn('procedure SetItem(AIndex: Integer; const AValue: %s);',[ASymbol.ElType.Name]);
    WriteLn('protected');
      Indent();WriteLn('function GetLength():Integer;override;');
      Indent();WriteLn('procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;');
      Indent();WriteLn('procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;');
    WriteLn('public');
      Indent();WriteLn('class function GetItemTypeInfo():PTypeInfo;override;');
      Indent();WriteLn('procedure SetLength(const ANewSize : Integer);override;');
      Indent();WriteLn('procedure Assign(Source: TPersistent); override;');
      Indent();WriteLn('property Item[AIndex:Integer] : %s read GetItem write SetItem; default;',[ASymbol.ElType.Name]);
    WriteLn('end;');
  finally
    EndAutoIndent();
    DecIndent();
  end;

  SetCurrentStream(FImpStream);
  NewLine();
  WriteLn('{ %s }',[ASymbol.Name]);

  NewLine();
  WriteLn('function %s.GetItem(AIndex: Integer): %s;',[ASymbol.Name,ASymbol.ElType.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('CheckIndex(AIndex);');
    Indent();WriteLn('Result := FData[AIndex];');
  DecIndent();
  WriteLn('end;');

  NewLine();
  WriteLn('procedure %s.SetItem(AIndex: Integer;const AValue: %S);',[ASymbol.Name,ASymbol.ElType.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('CheckIndex(AIndex);');
    Indent();WriteLn('FData[AIndex] := AValue;');
  DecIndent();
  WriteLn('end;');

  NewLine();
  WriteLn('function %s.GetLength(): Integer;',[ASymbol.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('Result := System.Length(FData);');
  DecIndent();
  WriteLn('end;');

  NewLine();
  WriteLn('procedure %s.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);',[ASymbol.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('AStore.Put(%s,TypeInfo(%s),FData[AIndex]);',[QuotedStr(SymbolTable.GetArrayItemName(ASymbol)),ASymbol.ElType.Name]);
  DecIndent();
  WriteLn('end;');

  NewLine();
  IncIndent();
  WriteLn('procedure %s.LoadItem(AStore: IFormatterBase;const AIndex: Integer);',[ASymbol.Name]);
  WriteLn('var');
  Indent();WriteLn('sName : string;');
  WriteLn('begin');
    Indent();WriteLn('sName := %s;',[QuotedStr(SymbolTable.GetArrayItemName(ASymbol))]);
    Indent();WriteLn('AStore.Get(TypeInfo(%s),sName,FData[AIndex]);',[ASymbol.ElType.Name]);
  DecIndent();
  WriteLn('end;');

  NewLine();
  WriteLn('class function %s.GetItemTypeInfo(): PTypeInfo;',[ASymbol.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('Result := TypeInfo(%s);',[ASymbol.ElType.Name]);
  DecIndent();
  WriteLn('end;');

  NewLine();
  IncIndent();
  WriteLn('procedure %s.SetLength(const ANewSize: Integer);',[ASymbol.Name]);
  WriteLn('var');
  Indent();WriteLn('i : Integer;');
  WriteLn('begin');
    Indent();WriteLn('if ( ANewSize < 0 ) then');
      Indent();Indent();WriteLn('i := 0');
    Indent();WriteLn('else');
      Indent();Indent();WriteLn('i := ANewSize;');
    Indent();WriteLn('System.SetLength(FData,i);');
  DecIndent();
  WriteLn('end;');

  NewLine();
  IncIndent();
  WriteLn('procedure %s.Assign(Source: TPersistent);',[ASymbol.Name]);
  WriteLn('var');
  Indent();WriteLn('src : %s;',[ASymbol.Name]);
  Indent();WriteLn('i, c : PtrInt;');
  WriteLn('begin');
    Indent();WriteLn('if Assigned(Source) and Source.InheritsFrom(%s) then begin',[ASymbol.Name]);
    IncIndent();
      Indent();WriteLn('src := %s(Source);',[ASymbol.Name]);
      Indent();WriteLn('c := src.Length;');
      Indent();WriteLn('Self.SetLength(c);');
      Indent();WriteLn('if ( c > 0 ) then begin');
      IncIndent();
        Indent();WriteLn('for i := 0 to Pred(c) do begin');
        IncIndent(); Indent(); WriteLn('Self[i] := src[i];'); DecIndent();
        Indent();WriteLn('end;');
      DecIndent();
      Indent();WriteLn('end;');
    DecIndent();
    Indent();WriteLn('end else begin');
      IncIndent(); Indent(); WriteLn('inherited Assign(Source);'); DecIndent();
    Indent();WriteLn('end;');
  DecIndent();
  WriteLn('end;');
end;

procedure TInftGenerator.WriteObjectCollection(ASymbol : TPasArrayType);
begin
  SetCurrentStream(FDecStream);
  NewLine();
  WriteDocIfEnabled(ASymbol);
  IncIndent();
  BeginAutoIndent();
  try
    WriteLn('%s = class(TObjectCollectionRemotable)',[ASymbol.Name]);
    WriteLn('private');
      Indent();WriteLn('function GetItem(AIndex: Integer): %s;',[ASymbol.ElType.Name]);
    WriteLn('public');
      Indent();WriteLn('class function GetItemClass():TBaseRemotableClass;override;');
      Indent();WriteLn('function Add(): %s; {$IFDEF USE_INLINE}inline;{$ENDIF}',[ASymbol.ElType.Name]);
      Indent();WriteLn('function AddAt(const APosition : Integer) : %s; {$IFDEF USE_INLINE}inline;{$ENDIF}',[ASymbol.ElType.Name]);
      Indent();WriteLn('property Item[AIndex:Integer] : %s Read GetItem;Default;',[ASymbol.ElType.Name]);
    WriteLn('end;');
  finally
    EndAutoIndent();
    DecIndent();
  end;

  SetCurrentStream(FImpStream);
  NewLine();
  WriteLn('{ %s }',[ASymbol.Name]);

  NewLine();
  WriteLn('function %s.GetItem(AIndex: Integer): %s;',[ASymbol.Name,ASymbol.ElType.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('Result := %s(Inherited GetItem(AIndex));',[ASymbol.ElType.Name]);
  DecIndent();
  WriteLn('end;');

  NewLine();
  WriteLn('class function %s.GetItemClass(): TBaseRemotableClass;',[ASymbol.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('Result:= %s;',[ASymbol.ElType.Name]);
  DecIndent();
  WriteLn('end;');
  
  NewLine();
  WriteLn('function %s.Add() : %s;',[ASymbol.Name,ASymbol.ElType.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('Result := %s(inherited Add());',[ASymbol.ElType.Name]);
  DecIndent();
  WriteLn('end;');
  
  NewLine();
  WriteLn('function %s.AddAt(const APosition : Integer) : %s;',[ASymbol.Name,ASymbol.ElType.Name]);
  WriteLn('begin');
  IncIndent();
    Indent();WriteLn('Result := %s(inherited AddAt(APosition));',[ASymbol.ElType.Name]);
  DecIndent();
  WriteLn('end;');
end;

function TInftGenerator.GenerateIntfName(AIntf: TPasElement): string;
begin
  Result := AIntf.Name;//ExtractserviceName(AIntf);
end;

procedure TInftGenerator.GenerateUnitHeader();
begin
  SetCurrentStream(FDecStream);
  WriteLn('{');
  WriteLn('This unit has been produced by ws_helper.');
  WriteLn('  Input unit name : "%s".',[SymbolTable.CurrentModule.Name]);
  WriteLn('  This unit name  : "%s".',[GetDestUnitName()]);
  WriteLn('  Date            : "%s".',[DateTimeToStr(Now())]);
  WriteLn('}');

  WriteLn('unit %s;',[GetDestUnitName()]);
  WriteLn('{$IFDEF FPC}');
  WriteLn('  {$mode objfpc} {$H+}');
  WriteLn('{$ENDIF}');
  WriteLn('{$IFNDEF FPC}');
  WriteLn('  {$DEFINE WST_RECORD_RTTI}');
  WriteLn('{$ENDIF}');
  WriteLn('interface');
  WriteLn('');
  WriteLn('uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;');
  WriteLn('');
  WriteLn('const');

  IncIndent();
  Indent();WriteLn('sNAME_SPACE = %s;',[QuotedStr(SymbolTable.GetExternalName(FSymbolTable.CurrentModule))]);
  Indent();WriteLn('sUNIT_NAME = %s;',[QuotedStr(FSymbolTable.CurrentModule.Name)]);
  DecIndent();
  
  WriteLn('');
  WriteLn('type');
  WriteLn('');
end;

procedure TInftGenerator.GenerateUnitImplementationHeader();
begin
  SetCurrentStream(FImpStream);
  WriteLn('');
  WriteLn('Implementation');
  WriteLn('uses metadata_repository, record_rtti, wst_types;');
  FImpTempStream.WriteLn('initialization');
end;

procedure TInftGenerator.GenerateUnitImplementationFooter();
begin
  SetCurrentStream(FImpStream);
  NewLine();
  NewLine();
  FImpTempStream.NewLine();
  FImpLastStream.NewLine();
  FImpLastStream.WriteLn('End.');
end;

procedure TInftGenerator.GenerateIntf(AIntf: TPasClassType);

  procedure WriteDec();
  begin
    Indent();
    WriteLn('%s = interface(IInvokable)',[GenerateIntfName(AIntf)]);
    if not IsStrEmpty(AIntf.InterfaceGUID) then begin
      Indent();Indent();WriteLn('[%s]',[QuotedStr(AIntf.InterfaceGUID)]);
    end;
  end;

  procedure WriteMethod(AMthd : TPasProcedure);
  var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList;
  begin
    Indent();
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write('function ');
    end else begin
      Write('procedure ');
    end;
    Write('%s(',[AMthd.Name]);

    if ( prmCnt > 0 ) then begin
      IncIndent();
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        if (k > 0 ) then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[AccessNames[prm.Access],prm.Name,prm.ArgType.Name]);
      end;
      DecIndent();
      NewLine();
      Indent();
    end;

    Write(')');
    if AMthd.InheritsFrom(TPasFunction) then begin
      Write(':%s',[TPasFunctionType(AMthd.ProcType).ResultEl.ResultType.Name]);
    end;
    WriteLn(';');
  end;

  procedure WriteMethods();
  var
    k : Integer;
    mbrs : TList;
    elt : TPasElement;
  begin
    IncIndent();
      mbrs := AIntf.Members;
      for k := 0 to Pred(mbrs.Count) do begin
        elt := TPasElement(mbrs[k]);
        if elt.InheritsFrom(TPasProcedure) then begin
          WriteMethod(TPasProcedure(elt));
        end;
      end;
    DecIndent();
  end;

begin
  SetCurrentStream(FDecStream);
  NewLine();
  IncIndent();
    WriteDec();
    WriteMethods();
    Indent(); WriteLn('end;');
  DecIndent();
end;

procedure TInftGenerator.GenerateTypeAlias(ASymbol: TPasAliasType);
var
  typeModifier : string;
begin
  try
    SetCurrentStream(FDecStream);
    WriteDocIfEnabled(ASymbol);
    if ASymbol.InheritsFrom(TPasTypeAliasType) then begin
      typeModifier := 'type ';
    end else begin
      typeModifier := '';
    end;
    NewLine();
    IncIndent();
      Indent();
      WriteLn('%s = %s%s;',[ASymbol.Name,typeModifier,ASymbol.DestType.Name]);
    DecIndent();
  except
    on e : Exception do
      GetLogger.Log(mtError,'TInftGenerator.GenerateTypeAlias()=',[ASymbol.Name, ' ;; ', e.Message]);
  end;
end;

procedure TInftGenerator.GenerateClass(ASymbol: TPasClassType);
var
  locClassPropNbr, locOptionalPropsNbr, locArrayPropsNbr, locPropCount : Integer;
  locPropList : TObjectList;
  
  procedure Prepare();
  var
    k : Integer;
    elt : TPasElement;
    p : TPasProperty;
  begin
    locPropCount := 0;
    locClassPropNbr   := 0;
    locArrayPropsNbr  := 0;
    locOptionalPropsNbr := 0;
    for k := 0 to Pred(ASymbol.Members.Count) do begin
      elt := TPasElement(ASymbol.Members[k]);
      if elt.InheritsFrom(TPasProperty) then begin
        p := TPasProperty(elt);
        locPropList.Add(p);
        Inc(locPropCount);
        if SymbolTable.IsOfType(p.VarType,TPasClassType) then
          Inc(locClassPropNbr);
        if SymbolTable.IsOfType(p.VarType,TPasArrayType) then
          Inc(locArrayPropsNbr);
        if AnsiSameText('HAS',Copy(p.StoredAccessorName,1,3))  then
          Inc(locOptionalPropsNbr);
      end;
    end;
    locClassPropNbr := locClassPropNbr + locArrayPropsNbr;
  end;
  
  procedure WriteDec();
  var
    decBuffer, s : string;
    elt : TPasElement;
    ultimAnc, trueAncestor : TPasType;
  begin
    if Assigned(ASymbol.AncestorType) then begin
      trueAncestor := ASymbol.AncestorType;
      if trueAncestor.InheritsFrom(TPasUnresolvedTypeRef) then begin
        elt := SymbolTable.FindElement(SymbolTable.GetExternalName(trueAncestor));
        if elt.InheritsFrom(TPasType) then begin
          trueAncestor := TPasType(elt);
        end;
      end;
      ultimAnc := GetUltimeType(trueAncestor);
      if ultimAnc.InheritsFrom(TPasNativeSimpleType) then begin
        trueAncestor := ultimAnc;
      end;
      if trueAncestor.InheritsFrom(TPasNativeSimpleType) and
         Assigned(TPasNativeSimpleType(trueAncestor).ExtendableType)
      then begin
        trueAncestor := TPasNativeSimpleType(trueAncestor).ExtendableType;
      end;
      s := Format('%s',[trueAncestor.Name]);
    end else begin
      s := '';//'TBaseComplexRemotable';
    end;
    if IsStrEmpty(s) then begin
      decBuffer := '';
    end else begin
      decBuffer := Format('(%s)',[s]);
    end;
    Indent();
    WriteLn('%s = class%s',[ASymbol.Name,decBuffer]);
  end;

  procedure WritePropertyField(AProp : TPasProperty);
  begin
    Indent();
    WriteLn('F%s : %s;',[AProp.Name,AProp.VarType.Name]);
  End;

  procedure WriteProperty(AProp : TPasProperty);
  var
    propName, locStore : string;
  begin
    propName := AProp.Name;
    if AnsiSameText('True',AProp.StoredAccessorName) then begin
      locStore := '';
    end else begin
      locStore := Format(' stored %s',[AProp.StoredAccessorName]);
    end;
    Indent();
    WriteLn('property %s : %s read F%s write F%s%s;',[propName,AProp.VarType.Name,propName,propName,locStore]);
    if not AnsiSameText(AProp.Name,SymbolTable.GetExternalName(AProp)) then begin
      FImpLastStream.Indent();
      FImpLastStream.WriteLn('GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)].RegisterExternalPropertyName(%s,%s);',[ASymbol.Name,QuotedStr(AProp.Name),QuotedStr(SymbolTable.GetExternalName(AProp))]);
    end;
    if SymbolTable.IsAttributeProperty(AProp) then begin
      FImpLastStream.Indent();
      FImpLastStream.WriteLn('%s.RegisterAttributeProperty(%s);',[ASymbol.Name,QuotedStr(AProp.Name)]);
    end;
  end;

  procedure WriteProperties();
  var
    k : Integer;
    p : TPasProperty;
    //pt : TPasElement;
  begin
    if ( locPropCount > 0 ) then begin
      Indent();
      WriteLn('private');
      IncIndent();
        for k := 0 to Pred(locPropCount) do begin
          p := TPasProperty(locPropList[k]);
          {if p.VarType.InheritsFrom(TPasUnresolvedTypeRef) then begin
            pt := SymbolTable.FindElement(SymbolTable.GetExternalName(p.VarType));
            if ( pt <> nil ) and pt.InheritsFrom(TPasType) and ( pt <> p.VarType ) then begin
              p.VarType.Release();
              p.VarType := pt as TPasType;
              p.VarType.AddRef();
            end;
          end;}
          WritePropertyField(p);
        end;
      DecIndent();
      //
      if ( locOptionalPropsNbr > 0 ) then begin
        Indent();
        WriteLn('private');
        IncIndent();
          for k := 0 to Pred(locPropCount) do begin
            p := TPasProperty(locPropList[k]);
            if AnsiSameText('HAS',Copy(p.StoredAccessorName,1,3)) then begin
              Indent();
              WriteLn('function %s() : Boolean;',[p.StoredAccessorName]);
            end;
          end;
        DecIndent();
      end;
      //
      if ( locArrayPropsNbr > 0 ) or ( locClassPropNbr > 0 ) then begin
        Indent();
        WriteLn('public');
      end;
      if ( locArrayPropsNbr > 0 ) or ( locClassPropNbr > 0 ) then begin
        IncIndent();
          Indent(); WriteLn('constructor Create();override;');
        DecIndent();
      end;

      if ( locArrayPropsNbr > 0 ) or ( locClassPropNbr > 0 ) then begin
        IncIndent();
          Indent(); WriteLn('destructor Destroy();override;');
        DecIndent();
      end;
      //
      Indent();
      WriteLn('published');
      IncIndent();
        For k := 0 To Pred(locPropCount) Do
          WriteProperty(TPasProperty(locPropList[k]));
      DecIndent();
    end;
  end;

  procedure WriteImp();
  var
    k : Integer;
    p : TPasProperty;
    ss : string;
    pte : TPasElement;
    pt : TPasType;
  begin
    if ( locClassPropNbr > 0 ) then begin
      NewLine();
      WriteLn('{ %s }',[ASymbol.Name]);
      
      if ( locClassPropNbr > 0 ) or ( locClassPropNbr > 0 ) then begin
        NewLine();
        WriteLn('constructor %s.Create();',[ASymbol.Name]);
        WriteLn('begin');
        IncIndent();
          Indent();
          WriteLn('inherited Create();');
          for k := 0 to Pred(locPropCount) do begin
            p := TPasProperty(locPropList[k]);
            if SymbolTable.IsOfType(p.VarType,TPasClassType) or
               SymbolTable.IsOfType(p.VarType,TPasArrayType)
            then begin
              Indent();
              if AnsiSameText(p.Name,p.VarType.Name) or
                 ( SymbolTable.IsOfType(p.VarType,TPasClassType) and Assigned(FindMember(TPasClassType(ASymbol),p.VarType.Name)) )
              then
                ss := Format('%s.%s',[SymbolTable.CurrentModule.Name,p.VarType.Name])
              else
                ss := p.VarType.Name;
              WriteLn('F%s := %s.Create();',[p.Name,ss{p.VarType.Name}]);
            end;
          end;
        DecIndent();
        WriteLn('end;');
      end;

      if ( locArrayPropsNbr > 0 ) or ( locClassPropNbr > 0 ) then begin
        NewLine();
        WriteLn('destructor %s.Destroy();',[ASymbol.Name]);
        WriteLn('begin');
        IncIndent();
          for k := 0 to Pred(locPropCount) do begin
            p := TPasProperty(locPropList[k]);
            if SymbolTable.IsOfType(p.VarType,TPasClassType) or
               SymbolTable.IsOfType(p.VarType,TPasArrayType)
            then begin
              Indent();
              WriteLn('if Assigned(F%s) then',[p.Name]);
                IncIndent();
                  Indent();
                  WriteLn('FreeAndNil(F%s);',[p.Name]) ;
                DecIndent();
            end;
          end;
          Indent();
          WriteLn('inherited Destroy();');
        DecIndent();
        WriteLn('end;');
      end;
    end;
    for k := 0 to Pred(locPropCount) do begin
      p := TPasProperty(locPropList[k]);
      if AnsiSameText('HAS',Copy(p.StoredAccessorName,1,3)) then begin
        NewLine();
        WriteLn('function %s.%s() : Boolean;',[ASymbol.Name,p.StoredAccessorName]);
        WriteLn('begin');
        IncIndent();
          Indent();
          pte := FSymbolTable.FindElement(p.VarType.Name);
          if ( pte <> nil ) and pte.InheritsFrom(TPasType) then begin
            pt := pte as TPasType;
            pt := GetUltimeType(pt);
            if pt.InheritsFrom(TPasEnumType) then begin
              WriteLn('Result := True;');
            end else if pt.InheritsFrom(TPasNativeSimpleType) and
                       ( AnsiPos('string', pt.Name) > 0 )
            then begin
              WriteLn('Result := ( F%s <> '''' );',[p.Name]);
            end else if pt.InheritsFrom(TPasNativeSimpleType) and
                       ( AnsiSameText(pt.Name,'Single') or
                         AnsiSameText(pt.Name,'Double') or
                         AnsiSameText(pt.Name,'Extended') or
                         AnsiSameText(pt.Name,'Currency') or
                         AnsiSameText(pt.Name,'Real') or
                         AnsiSameText(pt.Name,'Comp')
                       )
            then begin
              WriteLn('Result := ( F%s <> 0 );',[p.Name]);
            end else begin
              WriteLn('Result := ( F%s <> %s(0) );',[p.Name,p.VarType.Name]);
            end;
          end else begin
            WriteLn('Result := ( F%s <> %s(0) );',[p.Name,p.VarType.Name]);
          end;
        DecIndent();
        WriteLn('end;');
      end;
    end;
  end;

begin
  locPropList := TObjectList.Create(False);
  try
    Prepare();
    try
      SetCurrentStream(FDecStream);
      NewLine();
      WriteDocIfEnabled(ASymbol);
      IncIndent();
        WriteDec();
        WriteProperties();
        Indent(); WriteLn('end;');
      DecIndent();

      FImpTempStream.Indent();
      FImpTempStream.WriteLn('GetTypeRegistry().Register(%s,TypeInfo(%s),%s);',[sNAME_SPACE,ASymbol.Name,QuotedStr(SymbolTable.GetExternalName(ASymbol))]);

      SetCurrentStream(FImpStream);
        WriteImp();
    except
      on e : Exception do begin
        GetLogger.Log(mtError,'TInftGenerator.GenerateClass()=',[ASymbol.Name, ' ;; ', e.Message]);
        raise;
      end;
    end;
  finally
    FreeAndNil(locPropList);
  end;
end;

procedure TInftGenerator.GenerateEnum(ASymbol: TPasEnumType);
var
  itm : TPasEnumValue;
  i : Integer;
begin
  try
    SetCurrentStream(FDecStream);
    NewLine();
    WriteDocIfEnabled(ASymbol);
    IncIndent();
      Indent();WriteLn('%s = ( ',[ASymbol.Name]);

      FImpTempStream.Indent();
      FImpTempStream.WriteLn('GetTypeRegistry().Register(%s,TypeInfo(%s),%s);',[sNAME_SPACE,ASymbol.Name,QuotedStr(SymbolTable.GetExternalName(ASymbol))]);

      IncIndent();
        for i := 0 to Pred(ASymbol.Values.Count) do begin
          itm := TPasEnumValue(ASymbol.Values[i]);
          Indent();
          if ( i > 0 ) then
            WriteLn(',%s',[itm.Name])
          else
            WriteLn('%s',[itm.Name]);
          if SymbolTable.HasExternalName(itm) and
             ( not AnsiSameText(itm.Name,SymbolTable.GetExternalName(itm,False)) )
          then begin
            FImpTempStream.Indent();
            FImpTempStream.WriteLn('GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)].RegisterExternalPropertyName(%s,%s);',[ASymbol.Name,QuotedStr(itm.Name),QuotedStr(SymbolTable.GetExternalName(itm,False))]);
          end;
        end;
      DecIndent();
      Indent(); WriteLn(');');
    DecIndent();
  except
    on e : Exception do
      GetLogger.Log(mtError,'TInftGenerator.GenerateClass()=', [ASymbol.Name, ' ;; ', e.Message]);
  end;
end;

procedure TInftGenerator.GenerateArray(ASymbol: TPasArrayType);
var
  classItemArray : Boolean;
  eltType : TPasType;
begin
  eltType := ASymbol.ElType;
  if eltType.InheritsFrom(TPasUnresolvedTypeRef) then begin
    eltType := SymbolTable.FindElement(SymbolTable.GetExternalName(eltType)) as TPasType;
  end;
  classItemArray := SymbolTable.IsOfType(eltType,TPasClassType) or SymbolTable.IsOfType(eltType,TPasArrayType);

  if classItemArray then begin
    if FSymbolTable.IsCollection(ASymbol) then
      WriteObjectCollection(ASymbol)
    else
      WriteObjectArray(ASymbol);
  end else begin
    WriteSimpleTypeArray(ASymbol);
  end;

  FImpTempStream.Indent();
  FImpTempStream.WriteLn('GetTypeRegistry().Register(%s,TypeInfo(%s),%s);',[sNAME_SPACE,ASymbol.Name,QuotedStr(SymbolTable.GetExternalName(ASymbol))]);
  if ( SymbolTable.GetArrayItemName(ASymbol) <> SymbolTable.GetArrayItemExternalName(ASymbol) ) then begin
    FImpTempStream.Indent();
    FImpTempStream.WriteLn(
      'GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)].RegisterExternalPropertyName(sARRAY_ITEM,%s);',
      [ASymbol.Name,QuotedStr(SymbolTable.GetArrayItemExternalName(ASymbol))]
    );
  end;
  if ( SymbolTable.GetArrayStyle(ASymbol) = asEmbeded ) then begin
    FImpTempStream.Indent();
    FImpTempStream.WriteLn(
      'GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);',
      [ASymbol.Name,QuotedStr(SymbolTable.GetArrayItemExternalName(ASymbol))]
    );
  end;
end;

procedure TInftGenerator.GenerateRecord(ASymbol : TPasRecordType);
var
  strFieldList : string;
  
  procedure WriteDec();
  var
    itm : TPasVariable;
    i : PtrInt;
  begin
    SetCurrentStream(FDecStream);
    NewLine();
    WriteDocIfEnabled(ASymbol);
    IncIndent();
      Indent(); WriteLn('%s = record',[ASymbol.Name]);
      IncIndent();
        strFieldList := '';
        for i := 0 to Pred(ASymbol.Members.Count) do begin
          itm := TPasVariable(ASymbol.Members[i]);
          Indent();
          WriteLn('%s : %s;',[itm.Name,itm.VarType.Name]);
          if ( i > 0 ) then
            strFieldList := Format('%s;%s',[strFieldList,itm.Name])
          else
            strFieldList := itm.Name;
        end;
      DecIndent();
      Indent(); WriteLn('end;');
    DecIndent();
  end;

  procedure WriteRTTI();
  var
    itm : TPasVariable;
    k, c : PtrInt;
    offsetLine, typeLine : string;
  begin
    SetCurrentStream(FRttiFunc);
    NewLine();
    WriteLn('{$IFDEF %s}',[sRECORD_RTTI_DEFINE]);
    WriteLn('function __%s_TYPEINFO_FUNC__() : PTypeInfo;',[ASymbol.Name]);
    WriteLn('var');
      IncIndent();
        Indent(); WriteLn('p : ^%s;',[ASymbol.Name]);
        Indent(); WriteLn('r : %s;',[ASymbol.Name]);
      DecIndent();
    WriteLn('begin');
    IncIndent();
      Indent(); WriteLn('p := @r;');
      Indent(); WriteLn('Result := MakeRawTypeInfo(');
      IncIndent();
        Indent(); WriteLn('%s,',[QuotedStr(ASymbol.Name)]);
        Indent(); WriteLn('SizeOf(%s),',[ASymbol.Name]);
        offsetLine := '[ ';
        typeLine   := '[ ';
        c := ASymbol.Members.Count;
        if ( c > 0 ) then begin
          k := 1;
          itm := TPasVariable(ASymbol.Members[(k-1)]);
          offsetLine := offsetLine + Format('PtrUInt(@(p^.%s)) - PtrUInt(p)',[itm.Name]);
          typeLine := typeLine + Format('TypeInfo(%s)',[itm.VarType.Name]);
          Inc(k);
          for k := k to c do begin
            itm := TPasVariable(ASymbol.Members[(k-1)]);
            offsetLine := offsetLine + Format(', PtrUInt(@(p^.%s)) - PtrUInt(p)',[itm.Name]);
            typeLine := typeLine + Format(', TypeInfo(%s)',[itm.VarType.Name]);
          end;
        end;
        offsetLine := offsetLine + ' ]';
        typeLine := typeLine + ' ]';
        Indent(); WriteLn('%s,',[offsetLine]);
        Indent(); WriteLn('%s',[typeLine]);
      DecIndent();
      Indent(); WriteLn(');');
    DecIndent();
    WriteLn('end;');
    WriteLn('{$ENDIF %s}',[sRECORD_RTTI_DEFINE]);
  end;

  procedure WriteAttributeProperties();
  var
    itm : TPasVariable;
    k, c : PtrInt;
  begin
    c := ASymbol.Members.Count;
    for k := 0 to Pred(c) do begin
      itm := TPasVariable(ASymbol.Members[k]);
      if SymbolTable.IsAttributeProperty(itm) then begin
        Indent();
        WriteLn('RegisterAttributeProperty(TypeInfo(%s),%s);',[ASymbol.Name,QuotedStr(itm.Name)]);
      end;
    end;
  end;
  
var
  s : string;
begin
  try
    WriteDec();
    WriteRTTI();
    
    SetCurrentStream(FImpLastStream);
    NewLine();

      Indent();
      WriteLn(
        'GetTypeRegistry().Register(%s,TypeInfo(%s),%s).RegisterExternalPropertyName(%s,%s);',
        [ sNAME_SPACE,ASymbol.Name,QuotedStr(SymbolTable.GetExternalName(ASymbol)),
          QuotedStr(Format('__FIELDS__',[ASymbol.Name])),QuotedStr(strFieldList)
        ]
      );
      s := 'GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)]' +
             '.RegisterObject(' +
               'FIELDS_STRING,' +
               'TRecordRttiDataObject.Create(' +
                 'MakeRecordTypeInfo(%s),' +
                 'GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)].GetExternalPropertyName(''__FIELDS__'')' +
               ')' +
             ');';
      WriteLn('{$IFNDEF %s}',[sRECORD_RTTI_DEFINE]);
        Indent(); WriteLn(s,[ASymbol.Name,Format('TypeInfo(%s)',[ASymbol.Name]),ASymbol.Name]);
      WriteLn('{$ENDIF %s}',[sRECORD_RTTI_DEFINE]);

      WriteLn('{$IFDEF %s}',[sRECORD_RTTI_DEFINE]);
        Indent(); WriteLn(s,[ASymbol.Name,Format('__%s_TYPEINFO_FUNC__()',[ASymbol.Name]),ASymbol.Name]);
      WriteLn('{$ENDIF %s}',[sRECORD_RTTI_DEFINE]);
      WriteAttributeProperties();
    SetCurrentStream(FDecStream);
  except
    on e : Exception do
      GetLogger.Log(mtError,'TInftGenerator.GenerateRecord()=', [ASymbol.Name, ' ;; ', e.Message]);
  end;
end;

procedure TInftGenerator.GenerateCustomMetadatas();

  procedure WriteOperationDatas(AInftDef : TPasClassType; AOp : TPasProcedure);
  var
    k : Integer;
    pl : TStrings;
  begin
    pl := SymbolTable.Properties.FindList(AOp);
    if ( pl <> nil ) then begin
      for k := 0 to Pred(pl.Count) do begin
        if not IsStrEmpty(pl.ValueFromIndex[k]) then begin
          Indent();WriteLn('mm.SetOperationCustomData(');
            IncIndent();
              Indent(); WriteLn('%s,',[sUNIT_NAME]);
              Indent(); WriteLn('%s,',[QuotedStr(AInftDef.Name)]);
              Indent(); WriteLn('%s,',[QuotedStr(AOp.Name)]);
              Indent(); WriteLn('%s,',[QuotedStr(pl.Names[k])]);
              Indent(); WriteLn('%s' ,[QuotedStr(pl.ValueFromIndex[k])]);
            DecIndent();
          Indent();WriteLn(');');
        end;
      end;
    end;
  end;
  
  procedure WriteServiceDatas(ABinding : TwstBinding);
  var
    k : Integer;
    opList : TList;
    elt : TPasElement;
  begin
    if not IsStrEmpty(ABinding.Address) then begin
      Indent();WriteLn('mm.SetServiceCustomData(');
        IncIndent();
          Indent(); WriteLn('%s,',[sUNIT_NAME]);
          Indent(); WriteLn('%s,',[QuotedStr(ABinding.Intf.Name)]);
          Indent(); WriteLn('%s,',[QuotedStr('TRANSPORT_Address')]);
          Indent(); WriteLn('%s' ,[QuotedStr(ABinding.Address)]);
        DecIndent();
      Indent();WriteLn(');');
    end;
    
    if ( ABinding.BindingStyle = bsRPC ) then begin
      Indent();WriteLn('mm.SetServiceCustomData(');
        IncIndent();
          Indent(); WriteLn('%s,',[sUNIT_NAME]);
          Indent(); WriteLn('%s,',[QuotedStr(ABinding.Intf.Name)]);
          Indent(); WriteLn('%s,',[QuotedStr('FORMAT_Style')]);
          Indent(); WriteLn('%s' ,[QuotedStr('rpc')]);
        DecIndent();
      Indent();WriteLn(');');
    end else if ( ABinding.BindingStyle = bsDocument ) then begin
      Indent();WriteLn('mm.SetServiceCustomData(');
        IncIndent();
          Indent(); WriteLn('%s,',[sUNIT_NAME]);
          Indent(); WriteLn('%s,',[QuotedStr(ABinding.Intf.Name)]);
          Indent(); WriteLn('%s,',[QuotedStr('FORMAT_Style')]);
          Indent(); WriteLn('%s' ,[QuotedStr('document')]);
        DecIndent();
      Indent();WriteLn(');');
    end;

    opList := ABinding.Intf.Members;
    for k := 0 to Pred(opList.Count) do begin
      elt := TPasElement(opList[k]);
      if elt.InheritsFrom(TPasProcedure) then begin
        WriteOperationDatas(ABinding.Intf,TPasProcedure(elt));
      end;
    end;
  end;
  
var
  i : Integer;
begin
  SetCurrentStream(FImpStream);
  IncIndent();
  
  NewLine();NewLine();
  WriteLn('procedure Register_%s_ServiceMetadata();',[SymbolTable.CurrentModule.Name]);
  WriteLn('var');
  Indent(); WriteLn('mm : IModuleMetadataMngr;');
  WriteLn('begin');
  Indent();WriteLn('mm := GetModuleMetadataMngr();');
  Indent();WriteLn('mm.SetRepositoryNameSpace(%s, %s);',[sUNIT_NAME,sNAME_SPACE]);
  for i := 0 to Pred(SymbolTable.BindingCount) do begin
    WriteServiceDatas(SymbolTable.Binding[i]);
  end;
  
  WriteLn('end;');
  DecIndent();
end;

function TInftGenerator.GetDestUnitName(): string;
begin
  Result := SymbolTable.CurrentModule.Name;
end;

procedure TInftGenerator.InternalExecute();

  procedure SortRecords(AList : TList);
  var
    j, k : PtrInt;
    ordr_ls, mbrLs, locLs : TList;
    locMemberType : TPasType;
    rec, locRec : TPasRecordType;
    locStack : TStack;
    locElt : TPasElement;
  begin
    if ( AList.Count > 0 ) then begin
      locStack := nil;
      locLs := nil;
      ordr_ls := TList.Create();
      try
        locStack := TStack.Create();
        locLs := TList.Create();
        for j := 0 to Pred(AList.Count) do begin
          rec := TPasRecordType(AList[j]);
          if ( ordr_ls.IndexOf(rec) = -1 ) then begin
            locStack.Push(rec);
            while locStack.AtLeast(1) do begin
              locLs.Clear();
              locRec := TPasRecordType(locStack.Pop());
              if ( ordr_ls.IndexOf(locRec) = -1 ) then begin
                mbrLs := locRec.Members;
                for k := 0 to Pred(mbrLs.Count) do begin
                  locMemberType := TPasVariable(mbrLs[k]).VarType;
                  if locMemberType.InheritsFrom(TPasUnresolvedTypeRef) then begin
                    locElt := SymbolTable.FindElement(SymbolTable.GetExternalName(locMemberType));
                    if Assigned(locElt) and locElt.InheritsFrom(TPasType) then begin
                      locMemberType := locElt as TPasType;
                    end;
                  end;
                  if locMemberType.InheritsFrom(TPasRecordType) then begin
                    if ( ordr_ls.IndexOf(locMemberType) = -1 ) then
                      locLs.Add(locMemberType);
                  end;
                end; //for
                if ( locLs.Count > 0 ) then begin
                  locStack.Push(locRec);
                  for k := 0 to Pred(locLs.Count) do begin
                    locStack.Push(locLs[k]);
                  end;
                end else begin
                  ordr_ls.Add(locRec);
                end;
              end;
            end;
          end;
        end;
        Assert(not locStack.AtLeast(1));
        AList.Clear();
        for k := 0 to Pred(ordr_ls.Count) do begin
          AList.Add(ordr_ls[k]);
        end;
      finally
        FreeAndNil(locLs);
        FreeAndNil(locStack);
        FreeAndNil(ordr_ls);
      end;
    end;
  end;
  
var
  i,c, j, k : PtrInt;
  clssTyp : TPasClassType;
  gnrClssLst : TObjectList;
  objLst : TObjectList;
  typeList : TList;
  elt : TPasElement;
  classAncestor : TPasElement;
  tmpList : TList;
  intfCount : PtrInt;
  locBinding : TwstBinding;
begin
  intfCount := 0;
  objLst := nil;
  tmpList := nil;
  gnrClssLst := TObjectList.Create(False);
  try
    GenerateUnitHeader();
    GenerateUnitImplementationHeader();
    typeList := SymbolTable.CurrentModule.InterfaceSection.Declarations;
    c := Pred(typeList.Count);

    SetCurrentStream(FDecStream);
    IncIndent();
    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasUnresolvedTypeRef) then begin
        WriteLn('// %s = unable to resolve this symbol.',[elt.Name]);
      end;
    end;
    DecIndent();
    
    IncIndent();
    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasType) and
         ( not elt.InheritsFrom(TPasAliasType) ) and
         ( ( SymbolTable.IsOfType(TPasType(elt),TPasClassType) and ( TPasClassType(GetUltimeType(TPasType(elt))).ObjKind = okClass ) ) or
           SymbolTable.IsOfType(TPasType(elt),TPasArrayType)
         )
      then begin
        Indent();
        WriteLn('%s = class;',[elt.Name]);
      end;
    end;
    DecIndent();

    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasEnumType) then begin
        GenerateEnum(TPasEnumType(elt));
      end;
    end;

    tmpList := TList.Create();
    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasRecordType) then begin
        tmpList.Add(elt);
      end;
    end;
    if ( tmpList.Count > 0 ) then begin
      SortRecords(tmpList);
      for i := 0 to Pred(tmpList.Count) do begin
        GenerateRecord(TPasRecordType(tmpList[i]));
      end;
    end;

    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasAliasType) then begin
        GenerateTypeAlias(TPasAliasType(elt));
      end;
    end;

    objLst := TObjectList.Create();
    objLst.OwnsObjects := False;
    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okClass ) then begin
        clssTyp := TPasClassType(elt);
        if ( gnrClssLst.IndexOf(clssTyp) = -1 ) then begin
          objLst.Clear();
          while Assigned(clssTyp) do begin
            objLst.Add(clssTyp);
            classAncestor := clssTyp.AncestorType;
            if Assigned(classAncestor) and classAncestor.InheritsFrom(TPasUnresolvedTypeRef) then begin
              classAncestor := SymbolTable.FindElement(SymbolTable.GetExternalName(classAncestor));
            end;
            if Assigned(classAncestor) and classAncestor.InheritsFrom(TPasClassType) then begin
              clssTyp := classAncestor as TPasClassType;
            end else begin
              clssTyp := nil;
            end;
          end;

          k := Pred(objLst.Count);
          for j := 0 to k do begin
            clssTyp := objLst[k-j] as TPasClassType;
            if ( gnrClssLst.IndexOf(clssTyp) = -1 ) then begin
              if ( FSymbolTable.CurrentModule.InterfaceSection.Declarations.IndexOf(clssTyp) <> -1 ) then begin
                GenerateClass(clssTyp);
                gnrClssLst.Add(clssTyp);
              end;
            end;
          end;
        end;
      end;
    end;

    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasArrayType) then begin
        GenerateArray(TPasArrayType(elt));
      end;
    end;

    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
        GenerateIntf(TPasClassType(elt));
        Inc(intfCount);
      end;
    end;

    if ( goDocumentWrappedParameter in Self.Options ) then begin
      c := FSymbolTable.BindingCount;
      if ( c > 0 ) then begin
        for i := 0 to ( c - 1 ) do begin
          locBinding := FSymbolTable.Binding[i];
          if ( locBinding.BindingStyle = bsDocument ) then begin
            clssTyp := DeduceEasyInterfaceForDocStyle(locBinding.Intf,FSymbolTable);
            try
              GenerateIntf(clssTyp);
            finally
              clssTyp.Release();
            end;
          end;
        end;
      end;
    end;
    
    if ( intfCount > 0 ) then begin
      SetCurrentStream(FDecStream);
      NewLine();
      IncIndent();
      Indent(); WriteLn('procedure Register_%s_ServiceMetadata();',[SymbolTable.CurrentModule.Name]);
      DecIndent();
      GenerateCustomMetadatas();
    end;

    FImpLastStream.NewLine();
    GenerateUnitImplementationFooter();
    FSrcMngr.Merge(GetDestUnitName() + '.pas',[FDecStream,FImpStream,FRttiFunc,FImpTempStream,FImpLastStream]);
    FDecStream := nil;
    FImpStream := nil;
    FImpTempStream := nil;
  finally
    FreeAndNil(tmpList);
    FreeAndNil(objLst);
    FreeAndNil(gnrClssLst);
  end;
end;

procedure TInftGenerator.PrepareModule();
begin
  FDecStream := SrcMngr.CreateItem(GetDestUnitName() + '.dec');
  FImpStream := SrcMngr.CreateItem(GetDestUnitName() + '.imp');
  FImpTempStream := SrcMngr.CreateItem(GetDestUnitName() + '.tmp_imp');
  FImpLastStream := SrcMngr.CreateItem(GetDestUnitName() + '.tmp_imp_last');
  FRttiFunc := SrcMngr.CreateItem(GetDestUnitName() + '.tmp_rtti_func');
  FImpTempStream.IncIndent();
  FImpLastStream.IncIndent();
end;

procedure TInftGenerator.Execute();
var
  oldCurrent, mdl : TPasModule;
  i : PtrInt;
  mdlList : TList;
begin
  oldCurrent := SymbolTable.CurrentModule;
  try
    mdlList := SymbolTable.Package.Modules;
    for i := 0 to Pred(mdlList.Count) do begin
      mdl := TPasModule(mdlList[i]);
      if not mdl.InheritsFrom(TPasNativeModule) then begin
        SymbolTable.SetCurrentModule(mdl);
        PrepareModule();
        InternalExecute();
      end;
    end;
  finally
    SymbolTable.SetCurrentModule(oldCurrent);
  end;
end;

end.
