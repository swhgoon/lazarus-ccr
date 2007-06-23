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


unit generator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  parserdefs, source_utils;
  
const
  sWST_EXTENSION = 'wst';
  
type

  { TBaseGenerator }

  TBaseGenerator = class
  Private
    FSrcMngr  : ISourceManager;
    FCurrentStream : ISourceStream;
    FSymbolTable: TSymbolTable;
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
    
    function ExtractserviceName(AIntf : TInterfaceDefinition):String;
  Public
    constructor Create(
      ASymTable : TSymbolTable;
      ASrcMngr  : ISourceManager
    );
    procedure Execute();virtual;abstract;
    property SymbolTable : TSymbolTable Read FSymbolTable;
    property SrcMngr : ISourceManager Read FSrcMngr;
  End;

  { TProxyGenerator }

  TProxyGenerator = class(TBaseGenerator)
  Private
    FDecStream : ISourceStream;
    FDecProcStream : ISourceStream;
    FImpStream : ISourceStream;

    function GenerateClassName(AIntf : TInterfaceDefinition):String;
    
    procedure GenerateUnitHeader();
    procedure GenerateUnitImplementationHeader();
    procedure GenerateUnitImplementationFooter();

    procedure GenerateProxyIntf(AIntf : TInterfaceDefinition);
    procedure GenerateProxyImp(AIntf : TInterfaceDefinition);
    
    function GetDestUnitName():string;
  Public
    constructor Create(
      ASymTable : TSymbolTable;
      ASrcMngr  : ISourceManager
    );
    procedure Execute();override;
  End;

  { TStubGenerator }

  TBinderGenerator = class(TBaseGenerator)
  Private
    FDecStream : ISourceStream;
    FImpStream : ISourceStream;

    function GenerateClassName(AIntf : TInterfaceDefinition):String;

    procedure GenerateUnitHeader();
    procedure GenerateUnitImplementationHeader();
    procedure GenerateUnitImplementationFooter();

    procedure GenerateIntf(AIntf : TInterfaceDefinition);
    procedure GenerateImp(AIntf : TInterfaceDefinition);

    function GetDestUnitName():string;
  Public
    constructor Create(
      ASymTable : TSymbolTable;
      ASrcMngr  : ISourceManager
    );
    procedure Execute();override;
  End;

  { TImplementationGenerator }

  TImplementationGenerator = class(TBaseGenerator)
  Private
    FDecStream : ISourceStream;
    FImpStream : ISourceStream;

    function GenerateClassName(AIntf : TInterfaceDefinition):String;

    procedure GenerateUnitHeader();
    procedure GenerateUnitImplementationHeader();
    procedure GenerateUnitImplementationFooter();

    procedure GenerateIntf(AIntf : TInterfaceDefinition);
    procedure GenerateImp(AIntf : TInterfaceDefinition);

    function GetDestUnitName():string;
  Public
    constructor Create(
      ASymTable : TSymbolTable;
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
  private
    function GenerateIntfName(AIntf : TInterfaceDefinition):string;

    procedure GenerateUnitHeader();
    procedure GenerateUnitImplementationHeader();
    procedure GenerateUnitImplementationFooter();

    procedure GenerateIntf(AIntf : TInterfaceDefinition);
    procedure GenerateTypeAlias(ASymbol : TTypeAliasDefinition);
    procedure GenerateClass(ASymbol : TClassTypeDefinition);
    procedure GenerateEnum(ASymbol : TEnumTypeDefinition);
    procedure GenerateArray(ASymbol : TArrayDefinition);

    procedure GenerateCustomMetadatas();
    function GetDestUnitName():string;
  public
    constructor Create(
      ASymTable : TSymbolTable;
      ASrcMngr  : ISourceManager
    );
    procedure Execute();override;
  end;
  
  
  
implementation
uses parserutils, Contnrs;

Const sPROXY_BASE_CLASS = 'TBaseProxy';
      sBINDER_BASE_CLASS = 'TBaseServiceBinder';
      sIMP_BASE_CLASS = 'TBaseServiceImplementation';
      sSERIALIZER_CLASS  = 'IFormatterClient';
      //RETURN_PARAM_NAME = 'return';
      RETURN_VAL_NAME = 'returnVal';
      sNAME_SPACE = 'sNAME_SPACE';
      sUNIT_NAME = 'sUNIT_NAME';

      sPRM_NAME = 'strPrmName';
      sLOC_SERIALIZER = 'locSerializer';

{ TProxyGenerator }

function TProxyGenerator.GenerateClassName(AIntf: TInterfaceDefinition): String;
begin
  Result := ExtractserviceName(AIntf);
  Result := Format('T%s_Proxy',[Result]);
end;

procedure TProxyGenerator.GenerateUnitHeader();
begin
  SetCurrentStream(FDecStream);
  WriteLn('{');
  WriteLn('This unit has been produced by ws_helper.');
  WriteLn('  Input unit name : "%s".',[SymbolTable.Name]);
  WriteLn('  This unit name  : "%s".',[GetDestUnitName()]);
  WriteLn('  Date            : "%s".',[DateTimeToStr(Now())]);
  WriteLn('}');
  WriteLn('');
  WriteLn('Unit %s;',[GetDestUnitName()]);
  WriteLn('{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}');
  WriteLn('Interface');
  WriteLn('');
  WriteLn('Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, %s;',[SymbolTable.Name]);
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
  WriteLn('  {$i %s.%s}',[SymbolTable.Name,sWST_EXTENSION]);
  NewLine();
  s := Format('Register_%s_ServiceMetadata',[SymbolTable.Name]);
  WriteLn('  {$IF DECLARED(%s)}',[s]);
  WriteLn('  %s();',[s]);
  WriteLn('  {$IFEND}');
  WriteLn('End.');
end;

constructor TProxyGenerator.Create(
  ASymTable : TSymbolTable;
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
  intf : TInterfaceDefinition;
begin
  GenerateUnitHeader();
  GenerateUnitImplementationHeader();
  c := Pred(SymbolTable.Count);
  For i := 0 To c Do Begin
    If SymbolTable.Item[i] Is TInterfaceDefinition Then Begin
      intf := SymbolTable.Item[i] As TInterfaceDefinition;
      GenerateProxyIntf(intf);
      GenerateProxyImp(intf);
    End;
  End;
  GenerateUnitImplementationFooter();
  FSrcMngr.Merge(GetDestUnitName() + '.pas',[FDecStream,FDecProcStream,FImpStream]);
  FDecStream := Nil;
  FImpStream := Nil;
end;

function TProxyGenerator.GetDestUnitName(): string;
begin
  Result := Format('%s_proxy',[SymbolTable.Name]);
end;

procedure TProxyGenerator.GenerateProxyIntf(AIntf: TInterfaceDefinition);

  procedure WriteDec();
  begin
    Indent();
    WriteLn('%s=class(%s,%s)',[GenerateClassName(AIntf),sPROXY_BASE_CLASS,AIntf.Name]);
    FDecProcStream.IncIndent();
    try
      FDecProcStream.NewLine();
      FDecProcStream.Indent();
      FDecProcStream.WriteLn('Function wst_CreateInstance_%s(const AFormat : string = %s; const ATransport : string = %s):%s;',[AIntf.Name,QuotedStr('SOAP:'),QuotedStr('HTTP:'),AIntf.Name]);
    finally
      FDecProcStream.DecIndent();
    end;
  end;
  
  procedure WriteMethod(AMthd : TMethodDefinition);
  Var
    prmCnt,k : Integer;
    prm : TParameterDefinition;
  Begin
    Indent();
    prmCnt := AMthd.ParameterCount;
    If ( AMthd.MethodType = mtProcedure ) Then
      Write('procedure ')
    Else Begin
      Write('function ');
      Dec(prmCnt);
    End;
    Write('%s(',[AMthd.Name]);

    If ( prmCnt > 0 ) Then Begin
      IncIndent();
      For k := 0 To Pred(prmCnt) Do Begin
        prm := AMthd.Parameter[k];
        If (k > 0 ) Then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[ParameterModifierMAP[prm.Modifier],prm.Name,prm.DataType.Name]);
      End;
      DecIndent();
      NewLine();
      Indent();
    End;

    Write(')');
    If ( AMthd.MethodType = mtFunction ) Then Begin
      Write(':%s',[AMthd.Parameter[prmCnt].DataType.Name]);
    End;
    WriteLn(';');
  End;
  
  procedure WriteMethods();
  Var
    k : Integer;
  begin
    If ( AIntf.MethodCount = 0 ) Then
      Exit;
    //IncIndent();
      Indent();
      WriteLn('Protected');
      IncIndent();
        Indent();WriteLn('class function GetServiceType() : PTypeInfo;override;');
        For k := 0 To Pred(AIntf.MethodCount) Do
          WriteMethod(AIntf.Method[k]);
      DecIndent();
    //DecIndent();
  end;
  
begin
  SetCurrentStream(FDecStream);
  NewLine();
  IncIndent();
    WriteDec();
    WriteMethods();
    Indent(); WriteLn('End;');
  DecIndent();
end;

procedure TProxyGenerator.GenerateProxyImp(AIntf: TInterfaceDefinition);
Var
  strClassName : String;
  
  procedure WriteDec();
  begin
    NewLine();
    WriteLn('Function wst_CreateInstance_%s(const AFormat : string; const ATransport : string):%s;',[AIntf.Name,AIntf.Name]);
    WriteLn('Begin');
      IncIndent();
      try
        Indent();
        WriteLn(
          'Result := %s.Create(%s,AFormat+%s,ATransport + %s);',
          [ strClassName,QuotedStr(AIntf.Name),
            Format('GetServiceDefaultFormatProperties(TypeInfo(%s))',[AIntf.Name]),
            QuotedStr('address=') + Format(' + GetServiceDefaultAddress(TypeInfo(%s))',[AIntf.Name])
          ]
        );
      finally
        DecIndent();
      end;
    WriteLn('End;');
    NewLine();
    If ( AIntf.MethodCount > 0 ) Then
      WriteLn('{ %s implementation }',[strClassName]);
  end;

  procedure WriteMethodDec(AMthd : TMethodDefinition);
  Var
    prmCnt,k : Integer;
    prm : TParameterDefinition;
  Begin
    prmCnt := AMthd.ParameterCount;
    If ( AMthd.MethodType = mtProcedure ) Then
      Write('procedure ')
    Else Begin
      Write('function ');
      Dec(prmCnt);
    End;
    Write('%s.%s(',[strClassName,AMthd.Name]);

    If ( prmCnt > 0 ) Then Begin
      IncIndent();
      For k := 0 To Pred(prmCnt) Do Begin
        prm := AMthd.Parameter[k];
        If (k > 0 ) Then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[ParameterModifierMAP[prm.Modifier],prm.Name,prm.DataType.Name]);
      End;
      DecIndent();
      NewLine();
      Indent();
    End;

    Write(')');
    If ( AMthd.MethodType = mtFunction ) Then Begin
      Write(':%s',[AMthd.Parameter[prmCnt].DataType.Name]);
    End;
    WriteLn(';');
  End;

  procedure WriteMethodImp(AMthd : TMethodDefinition);
  Var
    prmCnt,k : Integer;
    prm : TParameterDefinition;
  Begin
    IncIndent();
    WriteLn('Var');

      Indent();WriteLn('%s : %s;',[sLOC_SERIALIZER,sSERIALIZER_CLASS]);
      Indent();WriteLn('%s : %s;',[sPRM_NAME,'string']);

    WriteLn('Begin');
    
      Indent();WriteLn('%s := GetSerializer();',[sLOC_SERIALIZER]);
      Indent();WriteLn('Try');IncIndent();

      Indent();WriteLn('%s.BeginCall(''%s'', GetTarget(),(Self as ICallContext));',[sLOC_SERIALIZER,AMthd.ExternalName]);
      IncIndent();
        prmCnt := AMthd.ParameterCount;
        If ( AMthd.MethodType = mtFunction ) Then
          Dec(prmCnt);
        For k := 0 To Pred(prmCnt) Do Begin
          prm := AMthd.Parameter[k];
          If ( prm.Modifier <> pmOut ) Then Begin
            Indent();WriteLn('%s.Put(%s, TypeInfo(%s), %s);',[sLOC_SERIALIZER,QuotedStr(prm.ExternalName),prm.DataType.Name,prm.Name]);
          End;
        End;
      DecIndent();
      Indent();WriteLn('%s.EndCall();',[sLOC_SERIALIZER]);
      
      WriteLn('');
      Indent();WriteLn('MakeCall();');
      WriteLn('');
      
      Indent();WriteLn('%s.BeginCallRead((Self as ICallContext));',[sLOC_SERIALIZER]);
      IncIndent();
        k:= Pred(AMthd.ParameterCount);
        If ( AMthd.MethodType = mtFunction ) Then Begin
          prm := AMthd.Parameter[k];
          //Indent();WriteLn('%s := TypeInfo(%s);',[sRES_TYPE_INFO,prm.DataType.Name]);
          if prm.DataType.NeedFinalization() then begin
            if prm.DataType.InheritsFrom(TClassTypeDefinition) or
               prm.DataType.InheritsFrom(TArrayDefinition)
            then begin
              Indent();WriteLn('TObject(Result) := Nil;');
            end else begin
              Indent();WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkInterface] ) Then',[prm.DataType.Name]);
              IncIndent();
                Indent();WriteLn('Pointer(Result) := Nil;');
              DecIndent();
            end;
          end;
          Indent();WriteLn('%s := %s;',[sPRM_NAME,QuotedStr(prm.ExternalName)]);//Indent();WriteLn('%s := %s;',[sPRM_NAME,QuotedStr(RETURN_PARAM_NAME)]);
          Indent();WriteLn('%s.Get(TypeInfo(%s), %s, %s);',[sLOC_SERIALIZER,prm.DataType.Name,sPRM_NAME,'Result']);
        End;
        //--------------------------------
        for k := 0 to Pred(prmCnt) do begin
          prm := AMthd.Parameter[k];
          if ( prm.Modifier = pmOut ) then begin
            if prm.DataType.NeedFinalization() then begin
              if prm.DataType.InheritsFrom(TClassTypeDefinition) or
                 prm.DataType.InheritsFrom(TArrayDefinition)
              then begin
                Indent();WriteLn('TObject(%s) := Nil;',[prm.Name]);
              end else begin
                Indent();WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkInterface] ) Then',[prm.DataType.Name]);
                IncIndent();
                  Indent();WriteLn('Pointer(%s) := Nil;',[prm.Name]);
                DecIndent();
              end;
            end;
          end;
        end;
        //--------------------------------

        For k := 0 To Pred(prmCnt) Do Begin
          prm := AMthd.Parameter[k];
          If ( prm.Modifier In [pmVar, pmOut] ) Then Begin
            Indent();WriteLn('%s := %s;',[sPRM_NAME,QuotedStr(prm.ExternalName)]);
            Indent();WriteLn('%s.Get(TypeInfo(%s), %s, %s);',[sLOC_SERIALIZER,prm.DataType.Name,sPRM_NAME,prm.Name]);
          End;
        End;
      DecIndent();

      
      WriteLn('');
      DecIndent();
      Indent();WriteLn('Finally');
        IncIndent();
          Indent();WriteLn('%s.Clear();',[sLOC_SERIALIZER]);
        DecIndent();
      Indent();WriteLn('End;');DecIndent();
      
    WriteLn('End;');
  End;

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
  Var
    k : Integer;
  begin
    WriteTypeInfoMethod();
    For k := 0 To Pred(AIntf.MethodCount) Do Begin
      WriteMethodDec(AIntf.Method[k]);
      WriteMethodImp(AIntf.Method[k]);
      WriteLn('');
    End;
  end;
  
begin
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

function TBaseGenerator.ExtractserviceName(AIntf: TInterfaceDefinition): String;
begin
  Result := AIntf.Name;
  If upCase(Result[1]) = 'I' Then
    Delete(Result,1,1);
end;

constructor TBaseGenerator.Create(ASymTable: TSymbolTable; ASrcMngr: ISourceManager);
begin
  Assert(Assigned(ASymTable));
  Assert(Assigned(ASrcMngr));
  FSrcMngr :=ASrcMngr;
  FCurrentStream := Nil;
  FSymbolTable := ASymTable;
end;

{ TBinderGenerator }

function TBinderGenerator.GenerateClassName(AIntf: TInterfaceDefinition): String;
begin
  Result := ExtractserviceName(AIntf);
  Result := Format('T%s_ServiceBinder',[Result]);
end;

procedure TBinderGenerator.GenerateUnitHeader();
begin
  SetCurrentStream(FDecStream);
  WriteLn('{');
  WriteLn('This unit has been produced by ws_helper.');
  WriteLn('  Input unit name : "%s".',[SymbolTable.Name]);
  WriteLn('  This unit name  : "%s".',[GetDestUnitName()]);
  WriteLn('  Date            : "%s".',[DateTimeToStr(Now())]);
  WriteLn('}');

  WriteLn('unit %s;',[GetDestUnitName()]);
  WriteLn('{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}');
  WriteLn('interface');
  WriteLn('');
  WriteLn('uses SysUtils, Classes, base_service_intf, server_service_intf, %s;',[SymbolTable.Name]);
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
  s := Format('Register_%s_NameSpace',[SymbolTable.Name]);
  WriteLn('  {$IF DECLARED(%s)}',[s]);
  WriteLn('  %s();',[s]);
  WriteLn('  {$ENDIF}');
  NewLine();
  WriteLn('  {$i %s.%s}',[SymbolTable.Name,sWST_EXTENSION]);
  NewLine();
  WriteLn('End.');
end;

procedure TBinderGenerator.GenerateIntf(AIntf: TInterfaceDefinition);
  procedure WriteDec();
  begin
    Indent();
    WriteLn('%s=class(%s)',[GenerateClassName(AIntf),sBINDER_BASE_CLASS]);
  end;

  procedure WriteConstructor();
  Begin
    Indent();
      WriteLn('constructor Create();')
  End;

  procedure WriteMethod(AMthd : TMethodDefinition);
  Begin
    Indent();
      WriteLn('procedure %sHandler(AFormatter:IFormatterResponse);',[AMthd.Name])
  End;

  procedure WriteMethods();
  Var
    k : Integer;
  begin
    If ( AIntf.MethodCount = 0 ) Then
      Exit;
      Indent();WriteLn('Protected');
      IncIndent();
        For k := 0 To Pred(AIntf.MethodCount) Do
          WriteMethod(AIntf.Method[k]);
      DecIndent();
      
      Indent();WriteLn('Public');
        Indent();WriteConstructor();
  end;
  
  procedure GenerateFactoryClass();
  Begin
    NewLine();
    IncIndent();BeginAutoIndent();
      WriteLn('T%s_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)',[ExtractserviceName(AIntf)]);
      WriteLn('protected');

      IncIndent();
        WriteLn('function CreateInstance():IInterface;');
      DecIndent();
      WriteLn('End;');
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
    Indent();WriteLn('End;');
  DecIndent();
  
  GenerateFactoryClass();
  GenerateRegistrationProc();
end;

procedure TBinderGenerator.GenerateImp(AIntf: TInterfaceDefinition);
Var
  strClassName : String;

  procedure WriteDec();
  begin
    If ( AIntf.MethodCount > 0 ) Then
      WriteLn('{ %s implementation }',[strClassName]);
  end;

  procedure WriteMethodDec(AMthd : TMethodDefinition);
  Begin
    WriteLn('procedure %s.%sHandler(AFormatter:IFormatterResponse);',[strClassName,AMthd.Name]);
  End;
  
  procedure WriteMethodImp(AMthd : TMethodDefinition);
  Var
    prmCnt,k : Integer;
    prm : TParameterDefinition;
    strBuff : string;
  Begin
    prmCnt := AMthd.ParameterCount;
    If ( AMthd.MethodType = mtFunction ) Then
      Dec(prmCnt);

    WriteLn('Var');
    IncIndent();BeginAutoIndent();
      WriteLn('cllCntrl : ICallControl;');
      WriteLn('tmpObj : %s;',[AIntf.Name]);
      WriteLn('callCtx : ICallContext;');
      If ( prmCnt > 0 ) Or ( AMthd.MethodType = mtFunction ) Then Begin
        WriteLn('%s : string;',[sPRM_NAME]);
        WriteLn('procName,trgName : string;');
      End;
      If ( prmCnt > 0 ) Then Begin
        For k := 0 To Pred(prmCnt) Do Begin
          prm := AMthd.Parameter[k];
          WriteLn('%s : %s;',[prm.Name,prm.DataType.Name]);
        End;
      End;
      If ( AMthd.MethodType = mtFunction ) Then Begin
        WriteLn('%s : %s;',[RETURN_VAL_NAME,AMthd.Parameter[prmCnt].DataType.Name]);
        //WriteLn('%s : %s;',[sLOC_TYPE_INFO,'PTypeInfo']);
      End;
    DecIndent();EndAutoIndent();
    
    WriteLn('Begin');
    IncIndent();BeginAutoIndent();

      WriteLn('callCtx := GetCallContext();');
      If ( AMthd.MethodType = mtFunction ) Then Begin
        prm := AMthd.Parameter[prmCnt];
        If prm.DataType.NeedFinalization() Then Begin
          if prm.DataType.InheritsFrom(TClassTypeDefinition) then begin
            WriteLn('TObject(%s) := Nil;',[RETURN_VAL_NAME]);
          end else begin
            WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkInterface] ) Then',[prm.DataType.Name]);
            IncIndent();
              WriteLn('Pointer(%s) := Nil;',[RETURN_VAL_NAME]);
            DecIndent();
          end;
        End;
      End;

      For k := 0 To Pred(prmCnt) Do Begin
        prm := AMthd.Parameter[k];
        If prm.DataType.NeedFinalization() Then Begin
          if prm.DataType.InheritsFrom(TClassTypeDefinition) then begin
            WriteLn('TObject(%s) := Nil;',[prm.Name]);
          end else begin
            WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkObject,tkInterface] ) Then',[prm.DataType.Name]);
            IncIndent();
              WriteLn('Pointer(%s) := Nil;',[prm.Name]);
            DecIndent();
          end;
        End;
      End;

      NewLine();
      For k := 0 To Pred(prmCnt) Do Begin
        prm := AMthd.Parameter[k];
        Write('%s := %s;',[sPRM_NAME,QuotedStr(prm.ExternalName)]);
          WriteLn('AFormatter.Get(TypeInfo(%s),%s,%s);',[prm.DataType.Name,sPRM_NAME,prm.Name]);
        If prm.DataType.NeedFinalization() Then Begin
          if prm.DataType.InheritsFrom(TClassTypeDefinition) then begin
            WriteLn('If Assigned(Pointer(%s)) Then',[prm.Name]);
            IncIndent();
              WriteLn('callCtx.AddObjectToFree(TObject(%s));',[prm.Name]);
            DecIndent();
          end else begin
            WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind = tkClass ) And Assigned(Pointer(%s)) Then',[prm.DataType.Name,prm.Name]);
            IncIndent();
              WriteLn('callCtx.AddObjectToFree(TObject(%s));',[prm.Name]);
            DecIndent();
          end;
        End;
      End;

      NewLine();
      WriteLn('tmpObj := Self.GetFactory().CreateInstance() as %s;',[AIntf.Name]);
      WriteLn('if Supports(tmpObj,ICallControl,cllCntrl) then');
      Indent();WriteLn('cllCntrl.SetCallContext(GetCallContext());');
      NewLine();

      If ( AMthd.MethodType = mtFunction ) Then
        Write('%s := tmpObj.%s(',[RETURN_VAL_NAME,AMthd.Name])
      Else
        Write('tmpObj.%s(',[AMthd.Name]);
      strBuff := '';
      For k := 0 To Pred(prmCnt) Do Begin
        prm := AMthd.Parameter[k];
        strBuff := strBuff + Format('%s,',[prm.Name]);
      End;
      If ( prmCnt > 0 ) Then
        Delete(strBuff,Length(strBuff),1);
      strBuff := strBuff + ');';
      EndAutoIndent();
        WriteLn(strBuff);
      BeginAutoIndent();

      If ( AMthd.MethodType = mtFunction ) Then Begin
        prm := AMthd.Parameter[prmCnt];
        If prm.DataType.NeedFinalization() Then Begin
          if prm.DataType.InheritsFrom(TClassTypeDefinition) then
            WriteLn('If Assigned(TObject(%s)) Then',[RETURN_VAL_NAME])
          else
            WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind = tkClass ) And Assigned(Pointer(%s)) Then',[prm.DataType.Name,RETURN_VAL_NAME]);
          IncIndent();
            WriteLn('callCtx.AddObjectToFree(TObject(%s));',[RETURN_VAL_NAME]);
          DecIndent();
        End;
      End;
      NewLine();

      WriteLn('procName := AFormatter.GetCallProcedureName();');
      WriteLn('trgName := AFormatter.GetCallTarget();');
      WriteLn('AFormatter.Clear();');

      WriteLn('AFormatter.BeginCallResponse(procName,trgName);');
        IncIndent();
        if ( AMthd.MethodType = mtFunction ) then begin
          //WriteLn('AFormatter.Put(%s,TypeInfo(%s),%s);',[QuotedStr(RETURN_PARAM_NAME),AMthd.Parameter[prmCnt].DataType.Name,RETURN_VAL_NAME]);
          WriteLn('AFormatter.Put(%s,TypeInfo(%s),%s);',[QuotedStr(prm.ExternalName),AMthd.Parameter[prmCnt].DataType.Name,RETURN_VAL_NAME]);
        end;
        For k := 0 To Pred(prmCnt) Do Begin
          prm := AMthd.Parameter[k];
          If ( prm.Modifier In [pmOut,pmVar] ) Then
            WriteLn('AFormatter.Put(%s,TypeInfo(%s),%s);',[QuotedStr(prm.ExternalName),prm.DataType.Name,prm.Name]);
        End;
        DecIndent();
      WriteLn('AFormatter.EndCallResponse();');
      NewLine();
      WriteLn('callCtx := Nil;');

    DecIndent();EndAutoIndent();
    WriteLn('End;');
  End;

  procedure WriteConstructor();
  Var
    k : Integer;
    mtd : TMethodDefinition;
  Begin
    NewLine();
    WriteLn('constructor %s.Create();',[strClassName]);
    WriteLn('Begin');
    IncIndent();
    BeginAutoIndent();
      WriteLn('Inherited Create(GetServiceImplementationRegistry().FindFactory(%s));',[QuotedStr(AIntf.Name)]);
      For k := 0 To Pred(AIntf.MethodCount) Do Begin
        mtd := AIntf.Method[k];
        WriteLn('RegisterVerbHandler(%s,@%sHandler);',[QuotedStr(mtd.Name),mtd.Name]);
      End;
    EndAutoIndent();
    DecIndent();
    WriteLn('End;');
    NewLine();
  End;

  procedure WriteMethods();
  Var
    k : Integer;
  begin
    For k := 0 To Pred(AIntf.MethodCount) Do Begin
      WriteMethodDec(AIntf.Method[k]);
      WriteMethodImp(AIntf.Method[k]);
      WriteLn('');
    End;
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
      WriteLn('function %s.CreateInstance():IInterface;',[strBuff]);
      WriteLn('Begin');
        IncIndent();
          WriteLn('Result := %s.Create() as IInterface;',[strClassName]);
        DecIndent();
      WriteLn('End;');
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
  Result := Format('%s_binder',[SymbolTable.Name]);
end;

constructor TBinderGenerator.Create(ASymTable: TSymbolTable;ASrcMngr: ISourceManager);
begin
  Inherited Create(ASymTable,ASrcMngr);
  FDecStream := SrcMngr.CreateItem(GetDestUnitName() + '.dec');
  FImpStream := SrcMngr.CreateItem(GetDestUnitName() + '.imp');
end;

procedure TBinderGenerator.Execute();
Var
  i,c : Integer;
  intf : TInterfaceDefinition;
begin
  GenerateUnitHeader();
  GenerateUnitImplementationHeader();
  c := Pred(SymbolTable.Count);
  For i := 0 To c Do Begin
    If SymbolTable.Item[i] Is TInterfaceDefinition Then Begin
      intf := SymbolTable.Item[i] As TInterfaceDefinition;
      GenerateIntf(intf);
      GenerateImp(intf);
    End;
  End;
  GenerateUnitImplementationFooter();
  FSrcMngr.Merge(GetDestUnitName() + '.pas',[FDecStream,FImpStream]);
  FDecStream := Nil;
  FImpStream := Nil;
end;

{ TImplementationGenerator }

function TImplementationGenerator.GenerateClassName(AIntf: TInterfaceDefinition): String;
begin
  Result := ExtractserviceName(AIntf);
  Result := Format('T%s_ServiceImp',[Result]);
end;

procedure TImplementationGenerator.GenerateUnitHeader();
begin
  SetCurrentStream(FDecStream);
  WriteLn('{');
  WriteLn('This unit has been produced by ws_helper.');
  WriteLn('  Input unit name : "%s".',[SymbolTable.Name]);
  WriteLn('  This unit name  : "%s".',[GetDestUnitName()]);
  WriteLn('  Date            : "%s".',[DateTimeToStr(Now())]);
  WriteLn('}');

  WriteLn('Unit %s;',[GetDestUnitName()]);
  WriteLn('{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}');
  WriteLn('Interface');
  WriteLn('');
  WriteLn('Uses SysUtils, Classes, ');
  WriteLn('     base_service_intf, server_service_intf, server_service_imputils, %s;',[SymbolTable.Name]);
  WriteLn('');
  WriteLn('Type');
  WriteLn('');
end;

procedure TImplementationGenerator.GenerateUnitImplementationHeader();
begin
  SetCurrentStream(FImpStream);
  WriteLn('');
  WriteLn('Implementation');
end;

procedure TImplementationGenerator.GenerateUnitImplementationFooter();
begin
  NewLine();
  WriteLn('End.');
end;

procedure TImplementationGenerator.GenerateIntf(AIntf: TInterfaceDefinition);
  procedure WriteDec();
  begin
    Indent();
    WriteLn('%s=class(%s,%s)',[GenerateClassName(AIntf),sIMP_BASE_CLASS,AIntf.Name]);
  end;

  procedure WriteMethod(AMthd : TMethodDefinition);
  Var
    prmCnt,k : Integer;
    prm : TParameterDefinition;
  Begin
    Indent();
    prmCnt := AMthd.ParameterCount;
    If ( AMthd.MethodType = mtProcedure ) Then
      Write('procedure ')
    Else Begin
      Write('function ');
      Dec(prmCnt);
    End;
    Write('%s(',[AMthd.Name]);

    If ( prmCnt > 0 ) Then Begin
      IncIndent();
      For k := 0 To Pred(prmCnt) Do Begin
        prm := AMthd.Parameter[k];
        If (k > 0 ) Then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[ParameterModifierMAP[prm.Modifier],prm.Name,prm.DataType.Name]);
      End;
      DecIndent();
      NewLine();
      Indent();
    End;

    Write(')');
    If ( AMthd.MethodType = mtFunction ) Then Begin
      Write(':%s',[AMthd.Parameter[prmCnt].DataType.Name]);
    End;
    WriteLn(';');
  End;

  procedure WriteMethods();
  Var
    k : Integer;
  begin
    If ( AIntf.MethodCount = 0 ) Then
      Exit;
    Indent();WriteLn('Protected');
    IncIndent();
      For k := 0 To Pred(AIntf.MethodCount) Do
        WriteMethod(AIntf.Method[k]);
    DecIndent();
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

procedure TImplementationGenerator.GenerateImp(AIntf: TInterfaceDefinition);
Var
  strClassName : String;

  procedure WriteDec();
  begin
    If ( AIntf.MethodCount > 0 ) Then
      WriteLn('{ %s implementation }',[strClassName]);
  end;

  procedure WriteMethodDec(AMthd : TMethodDefinition);
  Var
    prmCnt,k : Integer;
    prm : TParameterDefinition;
  Begin
    prmCnt := AMthd.ParameterCount;
    If ( AMthd.MethodType = mtProcedure ) Then
      Write('procedure ')
    Else Begin
      Write('function ');
      Dec(prmCnt);
    End;
    Write('%s.%s(',[strClassName,AMthd.Name]);

    If ( prmCnt > 0 ) Then Begin
      IncIndent();
      For k := 0 To Pred(prmCnt) Do Begin
        prm := AMthd.Parameter[k];
        If (k > 0 ) Then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[ParameterModifierMAP[prm.Modifier],prm.Name,prm.DataType.Name]);
      End;
      DecIndent();
      NewLine();
      Indent();
    End;

    Write(')');
    If ( AMthd.MethodType = mtFunction ) Then Begin
      Write(':%s',[AMthd.Parameter[prmCnt].DataType.Name]);
    End;
    WriteLn(';');
  End;

  procedure WriteMethodImp(AMthd : TMethodDefinition);
  Begin
    WriteLn('Begin');
    WriteLn('// your code here');
    WriteLn('End;');
  End;

  procedure WriteMethods();
  Var
    k : Integer;
  begin
    For k := 0 To Pred(AIntf.MethodCount) Do Begin
      WriteMethodDec(AIntf.Method[k]);
      WriteMethodImp(AIntf.Method[k]);
      WriteLn('');
    End;
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
          WriteLn('GetServiceImplementationRegistry().Register(%s,TImplementationFactory.Create(%s) as IServiceImplementationFactory);',[QuotedStr(AIntf.Name),strClassName]);
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
  Result := Format('%s_imp',[SymbolTable.Name]);
end;

constructor TImplementationGenerator.Create(ASymTable: TSymbolTable;ASrcMngr: ISourceManager);
begin
  Inherited Create(ASymTable,ASrcMngr);
  FDecStream := SrcMngr.CreateItem(GetDestUnitName() + '.dec');
  FImpStream := SrcMngr.CreateItem(GetDestUnitName() + '.imp');
end;

procedure TImplementationGenerator.Execute();
Var
  i,c : Integer;
  intf : TInterfaceDefinition;
begin
  GenerateUnitHeader();
  GenerateUnitImplementationHeader();
  c := Pred(SymbolTable.Count);
  For i := 0 To c Do Begin
    If SymbolTable.Item[i] Is TInterfaceDefinition Then Begin
      intf := SymbolTable.Item[i] As TInterfaceDefinition;
      GenerateIntf(intf);
      GenerateImp(intf);
    End;
  End;
  GenerateUnitImplementationFooter();
  FSrcMngr.Merge(GetDestUnitName() + '.pas',[FDecStream,FImpStream]);
  FDecStream := Nil;
  FImpStream := Nil;
end;

{ TInftGenerator }

function TInftGenerator.GenerateIntfName(AIntf: TInterfaceDefinition): string;
begin
  Result := ExtractserviceName(AIntf);
end;

procedure TInftGenerator.GenerateUnitHeader();
begin
  SetCurrentStream(FDecStream);
  WriteLn('{');
  WriteLn('This unit has been produced by ws_helper.');
  WriteLn('  Input unit name : "%s".',[SymbolTable.Name]);
  WriteLn('  This unit name  : "%s".',[GetDestUnitName()]);
  WriteLn('  Date            : "%s".',[DateTimeToStr(Now())]);
  WriteLn('}');

  WriteLn('unit %s;',[GetDestUnitName()]);
  WriteLn('{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}');
  WriteLn('interface');
  WriteLn('');
  WriteLn('uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;');
  WriteLn('');
  WriteLn('const');

  IncIndent();
  Indent();WriteLn('sNAME_SPACE = %s;',[QuotedStr(FSymbolTable.ExternalName)]);
  Indent();WriteLn('sUNIT_NAME = %s;',[QuotedStr(FSymbolTable.Name)]);
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
  WriteLn('uses metadata_repository;');
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

procedure TInftGenerator.GenerateIntf(AIntf: TInterfaceDefinition);

  procedure WriteDec();
  begin
    Indent();
    WriteLn('%s = interface(IInvokable)',[GenerateIntfName(AIntf)]);
    if not IsStrEmpty(AIntf.InterfaceGUID) then begin
      Indent();Indent();WriteLn('[%s]',[QuotedStr(AIntf.InterfaceGUID)]);
    end;
  end;

  procedure WriteMethod(AMthd : TMethodDefinition);
  Var
    prmCnt,k : Integer;
    prm : TParameterDefinition;
  Begin
    Indent();
    prmCnt := AMthd.ParameterCount;
    If ( AMthd.MethodType = mtProcedure ) Then
      Write('procedure ')
    Else Begin
      Write('function ');
      Dec(prmCnt);
    End;
    Write('%s(',[AMthd.Name]);

    If ( prmCnt > 0 ) Then Begin
      IncIndent();
      For k := 0 To Pred(prmCnt) Do Begin
        prm := AMthd.Parameter[k];
        If (k > 0 ) Then
          Write('; ');
        NewLine();
        Indent();
        Write('%s %s : %s',[ParameterModifierMAP[prm.Modifier],prm.Name,prm.DataType.Name]);
      End;
      DecIndent();
      NewLine();
      Indent();
    End;

    Write(')');
    If ( AMthd.MethodType = mtFunction ) Then Begin
      Write(':%s',[AMthd.Parameter[prmCnt].DataType.Name]);
    End;
    WriteLn(';');
  End;

  procedure WriteMethods();
  Var
    k : Integer;
  begin
    If ( AIntf.MethodCount = 0 ) Then
      Exit;
      IncIndent();
        For k := 0 To Pred(AIntf.MethodCount) Do
          WriteMethod(AIntf.Method[k]);
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

procedure TInftGenerator.GenerateTypeAlias(ASymbol: TTypeAliasDefinition);
begin
  try
    SetCurrentStream(FDecStream);
    NewLine();
    IncIndent();
      Indent();
      WriteLn('%s = type %s;',[ASymbol.Name,ASymbol.BaseType.Name]);
    DecIndent();
  except
    on e : Exception do
      System.WriteLn('TInftGenerator.GenerateTypeAlias()=', ASymbol.Name, ' ;; ', e.Message);
  end;
end;

procedure TInftGenerator.GenerateClass(ASymbol: TClassTypeDefinition);
var
  locClassPropNbr, locStoredPropsNbr, locArrayPropsNbr : Integer;
  loc_BaseComplexSimpleContentRemotable : TClassTypeDefinition;
  
  procedure Prepare();
  var
    k : Integer;
    p : TPropertyDefinition;
  begin
    locClassPropNbr   := 0;
    locStoredPropsNbr := 0;
    locArrayPropsNbr  := 0;
    for k := 0 to Pred(ASymbol.PropertyCount) do begin
      p := ASymbol.Properties[k];
      if ( p.StorageOption = soOptional ) then
        Inc(locStoredPropsNbr);
      if p.DataType.InheritsFrom(TClassTypeDefinition) then
        Inc(locClassPropNbr);
      if p.DataType.InheritsFrom(TArrayDefinition) then
        Inc(locArrayPropsNbr);
    end;
    locClassPropNbr := locClassPropNbr + locArrayPropsNbr;
  end;
  
  procedure WriteDec();
  var
    s : string;
  begin
    if Assigned(ASymbol.Parent) then begin
      {if ASymbol.Parent.InheritsFrom(TNativeSimpleTypeDefinition) and
         Assigned(TNativeSimpleTypeDefinition(ASymbol.Parent).BoxedType)
      then begin
        s := Format('%s',[TNativeSimpleTypeDefinition(ASymbol.Parent).BoxedType.Name]);
      end else begin
        s := Format('%s',[ASymbol.Parent.Name]);
      end;}
      s := Format('%s',[ASymbol.Parent.Name]);
    end else begin
      s := 'XX';//'TBaseComplexRemotable';
    end;
    Indent();
    WriteLn('%s = class(%s)',[ASymbol.Name,s]);
  end;

  procedure WritePropertyField(AProp : TPropertyDefinition);
  begin
    Indent();
    WriteLn('F%s : %s;',[AProp.Name,AProp.DataType.Name]);
  End;

  procedure WriteProperty(AProp : TPropertyDefinition);
  var
    propName, locStore : string;
  begin
    propName := AProp.Name;
    case AProp.StorageOption of
      soAlways     : locStore := '';
      soNever      : locStore := ' stored False';
      soOptional   : locStore := Format(' stored Has%s',[AProp.Name]);
    end;
    Indent();
    WriteLn('property %s : %s read F%s write F%s%s;',[propName,AProp.DataType.Name,propName,propName,locStore]);
    if not AnsiSameText(AProp.Name,AProp.ExternalName) then begin
      FImpLastStream.Indent();
      FImpLastStream.WriteLn('GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)].RegisterExternalPropertyName(%s,%s);',[ASymbol.Name,QuotedStr(AProp.Name),QuotedStr(AProp.ExternalName)]);
    end;
    if AProp.IsAttribute then begin
      FImpLastStream.Indent();
      FImpLastStream.WriteLn('%s.RegisterAttributeProperty(%s);',[ASymbol.Name,QuotedStr(AProp.Name)]);
    end;
  end;

  procedure WriteProperties();
  Var
    k : Integer;
    p : TPropertyDefinition;
  begin
    If ( ASymbol.PropertyCount > 0 ) Then begin
      Indent();
      WriteLn('private');
      IncIndent();
        for k := 0 to Pred(ASymbol.PropertyCount) do begin
          p := ASymbol.Properties[k];
          WritePropertyField(p);
        end;
      DecIndent();
      //
      if ( locStoredPropsNbr > 0 ) then begin
        Indent();
        WriteLn('private');
        IncIndent();
          for k := 0 to Pred(ASymbol.PropertyCount) do begin
            p := ASymbol.Properties[k];
            if ( p.StorageOption = soOptional ) then begin
              Indent();
              WriteLn('function Has%s() : Boolean;',[p.Name]);
            end;
          end;
        DecIndent();
      end;
      //
      if ( locArrayPropsNbr > 0 ) or ( locClassPropNbr > 0 ) then begin
        Indent();
        WriteLn('public');
      end;
      if ( locArrayPropsNbr > 0 ) then begin
        IncIndent();
          Indent(); WriteLn('constructor Create();override;');
        DecIndent();
      end;

      if ( locClassPropNbr > 0 ) then begin
        IncIndent();
          Indent(); WriteLn('destructor Destroy();override;');
        DecIndent();
      end;
      //
      Indent();
      WriteLn('published');
      IncIndent();
        For k := 0 To Pred(ASymbol.PropertyCount) Do
          WriteProperty(ASymbol.Properties[k]);
      DecIndent();
    end;
  end;

  procedure WriteImp();
  var
    k : Integer;
    p : TPropertyDefinition;
  begin
    if ( locClassPropNbr > 0 ) or ( locStoredPropsNbr > 0 ) then begin
      NewLine();
      WriteLn('{ %s }',[ASymbol.Name]);
      
      if ( locArrayPropsNbr > 0 ) then begin
        NewLine();
        WriteLn('constructor %s.Create();',[ASymbol.Name]);
        WriteLn('begin');
        IncIndent();
          Indent();
          WriteLn('inherited Create();');
          for k := 0 to Pred(ASymbol.PropertyCount) do begin
            p := ASymbol.Properties[k];
            if p.DataType.InheritsFrom(TArrayDefinition) then begin
              Indent();
              WriteLn('F%s := %s.Create();',[p.Name,p.DataType.Name]);
            end;
          end;
        DecIndent();
        WriteLn('end;');
      end;

      if ( locClassPropNbr > 0 ) then begin
        NewLine();
        WriteLn('destructor %s.Destroy();',[ASymbol.Name]);
        WriteLn('begin');
        IncIndent();
          for k := 0 to Pred(ASymbol.PropertyCount) do begin
            p := ASymbol.Properties[k];
            if p.DataType.InheritsFrom(TClassTypeDefinition) then begin
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
      
      if ( locStoredPropsNbr > 0 ) then begin
        for k := 0 to Pred(ASymbol.PropertyCount) do begin
          p := ASymbol.Properties[k];
          if ( p.StorageOption = soOptional ) then begin
            NewLine();
            WriteLn('function %s.Has%s() : Boolean;',[ASymbol.Name,p.Name]);
            WriteLn('begin');
            IncIndent();
              Indent();
              WriteLn('Result := True;');
            DecIndent();
            WriteLn('end;');
          end;
        end;
      end;

    end;
  end;

begin
  Prepare();
  try
    loc_BaseComplexSimpleContentRemotable := FSymbolTable.ByName('TBaseComplexSimpleContentRemotable') as TClassTypeDefinition;
    SetCurrentStream(FDecStream);
    NewLine();
    IncIndent();
      WriteDec();
      WriteProperties();
      Indent(); WriteLn('end;');
    DecIndent();

    FImpTempStream.Indent();
    FImpTempStream.WriteLn('GetTypeRegistry().Register(%s,TypeInfo(%s),%s);',[sNAME_SPACE,ASymbol.Name,QuotedStr(ASymbol.ExternalName)]);

    SetCurrentStream(FImpStream);
      WriteImp();
  except
    on e : Exception do
      System.WriteLn('TInftGenerator.GenerateClass()=', ASymbol.Name, ' ;; ', e.Message);
  end;
end;

procedure TInftGenerator.GenerateEnum(ASymbol: TEnumTypeDefinition);
var
  itm : TEnumItemDefinition;
  i : Integer;
begin
  try
    SetCurrentStream(FDecStream);
    NewLine();
    IncIndent();
      Indent();WriteLn('%s = ( ',[ASymbol.Name]);

      FImpTempStream.Indent();
      FImpTempStream.WriteLn('GetTypeRegistry().Register(%s,TypeInfo(%s),%s);',[sNAME_SPACE,ASymbol.Name,QuotedStr(ASymbol.ExternalName)]);

      IncIndent();
        for i := 0 to Pred(ASymbol.ItemCount) do begin
          itm := ASymbol.Item[i];
          Indent();
          if ( i > 0 ) then
            WriteLn(',%s',[itm.Name])
          else
            WriteLn('%s',[itm.Name]);
          if not AnsiSameText(itm.Name,itm.ExternalName) then begin
            FImpTempStream.Indent();
            FImpTempStream.WriteLn('GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)].RegisterExternalPropertyName(%s,%s);',[ASymbol.Name,QuotedStr(itm.Name),QuotedStr(itm.ExternalName)]);
          end;
        end;
      DecIndent();
      Indent(); WriteLn(');');
    DecIndent();
  except
    on e : Exception do
      System.WriteLn('TInftGenerator.GenerateClass()=', ASymbol.Name, ' ;; ', e.Message);
  end;
end;

procedure TInftGenerator.GenerateArray(ASymbol: TArrayDefinition);

  procedure WriteObjectArray();
  begin
    SetCurrentStream(FDecStream);
    NewLine();
    IncIndent();
    BeginAutoIndent();
    try
      WriteLn('%s = class(TBaseObjectArrayRemotable)',[ASymbol.Name]);
      WriteLn('private');
        Indent();WriteLn('function GetItem(AIndex: Integer): %s;',[ASymbol.ItemType.Name]);
      WriteLn('public');
        Indent();WriteLn('class function GetItemClass():TBaseRemotableClass;override;');
        Indent();WriteLn('property Item[AIndex:Integer] : %s Read GetItem;Default;',[ASymbol.ItemType.Name]);
      WriteLn('end;');
    finally
      EndAutoIndent();
      DecIndent();
    end;
    
    SetCurrentStream(FImpStream);
    NewLine();
    WriteLn('{ %s }',[ASymbol.Name]);

    NewLine();
    WriteLn('function %s.GetItem(AIndex: Integer): %s;',[ASymbol.Name,ASymbol.ItemType.Name]);
    WriteLn('begin');
    IncIndent();
      Indent();WriteLn('Result := Inherited GetItem(AIndex) As %s;',[ASymbol.ItemType.Name]);
    DecIndent();
    WriteLn('end;');

    NewLine();
    WriteLn('class function %s.GetItemClass(): TBaseRemotableClass;',[ASymbol.Name]);
    WriteLn('begin');
    IncIndent();
      Indent();WriteLn('Result:= %s;',[ASymbol.ItemType.Name]);
    DecIndent();
    WriteLn('end;');
  end;

  procedure WriteSimpleTypeArray();
  begin
    SetCurrentStream(FDecStream);
    NewLine();
    IncIndent();
    BeginAutoIndent();
    try
      WriteLn('%s = class(TBaseSimpleTypeArrayRemotable)',[ASymbol.Name]);
      WriteLn('private');
        Indent();WriteLn('FData : array of %s;',[ASymbol.ItemType.Name]);
      WriteLn('private');
        Indent();WriteLn('function GetItem(AIndex: Integer): %s;',[ASymbol.ItemType.Name]);
        Indent();WriteLn('procedure SetItem(AIndex: Integer; const AValue: %s);',[ASymbol.ItemType.Name]);
      WriteLn('protected');
        Indent();WriteLn('function GetLength():Integer;override;');
        Indent();WriteLn('procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;');
        Indent();WriteLn('procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;');
      WriteLn('public');
        Indent();WriteLn('class function GetItemTypeInfo():PTypeInfo;override;');
        Indent();WriteLn('procedure SetLength(const ANewSize : Integer);override;');
        Indent();WriteLn('property Item[AIndex:Integer] : %s read GetItem write SetItem; default;',[ASymbol.ItemType.Name]);
      WriteLn('end;');
    finally
      EndAutoIndent();
      DecIndent();
    end;

    SetCurrentStream(FImpStream);
    NewLine();
    WriteLn('{ %s }',[ASymbol.Name]);

    NewLine();
    WriteLn('function %s.GetItem(AIndex: Integer): %s;',[ASymbol.Name,ASymbol.ItemType.Name]);
    WriteLn('begin');
    IncIndent();
      Indent();WriteLn('CheckIndex(AIndex);');
      Indent();WriteLn('Result := FData[AIndex];');
    DecIndent();
    WriteLn('end;');

    NewLine();
    WriteLn('procedure %s.SetItem(AIndex: Integer;const AValue: %S);',[ASymbol.Name,ASymbol.ItemType.Name]);
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
      Indent();WriteLn('AStore.Put(%s,TypeInfo(%s),FData[AIndex]);',[QuotedStr(ASymbol.ItemName),ASymbol.ItemType.Name]);
    DecIndent();
    WriteLn('end;');

    NewLine();
    IncIndent();
    WriteLn('procedure %s.LoadItem(AStore: IFormatterBase;const AIndex: Integer);',[ASymbol.Name]);
    WriteLn('var');
    Indent();WriteLn('sName : string;');
    WriteLn('begin');
      Indent();WriteLn('sName := %s;',[QuotedStr(ASymbol.ItemName)]);
      Indent();WriteLn('AStore.Get(TypeInfo(%s),sName,FData[AIndex]);',[ASymbol.ItemType.Name]);
    DecIndent();
    WriteLn('end;');
    
    NewLine();
    WriteLn('class function %s.GetItemTypeInfo(): PTypeInfo;',[ASymbol.Name]);
    WriteLn('begin');
    IncIndent();
      Indent();WriteLn('Result := TypeInfo(%s);',[ASymbol.ItemType.Name]);
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
  end;

var
  classItemArray : Boolean;
begin
  classItemArray := ( ASymbol.ItemType is TClassTypeDefinition ) or
                    ( ASymbol.ItemType is TArrayDefinition ) ;

  if classItemArray then begin
    WriteObjectArray();
  end else begin
    WriteSimpleTypeArray();
  end;
  
  FImpTempStream.Indent();
  FImpTempStream.WriteLn('GetTypeRegistry().Register(%s,TypeInfo(%s),%s);',[sNAME_SPACE,ASymbol.Name,QuotedStr(ASymbol.ExternalName)]);
  if ( ASymbol.ItemName <> ASymbol.ItemExternalName ) then begin
    FImpTempStream.Indent();
    FImpTempStream.WriteLn(
      'GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)].RegisterExternalPropertyName(sARRAY_ITEM,%s);',
      [ASymbol.Name,QuotedStr(ASymbol.ItemExternalName)]
    );
  end;
  if ( ASymbol.Style = asEmbeded ) then begin
    FImpTempStream.Indent();
    FImpTempStream.WriteLn(
      'GetTypeRegistry().ItemByTypeInfo[TypeInfo(%s)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);',
      [ASymbol.Name,QuotedStr(ASymbol.ItemExternalName)]
    );
  end;
end;

procedure TInftGenerator.GenerateCustomMetadatas();

  procedure WriteOperationDatas(AInftDef : TInterfaceDefinition; AOp : TMethodDefinition);
  var
    k : Integer;
    pl : TStrings;
  begin
    pl := AOp.Properties;
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
  
  procedure WriteServiceDatas(AIntf : TInterfaceDefinition);
  var
    k : Integer;
  begin
    if not IsStrEmpty(AIntf.Address) then begin
      Indent();WriteLn('mm.SetServiceCustomData(');
        IncIndent();
          Indent(); WriteLn('%s,',[sUNIT_NAME]);
          Indent(); WriteLn('%s,',[QuotedStr(AIntf.Name)]);
          Indent(); WriteLn('%s,',[QuotedStr('TRANSPORT_Address')]);
          Indent(); WriteLn('%s' ,[QuotedStr(AIntf.Address)]);
        DecIndent();
      Indent();WriteLn(');');
    end;
    
    if ( AIntf.BindingStyle = bsRPC ) then begin
      Indent();WriteLn('mm.SetServiceCustomData(');
        IncIndent();
          Indent(); WriteLn('%s,',[sUNIT_NAME]);
          Indent(); WriteLn('%s,',[QuotedStr(AIntf.Name)]);
          Indent(); WriteLn('%s,',[QuotedStr('FORMAT_Style')]);
          Indent(); WriteLn('%s' ,[QuotedStr('rpc')]);
        DecIndent();
      Indent();WriteLn(');');
    end else if ( AIntf.BindingStyle = bsDocument ) then begin
      Indent();WriteLn('mm.SetServiceCustomData(');
        IncIndent();
          Indent(); WriteLn('%s,',[sUNIT_NAME]);
          Indent(); WriteLn('%s,',[QuotedStr(AIntf.Name)]);
          Indent(); WriteLn('%s,',[QuotedStr('FORMAT_Style')]);
          Indent(); WriteLn('%s' ,[QuotedStr('document')]);
        DecIndent();
      Indent();WriteLn(');');
    end;

    for k := 0 to Pred(AIntf.MethodCount) do begin
      WriteOperationDatas(AIntf,AIntf.Method[k]);
    end;
  end;
  
var
  i : Integer;
begin
  SetCurrentStream(FImpStream);
  IncIndent();
  
  NewLine();NewLine();
  WriteLn('procedure Register_%s_ServiceMetadata();',[SymbolTable.Name]);
  WriteLn('var');
  Indent(); WriteLn('mm : IModuleMetadataMngr;');
  WriteLn('begin');
  Indent();WriteLn('mm := GetModuleMetadataMngr();');
  Indent();WriteLn('mm.SetRepositoryNameSpace(%s, %s);',[sUNIT_NAME,sNAME_SPACE]);
  for i := 0 to Pred(SymbolTable.Count) do begin
    if SymbolTable.Item[i] is TInterfaceDefinition then begin
      WriteServiceDatas(SymbolTable.Item[i] as TInterfaceDefinition);
    end;
  end;
  
  WriteLn('end;');
  DecIndent();
end;

function TInftGenerator.GetDestUnitName(): string;
begin
  Result := SymbolTable.Name;
end;

constructor TInftGenerator.Create(
  ASymTable : TSymbolTable;
  ASrcMngr  : ISourceManager
);
begin
  inherited Create(ASymTable,ASrcMngr);
  FDecStream := SrcMngr.CreateItem(GetDestUnitName() + '.dec');
  FImpStream := SrcMngr.CreateItem(GetDestUnitName() + '.imp');
  FImpTempStream := SrcMngr.CreateItem(GetDestUnitName() + '.tmp_imp');
  FImpLastStream := SrcMngr.CreateItem(GetDestUnitName() + '.tmp_imp_last');
  FImpTempStream.IncIndent();
  FImpLastStream.IncIndent();
end;

procedure TInftGenerator.Execute();
var
  i,c, j, k : Integer;
  clssTyp : TClassTypeDefinition;
  gnrClssLst : TObjectList;
  objLst : TObjectList;
begin
  objLst := nil;
  gnrClssLst := TObjectList.Create(False);
  try
    GenerateUnitHeader();
    GenerateUnitImplementationHeader();
    c := Pred(SymbolTable.Count);

    SetCurrentStream(FDecStream);
    IncIndent();
    for i := 0 to c do begin
      if SymbolTable.Item[i] is TForwardTypeDefinition then begin
        WriteLn('// %s = unable to resolve this symbol.',[SymbolTable.Item[i].Name]);
      end;
    end;
    DecIndent();
    
    IncIndent();
    for i := 0 to c do begin
      if ( SymbolTable.Item[i] is TClassTypeDefinition ) or
         ( SymbolTable.Item[i] is TArrayDefinition )
      then begin
        Indent();
        WriteLn('%s = class;',[SymbolTable.Item[i].Name]);
      end;
    end;
    DecIndent();

    for i := 0 to c do begin
      if SymbolTable.Item[i] is TEnumTypeDefinition then begin
        GenerateEnum(SymbolTable.Item[i] as TEnumTypeDefinition);
      end;
    end;

    for i := 0 to c do begin
      if SymbolTable.Item[i] is TTypeAliasDefinition then begin
        GenerateTypeAlias(SymbolTable.Item[i] as TTypeAliasDefinition);
      end;
    end;

    objLst := TObjectList.Create();
    objLst.OwnsObjects := False;
    for i := 0 to c do begin
      if SymbolTable.Item[i].InheritsFrom(TClassTypeDefinition) then begin
        clssTyp := SymbolTable.Item[i] as TClassTypeDefinition;
        if ( gnrClssLst.IndexOf(clssTyp) = -1 ) then begin
          while ( objLst.Count > 0 ) do begin
            objLst.Clear();
          end;
          while Assigned(clssTyp) do begin
            objLst.Add(clssTyp);
            if Assigned(clssTyp.Parent) and clssTyp.Parent.InheritsFrom(TClassTypeDefinition) then begin
              clssTyp := clssTyp.Parent as TClassTypeDefinition;
            end else begin
              clssTyp := nil;
            end;
          end;

          k := Pred(objLst.Count);
          for j := 0 to k do begin
            clssTyp := objLst[k-j] as TClassTypeDefinition;
            if ( gnrClssLst.IndexOf(clssTyp) = -1 ) then begin
              if ( FSymbolTable.IndexOf(clssTyp) <> -1 ) then begin
                GenerateClass(clssTyp);
                gnrClssLst.Add(clssTyp);
              end;
            end;
          end;
        end;
      end;
    end;

    for i := 0 to c do begin
      if SymbolTable.Item[i] is TArrayDefinition then begin
        GenerateArray(SymbolTable.Item[i] as TArrayDefinition);
      end;
    end;

    for i := 0 to c do begin
      if SymbolTable.Item[i] is TInterfaceDefinition then begin
        GenerateIntf(SymbolTable.Item[i] as TInterfaceDefinition);
      end;
    end;

    NewLine();
    IncIndent();
    Indent(); WriteLn('procedure Register_%s_ServiceMetadata();',[SymbolTable.Name]);
    DecIndent();
    GenerateCustomMetadatas();
    
    GenerateUnitImplementationFooter();
    FSrcMngr.Merge(GetDestUnitName() + '.pas',[FDecStream,FImpStream,FImpTempStream,FImpLastStream]);
    FDecStream := nil;
    FImpStream := nil;
    FImpTempStream := nil;
  finally
    FreeAndNil(objLst);
    FreeAndNil(gnrClssLst);
  end;
end;

end.
