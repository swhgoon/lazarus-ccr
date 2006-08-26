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
  
Type

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


implementation
uses parserutils;

Const sPROXY_BASE_CLASS = 'TBaseProxy';
      sBINDER_BASE_CLASS = 'TBaseServiceBinder';
      sIMP_BASE_CLASS = 'TBaseServiceImplementation';
      sSERIALIZER_CLASS  = 'IFormatterClient';
      RETURN_PARAM_NAME = 'return';
      RETURN_VAL_NAME = 'returnVal';

      sPRM_NAME = 'strPrmName';
      sLOC_SERIALIZER = 'locSerializer';
      //sRES_TYPE_INFO  = 'resTypeInfo';
      //sLOC_TYPE_INFO  = 'locTypeInfo';

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
  
  WriteLn('Unit %s;',[GetDestUnitName()]);
  WriteLn('{$mode objfpc}{$H+}');
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
  WriteLn('uses LResources, metadata_repository;');
end;

procedure TProxyGenerator.GenerateUnitImplementationFooter();
var
  s :string;
begin
  SetCurrentStream(FImpStream);
  NewLine();
  WriteLn('initialization');
  WriteLn('  {$i %s.lrs}',[SymbolTable.Name]);
  NewLine();
  s := Format('Register_%s_ServiceMetadata',[SymbolTable.Name]);
  WriteLn('  {$IF DECLARED(%s)}',[s]);
  WriteLn('  %s();',[s]);
  WriteLn('  {$ENDIF}');
  WriteLn('End.');
end;

constructor TProxyGenerator.Create(
  ASymTable : TSymbolTable;
  ASrcMngr  : ISourceManager
);
begin
  Inherited Create(ASymTable,ASrcMngr);
  FDecStream := SrcMngr.CreateItem(GetDestUnitName() + '.dec');
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
  FSrcMngr.Merge(GetDestUnitName() + '.pas',[FDecStream,FImpStream]);
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
      //If ( AMthd.MethodType = mtFunction ) Then
        //Indent();WriteLn('%s : %s;',[sRES_TYPE_INFO,'PTypeInfo']);
    WriteLn('Begin');
    
      Indent();WriteLn('%s := GetSerializer();',[sLOC_SERIALIZER]);
      Indent();WriteLn('Try');IncIndent();

      Indent();WriteLn('%s.BeginCall(''%s'', GetTarget(),(Self as ICallContext));',[sLOC_SERIALIZER,AMthd.Name]);
      IncIndent();
        prmCnt := AMthd.ParameterCount;
        If ( AMthd.MethodType = mtFunction ) Then
          Dec(prmCnt);
        For k := 0 To Pred(prmCnt) Do Begin
          prm := AMthd.Parameter[k];
          If ( prm.Modifier <> pmOut ) Then Begin
            Indent();WriteLn('%s.Put(''%s'', TypeInfo(%s), %s);',[sLOC_SERIALIZER,prm.Name,prm.DataType.Name,prm.Name]);
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
            if prm.DataType.InheritsFrom(TClassTypeDefinition) then begin
              Indent();WriteLn('Pointer(Result) := Nil;');
            end else begin
              Indent();WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkInterface] ) Then',[prm.DataType.Name]);
              IncIndent();
                Indent();WriteLn('Pointer(Result) := Nil;');
              DecIndent();
            end;
          end;
          Indent();WriteLn('%s := %s;',[sPRM_NAME,QuotedStr(RETURN_PARAM_NAME)]);
          Indent();WriteLn('%s.Get(TypeInfo(%s), %s, %s);',[sLOC_SERIALIZER,prm.DataType.Name,sPRM_NAME,prm.Name]);
        End;
        //--------------------------------
        for k := 0 to Pred(prmCnt) do begin
          prm := AMthd.Parameter[k];
          if ( prm.Modifier = pmOut ) then begin
            if prm.DataType.InheritsFrom(TClassTypeDefinition) then begin
              Indent();WriteLn('Pointer(%s) := Nil;',[prm.Name]);
            end else begin
              Indent();WriteLn('If ( PTypeInfo(TypeInfo(%s))^.Kind in [tkClass,tkInterface] ) Then',[prm.DataType.Name]);
              IncIndent();
                Indent();WriteLn('Pointer(%s) := Nil;',[prm.Name]);
              DecIndent();
            end;
          end;
        end;
        //--------------------------------

        For k := 0 To Pred(prmCnt) Do Begin
          prm := AMthd.Parameter[k];
          If ( prm.Modifier In [pmVar, pmOut] ) Then Begin
            Indent();WriteLn('%s := %s;',[sPRM_NAME,QuotedStr(prm.Name)]);
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
  WriteLn('{$mode objfpc}{$H+}');
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
  WriteLn('uses TypInfo, LResources,metadata_repository;');
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
  WriteLn('  {$i %s.lrs}',[SymbolTable.Name]);
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
      If ( AMthd.MethodType = mtFunction ) Or ( prmCnt > 0 ) Then Begin
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
            WriteLn('Pointer(%s) := Nil;',[RETURN_VAL_NAME]);
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
            WriteLn('Pointer(%s) := Nil;',[prm.Name]);
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
        Write('%s := %s;',[sPRM_NAME,QuotedStr(prm.Name)]);
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
            WriteLn('If Assigned(Pointer(%s)) Then',[RETURN_VAL_NAME])
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
      //BeginAutoIndent();
        IncIndent();
        If ( AMthd.MethodType = mtFunction ) Then
          WriteLn('AFormatter.Put(%s,TypeInfo(%s),%s);',[QuotedStr(RETURN_PARAM_NAME),AMthd.Parameter[prmCnt].DataType.Name,RETURN_VAL_NAME]);
        For k := 0 To Pred(prmCnt) Do Begin
          prm := AMthd.Parameter[k];
          If ( prm.Modifier In [pmOut,pmVar] ) Then
            WriteLn('AFormatter.Put(%s,TypeInfo(%s),%s);',[QuotedStr(prm.Name),prm.DataType.Name,prm.Name]);
        End;
        DecIndent();
      WriteLn('AFormatter.EndCallResponse();');
      NewLine();
      WriteLn('callCtx := Nil;');
      //EndAutoIndent();

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
  WriteLn('{$mode objfpc}{$H+}');
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

end.
