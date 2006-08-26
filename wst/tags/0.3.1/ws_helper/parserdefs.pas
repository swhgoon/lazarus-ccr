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


unit parserdefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

Type

  ESymbolException = class(Exception)
  End;

  { TAbstractSymbolDefinition }

  TAbstractSymbolDefinition = class
  private
    FName: String;
  Public
    constructor Create(Const AName : String);
    Property Name : String Read FName;
  End;

  { TTypeDefinition }

  TTypeDefinition = class(TAbstractSymbolDefinition)
  public
    function NeedFinalization():Boolean;virtual;
  end;

  { TEnumItemDefinition }

  TEnumItemDefinition = class(TAbstractSymbolDefinition)
  private
    FOrder: Integer;
  Public
    constructor Create(Const AName : String; Const AOrder : Integer);
    Property Order : Integer Read FOrder;
  End;

  { TEnumTypeDefinition }

  TEnumTypeDefinition = class(TTypeDefinition)
  Private
    FItemList : TObjectList;
  private
    function GetItem(Index: Integer): TEnumItemDefinition;
    function GetItemCount: Integer;
  Public
    constructor Create(Const AName : String);
    destructor Destroy();override;
    function NeedFinalization():Boolean;override;
    Procedure AddItem(AItem:TEnumItemDefinition);
    function FindItem(Const AName:String):TEnumItemDefinition;
    Property ItemCount : Integer Read GetItemCount;
    Property Item[Index:Integer]:TEnumItemDefinition Read GetItem;
  End;

  { TClassTypeDefinition }

  TClassTypeDefinition = class(TTypeDefinition)
  public
    function NeedFinalization():Boolean;override;
  end;

  TParameterModifier = ( pmNone, pmConst, pmVar, pmOut );

  { TParameterDefinition }

  TParameterDefinition = class(TAbstractSymbolDefinition)
  private
    FDataType: TTypeDefinition;
    FModifier: TParameterModifier;
  Public
    constructor Create(
      Const AName     : String;
      Const AModifier : TParameterModifier;
            ADataType     : TTypeDefinition
    );
    property Modifier : TParameterModifier Read FModifier;
    property DataType : TTypeDefinition Read FDataType;
  End;

  TMethodType = ( mtProcedure, mtFunction );
Const
      ParameterModifierMAP : Array[TParameterModifier] Of String =
        ( '', 'Const', 'Var', 'Out' );
Type

  { TMethodDefinition }

  TMethodDefinition = class(TAbstractSymbolDefinition)
  private
    FMethodType: TMethodType;
    FParameterList : TObjectList;
    function GetParameter(Index: Integer): TParameterDefinition;
    function GetParameterCount: Integer;
  Public
    constructor Create(Const AName : String; Const AMethodType : TMethodType);
    destructor Destroy();override;
    function AddParameter(
      Const AName     : String;
      Const AModifier : TParameterModifier;
            ADataType     : TTypeDefinition
    ):TParameterDefinition;
    function GetParameterIndex(Const AName : String):Integer;
    function FindParameter(Const AName : String):TParameterDefinition;
    property MethodType : TMethodType Read FMethodType;
    property ParameterCount : Integer Read GetParameterCount;
    property Parameter[Index:Integer] : TParameterDefinition Read GetParameter;
  End;

  { TInterfaceDefinition }

  TInterfaceDefinition = class(TAbstractSymbolDefinition)
  Private
    FInterfaceGUID: string;
    FMethodList : TObjectList;
    function GetMethod(Index: Integer): TMethodDefinition;
    function GetMethodCount: Integer;
  Public
    constructor Create(Const AName : String);
    destructor Destroy();override;
    function GetMethodIndex(Const AName : String):Integer;
    function FindMethod(Const AName : String):TMethodDefinition;
    function AddMethod(
      Const AName : String;
      Const AMethodType : TMethodType
    ):TMethodDefinition;
    Property MethodCount : Integer Read GetMethodCount;
    Property Method[Index:Integer] : TMethodDefinition Read GetMethod;
    property InterfaceGUID : string read FInterfaceGUID write FInterfaceGUID;
  End;
  
  { TSymbolTable }

  TSymbolTable = class(TAbstractSymbolDefinition)
  Private
    FList : TObjectList;
    procedure CheckIndex(Const AIndex : Integer);
    function GetCount: Integer;
    function GetItem(Index: Integer): TAbstractSymbolDefinition;
    procedure SetName(const AValue: String);
  Public
    constructor Create(Const AName : String);
    destructor Destroy();override;
    procedure Clear();
    function Add(ASym : TAbstractSymbolDefinition):Integer;
    function IndexOf(Const AName : String):Integer;overload;
    function IndexOf(ASym : TAbstractSymbolDefinition):Integer;overload;
    function Find(Const AName : String):TAbstractSymbolDefinition;
    function ByName(Const AName : String):TAbstractSymbolDefinition;
    Property Name : String Read FName Write SetName;
    Property Count : Integer Read GetCount;
    Property Item[Index:Integer] : TAbstractSymbolDefinition Read GetItem;
  End;

implementation
uses StrUtils, parserutils;

{ TAbstractSymbolDefinition }

constructor TAbstractSymbolDefinition.Create(const AName: String);
begin
  Assert(Not IsStrEmpty(AName));
  FName := AName;
end;

{ TParameterDefinition }

constructor TParameterDefinition.Create(
  const AName: String;
  const AModifier: TParameterModifier;
  ADataType: TTypeDefinition
);
begin
  Inherited Create(AName);
  Assert(Assigned(ADataType));
  FModifier := AModifier;
  FDataType := ADataType;
end;

{ TMethodDefinition }

function TMethodDefinition.GetParameter(Index: Integer): TParameterDefinition;
begin
  Result := FParameterList[Index] As TParameterDefinition;
end;

function TMethodDefinition.GetParameterCount: Integer;
begin
  Result := FParameterList.Count;
end;

constructor TMethodDefinition.Create(
  const AName: String;
  const AMethodType: TMethodType
);
begin
  Inherited Create(AName);
  FMethodType := AMethodType;
  FParameterList := TObjectList.create(True);
end;

destructor TMethodDefinition.Destroy();
begin
  FreeAndNil(FParameterList);
  inherited Destroy();
end;

function TMethodDefinition.AddParameter(
  Const AName     : String;
  Const AModifier : TParameterModifier;
        ADataType     : TTypeDefinition
): TParameterDefinition;
begin
  If ( GetParameterIndex(Name) = -1 ) Then Begin
    Result := TParameterDefinition.Create(AName,AModifier,ADataType);
    FParameterList.Add(Result);
  End Else Begin
    Raise ESymbolException.CreateFmt('Duplicated parameter : %s.%s',[Name,AName]);
  End;
end;

function TMethodDefinition.GetParameterIndex(const AName: String): Integer;
begin
  For Result := 0 To Pred(ParameterCount) Do
    If AnsiSameText(AName,Parameter[Result].Name) Then
      Exit;
  Result := -1;
end;

function TMethodDefinition.FindParameter(
  const AName: String
): TParameterDefinition;
Var
  i : Integer;
begin
  i := GetParameterIndex(AName);
  If ( i > -1 ) Then
    Result := Parameter[i]
  Else
    Result := Nil;
end;

{ TInterfaceDefinition }

function TInterfaceDefinition.GetMethod(Index: Integer): TMethodDefinition;
begin
  Result := FMethodList[Index] As TMethodDefinition;
end;

function TInterfaceDefinition.GetMethodCount: Integer;
begin
  Result := FMethodList.Count;
end;

constructor TInterfaceDefinition.Create(const AName: String);
begin
  Inherited Create(AName);
  FMethodList := TObjectList.create(True);
end;

destructor TInterfaceDefinition.Destroy();
begin
  FreeAndNil(FMethodList);
  inherited Destroy();
end;

function TInterfaceDefinition.GetMethodIndex(const AName: String): Integer;
begin
  For Result := 0 To Pred(MethodCount) Do
    If AnsiSameText(AName,Method[Result].Name) Then
      Exit;
  Result := -1;
end;

function TInterfaceDefinition.FindMethod(const AName: String): TMethodDefinition;
Var
  i : Integer;
begin
  i := GetMethodIndex(AName);
  If ( i > -1 ) Then
    Result := Method[i]
  Else
    Result := Nil;
end;

function TInterfaceDefinition.AddMethod(
  Const AName : String;
  Const AMethodType : TMethodType
):TMethodDefinition;
begin
  If ( GetMethodIndex(Name) = -1 ) Then Begin
    Result := TMethodDefinition.Create(AName,AMethodType);
    FMethodList.Add(Result);
  End Else Begin
    Raise ESymbolException.CreateFmt('Duplicated methode : %s.%s',[Name,AName]);
  End;
end;

{ TSymbolTable }

procedure TSymbolTable.CheckIndex(const AIndex: Integer);
begin
  If ( AIndex < 0 ) Or ( AIndex >= Count ) Then
    Raise ESymbolException.CreateFmt('Invalid Table Index : %d',[AIndex]);
end;

function TSymbolTable.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSymbolTable.GetItem(Index: Integer): TAbstractSymbolDefinition;
begin
  CheckIndex(Index);
  Result := FList[Index] As TAbstractSymbolDefinition;
end;

procedure TSymbolTable.SetName(const AValue: String);
begin
  if ( FName = AValue ) then exit;
  FName := AValue;
end;

constructor TSymbolTable.Create(Const AName : String);
begin
  Inherited Create(AName);
  FList := TObjectList.Create(True);
end;

destructor TSymbolTable.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TSymbolTable.Clear();
begin
  FList.Clear();
end;

function TSymbolTable.Add(ASym: TAbstractSymbolDefinition): Integer;
begin
  Result := IndexOf(ASym);
  If ( Result = -1 ) Then Begin
    If ( IndexOf(ASym.Name) <> -1 ) Then
      Raise ESymbolException.CreateFmt('Duplicated symbol name : %s',[ASym.Name]);
    Result := FList.Add(ASym);
  End;
end;

function TSymbolTable.IndexOf(const AName: String): Integer;
begin
  For Result := 0 To Pred(Count) Do
    If AnsiSameText(AName,Item[Result].Name) Then
      Exit;
  Result := -1;
end;

function TSymbolTable.IndexOf(ASym: TAbstractSymbolDefinition): Integer;
begin
  Result := FList.IndexOf(ASym);
end;

function TSymbolTable.Find(const AName: String): TAbstractSymbolDefinition;
Var
  i : Integer;
begin
  i := IndexOf(AName);
  If ( i > -1 ) Then
    Result := Item[i]
  Else
    Result := Nil;
end;

function TSymbolTable.ByName(const AName: String): TAbstractSymbolDefinition;
begin
  Result := Find(AName);
  If Not Assigned(Result) Then
    Raise ESymbolException.CreateFmt('No such Symbol : %s',[AName]);
end;

{ TEnumItemDefinition }

constructor TEnumItemDefinition.Create(const AName: String; Const AOrder: Integer);
begin
  Inherited Create(AName);
  FOrder := AOrder;
end;

{ TEnumTypeDefinition }

function TEnumTypeDefinition.GetItem(Index: Integer): TEnumItemDefinition;
begin
  Result := FItemList[Index] As TEnumItemDefinition;
end;

function TEnumTypeDefinition.GetItemCount: Integer;
begin
  Result := FItemList.Count;
end;

constructor TEnumTypeDefinition.Create(const AName: String);
begin
  Inherited Create(AName);
  FItemList := TObjectList.Create(False);
end;

destructor TEnumTypeDefinition.Destroy();
begin
  FItemList.Free();
  inherited Destroy();
end;

function TEnumTypeDefinition.NeedFinalization(): Boolean;
begin
  Result := False;
end;

procedure TEnumTypeDefinition.AddItem(AItem:TEnumItemDefinition);
Begin
  If ( FItemList.IndexOf(AItem) = -1 ) Then
    FItemList.Add(AItem);
end;

function TEnumTypeDefinition.FindItem(const AName: String): TEnumItemDefinition;
Var
  i,c : Integer;
begin
  c := Pred(ItemCount);
  For i := 0 To c Do Begin
    If AnsiSameText(AName,Item[i].Name) Then Begin
      Result := Item[i];
      Exit;
    End;
  End;
  Result := Nil;
end;

{ TTypeDefinition }
const SIMPLE_TYPES : Array[0..12] Of string = (
        'string', 'integer', 'smallint', 'shortint', 'char', 'boolean',
        'byte', 'word', 'longint', 'int64',
        'single', 'double', 'extended'
      );
function TTypeDefinition.NeedFinalization(): Boolean;
begin
  Result := ( AnsiIndexText(Name,SIMPLE_TYPES) = -1 );
end;

{ TClassTypeDefinition }

function TClassTypeDefinition.NeedFinalization(): Boolean;
begin
  Result := True;
end;

end.
