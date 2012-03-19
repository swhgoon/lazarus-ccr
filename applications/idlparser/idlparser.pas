unit idlParser;

{ Unit which parses idl (interface description language) files into a TIDLList
  struct.

  Copyright (C) 2012 Joost van der Sluis/CNOC joost@cnoc.nl

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,contnrs;

type
  TMemberType=(mtFunc,mtAttribute,mtConst);
  TParamInOutType=(piNormal,piOut,piIn,piInOut);

  { TIDLMemberParameter }

  TIDLMemberParameter = class
  private
    FParamInOutType: TParamInOutType;
    FParamName: string;
    FParamType: string;
    FParamTypeUnsigned: boolean;
  public
    property ParamType : string read FParamType write FParamType;
    property ParamTypeUnsigned: boolean read FParamTypeUnsigned write FParamTypeUnsigned;
    property ParamName: string read FParamName write FParamName;
    property ParamInOutType: TParamInOutType read FParamInOutType write FParamInOutType;
  end;

  TIDLMemberParameterList = class(TObjectList);

  { TIDLMember }

  TIDLMember = class
  private
    FMemberName: string;
    FMemberReadonly: boolean;
    FMemberType: TMemberType;
    FParams: TIDLMemberParameterList;
    FReturnType: string;
    FConstValue: string;
    FReturnTypeUnsigned: boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property MemberType : TMemberType read FMemberType write FMemberType;
    property ReturnType : string read FReturnType write FReturnType;
    property ReturnTypeUnsigned: boolean read FReturnTypeUnsigned write FReturnTypeUnsigned;
    property MemberName: string read FMemberName write FMemberName;
    property MemberReadonly: boolean read FMemberReadonly write FMemberReadonly;
    property Params: TIDLMemberParameterList read FParams;
    property ConstValue: string read FConstValue write FConstValue;
  end;

  TIDLMemberList = class(TObjectList);

  TIDL = class
  private
    FInterfaceName: string;
    FInterfaceType: string;
    Fmembers: TIDLMemberList;
    FUUID: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property InterfaceName: string read FInterfaceName write FInterfaceName;
    property InterfaceType: string read FInterfaceType write FInterfaceType;
    property UUID: string read FUUID write FUUID;
    property members: TIDLMemberList read Fmembers;
  end;

  TIDLList = class(TObjectList);


procedure ParseFile(const AnIdlList: TIDLList; const IDLtext: tstrings);

implementation

const
  idlInterface = 'interface';
  idlUUID = 'uuid';
  idlattribute = 'attribute';
  idlconst = 'const';
  idlreadonly = 'readonly';
  idlInterfaceEnd = ';';
  idlUnsigned = 'unsigned';
  idlMemberEnd = ';';
  idlInterfaceTypeSeparator = ':';
  idlInterfaceBlockStart = '{';
  idlInterfaceBlockEnd = '}';
  idlStartMultiLineComment = '/*';
  idlEndMultiLineComment = '*/';
  idlStartExtension = '%{';
  idlEndExtension = '%}';
  idlStartSingleLineComment = '//';
  idlEndSingleLineComment = #10;
  idlStartIDLAttributesBlock = '[';
  idlEndIDLAttributesBlock = ']';
  idlStartUUID = '(';
  idlEndUUID = ')';
  idlSeparateFuncParams = ',';
  idlStartFuncParams = '(';
  idlEndFuncParams = ')';
  idlParamIn = 'in';
  idlParamOut = 'out';
  idlConstAssign = '=';

procedure ParseFile(const AnIdlList: TIDLList; const IDLtext: tstrings);

type TParseState = (psBegin,
                    psMultiLineComment,
                    psSingleLineComment,
                    psInterface,
                    psInterfaceType,
                    psInterfaceBlock,
                    psInterfaceBlockFuncName,
                    psInterfaceBlockFuncParams,
                    psInterfaceBlockFuncParamName,
                    psInterfaceAfterFuncParams,
                    psParamAttributeBlock,
                    psConstValue,
                    psIDLAttributes,
                    psSearchUUID,
                    psUUID,
                    psExtension,
                    psWord);

var
  PriorState: TParseState;
  ParseState: TParseState;
  IDLString: string;
  pCurrent: pchar;
  AWord: string;
  LineNr: integer;
  pWordStart: pchar;
  wordchars: set of char;
  UUIDAttribute: string;
  CurrentIDL: TIDL;
  CurrentIDLMember: TIDLMember;
  CurrentIDLMemberParam: TIDLMemberParameter;
  IsAttribute, IsReadonly: boolean;
  IsConst: boolean;
  IsParamIn, IsParamOut: boolean;
  IsUnsigned: boolean;

  function CheckChar(const ACheckForString: string; ASetParseState: TParseState): boolean;
  begin
    result := false;
    if CompareChar(pCurrent^,ACheckForString[1], length(ACheckForString))=0 then
      begin
      ParseState := ASetParseState;
      inc(pcurrent,length(ACheckForString));
      result := True;
      end;
  end;

  function CheckChar(const ACheckForString: string; ASetParseState, ASetPriorParseState: TParseState): boolean;
  begin
    result := CheckChar(ACheckForString, ASetParseState);
    if result then
      PriorState:=ASetPriorParseState;
  end;

  function CheckStartWord(ASetParseState, ASetPriorParseState: TParseState; AllowMinus: boolean = false): boolean;
  begin
    result := false;
    wordchars:=['a'..'z','A'..'Z','0'..'9','_'];
    if AllowMinus then include(wordchars,'-');
    if pCurrent^ in wordchars then
      begin
      pWordStart:=pCurrent;
      PriorState:=ASetPriorParseState;
      ParseState := ASetParseState;
      inc(pcurrent);
      result := True;
      end;
  end;

  function CheckEndWord(ASetParseState: TParseState): boolean;
  var
    i: integer;
  begin
    result := false;
    if not (pCurrent^ in wordchars) then
      begin
      i := pCurrent-pWordStart;
      SetLength(AWord,i);
      move(pWordStart^,AWord[1],i);
      if PriorState = psInterface then
        CurrentIDL.InterfaceName:=AWord
      else if PriorState = psInterfaceType then
        CurrentIDL.InterfaceType:=AWord
      else if PriorState = psSearchUUID then
        UUIDAttribute:=AWord
      else if PriorState = psInterfaceBlockFuncName then
        CurrentIDLMember.MemberName:=AWord
      else if PriorState = psInterfaceBlockFuncParamName then
        CurrentIDLMemberParam.ParamName:=AWord;
      ParseState := ASetParseState;
      result := True;
      end;
  end;

  function CheckStartConst: boolean;
  begin
    result := CheckChar(idlConstAssign,psConstValue);
    if Result then
      begin
      pWordStart:=pCurrent;
      ParseState := psConstValue;
      inc(pcurrent);
      end;
  end;

  function CheckEndConst: boolean;
  var
    i: integer;
  begin
    result := CheckChar(idlMemberEnd,psInterfaceBlock);
    if result then
      begin
      i := pCurrent-pWordStart-1;
      SetLength(AWord,i);
      move(pWordStart^,AWord[1],i);
      CurrentIDLMember.ConstValue:=AWord;
      ParseState := psInterfaceBlock;
      inc(pcurrent);
      end;
  end;

  function CheckInterfaceStart: boolean;
  begin
    result := CheckChar(idlInterface, psInterface);
    if result then
      begin
      CurrentIDL := TIDL.Create;
      AnIdlList.Add(CurrentIDL);
      CurrentIDL.UUID:=UUIDAttribute;
      UUIDAttribute:='';
      end;
  end;

  function CheckFuncStart: boolean;
  begin
    result := CheckStartWord(psWord, psInterfaceBlockFuncName);
    if result then
      begin
      CurrentIDLMember := TIDLMember.Create;
      if Isconst then
        CurrentIDLMember.MemberType:=mtConst
      else if IsAttribute then
        CurrentIDLMember.MemberType:=mtAttribute
      else
        CurrentIDLMember.MemberType:=mtFunc;
      CurrentIDLMember.MemberReadonly:=IsReadonly;
      IsAttribute:=false;
      IsConst:=false;
      IsReadonly:=false;
      CurrentIDL.members.Add(CurrentIDLMember);
      end;
  end;

  function CheckParamStart: boolean;
  begin
    result := CheckStartWord(psWord, psInterfaceBlockFuncParamName);
    if result then
      begin
      CurrentIDLMemberParam := TIDLMemberParameter.Create;
      if IsParamIn and IsParamOut then
        CurrentIDLMemberParam.ParamInOutType:=piInOut
      else if IsParamIn then
        CurrentIDLMemberParam.ParamInOutType:=piIn
      else if IsParamOut then
        CurrentIDLMemberParam.ParamInOutType:=piOut
      else
        CurrentIDLMemberParam.ParamInOutType:=piNormal;
      IsParamIn:=false;
      IsParamOut:=false;
      CurrentIDLMember.Params.Add(CurrentIDLMemberParam);
      end;
  end;

  function CheckAttributeStart: boolean;
  begin
    result := CheckChar(idlattribute, psInterfaceBlock);
    if result then
      IsAttribute := True;
  end;

  function CheckConstStart: boolean;
  begin
    result := CheckChar(idlconst, psInterfaceBlock);
    if result then
      IsConst := True;
  end;

  function CheckUnsigned: boolean;
  begin
    result := CheckChar(idlUnsigned, ParseState);
    if result then
      IsUnsigned := True;
  end;


  function CheckAttributeReadOnly: boolean;
  begin
    result := CheckChar(idlreadonly, psInterfaceBlock);
    if result then
      IsReadonly := True;
  end;

  function CheckParamIn: boolean;
  begin
    result := CheckChar(idlParamIn, psInterfaceBlockFuncParams);
    if result then
      IsParamIn := True;
  end;

  function CheckParamOut: boolean;
  begin
    result := CheckChar(idlParamOut, psInterfaceBlockFuncParams);
    if result then
      IsParamOut := True;
  end;


begin
  LineNr := 0;
  ParseState:=psBegin;
  IDLString:=IDLtext.Text;
  if length(IDLString)=0 then
    Exit;

  IsAttribute:=false;
  IsReadonly:=false;
  IsUnsigned:=false;
  IsConst:=false;
  IsParamIn:=false;
  IsParamOut:=false;
  UUIDAttribute:='';
  pCurrent:=@IDLString[1];
  while pCurrent^ <> #0 do
    begin
    case ParseState of
      psBegin:
        begin
        if not (CheckChar(idlStartMultiLineComment,psMultiLineComment,ParseState) or
                CheckChar(idlStartExtension,psExtension,ParseState) or
                CheckInterfaceStart or
                CheckChar(idlStartIDLAttributesBlock,psIDLAttributes,ParseState) or
                CheckChar(idlInterface,psInterface)) then
          inc(pCurrent);
        end;
      psMultiLineComment:
        begin
        if not (CheckChar(idlEndMultiLineComment,PriorState)) then
          inc(pCurrent);
        end;
      psExtension:
        begin
        if not (CheckChar(idlEndExtension,PriorState)) then
          inc(pCurrent);
        end;
      psSingleLineComment:
        begin
        if not (CheckChar(idlEndSingleLineComment,PriorState)) then
          inc(pCurrent);
        end;
      psParamAttributeBlock:
        begin
        if not (CheckChar(idlEndIDLAttributesBlock,PriorState)) then
          inc(pCurrent);
        end;
      psIDLAttributes:
        begin
        if not (CheckChar(idlEndIDLAttributesBlock,psBegin) or
                CheckChar(idlUUID, psSearchUUID)) then
          inc(pCurrent);
        end;
      psSearchUUID:
        begin
        if not (CheckChar(idlStartUUID,psUUID) or
               CheckChar(idlEndUUID, psIDLAttributes)) then
          inc(pCurrent);
        end;
      psUUID:
        begin
        if not CheckStartWord(psWord,psSearchUUID,true) then
          inc(pCurrent);
        end;
      psInterface, psInterfaceType:
        begin
        if not (CheckStartWord(psWord,ParseState) or
                CheckChar(idlInterfaceBlockStart,psInterfaceBlock,ParseState) or
                CheckChar(idlStartMultiLineComment,psMultiLineComment,ParseState) or
                CheckChar(idlStartSingleLineComment,psSingleLineComment,ParseState) or
                CheckChar(idlStartExtension,psExtension,ParseState) or
                CheckChar(idlInterfaceTypeSeparator, psInterfaceType) or
                CheckChar(idlInterfaceEnd, psBegin)) then
          inc(pCurrent);
        end;
      psInterfaceBlock:
        begin
        if not (CheckChar(idlInterfaceBlockEnd,psInterface) or
                CheckChar(idlStartMultiLineComment,psMultiLineComment,ParseState) or
                CheckChar(idlStartExtension,psExtension,ParseState) or
                CheckChar(idlStartSingleLineComment,psSingleLineComment,ParseState) or
                CheckChar(idlStartIDLAttributesBlock,psParamAttributeBlock,ParseState) or
                CheckAttributeStart or
                CheckAttributeReadOnly or
                CheckConstStart or
                CheckUnsigned or
                CheckFuncStart) then
          inc(pCurrent)
        end;
      psInterfaceBlockFuncName:
        begin
        if CurrentIDLMember.ReturnType = '' then
          begin
          CurrentIDLMember.ReturnType:=aword;
          CurrentIDLMember.ReturnTypeUnsigned := IsUnsigned;
          IsUnsigned:=false;
          end;
        if not (CheckStartWord(psWord, psInterfaceBlockFuncName) or
                CheckChar(idlStartFuncParams,psInterfaceBlockFuncParams) or
                CheckChar(idlMemberEnd,psInterfaceBlock) or
                CheckStartConst or
                CheckChar(idlStartMultiLineComment,psMultiLineComment,ParseState)) then
          inc(pCurrent)
        end;
      psInterfaceBlockFuncParams:
        begin
        if not (CheckChar(idlStartMultiLineComment,psMultiLineComment,ParseState) or
                CheckChar(idlStartIDLAttributesBlock,psParamAttributeBlock,ParseState) or
                CheckParamIn or
                CheckParamOut or
                CheckUnsigned or
                CheckParamStart or
                CheckChar(idlEndFuncParams,psInterfaceAfterFuncParams)) then
          inc(pCurrent)
        end;
      psInterfaceAfterFuncParams:
        begin
        // voor een definitie als: 'nsIDOMNode setNamedItem(in nsIDOMNode arg) raises(DOMException);'
        // negeer in dat geval alles na de parameters
        if not (CheckChar(idlStartMultiLineComment,psMultiLineComment,ParseState) or
                CheckChar(idlMemberEnd,psInterfaceBlock)) then
          inc(pCurrent)
        end;

      psInterfaceBlockFuncParamName:
        begin
        if CurrentIDLMemberParam.ParamType = '' then
          begin
          CurrentIDLMemberParam.ParamType:=aword;
          CurrentIDLMemberParam.ParamTypeUnsigned := IsUnsigned;
          IsUnsigned:=false;
          end;
        if not (CheckStartWord(psWord, psInterfaceBlockFuncParamName) or
                CheckChar(idlStartMultiLineComment,psMultiLineComment,ParseState) or
                CheckChar(idlSeparateFuncParams,psInterfaceBlockFuncParams) or
                CheckChar(idlEndFuncParams,psInterfaceAfterFuncParams)) then
        inc(pCurrent)
        end;
      psConstValue:
        begin
        if not (CheckChar(idlStartMultiLineComment,psMultiLineComment,ParseState) or
                CheckChar(idlStartSingleLineComment,psSingleLineComment,ParseState) or
                CheckEndConst) then
        inc(pCurrent)
        end;
      psWord:
        begin
        if not CheckEndWord(PriorState) then
          inc(pCurrent);
        end;
      end;
    end;
end;

{ TIDLMember }

constructor TIDLMember.Create;
begin
  FParams := TIDLMemberParameterList.create;
end;

destructor TIDLMember.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

{ TIDL }

constructor TIDL.Create;
begin
  Fmembers := TIDLMemberList.create;
end;

destructor TIDL.Destroy;
begin
  Fmembers.free;
  inherited Destroy;
end;

end.

