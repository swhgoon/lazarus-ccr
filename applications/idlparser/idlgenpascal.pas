unit idlGenPascal;

{ Unit which generates a pascal source file from a TIDLList struct.

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
  Classes, SysUtils, strutils,
  idlParser;

procedure GeneratePascalSource(const AnIdlList: TIDLList; const PascalCode: tstrings;TypeConvList, CTypesList: TStrings; AlwaysAddPrefixToParam: boolean; AForwardDeclList: TStrings = nil);

implementation

function CTypeToPascalType(AValue: string; unsigned: boolean; TypeConvList, CTypesList: TStrings): string;
begin
  if TypeConvList.Values[AValue]<>'' then
    result := TypeConvList.Values[AValue]
  else if CTypesList.IndexOf(AValue) > -1 then
    begin
    result := 'c';
    if unsigned then result := Result + 'u';
    result := result+AValue;
    end
  else
    result := AValue;
end;


function HasDoubleIdentifier(const AnIDL: TIDL; AValue: string): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to AnIDL.members.Count-1 do
    if sametext((AnIDL.members.Items[i] as TIDLMember).MemberName, AValue) then
      begin
      result := true;
      break;
      end;
end;

function CValueToPascalValue(AValue: string) : string;
begin
  AValue := trim(AValue);
  if copy(AValue,1,2)='0x' then
    result := '$'+copy(AValue,3,16)
  else
    begin
    if (pos('''',AValue)<0) and (pos('"',AValue)<0) then
      // the constant does not contain any strings
      begin
      AValue := StringsReplace(AValue,['<<','>>'],['shl','shr'],[rfReplaceAll]);
      end;
    result := AValue;
    end
end;

function IdentifierNameToPascalName(AName: string) : string;
begin
  case lowercase(AName) of
    'type': result := 'a'+AName;
    'end' : result := 'an'+AName;
    'implementation' : result := 'an'+AName;
    'set' : result := 'a'+AName;
  else
    result := AName;
  end;
end;

procedure GeneratePascalSource(const AnIdlList: TIDLList; const PascalCode: tstrings;TypeConvList, CTypesList: TStrings; AlwaysAddPrefixToParam: boolean; AForwardDeclList: TStrings = nil);

var
  i,l,m: integer;
  anIDL: TIDL;
  anIDLMember: TIDLMember;
  anIDLMemberParameter: TIDLMemberParameter;
  s: string;
  Consts: string;
  ml: boolean;
  AParamName: string;
  PasType: string;

begin
  PascalCode.add('type');
  for i := 0 to AnIdlList.Count-1 do
    begin
    ml := False;
    consts := '';
    anIDL := TIDL(AnIdlList.Items[i]);
    s := '  ' + anIDL.InterfaceName + ' = interface';
    if anIDL.InterfaceType<>'' then
      s := s + '(' + anIDL.InterfaceType + ')';
    if anIDL.UUID<>'' then
      begin
      s := s + LineEnding + '  [''{' + AnIDL.uuid + '}'']' + LineEnding;
      ml := true;
      end;
    if anIDL.members.Count>0 then
      begin
      ml := true;
      for l := 0 to anIDL.members.Count-1 do
        begin
        anIDLMember := TIDLMember(anIDL.members.Items[l]);
        if anIDLMember.MemberType=mtFunc then
          begin
          if anIDLMember.ReturnType = 'void' then
            s := s + '    procedure '
          else
            s := s + '    function ';
          s := s + IdentifierNameToPascalName(anIDLMember.MemberName) + '(';
          for m := 0 to anIDLMember.Params.Count-1 do
            begin
            anIDLMemberParameter := (anIDLMember.Params.Items[m]) as TIDLMemberParameter;
            AParamName := IdentifierNameToPascalName(anIDLMemberParameter.ParamName);
            if AlwaysAddPrefixToParam or HasDoubleIdentifier(anIDL,AParamName) then // It could be that the name is used in a inherited class
              begin
              if AParamName[1] in ['a','e','o','u','i'] then
                AParamName := 'an'+AParamName
              else
                AParamName := 'a'+AParamName;
              end;

            if m > 0 then s := s + '; ';
            if anIDLMemberParameter.ParamInOutType=piOut then
              s := s + 'out ';
            s := s + AParamName +': ' + CTypeToPascalType(anIDLMemberParameter.ParamType,anIDLMemberParameter.ParamTypeUnsigned,TypeConvList,CTypesList);
            end;
          s := s + ')';
          if anIDLMember.ReturnType <> 'void' then
            s := s + ' : '+ CTypeToPascalType(anIDLMember.ReturnType,anIDLMember.ReturnTypeUnsigned,TypeConvList,CTypesList);
          s := s + '; safecall;'+ LineEnding
          end
        else if anIDLMember.MemberType=mtAttribute then
          begin
          PasType:= CTypeToPascalType(anIDLMember.ReturnType, anIDLMember.ReturnTypeUnsigned,TypeConvList,CTypesList);
          s := s + '    function Get' +anIDLMember.MemberName + '(): ' + PasType + '; safecall;' + LineEnding;
          if not anIDLMember.MemberReadonly then
            s := s + '    procedure Set' +anIDLMember.MemberName + '(a'+anIDLMember.MemberName+': '+ PasType+'); safecall;' + LineEnding;

          s := s + '    property ' +IdentifierNameToPascalName(anIDLMember.MemberName)+  ' : '+PasType+
               ' read Get' +anIDLMember.MemberName;
          if not anIDLMember.MemberReadonly then
            s := s + ' write Set' +anIDLMember.MemberName;
          s := s + ';' +LineEnding;

          end
        else
          Consts:=Consts + '  ' + anIDL.InterfaceName +'_'+ anIDLMember.MemberName + '=' + CValueToPascalValue(anIDLMember.ConstValue) + ';'+LineEnding;
        end;
      end;


    if ml then
      s := LineEnding + s + LineEnding+'  end;' + LineEnding
    else
      s := s + ';';

    if assigned(AForwardDeclList) and (anIDL.InterfaceType='') then
      begin
      if AForwardDeclList.Count=0 then
        AForwardDeclList.Add('type');
      AForwardDeclList.Add(s);
      end
    else
      PascalCode.Add(s);

    if consts<>'' then
      begin
      PascalCode.Add('const');
      PascalCode.Add(Consts);
      end;

    end;

end;


end.

