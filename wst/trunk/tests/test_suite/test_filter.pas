{   This file is part of the Web Service Toolkit
    Copyright (c) 2011 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}  

unit test_filter; 

interface   
uses 
  SysUtils, 
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  wst_types, filter_intf, base_service_intf;

type
  
  { TTest_DataFilter }

  TTest_DataFilter = class(TTestCase)
  protected
    procedure DoFilter(AFilter : IDataFilter); 
    function CreateRegistry() : IDataFilterRegistry;
  published
    procedure one_filter();
    procedure two_filter();
    procedure three_filter();
    
    procedure ParseDataFilterString_empty();
    procedure ParseDataFilterString_one();
    procedure ParseDataFilterString_two();
    procedure ParseDataFilterString_three();
    
    procedure GetFilterString_empty();
    procedure GetFilterString_one();
    procedure GetFilterString_two();
    procedure GetFilterString_three();
  end;

  { THexDataFilter }

  THexDataFilter = class(TBaseFilter,IDataFilter)
  protected
    function GetName() : string;override;
    function DoExecuteInput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;override;
    function DoExecuteOutput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;override;
  end;            

  { TMod256DataFilter }

  TMod256DataFilter = class(TBaseFilter,IDataFilter)
  protected
    function GetName() : string;override;
    function DoExecuteInput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;override;
    function DoExecuteOutput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;override;
  end;     

  { TXorDataFilter }

  TXorDataFilter = class(TBaseFilter,IDataFilter)
  private
    FXorConstant : Byte;
  protected
    function GetName() : string;override;
    function ExecuteXor(const AData; const ASize : Integer) : TByteDynArray;
    function DoExecuteInput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;override;
    function DoExecuteOutput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;override;
  published
    property XorConstant : Byte read FXorConstant write FXorConstant;
  end;                      
  
implementation
uses
  Classes,
  wst_consts, basex_encode, imp_utils;

       
{ TXorDataFilter }

function TXorDataFilter.GetName() : string;  
begin
  Result := 'xor';
end;

function TXorDataFilter.ExecuteXor(const AData; const ASize : Integer) : TByteDynArray;  
var
  i : Integer;
  pd, pr : PByte;
begin
  SetLength(Result,ASize);
  pd := PByte(@AData);
  pr := @Result[Low(Result)];
  for i := 1 to ASize do begin
    pr^ := pd^ xor XorConstant;
    Inc(pr);
    Inc(pd);
  end;
end;

function TXorDataFilter.DoExecuteInput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
begin
  Result := ExecuteXor(AData,ASize);
end;

function TXorDataFilter.DoExecuteOutput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
begin                               
  Result := ExecuteXor(AData,ASize);
end;

{ TTest_DataFilter }

procedure TTest_DataFilter.DoFilter(AFilter : IDataFilter); 
var
  f : IDataFilter;
  inBuffer, outBuffer, calcBuffer : TByteDynArray;
  s, s2 : ansistring;
begin
  s := 'abcdefghijklmnopqrtsuvwxyz0123456789.,;:+/*-=';
  SetLength(inBuffer,Length(s));
  Move(s[1],inBuffer[0],Length(inBuffer));
  
  f := AFilter;
  outBuffer := f.ExecuteInput(inBuffer[0],Length(inBuffer),nil);
    Check(Length(outBuffer) > 0);
  calcBuffer := f.ExecuteOutput(outBuffer[0],Length(outBuffer),nil);
    Check(Length(calcBuffer) > 0);
  SetLength(s2,Length(calcBuffer));
  Move(calcBuffer[0],s2[1],Length(calcBuffer));
  CheckEquals(s,s2);   
end;

function TTest_DataFilter.CreateRegistry() : IDataFilterRegistry; 
begin
  Result := TDataFilterRegistry.Create() as IDataFilterRegistry;
  Result.Register('xor',TSimpleItemFactory.Create(TXorDataFilter) as IItemFactory);
  Result.Register('mod256',TSimpleItemFactory.Create(TMod256DataFilter) as IItemFactory);
  Result.Register('hex',TSimpleItemFactory.Create(THexDataFilter) as IItemFactory);
end;

procedure TTest_DataFilter.one_filter(); 
begin
  DoFilter(THexDataFilter.Create() as IDataFilter);
  DoFilter(TMod256DataFilter.Create() as IDataFilter);
  DoFilter(TXorDataFilter.Create() as IDataFilter);
end;

procedure TTest_DataFilter.two_filter(); 
var
  f : IDataFilter;
begin
  f := THexDataFilter.Create() as IDataFilter;
  f.SetNext(TMod256DataFilter.Create() as IDataFilter);
  DoFilter(f);
  
  f := TXorDataFilter.Create() as IDataFilter;
  f.SetNext(THexDataFilter.Create() as IDataFilter);
  DoFilter(f);
end;

procedure TTest_DataFilter.three_filter(); 
var
  f : IDataFilter;
begin
  f := THexDataFilter.Create() as IDataFilter;
  f.SetNext(TMod256DataFilter.Create() as IDataFilter);
  f.GetNext().SetNext(TXorDataFilter.Create() as IDataFilter);
  DoFilter(f);
  
  f := TXorDataFilter.Create() as IDataFilter;
  f.SetNext(THexDataFilter.Create() as IDataFilter);
  f.GetNext().SetNext(TMod256DataFilter.Create() as IDataFilter);
  DoFilter(f);                           
end;

procedure TTest_DataFilter.ParseDataFilterString_empty(); 
var
  fr : IDataFilterRegistry;
  f : IDataFilter;
begin
  fr := CreateRegistry();
  f := ParseDataFilterString('',fr);
  CheckNull(f);
end;

procedure TTest_DataFilter.ParseDataFilterString_one();   
var
  fr : IDataFilterRegistry;
  f : IDataFilter;
begin
  fr := CreateRegistry();
  
  f := ParseDataFilterString('hex',fr);
  CheckNotNull(f);
  CheckNull(f.GetNext());
  CheckEquals('hex',f.GetName());
  
  f := ParseDataFilterString('xor,XorConstant>17',fr);
  CheckNotNull(f);
  CheckNull(f.GetNext());
  CheckEquals('xor',f.GetName());
  CheckEquals('17',f.GetPropertyManager().GetProperty('XorConstant'))
end;

procedure TTest_DataFilter.ParseDataFilterString_two();              
var
  fr : IDataFilterRegistry;
  f : IDataFilter;
begin
  fr := CreateRegistry();
  
  f := ParseDataFilterString('hex&mod256',fr);
  CheckNotNull(f);
  CheckEquals('hex',f.GetName());
  CheckNotNull(f.GetNext());
  CheckEquals('mod256',f.GetNext().GetName());
  CheckNull(f.GetNext().GetNext());
  
  f := ParseDataFilterString('xor,XorConstant>17&hex',fr);
  CheckNotNull(f);
  CheckEquals('xor',f.GetName());
  CheckEquals('17',f.GetPropertyManager().GetProperty('XorConstant'));
  CheckNotNull(f.GetNext());
  CheckEquals('hex',f.GetNext().GetName());
  CheckNull(f.GetNext().GetNext());   
  
  f := ParseDataFilterString('hex&xor,XorConstant>17',fr);
  CheckNotNull(f);
  CheckEquals('hex',f.GetName());
  CheckNotNull(f.GetNext());
  CheckEquals('xor',f.GetNext().GetName());
  CheckEquals('17',f.GetNext().GetPropertyManager().GetProperty('XorConstant'));
  CheckNull(f.GetNext().GetNext());   
end;

procedure TTest_DataFilter.ParseDataFilterString_three(); 
var
  fr : IDataFilterRegistry;
  f : IDataFilter;
begin
  fr := CreateRegistry();
  
  f := ParseDataFilterString('hex&mod256&xor,XorConstant>17',fr);
  CheckNotNull(f);
  CheckEquals('hex',f.GetName());
  CheckNotNull(f.GetNext());
  CheckEquals('mod256',f.GetNext().GetName());
  CheckNotNull(f.GetNext().GetNext());
  CheckEquals('xor',f.GetNext().GetNext().GetName());
  CheckEquals('17',f.GetNext().GetNext().GetPropertyManager().GetProperty('XorConstant'));
  CheckNull(f.GetNext().GetNext().GetNext());   
  
  f := ParseDataFilterString('xor,XorConstant>17&hex&mod256',fr);
  CheckNotNull(f);
  CheckEquals('xor',f.GetName());
  CheckEquals('17',f.GetPropertyManager().GetProperty('XorConstant'));
  CheckNotNull(f.GetNext());
  CheckEquals('hex',f.GetNext().GetName());
  CheckNotNull(f.GetNext().GetNext());
  CheckEquals('mod256',f.GetNext().GetNext().GetName());
  CheckNull(f.GetNext().GetNext().GetNext());                 
  
  f := ParseDataFilterString('hex&xor,XorConstant>17&mod256',fr);
  CheckNotNull(f);
  CheckEquals('hex',f.GetName());
  CheckNotNull(f.GetNext());
  CheckEquals('xor',f.GetNext().GetName());
  CheckEquals('17',f.GetNext().GetPropertyManager().GetProperty('XorConstant'));  
  CheckNotNull(f.GetNext().GetNext());
  CheckEquals('mod256',f.GetNext().GetNext().GetName());
  CheckNull(f.GetNext().GetNext().GetNext());         
end;

procedure TTest_DataFilter.GetFilterString_empty(); 
begin
  CheckEquals('',GenerateFilterString(nil));
end;

procedure TTest_DataFilter.GetFilterString_one(); 
var
  f : IDataFilter;
begin
  f := THexDataFilter.Create() as IDataFilter;
  CheckEquals('hex',GenerateFilterString(f));
  
  f := TXorDataFilter.Create() as IDataFilter;
  f.GetPropertyManager().SetProperty('XorConstant','37');
  CheckEquals('xor,XorConstant>37',GenerateFilterString(f));
end;

procedure TTest_DataFilter.GetFilterString_two();      
var
  f : IDataFilter;
begin
  f := THexDataFilter.Create() as IDataFilter;
  f.SetNext(TMod256DataFilter.Create() as IDataFilter);
  CheckEquals('hex&mod256',GenerateFilterString(f));
  
  f := TXorDataFilter.Create() as IDataFilter;
  f.GetPropertyManager().SetProperty('XorConstant','37');
  f.SetNext(THexDataFilter.Create() as IDataFilter);
  CheckEquals('xor,XorConstant>37&hex',GenerateFilterString(f));
  
  f := THexDataFilter.Create() as IDataFilter;
  f.SetNext(TXorDataFilter.Create() as IDataFilter);
  f.GetNext().GetPropertyManager().SetProperty('XorConstant','37');
  CheckEquals('hex&xor,XorConstant>37',GenerateFilterString(f));
end;

procedure TTest_DataFilter.GetFilterString_three();        
var
  f : IDataFilter;
begin
  f := THexDataFilter.Create() as IDataFilter;
  f.SetNext(TMod256DataFilter.Create() as IDataFilter);
  f.GetNext().SetNext(TXorDataFilter.Create() as IDataFilter);
  f.GetNext().GetNext().GetPropertyManager().SetProperty('XorConstant','37');
  CheckEquals('hex&mod256&xor,XorConstant>37',GenerateFilterString(f));
  
  f := TXorDataFilter.Create() as IDataFilter;
  f.GetPropertyManager().SetProperty('XorConstant','37');
  f.SetNext(THexDataFilter.Create() as IDataFilter);   
  f.GetNext().SetNext(TMod256DataFilter.Create() as IDataFilter);
  CheckEquals('xor,XorConstant>37&hex&mod256',GenerateFilterString(f));
  
  f := THexDataFilter.Create() as IDataFilter;
  f.SetNext(TXorDataFilter.Create() as IDataFilter);
  f.GetNext().GetPropertyManager().SetProperty('XorConstant','37');  
  f.GetNext().SetNext(TMod256DataFilter.Create() as IDataFilter);
  CheckEquals('hex&xor,XorConstant>37&mod256',GenerateFilterString(f));
end;

{ TMod256DataFilter }

function TMod256DataFilter.GetName() : string;  
begin
  Result := 'mod256';
end;

function TMod256DataFilter.DoExecuteInput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
begin
  SetLength(Result,ASize + 1);
  Move(AData,Result[0],ASize);
  Result[ASize] := ASize mod 256;
end;

function TMod256DataFilter.DoExecuteOutput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
var
  m : Byte;
begin
  if (ASize < 1) then
    raise Exception.Create(SERR_InvalidEncodedData); 
  m := PByteArray(@AData)^[ASize - 1];
  if (m <> ((ASize - 1) mod 256) ) then
    raise Exception.Create(SERR_InvalidEncodedData); 
  
  SetLength(Result,ASize - 1);
  Move(AData,Result[0],ASize-1);
end;

{ THexDataFilter }

function THexDataFilter.GetName() : string;  
begin
  Result := 'hex';
end;

function THexDataFilter.DoExecuteInput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
begin
  if (ASize < 1) then begin
    Result := nil;
    exit;
  end;
  
  SetLength(Result,(2*ASize));
  Base16Encode(AData,ASize,PAnsiChar(@Result[0]));
end;

function THexDataFilter.DoExecuteOutput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
var
  i : Integer;
begin 
  if (ASize < 1) then begin
    Result := nil;
    exit;
  end;         
  
  SetLength(Result, (ASize div 2));
  i := Base16Decode(PAnsiChar(@AData),Result[0],Length(Result));
  if (i <> Length(Result)) then
    SetLength(Result,i);
end;

initialization
  RegisterTest('DataFilter',TTest_DataFilter.Suite);   
  
end.

