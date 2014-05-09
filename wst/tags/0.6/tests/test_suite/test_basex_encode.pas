{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_basex_encode;

interface
uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  TypInfo,
  wst_types, test_suite_utils, basex_encode;

type

  { TTest_Base64 }

  TTest_Base64 = class(TWstBaseTest)
  protected
    procedure Check_Encode(const AIn, AExpect : string);
    procedure Check_Decode(const AIn, AExpect : string; const AOptions : TBaseXOptions = [xoDecodeIgnoreIllegalChar]);
  published
    procedure Encode_empty();
    procedure Encode_f();
    procedure Encode_fo();
    procedure Encode_foo();
    procedure Encode_foob();
    procedure Encode_fooba();
    procedure Encode_foobar();

    procedure Decode_f();
    procedure Decode_fo();
    procedure Decode_foo();
    procedure Decode_foob();
    procedure Decode_fooba();
    procedure Decode_foobar();
    procedure Decode_illegal_char();
    procedure Decode_empty();
  end;
  
  { TTest_Base32 }

  TTest_Base32 = class(TWstBaseTest)
  protected
    procedure Check_Encode(const AIn, AExpect : string);
    procedure Check_Decode(const AIn, AExpect : string; const AOptions : TBaseXOptions = [xoDecodeIgnoreIllegalChar]);
  published
    procedure Encode_empty();
    procedure Encode_f();
    procedure Encode_fo();
    procedure Encode_foo();
    procedure Encode_foob();
    procedure Encode_fooba();
    procedure Encode_foobar();

    procedure Decode_f();
    procedure Decode_fo();
    procedure Decode_foo();
    procedure Decode_foob();
    procedure Decode_fooba();
    procedure Decode_foobar();
    procedure Decode_illegal_char();
    procedure Decode_empty();
  end;   

  TTest_Base16 = class(TWstBaseTest)
  protected
    procedure Check_Encode(const AIn, AExpect : string);
    procedure Check_Decode(const AIn, AExpect : string; const AOptions : TBaseXOptions = [xoDecodeIgnoreIllegalChar]);
  published
    procedure Encode_empty();
    procedure Encode_f();
    procedure Encode_fo();
    procedure Encode_foo();
    procedure Encode_foob();
    procedure Encode_fooba();
    procedure Encode_foobar();

    procedure Decode_f();
    procedure Decode_fo();
    procedure Decode_foo();
    procedure Decode_foob();
    procedure Decode_fooba();
    procedure Decode_foobar();
    procedure Decode_illegal_char(); 
  end;

implementation

{ TTest_Base64 }

procedure TTest_Base64.Check_Decode(const AIn, AExpect: string; const AOptions : TBaseXOptions);
var
  locRes : TByteDynArray;
begin
  locRes := Base64Decode(AIn,AOptions);
  CheckEquals(StringToByteArray(AExpect),locRes);
end;

procedure TTest_Base64.Check_Encode(const AIn, AExpect: string);
var
  locRes : string;
begin
  locRes := Base64Encode(AIn);
  CheckEquals(AExpect,locRes);
end;

procedure TTest_Base64.Decode_f();
begin
  Check_Decode('Zg==','f');
end;

procedure TTest_Base64.Decode_fo();
begin
  Check_Decode('Zm8=','fo');
end;

procedure TTest_Base64.Decode_foo();
begin
  Check_Decode('Zm9v','foo');
end;

procedure TTest_Base64.Decode_foob();
begin
  Check_Decode('Zm9vYg==','foob');
end;

procedure TTest_Base64.Decode_fooba();
begin
  Check_Decode('Zm9vYmE=','fooba');
end;

procedure TTest_Base64.Decode_foobar();
begin
  Check_Decode('Zm9vYmFy','foobar');
end;

procedure TTest_Base64.Decode_illegal_char();
var
  ok : Boolean;
begin
  ok := False;
  try
    Check_Decode('Zm9'#200'vY' + sLineBreak + 'm'#0'Fy','foobar',[]);
  except
    on e : EBase64Exception do
      ok := True;
  end;
  CheckEquals(True,ok);

  Check_Decode('Zm9'#200'vY' + sLineBreak + 'm'#0'Fy','foobar',[xoDecodeIgnoreIllegalChar]);
  Check_Decode('Zm9'#200'vY' + sLineBreak + 'm'#0'Fy' + sLineBreak,'foobar',[xoDecodeIgnoreIllegalChar]);
  Check_Decode('Zm9'#200'vY' + sLineBreak + 'm'#0'Fy' + sLineBreak + sLineBreak,'foobar',[xoDecodeIgnoreIllegalChar]);
end;

procedure TTest_Base64.Decode_empty();
var
  ok : Boolean;
begin
  ok := False;
  try
    Check_Decode(sLineBreak,'',[]);
  except
    on e : EBase64Exception do
      ok := True;
  end;
  CheckEquals(True,ok);

  Check_Decode('','',[]);
  Check_Decode(#0,'',[xoDecodeIgnoreIllegalChar]);
  Check_Decode(sLineBreak,'',[xoDecodeIgnoreIllegalChar]);
  Check_Decode(sLineBreak + sLineBreak,'',[xoDecodeIgnoreIllegalChar]);
  Check_Decode(sLineBreak + sLineBreak + sLineBreak,'',[xoDecodeIgnoreIllegalChar]);
end;

procedure TTest_Base64.Encode_empty();
begin
  Check_Encode('','');
end;

procedure TTest_Base64.Encode_f();
begin
  Check_Encode('f','Zg==');
end;

procedure TTest_Base64.Encode_fo();
begin
  Check_Encode('fo','Zm8=');
end;

procedure TTest_Base64.Encode_foo();
begin
  Check_Encode('foo','Zm9v');
end;

procedure TTest_Base64.Encode_foob();
begin
  Check_Encode('foob','Zm9vYg==');
end;

procedure TTest_Base64.Encode_fooba();
begin
  Check_Encode('fooba','Zm9vYmE=');
end;

procedure TTest_Base64.Encode_foobar();
var
  a, b : string;
begin
  a := 'foobar';
  b := 'Zm9vYmFy';
  Check_Encode(a,b);
  //Check_Encode('foobar','Zm9vYmFy');
end;

{ TTest_Base32 }

procedure TTest_Base32.Check_Decode(const AIn, AExpect: string; const AOptions : TBaseXOptions);
var
  locRes : TByteDynArray;
begin
  locRes := Base32Decode(AIn,AOptions);
  CheckEquals(StringToByteArray(AExpect),locRes);
end;

procedure TTest_Base32.Check_Encode(const AIn, AExpect: string);
var
  locRes : string;
begin
  locRes := Base32Encode(AIn);
  CheckEquals(AExpect,locRes);
end;

procedure TTest_Base32.Decode_f();
begin
  Check_Decode('MY======','f');
end;

procedure TTest_Base32.Decode_fo();
begin
  Check_Decode('MZXQ====','fo');
end;

procedure TTest_Base32.Decode_foo();
begin
  Check_Decode('MZXW6===','foo');
end;

procedure TTest_Base32.Decode_foob();
begin
  Check_Decode('MZXW6YQ=','foob');
end;

procedure TTest_Base32.Decode_fooba();
begin
  Check_Decode('MZXW6YTB','fooba');
end;

procedure TTest_Base32.Decode_foobar();
begin
  Check_Decode('MZXW6YTBOI======','foobar');
end;

procedure TTest_Base32.Decode_illegal_char();
var
  ok : Boolean;
begin
  ok := False;
  try
    Check_Decode('MZX'#200'W6' + sLineBreak + 'Y'#0'TB','fooba',[]);
  except
    on e : EBase32Exception do
      ok := True;
  end;
  CheckEquals(True,ok);

  Check_Decode('MZX'#200'W6' + sLineBreak + 'Y'#0'TB','fooba',[xoDecodeIgnoreIllegalChar]);
  Check_Decode('MZX'#200'W6' + sLineBreak + 'Y'#0'TB' + sLineBreak,'fooba',[xoDecodeIgnoreIllegalChar]);
  Check_Decode('MZX'#200'W6' + sLineBreak + 'Y'#0'TB' + sLineBreak + sLineBreak,'fooba',[xoDecodeIgnoreIllegalChar]);
end;

procedure TTest_Base32.Decode_empty();
var
  ok : Boolean;
begin
  ok := False;
  try
    Check_Decode(sLineBreak,'',[]);
  except
    on e : EBase32Exception do
      ok := True;
  end;
  CheckEquals(True,ok);

  Check_Decode('','',[]);
  Check_Decode(#0,'',[xoDecodeIgnoreIllegalChar]);
  Check_Decode(sLineBreak,'',[xoDecodeIgnoreIllegalChar]);
  Check_Decode(sLineBreak + sLineBreak,'',[xoDecodeIgnoreIllegalChar]);
  Check_Decode(sLineBreak + sLineBreak + sLineBreak,'',[xoDecodeIgnoreIllegalChar]);
end;

procedure TTest_Base32.Encode_empty();
begin
  Check_Encode('','');
end;

procedure TTest_Base32.Encode_f();
begin
  Check_Encode('f','MY======');
end;

procedure TTest_Base32.Encode_fo();
begin
  Check_Encode('fo','MZXQ====');
end;

procedure TTest_Base32.Encode_foo();
begin
  Check_Encode('foo','MZXW6===');
end;

procedure TTest_Base32.Encode_foob();
begin
  Check_Encode('foob','MZXW6YQ=');
end;

procedure TTest_Base32.Encode_fooba();
begin
  Check_Encode('fooba','MZXW6YTB');
end;

procedure TTest_Base32.Encode_foobar();
var
  a, b : string;
begin
  a := 'foobar';
  b := 'MZXW6YTBOI======';
  Check_Encode(a,b);
  //Check_Encode('foobar','Zm9vYmFy');
end;

{ TTest_Base16 }

procedure TTest_Base16.Check_Decode(const AIn, AExpect: string; const AOptions: TBaseXOptions);
var
  locRes : TByteDynArray;
begin
  locRes := Base16Decode(AIn,AOptions);
  CheckEquals(StringToByteArray(AExpect),locRes);
end;

procedure TTest_Base16.Check_Encode(const AIn, AExpect: string);
var
  locRes : string;
begin
  locRes := Base16Encode(AIn);
  CheckEquals(AExpect,locRes);
end;

procedure TTest_Base16.Decode_f();
begin
  Check_Decode('66','f');
end;

procedure TTest_Base16.Decode_fo();
begin
  Check_Decode('666F','fo');
end;

procedure TTest_Base16.Decode_foo();
begin
  Check_Decode('666F6F','foo');
end;

procedure TTest_Base16.Decode_foob();
begin
  Check_Decode('666F6F62','foob');
end;

procedure TTest_Base16.Decode_fooba();
begin
  Check_Decode('666F6F6261','fooba');
end;

procedure TTest_Base16.Decode_foobar();
begin
  Check_Decode('666F6F626172','foobar');
end;

procedure TTest_Base16.Decode_illegal_char();
var
  ok : Boolean;
begin
  ok := False;
  try
    Check_Decode('666'#200'F6F' + sLineBreak + '6'#1'26172','foobar',[]);
  except
    on e : EBase16Exception do
      ok := True;
  end;
  CheckEquals(True,ok);

  Check_Decode('666'#200'F6F' + sLineBreak + '6'#1'26172','foobar',[xoDecodeIgnoreIllegalChar]);
end;

procedure TTest_Base16.Encode_empty();
begin
  Check_Encode('','');
end;

procedure TTest_Base16.Encode_f();
begin
  Check_Encode('f','66');
end;

procedure TTest_Base16.Encode_fo();
begin
  Check_Encode('fo','666F');
end;

procedure TTest_Base16.Encode_foo();
begin
  Check_Encode('foo','666F6F');
end;

procedure TTest_Base16.Encode_foob();
begin
  Check_Encode('foob','666F6F62');
end;

procedure TTest_Base16.Encode_fooba();
begin
  Check_Encode('fooba','666F6F6261');
end;

procedure TTest_Base16.Encode_foobar();
begin
  Check_Encode('foobar','666F6F626172');
end;

initialization
  RegisterTest('Encoding',TTest_Base64.Suite);
  RegisterTest('Encoding',TTest_Base32.Suite);    
  RegisterTest('Encoding',TTest_Base16.Suite);

end.
