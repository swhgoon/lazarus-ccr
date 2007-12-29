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
  wst_types, basex_encode;

type

  TTest_Base64 = class(TTestCase)
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
  locRes : string;
begin
  locRes := Base64Decode(AIn,AOptions);
  CheckEquals(AExpect,locRes);
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

initialization
  RegisterTest('Encoding',TTest_Base64.Suite);

end.
