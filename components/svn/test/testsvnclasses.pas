unit TestSvnClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  svnclasses;

type

  { TTestSvnClasses }

  TTestSvnClasses= class(TTestCase)
  private
    function GetInfoFileName: string;
  published
    procedure TestHookUp;
    procedure TestLoadInfo;
  end; 

implementation

function TTestSvnClasses.GetInfoFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'info.xml';
end;

procedure TTestSvnClasses.TestHookUp;
var
  InfoFileName: string;
begin
  InfoFileName := GetInfoFileName;
  AssertTrue(InfoFileName + ' does not exist', FileExists(InfoFileName));
end;

procedure TTestSvnClasses.TestLoadInfo;
var
  SvnInfo: TSvnInfo;
begin
  SvnInfo := TSvnInfo.Create;
  try
    SvnInfo.LoadFromFile(GetInfoFileName);
    AssertEquals('Wrong revision', 10685, SvnInfo.Entry.Revision);
    AssertEquals('Wrong path', '.', SvnInfo.Entry.Path);
    AssertEquals('Wrong kind', ord(ekDirectory), ord(SvnInfo.Entry.Kind));
    AssertEquals('Wrong URL',
      'svn+ssh://www.freepascal.org/FPC/svn/lazarus/trunk',
      SvnInfo.Entry.URL);
  finally
    SvnInfo.Free;
  end;
end;

initialization

  RegisterTest(TTestSvnClasses); 
end.

