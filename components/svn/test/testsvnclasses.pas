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
    AssertEquals('Wrong repository root',
      'svn+ssh://www.freepascal.org/FPC/svn/lazarus',
      SvnInfo.Entry.Repository.Root);
    AssertEquals('Wrong repository UUID',
      '4005530d-fff6-0310-9dd1-cebe43e6787f',
      SvnInfo.Entry.Repository.UUID);
    AssertEquals('Wrong commit revision', 10680, SvnInfo.Entry.Commit.Revision);
    AssertEquals('Wrong commit author', 'jesus', SvnInfo.Entry.Commit.Author);
    AssertEquals('Wrong commit date',
      '2007-02-25T22:55:08.029980Z', SvnInfo.Entry.Commit.Date);
  finally
    SvnInfo.Free;
  end;
end;

initialization

  RegisterTest(TTestSvnClasses); 
end.

