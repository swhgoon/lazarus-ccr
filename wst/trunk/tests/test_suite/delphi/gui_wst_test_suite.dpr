program gui_wst_test_suite;

uses
  TestFramework,
  Forms,
  GUITestRunner,
  TextTestRunner,
  delphi_init_com,
  testmetadata_unit in '..\testmetadata_unit.pas',
  test_parsers in '..\test_parsers.pas',
  test_support in '..\test_support.pas',
  test_utilities in '..\test_utilities.pas',
  testformatter_unit in '..\testformatter_unit.pas',
  base_service_intf in '..\..\..\base_service_intf.pas',
  basex_encode in '..\..\..\basex_encode.pas',
  test_basex_encode in '..\test_basex_encode.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
