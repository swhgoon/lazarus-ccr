program lazedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, 
lazedit_translations {,newtableform, test_ed, eplus_commons, CopyLeft, eplus_config, HtmlCharMap,
  HtmlDialogs, NewHtmlDlgForm, AnchorDlgForm, PictureDlgForm}
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TEPlusForm, EPlusForm);
  Application.Run;
end.

