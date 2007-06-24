program typ_lib_edtr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  , umain, view_helper, source_utils, uabout, ufEnumedit,
  edit_helper, ufclassedit, wsdl_generator, ufpropedit, uinterfaceedit, udm,
  pascal_parser_intf, PasTree, PParser;

begin
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

