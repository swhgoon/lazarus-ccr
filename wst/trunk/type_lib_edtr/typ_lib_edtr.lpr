program typ_lib_edtr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  , uwsttypelibraryedit, view_helper, uabout, ufEnumedit,
  edit_helper, ufclassedit, ufpropedit, uinterfaceedit, udm,
  pascal_parser_intf, uprocedit, common_gui_utils, uargedit,
  umoduleedit, ubindingedit, ufrmsaveoption, ufarrayedit, generator,
  uftypealiasedit, ufrecordedit, wsdl_generator, xsd_parser;

begin
  Application.Initialize;
  Application.CreateForm(TfWstTypeLibraryEdit, fWstTypeLibraryEdit);
  Application.Run;
end.