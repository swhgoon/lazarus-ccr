program fpchess;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lnetvisual, mainform, chessdrawer, chessgame, chessconfig, tcpcomm;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformChess, formChess);
  Application.Run;
end.

