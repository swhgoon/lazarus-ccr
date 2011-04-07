program fpchess;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, laz_synapse, mainform, chessdrawer, chessgame, chessconfig,
  chesstcputils, IDelphiChess_Intf, wst_synapse;

//{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformChess, formChess);
  Application.Run;
end.

