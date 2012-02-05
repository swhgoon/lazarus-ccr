program fpchess;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lnetbase, mainform, chessdrawer, chessgame, chessconfig,
  chesstcputils, chessmodules, mod_samecomputer, mod_fics, mod_kcchess, 
selectPromotionPiece, ltelnetex, winboardConn, mod_winboard
  {$ifdef FPCHESS_WEBSERVICES}
  ,IDelphiChess_Intf
  {$endif};

//{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformChess, formChess);
  Application.CreateForm(TformPromotion, formPromotion);
  Application.Run;
end.

