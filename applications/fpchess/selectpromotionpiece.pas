unit selectPromotionPiece;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, chessConfig, chessgame;

type

  { TformPromotion }

  TformPromotion = class(TForm)
    imageRook: TImage;
    imageKnight: TImage;
    imageBishop: TImage;
    imageQueen: TImage;
    labelSelectPiece: TLabel;
    procedure FormShow(Sender: TObject);
    procedure imageBishopClick(Sender: TObject);
    procedure imageKnightClick(Sender: TObject);
    procedure imageQueenClick(Sender: TObject);
    procedure imageRookClick(Sender: TObject);
  private
    { private declarations }
    imageUrls : Array [1..4] of String;
    playerToMove : boolean; //true=white; false=black
  public
    { public declarations }
    constructor Create(playeriswhite: boolean);
  end; 

var
  formPromotion: TformPromotion;
  pieceChosen  : chessgame.Tchesstile;

implementation

{$R *.lfm}

{ TformPromotion }

procedure TformPromotion.FormShow(Sender: TObject);
begin
  imageRook.Picture.LoadFromFile(imageUrls[1]);
  imageKnight.Picture.LoadFromFile(imageUrls[2]);
  imageBishop.Picture.LoadFromFile(imageUrls[3]);
  imageQueen.Picture.LoadFromFile(imageUrls[4]);
end;

procedure TformPromotion.imageBishopClick(Sender: TObject);
begin
  if playerToMove then
    pieceChosen:= ctWBishop
  else
    pieceChosen:=ctBBishop;
  ModalResult:=mrOK;
end;

procedure TformPromotion.imageKnightClick(Sender: TObject);
begin
  if playerToMove then
    pieceChosen:= ctWKnight
  else
    pieceChosen:=ctBKnight;
  ModalResult:=mrOK;
end;

procedure TformPromotion.imageQueenClick(Sender: TObject);
begin
  if playerToMove then
    pieceChosen:= ctWQueen
  else
    pieceChosen:=ctBQueen;
  ModalResult:=mrOK;
end;

procedure TformPromotion.imageRookClick(Sender: TObject);
begin
  if playerToMove then
    pieceChosen:= ctWRook
  else
    pieceChosen:=ctBRook;
  ModalResult:=mrOK;
end;

constructor TformPromotion.Create(playeriswhite: boolean);
var
  dir : String;
begin
  inherited Create(nil);

  dir :=vChessConfig.GetCurrentSkinDir;
  playerToMove:=playeriswhite;
  if playeriswhite then
  begin
    imageUrls[1]:= dir + 'wrook.png';
    imageUrls[2]:= dir + 'wknight.png';
    imageUrls[3]:= dir + 'wbishop.png';
    imageUrls[4]:= dir + 'wqueen.png';
  end
  else
  begin
    imageUrls[1]:= dir + 'brook.png';
    imageUrls[2]:= dir + 'bknight.png';
    imageUrls[3]:= dir + 'bbishop.png';
    imageUrls[4]:= dir + 'bqueen.png';
  end;
end;

end.

