unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Buttons,
  //
  chessdrawer, chessgame, chessconfig;

type

  { TformChess }

  TformChess = class(TForm)
    BitBtn1: TBitBtn;
    btnSinglePlayer: TBitBtn;
    btnDirectComm: TBitBtn;
    BitBtn3: TBitBtn;
    comboStartColor: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    editPlayerName: TLabeledEdit;
    pageStart: TUNBPage;
    pageConfigConnection: TUNBPage;
    notebookMain: TUntabbedNotebook;
    pageConnecting: TUNBPage;
    ProgressBar1: TProgressBar;
    pageGame: TUNBPage;
    procedure FormCreate(Sender: TObject);
    procedure HandleMainScreenButton(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  formChess: TformChess;

implementation

{$R *.lfm}

{ TformChess }

procedure TformChess.HandleMainScreenButton(Sender: TObject);
begin
  if Sender = btnSinglePlayer then
  begin
    notebookMain.PageIndex := 2;
    vChessGame.StartNewGame(comboStartColor.ItemIndex);
  end
  else if Sender = btnDirectComm then notebookMain.PageIndex := 1;
end;

procedure TformChess.FormCreate(Sender: TObject);
begin
  // Creation of internal components
  vChessDrawer := TChessDrawer.Create(Self);
  vChessDrawer.Parent := pageGame;
  vChessDrawer.Top := 20;
  vChessDrawer.Left := 20;
  vChessDrawer.Height := INT_CHESSBOARD_SIZE;
  vChessDrawer.Width := INT_CHESSBOARD_SIZE;

  // Loading of resources
  vChessDrawer.LoadImages();
end;

end.

