unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, Spin,
  //
  chessdrawer, chessgame, chessconfig, chesstcputils;

type

  { TFormDrawerDelegate }

  TFormDrawerDelegate = class(TChessDrawerDelegate)
  public
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer); override;
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer); override;
  end;

  { TformChess }

  TformChess = class(TForm)
    btnConnect: TBitBtn;
    btnWebservice: TBitBtn;
    btnSinglePlayer: TBitBtn;
    btnDirectComm: TBitBtn;
    BitBtn3: TBitBtn;
    btnHotSeat: TBitBtn;
    Button1: TButton;
    checkTimer: TCheckBox;
    comboStartColor: TComboBox;
    editWebserviceURL: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    labelPos: TLabel;
    editRemoteID: TLabeledEdit;
    editLocalIP: TLabeledEdit;
    editPlayerName: TLabeledEdit;
    pageStart: TUNBPage;
    pageConfigConnection: TUNBPage;
    notebookMain: TUntabbedNotebook;
    pageConnecting: TUNBPage;
    ProgressBar1: TProgressBar;
    pageGame: TUNBPage;
    spinPlayerTime: TSpinEdit;
    timerChessTimer: TTimer;
    pageWebservice: TUNBPage;
    procedure btnConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HandleMainScreenButton(Sender: TObject);
    procedure pageBeforeShow(Sender: TObject; ANewPage: TUNBPage; ANewIndex: Integer);
    procedure timerChessTimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure UpdateCaptions;
  end; 

var
  formChess: TformChess;
  vFormDrawerDelegate: TFormDrawerDelegate;

implementation

{$R *.lfm}

const
  INT_PAGE_START = 0;
  INT_PAGE_CONFIGCONNECTION = 1;
  INT_PAGE_CONNECTING = 2;
  INT_PAGE_GAME = 3;

{ TformChess }

procedure TformChess.HandleMainScreenButton(Sender: TObject);
begin
  if Sender = btnSinglePlayer then
  begin
    notebookMain.PageIndex := INT_PAGE_GAME;
    vChessGame.StartNewGame(comboStartColor.ItemIndex, checkTimer.Checked, spinPlayerTime.Value);
  end
  else if Sender = btnHotSeat then
  begin
    notebookMain.PageIndex := INT_PAGE_GAME;
    vChessGame.StartNewGame(comboStartColor.ItemIndex, checkTimer.Checked, spinPlayerTime.Value);
  end
  else if Sender = btnDirectComm then notebookMain.PageIndex := INT_PAGE_CONFIGCONNECTION;
end;

procedure TformChess.pageBeforeShow(Sender: TObject; ANewPage: TUNBPage; ANewIndex: Integer);
begin
  if ANewIndex = INT_PAGE_CONFIGCONNECTION then
  begin
    editLocalIP.Text := ChessGetLocalIP();
  end;
end;

procedure TformChess.timerChessTimerTimer(Sender: TObject);
begin
  vChessGame.UpdateTimes();
  UpdateCaptions();
end;

procedure TformChess.UpdateCaptions;
var
  lStr: string;
begin
  if vChessGame.CurrentPlayerIsWhite then lStr := 'White playing'
  else lStr := 'Black playing';

  lStr := lStr + Format(' X: %d Y: %d',
    [vChessGame.MouseMovePos.X, vChessGame.MouseMovePos.Y]);

  lStr := lStr + Format(' White time: %d Black time: %d',
    [vChessGame.WhitePlayerTime, vChessGame.BlackPlayerTime]);

  formChess.labelPos.Caption := lStr;
end;

procedure TformChess.FormCreate(Sender: TObject);
begin
  // Creation of internal components
  vChessDrawer := TChessDrawer.Create(Self);
  vChessDrawer.Parent := pageGame;
  vChessDrawer.Top := 50;
  vChessDrawer.Left := 20;
  vChessDrawer.Height := INT_CHESSBOARD_SIZE;
  vChessDrawer.Width := INT_CHESSBOARD_SIZE;
  vChessDrawer.SetDelegate(vFormDrawerDelegate);

  // Loading of resources
  vChessDrawer.LoadImages();
end;

procedure TformChess.btnConnectClick(Sender: TObject);
begin
  notebookMain.PageIndex := INT_PAGE_CONNECTING;

end;

{ TFormDrawerDelegate }

procedure TFormDrawerDelegate.HandleMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  vChessGame.MouseMovePos := vChessGame.ClientToBoardCoords(Point(X, Y));
  formChess.UpdateCaptions;
end;

procedure TFormDrawerDelegate.HandleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lCoords: TPoint;
begin
  vChessGame.Dragging := False;

  lCoords := vChessGame.ClientToBoardCoords(Point(X, Y));
  if not vChessGame.MovePiece(vChessGame.DragStart, lCoords) then Exit;

  vChessDrawer.Invalidate;
  formChess.UpdateCaptions;
end;

procedure TFormDrawerDelegate.HandleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lCoords: TPoint;
begin
  lCoords := vChessGame.ClientToBoardCoords(Point(X, Y));
  if not vChessGame.CheckStartMove(lCoords) then Exit;

  vChessGame.Dragging := True;
  vChessGame.DragStart := lCoords;
  vChessDrawer.Invalidate;
  formChess.UpdateCaptions;
end;

initialization

  vFormDrawerDelegate := TFormDrawerDelegate.Create;

finalization

  vFormDrawerDelegate.Free;

end.

