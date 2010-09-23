unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, Spin,
  //
  chessdrawer, chessgame, chessconfig;

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
    BitBtn1: TBitBtn;
    btnSinglePlayer: TBitBtn;
    btnDirectComm: TBitBtn;
    BitBtn3: TBitBtn;
    btnHotSeat: TBitBtn;
    checkTimer: TCheckBox;
    comboStartColor: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    labelPos: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    editPlayerName: TLabeledEdit;
    pageStart: TUNBPage;
    pageConfigConnection: TUNBPage;
    notebookMain: TUntabbedNotebook;
    pageConnecting: TUNBPage;
    ProgressBar1: TProgressBar;
    pageGame: TUNBPage;
    spinPlayerTime: TSpinEdit;
    timerChessTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure HandleMainScreenButton(Sender: TObject);
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

{ TformChess }

procedure TformChess.HandleMainScreenButton(Sender: TObject);
begin
  if Sender = btnSinglePlayer then
  begin
    notebookMain.PageIndex := 3;
    vChessGame.StartNewGame(comboStartColor.ItemIndex, checkTimer.Checked, spinPlayerTime.Value);
  end
  else if Sender = btnHotSeat then
  begin
    notebookMain.PageIndex := 3;
    vChessGame.StartNewGame(comboStartColor.ItemIndex, checkTimer.Checked, spinPlayerTime.Value);
  end
  else if Sender = btnDirectComm then notebookMain.PageIndex := 1;
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

