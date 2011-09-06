{
}
unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, Spin,
  // fpchess
  chessdrawer, chessgame, chessconfig, chesstcputils,
  chessmodules, selectpromotionpiece;

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
    btnGameNewGame: TBitBtn;
    btnResign: TBitBtn;
    btnQuitGame: TBitBtn;
    btnQuit: TBitBtn;
    btnStartGame: TBitBtn;
    btnPlayAgainstAI: TButton;
    checkTimer: TCheckBox;
    comboGameMode: TComboBox;
    comboStartColor: TComboBox;
    editLocalIP: TLabeledEdit;
    editWebserviceURL: TLabeledEdit;
    Label1: TLabel;
    labelMode: TLabel;
    labelTime: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    editWebServiceAI: TLabeledEdit;
    labelPos: TLabel;
    editPlayerName: TLabeledEdit;
    memoDebug: TMemo;
    pageStart: TPage;
    notebookMain: TNotebook;
    panelModules: TPanel;
    pageGame: TPage;
    spinPlayerTime: TSpinEdit;
    timerChessTimer: TTimer;
    pageWebservice: TPage;
    procedure btnGameNewGameClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure btnQuitGameClick(Sender: TObject);
    procedure btnStartGameClick(Sender: TObject);
    procedure comboGameModeSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure timerChessTimerTimer(Sender: TObject);
  private
    { private declarations }
    function FormatTime(ATimeInMiliseconds: Integer): string;
    procedure UpdateChessModulesUI(ANewIndex: Integer);
    function HandlePawnPromotion(APiece: TChessTile): TChessTile;
  public
    { public declarations }
    procedure UpdateCaptions;
    procedure InitializeGameModel;
  end;

var
  formChess: TformChess;
  vFormDrawerDelegate: TFormDrawerDelegate;

procedure HandleOnMove(AFrom, ATo: TPoint);

implementation

{$R *.lfm}

const
  INT_PAGE_START = 0;
  INT_PAGE_GAME = 1;
  INT_PAGE_WEBSERVICE = 2;

{ TformChess }

procedure TformChess.timerChessTimerTimer(Sender: TObject);
begin
  vChessGame.UpdateTimes();
  UpdateCaptions();
  vChessDrawer.HandleOnTimer(Sender);
  GetCurrentChessModule().HandleOnTimer();
end;

function TformChess.FormatTime(ATimeInMiliseconds: Integer): string;
var
  lTimePart: Integer;
begin
  Result := '';

  // Hours
  lTimePart := ATimeInMiliseconds div (60*60*1000);
  if lTimePart > 0 then
    Result := IntToStr(lTimePart) + 'h';

  // Minutes
  lTimePart := (ATimeInMiliseconds div (60*1000)) mod 60;
  if (lTimePart > 0) or (Result <> '') then
    Result := Result + IntToStr(lTimePart) + 'm';

  // Seconds
  lTimePart := (ATimeInMiliseconds div (1000)) mod 60;
  Result := Result + IntToStr(lTimePart) + 's';

  // Miliseconds
  lTimePart := ATimeInMiliseconds mod (1000);
  Result := Result + IntToStr(lTimePart);
end;

procedure TformChess.UpdateChessModulesUI(ANewIndex: Integer);
var
  lModule: TChessModule;
begin
  if ANewIndex = gSelectedModuleIndex then Exit;

  lModule := GetChessModule(gSelectedModuleIndex);
  if lModule <> nil then lModule.HideUserInterface();
  GetChessModule(ANewIndex).ShowUserInterface(panelModules);
end;

function TformChess.HandlePawnPromotion(APiece: TChessTile): TChessTile;
var
  dlgPromotion: TformPromotion;
begin
  dlgPromotion := TformPromotion.Create(vChessGame.IsWhitePlayerTurn);
  try
    dlgPromotion.ShowModal;
  finally
    dlgPromotion.Free;
    Result := selectPromotionPiece.pieceChosen;
  end;
end;

procedure HandleOnMove(AFrom, ATo: TPoint);
var
  lStr: String;
begin
  lStr := vChessGame.GetCurrentPlayerColor();
  lStr := Format('%s executed the move %s-%s', [lStr, vChessGame.BoardPosToChessCoords(AFrom), vChessGame.BoardPosToChessCoords(ATo)]);
  formChess.MemoDebug.Lines.Add(lStr);
end;

procedure TformChess.UpdateCaptions;
var
  lStr, lStr2: string;
begin
  if vChessGame.IsWhitePlayerTurn then lStr := 'White playing'
  else lStr := 'Black playing';

  lStr2 := vChessGame.BoardPosToChessCoords(vChessGame.MouseMovePos);
  lStr := lStr + Format(' X: %d Y: %d = %s',
    [vChessGame.MouseMovePos.X, vChessGame.MouseMovePos.Y, lStr2]);

  formChess.labelPos.Caption := lStr;

  lStr := Format('White time: %s Black time: %s',
    [FormatTime(vChessGame.WhitePlayerTime), FormatTime(vChessGame.BlackPlayerTime)]);

  formChess.labelTime.Caption := lStr;

  lStr := GetChessModule(gSelectedModuleIndex).PlayingDescription;

  formChess.labelMode.Caption := lStr;
end;

procedure TformChess.InitializeGameModel;
begin
  vChessGame.StartNewGame(comboStartColor.ItemIndex, checkTimer.Checked, spinPlayerTime.Value);
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

  // Prepare the modules view
  InitializeGameModel();
  editLocalIP.Text := ChessGetLocalIP();
  PopulateChessModulesList(comboGameMode.Items);
  if GetChessModuleCount() >= 1 then
  begin
    comboGameMode.ItemIndex := 0;
    UpdateChessModulesUI(0);
    gSelectedModuleIndex := 0;
  end;
  gChessModulesDebugOutputDestiny := memoDebug;

  // Prepare the callbacks
  vChessGame.OnBeforeMove := @HandleOnMove;
  vChessGame.OnPawnPromotion := @HandlePawnPromotion;
end;

procedure TformChess.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TformChess.btnGameNewGameClick(Sender: TObject);
begin
  notebookMain.PageIndex := INT_PAGE_START;

  timerChessTimer.Enabled := False;
end;

procedure TformChess.btnQuitGameClick(Sender: TObject);
begin
  Close;
end;

procedure TformChess.btnStartGameClick(Sender: TObject);
var
  lModule: TChessModule;
begin
  InitializeGameModel();
  vChessGame.Enabled := False;

  notebookMain.PageIndex := INT_PAGE_GAME;

  gSelectedModuleIndex := comboGameMode.ItemIndex;
  lModule := GetChessModule(gSelectedModuleIndex);
  vChessGame.PlayerName := editPlayerName.Text;
  lModule.PrepareForGame();

  // Make sure this is done after lModule.PrepareForGame()
  vChessGame.Enabled := True;
  timerChessTimer.Enabled := True;
end;

procedure TformChess.comboGameModeSelect(Sender: TObject);
begin
  UpdateChessModulesUI(comboGameMode.ItemIndex);
  gSelectedModuleIndex := comboGameMode.ItemIndex;
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
  lModule: TChessModule;
begin
  lModule := GetChessModule(gSelectedModuleIndex);
  if not lModule.IsMovingAllowedNow() then Exit;

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
  lModule: TChessModule;
begin
  lModule := GetChessModule(gSelectedModuleIndex);
  if not lModule.IsMovingAllowedNow() then Exit;

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

