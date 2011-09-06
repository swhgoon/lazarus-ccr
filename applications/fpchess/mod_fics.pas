{
  For playing through the internet via FICS - Free Internet Chess Server

  Based on this article:
  http://blog.mekk.waw.pl/archives/7-How-to-write-a-FICS-bot-part-I.html

  FICS website:
  http://www.freechess.org/
}
unit mod_fics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, Forms, Controls, ExtCtrls, maskedit,
  lTelnetex, lnet,
  sorokinregexpr, // Rename to regexpr when FPC 2.8 comes with this
  chessmodules, chessgame, chessdrawer;

type

  { TFICSChessModule }

  TFICSChessModule = class(TChessModule)
  private
    radioConnectionType: TRadioGroup;
    textPassword: TStaticText;
    editPassword: TMaskEdit;
    textSecondPlayerName: TStaticText;
    editSecondPlayerName: TEdit;
  public
    SecondPlayerName: string;
    TelnetComm: TLTelnetClientEx;
    FICS_HOST: string;
    FICS_PORT: Integer;
    FICS_USER: string;
    FICS_PASSWORD: string;
    constructor Create(); override;
    destructor Destroy; override;
    procedure CreateUserInterface(); override;
    procedure ShowUserInterface(AParent: TWinControl); override;
    procedure HideUserInterface(); override;
    procedure FreeUserInterface(); override;
    procedure PrepareForGame(); override;
    function GetSecondPlayerName(): string; override;
    procedure HandleOnMove(AFrom, ATo: TPoint); override;
    procedure HandleOnTimer(); override;
    procedure HandleOnDebugOut(AStr: string);
  end;

const
  FICS_LineEnding = #10;
  OPEN_TIMEOUT = 1000000;
  PROTECT_LOGOUT_FREQ = 45 * 60 * 1000; // Frequency to issue commands to avoid disconnection, in miliseconds

implementation

{ TFICSChessModule }

constructor TFICSChessModule.Create;
begin
  inherited Create;

  TelnetComm := TLTelnetClientEx.Create(nil);
  TelnetComm.OnDebugOut := @HandleOnDebugOut;
  (*    $telnet = new Net::Telnet(
            Timeout => $OPEN_TIMEOUT,
            Binmode => 1,
            Errmode => 'die',
           );*)

  Name := 'mod_fics.pas';
  SelectionDescription := 'Play online - Free Internet Chess Server';
  PlayingDescription := 'Playing online - Free Internet Chess Server';
  Kind := cmkInternet;

  FICS_HOST := 'freechess.org';
  FICS_PORT := 5000;
  FICS_USER := 'FPChess';
  FICS_PASSWORD := '';
end;

destructor TFICSChessModule.Destroy;
begin
  TelnetComm.Free;

  inherited Destroy;
end;

procedure TFICSChessModule.CreateUserInterface;
begin
  radioConnectionType := TRadioGroup.Create(nil);
  radioConnectionType.SetBounds(10, 10, 300, 100);
  radioConnectionType.Caption := 'FICS Connection Type';
  radioConnectionType.Items.Add('Wait for a friend to connect to me');
  radioConnectionType.Items.Add('Connect to a friend (he needs to be waiting for the connection)');
  radioConnectionType.ItemIndex := 0;

  textPassword := TStaticText.Create(nil);
  textPassword.SetBounds(10, 110, 180, 20);
  textPassword.Caption := 'Your FICS Password';

  editPassword := TMaskEdit.Create(nil);
  editPassword.SetBounds(200, 110, 150, 20);
  editPassword.Text := '';

  textSecondPlayerName := TStaticText.Create(nil);
  textSecondPlayerName.SetBounds(10, 130, 180, 40);
  textSecondPlayerName.Caption := 'FICS Login of the other player';

  editSecondPlayerName := TEdit.Create(nil);
  editSecondPlayerName.SetBounds(200, 130, 150, 40);
  editSecondPlayerName.Text := 'fpchesse';
end;

procedure TFICSChessModule.ShowUserInterface(AParent: TWinControl);
begin
  radioConnectionType.Parent := AParent;
  textPassword.Parent := AParent;
  editPassword.Parent := AParent;
  textSecondPlayerName.Parent := AParent;
  editSecondPlayerName.Parent := AParent;
end;

procedure TFICSChessModule.HideUserInterface();
begin
  radioConnectionType.Parent := nil;
  textPassword.Parent := nil;
  editPassword.Parent := nil;
  textSecondPlayerName.Parent := nil;
  editSecondPlayerName.Parent := nil;
end;

procedure TFICSChessModule.FreeUserInterface;
begin
  radioConnectionType.Free;
  textPassword.Free;
  editPassword.Free;
  textSecondPlayerName.Free;
  editSecondPlayerName.Free;
end;

procedure TFICSChessModule.PrepareForGame;
var
  lResult, WaitTerminated: Boolean;
  lMsg: string;
begin
  FICS_USER := vChessGame.PlayerName;
  FICS_PASSWORD := editPassword.Text;

//  SecondPlayerName := editSecondPlayerName.Text;
  ChessModuleDebugLn('[TFICSChessModule.PrepareForGame]');

  //  Opening telnet connection. This is what happens when you issue telnet freechess.org 5000.

  lResult := TelnetComm.Connect(FICS_HOST, FICS_PORT);
  if not lResult then
  begin
    ChessModuleDebugLn('Failed to connect to FICS');
    Exit;
  end;

  repeat
    TelnetComm.CallAction; // repeat this to get info
    Application.ProcessMessages;
    Sleep(10);
  until TelnetComm.Connected; // wait until timeout or we actualy connected

  ChessModuleDebugLn('Connected to FICS');

  // If $FICS_PASSWORD is given, we peform normal full login (give username and password). FICS is standard enough to have Net::Telnet::login routine perform this process properly.
  if FICS_PASSWORD <> '' then
  begin
    //$telnet->login(Name => $FICS_USER, Password => $FICS_PASSWORD);
    //    $username = $FICS_USER;
    // print STDERR "Successfully logged as user $FICS_USER\n" if $VERBOSE;
  end
  // Now let's go to the guest login. Again, try logging to FICS via telnet as guest to understand what we are testing for here.
  else
  begin
    {
    $telnet->waitfor(
        Match => '/login[: ]*$/i',
        Match => '/username[: ]*$/i',
        Timeout => $OPEN_TIMEOUT);

        $telnet->print($FICS_USER);
        }
    TelnetComm.WaitFor(
      '.*login:.*',
      '.*username:.*',
      OPEN_TIMEOUT);

    // ... and we send our username once prompted.
    //ChessModuleDebugLn('Found the login!!!');
    ChessModuleDebugLn('Sending: ' + FICS_USER);
    TelnetComm.SendMessage(FICS_USER + FICS_LineEnding);

    // Now we read obtained lines scanning for some patterns.
    TelnetComm.WaitFor(
      '.*Press return to enter.*',
      '',
      OPEN_TIMEOUT);
    TelnetComm.SendMessage(FICS_LineEnding);

      (*if ($line =~ /("[^"]*" is a registered name|\S+ is already logged in)/) {
          die "Can not login as $FICS_USER: $1\n";
      }

      Bad luck, we picked the name used by somebody, it is not possible to login as guest with this nick.
      *)

    (*
    After accepting guest login we may face two things.
    First, FICS may accept our login and send us a message like
    Starting FICS session as BotTutorial.
    This means everything is OK and we can go on.
    Alternatively, FICS may notice another guest using the same name,
    in such case it will tell us something like BotTutorial is already
    logged in and will disconnect.

            print STDERR "Successfully logged as guest $username\n" if $VERBOSE;
                  my($pre, $match) = $telnet->waitfor(
                      Match => "/Starting FICS session as ([a-zA-Z0-9]+)/",
                      Match => "/\\S+ is already logged in/",
                      Timeout => $OPEN_TIMEOUT);
                  if ( $match =~ /Starting FICS session as ([a-zA-Z0-9]+)/ ) {
                      $username = $1;
                  }
                  else {
                      die "Can not login as $FICS_USER: $match\n";
                  }
              }
              *)
    TelnetComm.WaitFor(
      '.*Starting FICS session as.*',
      '',
      OPEN_TIMEOUT);
  end;

  // Remove those annoying message of people seeking chess adversaries
  TelnetComm.WaitFor(
    '.*fics%.*',
    '',
    OPEN_TIMEOUT);
  ChessModuleDebugLn('Sending: set seek 0');
  TelnetComm.SendMessage('set seek 0' + FICS_LineEnding);

  // Set the style
  TelnetComm.WaitFor(
    '.*fics%.*',
    '',
    OPEN_TIMEOUT);
  ChessModuleDebugLn('Sending: set style 11');
  TelnetComm.SendMessage('set style 11' + FICS_LineEnding);

  // Wait for a match
  if radioConnectionType.ItemIndex = 0 then
  begin
    vChessGame.FirstPlayerIsWhite := False;

    // Challenge: GuestZMYL (----) fpchess (----) unrated blitz 2 12.
    // You can "accept" or "decline", or propose different parameters.
    TelnetComm.WaitFor(
      '.*You can "accept" or "decline", or propose different parameters*',
      '',
      OPEN_TIMEOUT);
    ChessModuleDebugLn('Sending: accept');
    TelnetComm.SendMessage('accept' + FICS_LineEnding);

    // You accept the match offer from GuestZMYL.
    TelnetComm.WaitFor(
      '.*You accept the match offer from*',
      '',
      OPEN_TIMEOUT);
  end
  // Challenge a partner
  else
  begin
    TelnetComm.WaitFor(
      '.*fics%.*',
      '',
      OPEN_TIMEOUT);
    lMsg := 'match ' + editSecondPlayerName.Text + ' 60 White';
    ChessModuleDebugLn('Sending: ' + lMsg);
    TelnetComm.SendMessage(lMsg + FICS_LineEnding);

    // fpchess accepts the match offer.
    TelnetComm.WaitFor(
      '.*accepts the match offer*',
      '',
      OPEN_TIMEOUT);
  end;
end;

function TFICSChessModule.GetSecondPlayerName: string;
begin
//  Result := SecondPlayerName;
end;

// If a move came, it is because the local player did a move
// so send this move
procedure TFICSChessModule.HandleOnMove(AFrom, ATo: TPoint);
var
  lMsg: String;
begin
  lMsg := Format('%s-%s', [TChessGame.BoardPosToChessCoords(AFrom), TChessGame.BoardPosToChessCoords(ATo)]);
  ChessModuleDebugLn('Sending: ' + lMsg);
  TelnetComm.SendMessage(lMsg + FICS_LineEnding);

  // Wait until it shows our move
  TelnetComm.WaitFor(
    '.*[PRNBQK]/[abcdefgh][0123456789]-[abcdefgh][0123456789].*',
    '',
    OPEN_TIMEOUT);
end;

// listen for moves
procedure TFICSChessModule.HandleOnTimer;
var
  lIndex: Integer;
  lAnimation: TChessMoveAnimation;
  lFrom, lTo: TPoint;
  lMoveStr: String;
begin
  // Example output in style 11
  // #@#086GuestZMYL       :fpchess         *RNBQKBNR PPP  PP        P   PP  p   p            ppp ppprnbqkbnr003B3939-1628-0163P/a2-a4(1:03)@#@

  lIndex := TelnetComm.WaitFor(
    '.*[PRNBQK]/[abcdefgh][0123456789]-[abcdefgh][0123456789].*',
    '',
    0);
  //  if TelnetComm.LastMsg <> '' then
  //    lIndex := lIndex;
  if lIndex = 0 then
  begin
    lMoveStr := Copy(TelnetComm.LastMsg, Pos('/', TelnetComm.LastMsg)+1, 5);
    TChessGame.ChessMoveCoordsToBoardPos(lMoveStr, lFrom, lTo);
    lAnimation := TChessMoveAnimation.Create;
    lAnimation.AFrom := lFrom;
    lAnimation.ATo := lTo;
    vChessDrawer.AddAnimation(lAnimation);
  end;
end;

procedure TFICSChessModule.HandleOnDebugOut(AStr: string);
begin
  ChessModuleDebugOut(AStr);
end;

initialization
  RegisterChessModule(TFICSChessModule.Create);
end.

