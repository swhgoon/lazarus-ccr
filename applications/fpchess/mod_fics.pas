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
  StdCtrls, Forms, Controls,
  lTelnet,
  chessmodules, chessgame;

type

  { TFICSChessModule }

  TFICSChessModule = class(TChessModule)
  public
    SecondPlayerName: string;
    TelnetComm: TLTelnetClient;
    FICS_HOST: string;
    FICS_PORT: Integer;
    FICS_USER: string;
    FICS_PASSWORD: string;
    // Frequency to issue commands to avoid disconnection, in miliseconds
    PROTECT_LOGOUT_FREQ: Integer;
    constructor Create();
    destructor Destroy; override;
    procedure CreateUserInterface(); override;
    procedure ShowUserInterface(AParent: TWinControl); override;
    procedure HideUserInterface(); override;
    procedure FreeUserInterface(); override;
    procedure PrepareForGame(); override;
    function IsMovingAllowedNow(): Boolean; override;
    function GetSecondPlayerName(): string; override;
    procedure HandleOnMove(AFrom, ATo: TPoint); override;
  end;

implementation

{ TFICSChessModule }

constructor TFICSChessModule.Create;
begin
  inherited Create;

  TelnetComm := TLTelnetClient.Create(nil);
  (*    $telnet = new Net::Telnet(
            Timeout => $OPEN_TIMEOUT,
            Binmode => 1,
            Errmode => 'die',
           );*)

  Description := 'Play online via the Free Internet Chess Server';
  Kind := cmkSinglePlayer;

  FICS_HOST := 'freechess.org';
  FICS_PORT := 5000;
  FICS_USER := 'BotTutorial';
  FICS_PASSWORD := '';
  PROTECT_LOGOUT_FREQ := 45 * 60 * 1000;
end;

destructor TFICSChessModule.Destroy;
begin
  TelnetComm.Free;

  inherited Destroy;
end;

procedure TFICSChessModule.CreateUserInterface;
begin
{  textSecondPlayerName := TStaticText.Create(nil);
  textSecondPlayerName.SetBounds(20, 20, 180, 50);
  textSecondPlayerName.Caption := 'Name of the second player';

  editSecondPlayerName := TEdit.Create(nil);
  editSecondPlayerName.SetBounds(200, 20, 150, 50);
  editSecondPlayerName.Text := 'Second player';}
end;

procedure TFICSChessModule.ShowUserInterface(AParent: TWinControl);
begin
{  textSecondPlayerName.Parent := AParent;
  editSecondPlayerName.Parent := AParent;}
end;

procedure TFICSChessModule.HideUserInterface();
begin
{  textSecondPlayerName.Parent := nil;
  editSecondPlayerName.Parent := nil;}
end;

procedure TFICSChessModule.FreeUserInterface;
begin
{  textSecondPlayerName.Free;
  editSecondPlayerName.Free;}
end;

procedure TFICSChessModule.PrepareForGame;
var
  lResult: Boolean;
begin
//  SecondPlayerName := editSecondPlayerName.Text;
  ChessModuleDebugLn('[TFICSChessModule.PrepareForGame]');

  //  Opening telnet connection. This is what happens when you issue telnet freechess.org 5000.

  lResult := TelnetComm.Connect(FICS_HOST, FICS_PORT);

  if not lResult then
  begin
    ChessModuleDebugLn('Failed to connect to FICS');
    Exit;
    //      print STDERR "\n" if $VERBOSE;
  end;

  ChessModuleDebugLn('Connected to FICS');

  // If $FICS_PASSWORD is given, we peform normal full login (give username and password). FICS is standard enough to have Net::Telnet::login routine perform this process properly.
  if FICS_PASSWORD <> '' then
  begin
    //$telnet->login(Name => $FICS_USER, Password => $FICS_PASSWORD);
    //    $username = $FICS_USER;
    // print STDERR "Successfully logged as user $FICS_USER\n" if $VERBOSE;
  end
  else
  begin

  end;

(*

  Now let's go to the guest login. Again, try logging to FICS via telnet as guest to understand what we are testing for here.
      else {

          $telnet->waitfor(
              Match => '/login[: ]*$/i',
              Match => '/username[: ]*$/i',
              Timeout => $OPEN_TIMEOUT);

              $telnet->print($FICS_USER);

      ... and we send our username once prompted. Now we read obtained lines scanning for some patterns.
              while (1) {
                  my $line = $telnet->getline(Timeout => $LINE_WAIT_TIMEOUT);
                  next if $line =~ /^[\s\r\n]*$/;
                  if ($line =~ /Press return to enter/) {
                      $telnet->print();
                      last;
                  }

      Normal guest login here. We get Press return to enter suggestion and we do exactly that (we send empty line).
                  if ($line =~ /("[^"]*" is a registered name|\S+ is already logged in)/) {
                      die "Can not login as $FICS_USER: $1\n";
                  }

      Bad luck, we picked the name used by somebody, it is not possible to login as guest with this nick.
                  print STDERR "Ignored line: $line\n" if $VERBOSE;
              }

      Developing-helper note and the end of loop. We get further after last breaks the loop above.
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

      After accepting guest login we may face two things. First, FICS may accept our login and send us a message like Starting FICS session as BotTutorial. This means everything is OK and we can go on. Alternatively, FICS may notice another guest using the same name, in such case it will tell us something like BotTutorial is already logged in and will disconnect.
              print STDERR "Successfully logged as guest $username\n" if $VERBOSE;
          }
          *)
end;

function TFICSChessModule.IsMovingAllowedNow: Boolean;
begin
  Result := not (vChessGame.IsWhitePlayerTurn xor vChessGame.FirstPlayerIsWhite);
end;

function TFICSChessModule.GetSecondPlayerName: string;
begin
//  Result := SecondPlayerName;
end;

// If a move came, it is because the local player did a move
// so send this move and start listening for a move
procedure TFICSChessModule.HandleOnMove(AFrom, ATo: TPoint);
begin

end;

initialization
  RegisterChessModule(TFICSChessModule.Create);
end.

