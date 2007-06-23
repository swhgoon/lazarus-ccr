program user_client_console;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, TypInfo,
  user_service_intf_proxy,
  synapse_tcp_protocol, synapse_http_protocol, library_protocol,
  soap_formatter, binary_formatter,
  user_service_intf;

var
  UserServiceInst : UserService;

procedure ShowUser(AUser : TUser);
begin
  if ( AUser <> nil ) then begin
    WriteLn('  Name = ',AUser.UserName);
    WriteLn('  Category = ',GetEnumName(TypeInfo(TUserCategory),Ord(AUser.Category)));
    WriteLn('  e-Mail = ',AUser.eMail);
    WriteLn('  Preferences = ',AUser.Preferences);
  end else begin
    WriteLn('<Empty User>');
  end;
end;

procedure ShowUserArray(AArray : TUserArray);
var
  i, c : Integer;
  usr : TUser;
begin
  if ( AArray <> nil ) then begin
    c := AArray.Length;
    for i := 0 to Pred(c) do begin
      usr := AArray[i];
      WriteLn();
      WriteLn(Format('User[%d] : ',[(i+1)]));
      ShowUser(usr);
    end;
  end;
end;

procedure HandleShowAll();
var
  userArray : TUserArray;
begin
  userArray := UserServiceInst.GetList();
  try
    if ( userArray <> nil ) and ( userArray.Length > 0 ) then begin
      ShowUserArray(userArray);
    end else begin
      WriteLn('Empty Array.');
    end;
  finally
    FreeAndNil(userArray);
  end;
end;

procedure HandleAdd();

  function ReadItem(const APrompt : string; const ANonNull : Boolean):string ;
  begin
    Result := '';
    Write(APrompt);
    ReadLn(Result);
    Result := Trim(Result);
    if ANonNull and ( Length(Result) = 0 ) then
      Raise Exception.Create('Invalid User Name!');
  end;
var
  usr : TUser;
  buff : string;
begin
  buff := '';
  WriteLn('Adding a user :');
  try
    usr := TUser.Create();
    try
      usr.UserName := ReadItem('Enter user name : ',True);
      buff := UpperCase(ReadItem('Enter user Category( A : Admin; N : normal ) : ',True));
      if ( buff[1] = 'A' ) then
        usr.Category:= Admin
      else
        usr.Category:= Normal;
      usr.eMail := ReadItem('Enter user e-mail : ',False);
      usr.Preferences := ReadItem('Enter user Preferences : ',False);
      UserServiceInst.Add(usr);
    finally
      FreeAndNil(usr);
    end;
  except
    on e : Exception do begin
      WriteLn(e.Message);
    end;
  end;
end;

procedure HandleFindUser();
var
  user : TUser;
  buff : string;
begin
  Write('Enter User Name : ');
  ReadLn(buff);
  user := UserServiceInst.Find(buff);
  try
    ShowUser(user);
  finally
    FreeAndNil(user);
  end;
end;

type TTransportType = ( ttLibrary, ttTCP, ttHTTP );
procedure CreateProxy(const ATransportType :TTransportType);
const ADDRESS_MAP : array[TTransportType] of string = (
        'LIB:FileName=..\library_server\lib_server.dll;target=UserService',
        'TCP:Address=127.0.0.1;Port=1234;target=UserService',
        'http:Address=http://127.0.0.1:8080/wst/services/UserService'
        //'http:Address=http://127.0.0.1:8000/services/UserService'
      );
var
  buff : string;
begin
  buff := ADDRESS_MAP[ATransportType];
  if ( ATransportType = ttLibrary ) then
    buff := StringReplace(buff,'\',DirectorySeparator,[rfReplaceAll, rfIgnoreCase]);
  UserServiceInst := TUserService_Proxy.Create(
                       'UserService',
                       'binary:',
                       buff
                     );
end;

function ReadTransportType():TTransportType;
var
  buff : string;
begin
  WriteLn();
  WriteLn('Select a transport protocol : ');
  WriteLn('  L : Library, the lib_server project must have been built');
  WriteLn('  T : TCP, the tcp_server must have been built');
  WriteLn('  H : HTTP, the http_server must have been built');
  WriteLn();
  Write('Your selection : ');
  while True do begin
    ReadLn(buff);
    buff := UpperCase(Trim(buff));
    if ( Length(buff) > 0 ) and ( buff[1] in ['L','T', 'H'] ) then begin
      case buff[1] of
        'L' : Result := ttLibrary;
        'T' : Result := ttTCP;
        'H' : Result := ttHTTP;
      end;
      Break;
    end;
  end;
end;

var
  strBuffer : string;
  tt : TTransportType;
begin
  SYNAPSE_RegisterTCP_Transport();
  SYNAPSE_RegisterHTTP_Transport();
  LIB_Register_Transport();
  WriteLn('Sample Application using Web Services Toolkit');
  CreateProxy(ReadTransportType());
  WriteLn('Menu :');
  WriteLn(' L : Show the user list');
  WriteLn(' A : Add a new user');
  WriteLn(' F : Find a new');
  WriteLn(' C : Change the communication protocol');
  WriteLn(' X : Exit');
  WriteLn();
  Write('Choose a item : ');
  while True do begin
    strBuffer := '';
    ReadLn(strBuffer);
    strBuffer := UpperCase(Trim(strBuffer));
    if ( Length(strBuffer) > 0 ) then begin
      case strBuffer[1] of
        'L' : HandleShowAll();
        'A' : HandleAdd();
        'F' : HandleFindUser();
        'C' : CreateProxy(ReadTransportType());
        'X' : Break;
      end;
      WriteLn();
      Write('Choose a item : ');
    end;
  end;
end.

