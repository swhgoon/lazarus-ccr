{ query FreeDB for audio cd title informations
  written by Sebastian Kraft
  sebastian_kraft@gmx.de

  This software is free under the GNU Public License

  (c)2005
}

unit cddb;

{$mode objfpc}{$H+}

{$ifndef Darwin}{$define HAS_CDROM}{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef HAS_CDROM}cdrom, discid,{$endif}
  lnet, config, debug;


type

  {$ifndef HAS_CDROM}
  TTocEntry = Record
    min, sec, frame : Integer;
  end;
  PTocEntry = ^TTocEntry;
  {$endif}

  { TCddbObject }

  TCddbObject = class
     year, genre, artist, album: string;
     title: array[1..99] of string;
     ErrorMsg, Status, QueryString: string;
     CDromDrives : Array[1..10] of String;
     DriveCount, NrTracks: byte;
     Device: string;
     ErrorCode: Integer;
     Data: TStringList;
     TOCEntries: array[1..99] of TTocEntry;
     DiscID: integer;
     query_send, data_ready, receiving_data:boolean;
    function connect(server:string; port: word):boolean;
    procedure callevents;
    procedure query(drive, server:string; port: word);
    procedure Parsedata;
    function ReadTOC(drive:string):boolean;
    constructor create;
    destructor destroy;
  private
    { private declarations }
    orphantext: string;
    Connection: TLTcp;
    FServer, FUser, FSoftware, FVersion, FHostname: string;
    FPort: word;
    procedure OnReceiveProc(asocket: TLSocket);
    procedure OnErrorProc(const msg: string; asocket: TLSocket);
    procedure OnDisconnectProc(asocket: TLSocket);
    procedure OnConnectProc(asocket: TLSocket);
  public
    { public declarations }
  end;

implementation

uses functions;

type

  { TLEvents }

  TLEvents = class
   public
    procedure DsProc(aSocket: TLSocket);
    procedure ReProc(aSocket: TLSocket);
    procedure ErProc(const msg: string; aSocket: TLSocket);
  end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TLEvents }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TLEvents.DsProc(aSocket: TLSocket);
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TLEvents.ReProc(aSocket: TLSocket);
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TLEvents.ErProc(const msg: string; aSocket: TLSocket);
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TCddbObject }
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TCddbObject.connect(server: string; port: word): boolean;
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TCddbObject.OnReceiveProc(asocket: TLSocket);
var  s, s1, s2, tmp: string;
     deleted: boolean;
     posi: integer;
     Errorcode2: integer;
begin
     ErrorCode:=0;
     s:='';
     asocket.GetMessage(s);
     DebugOutLn(Format('Socket message length: %d', [Length(s)]), 1);
     if s<>'' then begin
        if length(s)>3 then begin
             posi:=pos(#13, s);
             s1:=Copy(s, 1, 3);
             if (posi<>0) then s2:=Copy(s, posi+2, 3);
             try
               TryStrToInt(s1, ErrorCode);
               TryStrToInt(s2, ErrorCode2);
               if Errorcode2 > 0 then Errorcode := Errorcode2;
              except
              end;
           end;
        DebugOutLn('-------------------------------------------------', 0);
        DebugOutLn(s, 0);
        DebugOutLn(errorcode, 0);
        DebugOutLn(s1, 0);
        DebugOutLn(s2, 0);
        DebugOutLn('-------------------------------------------------', 0);
      end;

    if (ErrorCode=200) and query_send then begin
        delete(s, 1, 4);
        tmp:=copy(s, 1, pos(' ',s));
        delete(s, 1, pos(' ', s));
        s1:=copy(s, 1, pos(' ',s));
        Connection.SendMessage('cddb read '+tmp+' '+s1+' '+#13+#10);
        DebugOutLn('cddb read '+tmp+' '+s1+' ', 0);
      end;
      
    if (ErrorCode=211) and query_send then begin
//        delete(s, 1, 4);
        delete(s, 1, pos(#10, s));
        tmp:=copy(s, 1, pos(' ',s));
        delete(s, 1, pos(' ', s));
        s1:=copy(s, 1, pos(' ',s));
        Connection.SendMessage('cddb read '+tmp+' '+s1+' '+#10+#13);
        DebugOutLn('cddb read '+tmp+' '+s1+' ', 0);
      end;


    if (ErrorCode=200) and (not query_send) then begin
        Connection.SendMessage('cddb query '+QueryString+#10+#13);
        DebugOutLn('cddb query '+QueryString, 0);
        query_send:=true;
       end;


    if (ErrorCode=210) and (query_send) then begin
        artist:='';
        album:='';
        delete(s, 1, pos(#10, s));
        receiving_data := true;
        Data.Clear;
        orphantext := '';
       end;

     if receiving_data then
        begin
         Data.Text := Data.Text + orphantext + s;
         if not (s[Length(s)] in [#10,#13]) then
            begin
              orphantext := Data[Data.Count-1];
              Data.Delete(Data.Count-1);
            end;
 //        writeln(' v v v v v v v v v v v v v v v');
 //        writeln(Data.Text);
 //        writeln(' ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^');
         if Data[Data.Count-1]= '.' then   //  End of data (".") in Count-1
            begin
              Parsedata;
              album:=copy(artist, pos(' / ', artist)+3, length(artist)-pos(' / ', artist)+3);
              delete(artist, pos(' / ', artist), length(artist)-pos(' / ', artist)+1);
              album:=Latin1toUTF8(album);
              data_ready:=true;
              receiving_data := false;
              DebugOutLn('CDDB data ready...', 0);
            end;
        end;

     s:='';
     s1:='';
     tmp:=''
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TCddbObject.OnErrorProc(const msg: string; asocket: TLSocket);
begin
  ErrorMsg:=msg;
  DebugOutLn(ErrorMsg, 0);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TCddbObject.OnDisconnectProc(asocket: TLSocket);
begin
  DebugOutLn('[TCddbObject.OnDisconnectProc] lost connection', 0);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TCddbObject.OnConnectProc(asocket: TLSocket);
var s:string;
begin
  asocket.GetMessage(s);
  DebugOutLn(s, 0);
  Connection.CallAction;
  DebugOutLn('connected to cddb server, sending hello...', 0);
  asocket.SendMessage('cddb hello '+FUser+' '+FHostname+' '+FSoftware+' '+FVersion+#13+#10);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TCddbObject.callevents;
begin
  Connection.CallAction;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TCddbObject.query(drive, server: string; port: word);
begin
  if NrTracks>0 then
  begin
    {$ifdef HAS_CDROM}
    discid:=(CDDBDiscID(TOCEntries, NrTracks));
    querystring:=GetCDDBQueryString(TOCEntries, NrTracks);
    DebugOutLn(QueryString, 0);
    DebugOutLn(hexStr(discid, 8), 0);
    {$endif}

    FServer:=server;
    FPort:=port;
    query_send:=false;
    Connection.Connect(FServer, FPort);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TCddbObject.Parsedata;
   var
     c: integer;
     s: string;
     i: integer;
   begin

    // TODO: initialize year, genre, artist, album, and title[n] to ''
     for c := 0 to Data.Count-1 do
         begin
           s := Data[c];
          { deleted:=false;
           if pos('#', s)=1 then begin
              delete(s, 1, pos(#10, s));
              deleted:=true;
             end;
           if pos('DISCID=', s)=1 then begin
              delete(s, 1, pos(#10, s));
              deleted:=true;
            end;      }

           if (pos('DTITLE=', s)=1) and (artist='') then begin
              artist:=Copy(s, 8, MaxInt);
              artist:=Latin1toUTF8(artist);
              delete(s, 1, pos(#10, s));
              //deleted:=true;
            end;
           if pos('TTITLE', s)=1 then begin
              TryStrToInt(Copy(s,7, Pos('=',s)-7 ), i);
              inc(i);
              title[i]:=Copy(s, pos('=', s)+1, MaxInt);
              title[i]:=Latin1toUTF8(title[i]);
              delete(s, 1, pos(#10, s));
              if i>8 then
                 DebugOutLn('title ---> '+title[i], 0);
             // deleted:=true;
             end;

           if (pos('EXTD=', s)=1) and (pos('YEAR:', s)<>0) then begin
              year:=Copy(s, pos('YEAR:', s)+6, 4);
{              delete(s, 1, pos(#10, s));
              deleted:=true;     }
            end;

         {  if (pos('EXTD=', s)=1) then begin
              delete(s, 1, pos(#10, s));
              deleted:=true;
            end;

           if (pos('PLAYORDER', s)=1)then begin
              delete(s, 1, pos(#10, s));
              deleted:=true;
            end;

           if (pos('EXTT', s)=1)then begin
              delete(s, 1, pos(#10, s));
              deleted:=true;
            end;            }
        {   if not deleted then delete(s, 1, pos(#10, s));}

         end;
   end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TCddbObject.ReadTOC(drive: string):boolean;
begin
    NrTracks:=0;
    Device:=drive;
    Try
      {$ifdef HAS_CDROM}
      NrTracks:= ReadCDTOC(drive, TOCEntries);
      {$endif}
    except
      result:=false;
    end;
    if NrTracks>100 then NrTracks:=0;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TCddbObject.create;
var b: byte;
begin
  Connection:=TLTcp.Create(nil);
  Connection.OnConnect:=@OnConnectProc;
  Connection.OnReceive:=@OnReceiveProc;
  Connection.OnDisconnect:=@OnDisconnectProc;
  Connection.OnError:=@OnErrorProc;
  Data := TStringList.Create;
  data_ready:=false;
  receiving_data := false;
  FUser:='cddbuser';
  FSoftware:='cddbobject';
  FVersion:='v0.1';
  FHostname:='localhost';
  DriveCount:=0;
  Try
     {$ifdef HAS_CDROM}
     DriveCount:=GetCDRomDevices(CDromDrives);
     {$endif}
     DebugOutLn(Format('%d CD-ROM drives autodetected', [DriveCount]), 0);
     For b:=1 to DriveCount do
       DebugOutLn(Format('Drive %d on device: %s',[b, CDRomDrives[b]]), 0);
  Except
     On E : exception do
       DebugOutLn('[TCddbObject.create] exception caught with message: '+E.Message, 0);
  end;

  if DriveCount=0 then
  begin
    CDromDrives[1]:=CactusConfig.CDRomDevice;
    inc(DriveCount);
  end;
  Connection.CallAction;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

destructor TCddbObject.destroy;
begin
  Connection.destroy;
  Data.Destroy;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

begin
end.

