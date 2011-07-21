{
helper application to control Cactus Jukebox from external scripts
currently only loading/appending files is implemented.

in future this can also control start/stop/next/... which makes it easy to 
interface Cactus Jukebox by LIRC

written by Sebastian Kraft, <c> 2006

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL



}


program cactus_remote;

{$mode objfpc}{$H+}

uses SimpleIPC, Sysutils, dos;

{$i cactus_const.inc}
       

var CactusIPC: TSimpleIPCClient;
    FileName,tmps, WorkingDir: String;
    tchar, tchar2: Char;
    i, ActionID: byte;
    invalid_param, extd_command: boolean;


begin
   WorkingDir:= ExtractFilePath(Paramstr(0));
   if paramcount > 0 then begin
	tmps:= paramstr(1);
        tchar:=tmps[2];
	if length(paramstr(1))>=3 then tchar2:=tmps[3];
        invalid_param:=false;
	extd_command:=false;
        case tchar of 
          'o' : begin ActionID:=OPEN_FILE; extd_command:=true; end;
          'e' : begin ActionID:=ENQUEU_FILE; extd_command:=true; end;
          'n' : begin ActionID:=OPEN_AS_NEXT; extd_command:=true; end;
	  'v' : if tchar2='u' then ActionID:=VOLUME_UP else ActionID:=VOLUME_DOWN;
	  'p' :	begin
		  case tchar2 of
		     'n' : ActionID:=NEXT_TRACK;
		     's' : ActionID:=START_PLAYING;
		     'x' : ActionID:=STOP_PLAYING;
		     'p' : ActionID:=PREV_TRACK; 
		     'b' : ActionID:=PAUSE_PLAYING;
		   end;
		end;
         else invalid_param:=true;
         end
      end
     else invalid_param:=true;

   if invalid_param then begin
        writeln('Cactus Remote is a tool to control Cactus Jukebox from external programs, scripts...');
        writeln('cactus_remote  <OPTION>');
        writeln;
        writeln(' Command line options:');
        writeln('    -o <File>  open file');
        writeln('    -e <File>  enqueu file to playlist');
        writeln('    -n <file>  enqueu file as next track in playlist');
	writeln();
        writeln('    -vu Volume up 10%');
        writeln('    -vd Volume down 10%');
        writeln();
        writeln('    -p<command> Control player');
        writeln('          n     Next track');	
        writeln('          p     Previous track');	
        writeln('          s     Start playing');	
        writeln('          x     Stop playing');	
	writeln('          b     Pause playing');

        writeln();
        writeln('    -h/--help  show this help');
        writeln();
        halt;
      end;


   FileName:= ExpandFileName(paramstr(2));
   writeln(Filename);
   CactusIPC:= TSimpleIPCClient.create(nil);	
   with CactusIPC do begin
     ServerID:='cactusjukeboxipc';
     try 
         connect;
     except 
            if (ActionID=OPEN_FILE) or (ActionID=ENQUEU_FILE) or (ActionID=OPEN_AS_NEXT) then begin
		writeln('   no instance of cactus jukebox found, trying to start one');
            	writeln;
		setCurrentDir(ExtractFilePath(Paramstr(0)));
		exec('cactus_jukebox','"'+Filename+'"');	
	    	//exec(WorkingDir+directoryseparator+'cactus_jukebox','"'+Filename+'"');	
	      end;	
            free;
            halt;
       end;     
   end;

   tmps:=inttostr(ActionID);
   writeln(tmps);
   if extd_command then CactusIPC.SendStringMessage(tmps+': '+FileName) 
	else  CactusIPC.SendStringMessage(tmps);
   CactusIPC.Free;
end.

