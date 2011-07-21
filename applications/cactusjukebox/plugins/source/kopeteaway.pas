library kopeteaway;

{$mode objfpc}{$H+}



uses
  Classes, SysUtils, plugintypes, unix;



CONST PluginInfo: TPluginInforec = (
	Name: 'Kopete Away Message Plugin';
	Author: 'Sebastian Kraft';
	Version: '0.1';
	Comment: 'This plugin ');

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function EventHandler(Event: TCactusEvent; msg: PChar): boolean;stdcall;
var tmps: string;
begin
  try
  //writeln('event received');
   //datastr:=tfmodplayerclass(data).currentTrack;
   //writeln(datastr);
   
   tmps:='Listening to: '+StrPas(msg);
   tmps:=StringReplace(tmps, ' ', '\ ', [rfReplaceAll]);
   tmps:=StringReplace(tmps, '''', '\''', [rfReplaceAll]);  
    
// tmps:=tmps;
   case Event of
     evnStartPlay: begin
	   shell('/usr/bin/dbus-send --type=method_call --dest=org.kde.kopete /Kopete org.kde.Kopete.setOnlineStatus :Away :'+tmps);
           //writeln(lo(dosexitcode));
         end;
     evnStopPlay: begin
	   shell('/usr/bin/dbus-send --type=method_call --dest=org.kde.kopete /Kopete org.kde.Kopete.setOnlineStatus :Online string:');
           //writeln(lo(dosexitcode));
	 end;
   end;
  result:=true;
  except result:=false;
  end;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function LoadPlugin(var CactusPlugIn: TCactusPluginClass): Boolean; export;
begin
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function GetPluginInfo: TPluginInfoRec;export;
begin
  result:=PluginInfo;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

exports  GetPluginInfo;
exports  LoadPlugin;
exports  EventHandler;

begin
end.
