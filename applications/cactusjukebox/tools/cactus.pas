{
helper application to start cactus jukebox in simple player mode

written by Sebastian Kraft, <c> 2006

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL



}

program cactus;
uses sysutils, dos;

var tmps: string;
    i: byte;	

begin
	setCurrentDir(ExtractFilePath(Paramstr(0)));
	
	//tmps:=' -p';
	for i:= 1 to ParamCount do tmps:=tmps+' '+ParamStr(i);
	writeln('starting cactus in player mode...');
	exec('cactus_jukebox',tmps);	
end.
