{----------------------------------------------------------------------------

                                esdsound.pas
                                
                         ESound non-visual component
                                for Lazarus
                                
                           Written by Anthony Maro
                        with help from Andrew Johnson

NOTE: Included in gamepack for convenience, no credit to be taken from Tony.

Initial Release November 14, 2002

THIS COMPONENT IS FREEWARE - USE AS YOU WILL TO FURTHER LINUX
If you release sourcecode that uses this control, please credit me or leave this
header intact.  If you release a compiled application that uses this code,
please credit me somewhere in a little bitty location so I can at least get
bragging rights!

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


Component Use:

Drop it on your form. (Duh!)
At runtime, call the play method giving a filename (with full path) to play it.

If you want to have instant access to the sounds (instead of the short delay
the first time you play them) call the cachesample method as in:

ESDSound1.CacheSample('/usr/share/sounds/mysound.wav','shortname');

Then you can play the cached sample with:

ESDSound1.Play('shortname');

The shortname is CASE SENSITIVE!

When the control is enabled it keeps an open connection to the esound sound
server.  Set it to disabled to close that connection and unload the esdlib
library.  Set it to enabled to reload the library and connect to the sound
server
---------------------------------------------------------------------------}

unit esdsound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
{$IFDEF UNIX}
+  oldlinux,
+{$ENDIF}
+  lresources;
+
+const
+   ESD_BUF_SIZE = 4 * 1024;
+   ESD_KEY_LEN = 16;
+   ESD_DEFAULT_PORT = 16001;
+   ESD_DEFAULT_RATE = 44100;
+   ESD_NAME_MAX = 128;
+   ESD_ENDIAN_KEY = (ord('E') shl 24) + (ord('N') shl 16) + (ord('D') shl 8) + (ord('N'));
+   ESD_MASK_BITS = $000F;
+   ESD_BITS8 = $0000;
+   ESD_BITS16 = $0001;
+   ESD_MASK_CHAN = $00F0;
+   ESD_MONO = $0010;
+   ESD_STEREO = $0020;
+   ESD_MASK_MODE = $0F00;
+   ESD_STREAM = $0000;
+   ESD_SAMPLE = $0100;
+   ESD_ADPCM = $0200;
+   ESD_MASK_FUNC = $F000;
+   ESD_PLAY = $1000;
+   ESD_MONITOR = $0000;
+   ESD_RECORD = $2000;
+   ESD_STOP = $0000;
+   ESD_LOOP = $2000;
+
+type
+  Tesd_proto = (ESD_PROTO_CONNECT,ESD_PROTO_LOCK,ESD_PROTO_UNLOCK,
+    ESD_PROTO_STREAM_PLAY,ESD_PROTO_STREAM_REC,
+    ESD_PROTO_STREAM_MON,ESD_PROTO_SAMPLE_CACHE,
+    ESD_PROTO_SAMPLE_FREE,ESD_PROTO_SAMPLE_PLAY,
+    ESD_PROTO_SAMPLE_LOOP,ESD_PROTO_SAMPLE_STOP,
+    ESD_PROTO_SAMPLE_KILL,ESD_PROTO_STANDBY,
+    ESD_PROTO_RESUME,ESD_PROTO_SAMPLE_GETID,
+    ESD_PROTO_STREAM_FILT,ESD_PROTO_SERVER_INFO,
+    ESD_PROTO_ALL_INFO,ESD_PROTO_SUBSCRIBE,
+    ESD_PROTO_UNSUBSCRIBE,ESD_PROTO_STREAM_PAN,
+    ESD_PROTO_SAMPLE_PAN,ESD_PROTO_STANDBY_MODE,
+    ESD_PROTO_LATENCY,ESD_PROTO_MAX);
+
+type
+  Pesd_format_t = ^Tesd_format_t;
+  Tesd_format_t = longint;
+
+  Pesd_proto_t = ^Tesd_proto_t;
+  Tesd_proto_t = longint;
+
+  Poctet = ^Toctet;
+  Toctet = byte;
+  
+  TSoundFile = class(TObject)
+  private
+    FFileName: String;
+    FHandle: Longint;
+    FName: String;
+    procedure SetFileName(const AValue: String);
+    procedure SetHandle(const AValue: Longint);
+  public
+        property FileName: String read FFileName write SetFileName;
+        property Name: String read FName write FName;
+        property Handle: Longint read FHandle write SetHandle;
+  end;
+  PSoundFile = ^TSoundFile;
+  
+  TESDSound = class(TComponent)
+  private
+    //functions from DLL's
+    esd_audio_open: function:Longint; cdecl;
+    esd_file_cache: function (esd:longint; name_prefix:Pchar; filename:Pchar):longint; cdecl;
+    esd_lock: function (esd:longint):longint; cdecl;
+    esd_unlock: function(esd:longint):longint; cdecl;
+    esd_audio_close: procedure; cdecl;
+    esd_sample_cache: function(esd:longint; format:Tesd_format_t; rate:longint; length:longint; name:Pchar):longint;cdecl;
+    esd_sample_play: function(esd:longint; sample:longint):longint;cdecl;
+    esd_open_sound: function(host:Pchar):longint;cdecl;
+    esd_sample_getid: function(esd:longint; name:Pchar):longint;cdecl;
+    esd_close: function(esd:longint):longint;cdecl;
+    
+    esdlib: Pointer;
+    FEnabled: Boolean;
+    FHandle: Longint;
+    SoundFiles: PSoundFile; // dunno about this one
+    NumSoundFiles: Integer;
+    procedure SetEnabled(const AValue: Boolean);
+    procedure SetHandle(const AValue: Longint);
+    function initsound: Boolean;
+{$IFDEF UNIX}
+    function LoadLibrary(Aname: PChar): Pointer;
+    procedure CloseLibrary(AHandle: Pointer);
+    procedure LoadProcAddress(AHandle: Pointer; Aname: PChar; Address: Pointer);
+{$ENDIF}
+  public
+    Constructor Create(AOwner: TComponent); override;
+    Destructor Destroy; override;
+    procedure Play(ASample: String);
+    property Handle: Longint read FHandle write SetHandle;
+    function CacheSample(ASample, AName: String): Boolean;
+  published
+    Property Enabled: Boolean read FEnabled write SetEnabled;
+  end;
+
+procedure Register;
+
+implementation
+
+{$IFDEF UNIX}
+
+{LINKLIB m}
+
+function dlopen(AFile: PChar; mode: LongInt): Pointer; cdecl; external 'dl';
+function dlclose(handle: Pointer): LongInt; cdecl; external 'dl';
+function dlsym(handle: Pointer; name:PChar): Pointer; cdecl; external 'dl';
+
+{$ENDIF}
+
+{ TESDSound }
+
+{$IFDEF UNIX}
+
+function TESDSound.LoadLibrary(Aname: PChar): Pointer;
+begin
+     {$ifdef debug}
+     if ShowDebug > 0 then writeln('TESDSound::LoadLibrary');
+     {$endif}
+     try
+          Result := dlopen(Aname, 1);
+     except
+     end;
+end;
+
+procedure TESDSound.CloseLibrary(AHandle: Pointer);
+begin
+     {$ifdef debug}
+     if ShowDebug > 0 then writeln('TESDSound::CloseLibrary');
+     {$endif}
+
+     if assigned(AHandle) Then dlclose(AHandle);
+
+end;
+
+procedure TESDSound.LoadProcAddress(AHandle: Pointer; Aname: PChar;
+  Address: Pointer);
+begin
+  PPointer(Address)^ := dlsym(Ahandle, AName);
+end;
+
+{$ENDIF}
+
+function TESDSound.initsound: Boolean;
+begin
+{$IFDEF UNIX}
+     {$ifdef debug}
+     if ShowDebug > 0 then writeln('TESDSound::InitSound');
+     {$endif}
+     // load the sound libraries, if possible
+     try
+          esdlib := LoadLibrary('libesd.so.0');
+          if longint(esdlib) < 1 then esdlib := LoadLibrary('libesd.so'); // try this too
+          
+          if longint(esdlib) > 0 then begin
+              LoadProcAddress(esdlib, 'esd_audio_open', @esd_audio_open);
+              LoadProcAddress(esdlib, 'esd_file_cache', @esd_file_cache);
+              LoadProcAddress(esdlib, 'esd_lock', @esd_lock);
+              LoadProcAddress(esdlib, 'esd_unlock', @esd_unlock);
+              LoadProcAddress(esdlib, 'esd_audio_close', @esd_audio_close);
+              LoadProcAddress(esdlib, 'esd_audio_open', @esd_audio_open);
+              LoadProcAddress(esdlib, 'esd_sample_cache', @esd_sample_cache);
+              LoadProcAddress(esdlib, 'esd_sample_play', @esd_sample_play);
+              LoadProcAddress(esdlib, 'esd_open_sound', @esd_open_sound);
+              LoadProcAddress(esdlib, 'esd_sample_getid', @esd_sample_getid);
+              LoadProcAddress(esdlib, 'esd_close', @esd_close);
+              Result := True;
+          end else result := False;
+     except
+           Result := False;
+     end;
+{$ENDIF}
+end;
+
+
+procedure TESDSound.SetEnabled(const AValue: Boolean);
+begin
+{$IFDEF UNIX}
+     {$ifdef debug}
+     if ShowDebug > 0 then writeln('TESDSound::SetEnabled');
+     {$endif}
+  if FEnabled=AValue then exit;
+  if AValue = false then begin
+     FEnabled := AValue;
+     if not(csDesigning in componentstate) then begin
+       if handle > 0 then begin
+              //esd_audio_close;
+              esd_close(handle);
+       end;
+       closelibrary(esdlib);
+     end; // if not designing
+     exit;
+  end;
+
+  if not(csDesigning in componentstate) then begin
+    InitSound;
+    handle := esd_open_sound(nil);
+    if handle > 0 then FEnabled := True;
+    FEnabled := True;
+  end else FEnabled := AValue;
+{$ENDIF}
+end;
+
+procedure TESDSound.SetHandle(const AValue: Longint);
+begin
+  if FHandle=AValue then exit;
+  FHandle:=AValue;
+end;
+
+constructor TESDSound.Create(AOwner: TComponent);
+begin
+     {$ifdef debug}
+     if ShowDebug > 0 then writeln('TESDSound::Create');
+     {$endif}
+
+     Reallocmem(SoundFiles, 0);
+     NumSoundFiles := 0;
+
+     inherited Create(AOwner);
+
+end;
+
+
+destructor TESDSound.Destroy;
+var
+   I: Integer;
+begin
+{$IFDEF UNIX}
+     {$ifdef debug}
+     if ShowDebug > 0 then writeln('TESDSound::Destroy');
+     {$endif}
+     if handle > 0 then begin
+            esd_close(handle);
+     end;
+     closelibrary(esdlib);
+     if NumSoundFiles > 0 then begin
+        for I := 0 to NumSoundFiles-1 do begin
            SoundFiles[i].Free;
        end;
     end;
{$ENDIF}

     ReallocMem(SoundFiles, 0);

  inherited Destroy;
end;

procedure TESDSound.Play(ASample: String);
var
   I: Integer;
begin
{$IFDEF UNIX}
     if not(FEnabled) then begin
        // error
        raise exception.create('TESDSound:Play sample while disabled');
        exit;
     end;

     if NumSoundFiles > 0 then begin
        for I := 0 to NumSoundFiles - 1 do begin
            // first look at the name
            if SoundFiles[I].Name = ASample then begin
               // found it!
               if SoundFiles[i].Handle > 0 then begin
                 esd_sample_play(handle, SoundFiles[i].Handle);
                  exit;
               end;
            end else
            if SoundFiles[i].FileName = ASample then begin
               // found it by filename!
               if SOundFiles[i].Handle > 0 then begin
                  esd_sample_play(handle, SoundFiles[i].Handle);
                  exit;
               end;
            end; // if found by filename
        end;
     end;
    
     // didn't find it so try caching it...
     if CacheSample(ASample, ASample) then begin
       // got it... try to play it...
        if NumSoundFiles > 0 then begin
          for I := 0 to NumSoundFiles - 1 do begin
              // first look at the name
              if SoundFiles[I].Name = ASample then begin
                 // found it!
                 if SoundFiles[i].Handle > 0 then begin
                    esd_sample_play(handle, SoundFiles[i].Handle);
                    exit;
                 end;
              end else
              if SoundFiles[i].FileName = ASample then begin
                 // found it by filename!
                 if SOundFiles[i].Handle > 0 then begin
                    esd_sample_play(handle, SoundFiles[i].Handle);
                    exit;
                 end; // if handle > 0
              end; // if found by filename
          end; // for I
        end; // if numsoundfiles > 0
     end;
     // oh well, I give up... just leave
{$ENDIF}
end;

function TESDSound.CacheSample(ASample, AName: String): Boolean;
var
   I: Integer;
   MySoundFile: TSoundFile;
begin
{$IFDEF UNIX}
     {$ifdef debug}
     if ShowDebug > 0 then writeln('TESDSound::CacheSample "'+ASample+'" as "'+AName+'"');
     {$endif}
     if not(FEnabled) then begin
        // error
        raise exception.create('TESDSound:Cache sample while disabled');
        exit;
     end;
     // see if it's already cached...
     if NumSoundFiles > 0 then begin
        for I := 0 to NumSoundFiles - 1 do begin
            if SoundFiles[I].Name = AName then begin
               // see if it's changing the file to use
               if SoundFiles[i].FileName = ASample then exit; // nope
               // change the file...
               // I don't know how to release the cache, so just cache the new one
               SoundFiles[i].Handle := esd_File_Cache(handle,'',PChar(ASample));
               if SoundFiles[i].Handle < 1 then Result := False else Result := True;
               exit;
            end; // if found the name
        end; // loop I
     end; // if possibly cached...
     
     // not loaded, so cache it
     NumSoundFiles := NumSoundFiles + 1;
     ReallocMem(SoundFiles,sizeof(TSoundFile) * NumSoundFiles);
     MySoundFile := TSoundFile.Create;
     SoundFiles[NumSoundFiles-1] := MySoundFile;
     with MySoundFile do begin
          Name := AName;
          FileName := ASample;
     end;
     MySoundFile.Handle := esd_File_Cache(handle,'', pchar(ASample));
     if MySoundFile.Handle < 1 then Result := False else Result := True;
{$ENDIF}
end;

{ TSoundFile }

procedure TSoundFile.SetFileName(const AValue: String);
begin
  if FFileName=AValue then exit;
  FFileName:=AValue;
end;

procedure TSoundFile.SetHandle(const AValue: Longint);
begin
  if FHandle=AValue then exit;
  FHandle:=AValue;
end;

procedure Register;
begin
  RegisterComponents('GamePack',[TESDSound]);
end;


initialization
{$I esdsound.lrs}
end.

