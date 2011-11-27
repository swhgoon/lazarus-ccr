library nativetest;
{$ifdef fpc}
 {$mode delphi}
{$endif}

uses
  SysUtils,
  jni in 'jni.pas',
  log in 'log.pas';

const curClass:JClass=nil;
      nativeCodeLoaded:JfieldID=nil;

function Java_com_bero_nativetest_Main_stringFromJNI(env:PJNIEnv;this:jobject):jstring;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
var x:single;
begin
 __android_log_write(ANDROID_LOG_INFO,'nativetest','Java_com_bero_nativetest_Main_stringFromJNI entered');
 curEnv^.SetLongField(curEnv,curClass,nativeCodeLoaded,1);
 x:=8;
 result:=env^.NewStringUTF(env,pchar('Hello from native free pascal code by BeRo to the java world on the android platform ! '+floattostr(x*0.5)));
 __android_log_write(ANDROID_LOG_INFO,'nativetest','Java_com_bero_nativetest_Main_stringFromJNI exited');
end;
     
const NativeMethods:array[0..0] of JNINativeMethod=
       ((name:'stringFromJNI';
         signature:'()Ljava/lang/String;';
         fnPtr:@Java_com_bero_nativetest_Main_stringFromJNI;));

function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 curVM:=vm;
 __android_log_write(ANDROID_LOG_INFO,'nativetest','JNI_OnLoad called');
 if curVM^.GetEnv(curVM,@curEnv,JNI_VERSION_1_6)<>JNI_OK then begin
  __android_log_write(ANDROID_LOG_FATAL,'nativetest','curVM^.GetEnv failed');
  result:=JNI_ERR;
  exit;
 end;

 curClass:=curEnv^.FindClass(curEnv,'com/bero/nativetest/Main');
 if not assigned(curClass) then begin
  __android_log_write(ANDROID_LOG_FATAL,'nativetest','curEnv^.FindClass failed');
  result:=JNI_ERR;
  exit;
 end;
 if curEnv^.RegisterNatives(curEnv,curClass,@NativeMethods[0],length(NativeMethods))<0 then begin
  __android_log_write(ANDROID_LOG_FATAL,'nativetest','curEnv^.RegisterNatives failed');
  result:=JNI_ERR;
  exit;
 end;

 nativeCodeLoaded:=curEnv^.GetFieldID(curEnv,curClass,'nativeCodeLoaded','J');
 if not assigned(nativeCodeLoaded) then begin
  __android_log_write(ANDROID_LOG_FATAL,'nativetest','curEnv^.GetFieldID failed');
  result:=JNI_ERR;
  exit;
 end;

 result:=JNI_VERSION_1_6;
end;

procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
end;

exports JNI_OnLoad name 'JNI_OnLoad',
        JNI_OnUnload name 'JNI_OnUnload',
        Java_com_bero_nativetest_Main_stringFromJNI name 'Java_com_bero_nativetest_Main_stringFromJNI';

begin
end.
