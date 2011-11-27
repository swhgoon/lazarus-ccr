library nativetest;

{$mode delphi}

uses
  SysUtils,
  jni,
  log;

const curClass:JClass=nil;
      nativeCodeLoaded:JfieldID=nil;

function Java_com_pascal_jnitest_AndroidJNITest_stringFromJNI(env:PJNIEnv;this:jobject):jstring; cdecl;
var x:single;
begin
  __android_log_write(ANDROID_LOG_INFO,'nativetest','Java_com_bero_nativetest_Main_stringFromJNI entered');
  curEnv^.SetLongField(curEnv,curClass,nativeCodeLoaded,1);
  x:=8;
  result:=env^.NewStringUTF(env,pchar('Hello from native free pascal code by BeRo to the java world on the android platform ! '+floattostr(x*0.5)));
  __android_log_write(ANDROID_LOG_INFO,'nativetest','Java_com_bero_nativetest_Main_stringFromJNI exited');
end;
     
function Java_com_pascal_jnitest_AndroidJNITest_intFromJNI(env:PJNIEnv;this:jobject): jint; cdecl;
begin
  Result := 8;
end;

const NativeMethods:array[0..1] of JNINativeMethod=
       ((name:'stringFromJNI';
         signature:'()Ljava/lang/String;';
         fnPtr:@Java_com_pascal_jnitest_AndroidJNITest_stringFromJNI;),
        (name:'intFromJNI';
         signature:'()I';
         fnPtr:@Java_com_pascal_jnitest_AndroidJNITest_intFromJNI;));

function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; cdecl;
begin
  curVM:=vm;
  __android_log_write(ANDROID_LOG_INFO,'nativetest','JNI_OnLoad called');
(*  __android_log_write(ANDROID_LOG_INFO,'nativetest',PChar(Format('CurVM=%x', [PtrInt(CurVM)])));
  if curVM^.GetEnv(curVM,@curEnv,JNI_VERSION_1_6)<>JNI_OK then begin
  __android_log_write(ANDROID_LOG_FATAL,'nativetest','curVM^.GetEnv failed');
  result:=JNI_ERR;
  exit;
  end;

  __android_log_write(ANDROID_LOG_INFO,'nativetest','Reading curClass');
  curClass:=curEnv^.FindClass(curEnv,'com/pascal/jnitest/AndroidJNITest');
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
  end;*)

  result:=JNI_VERSION_1_6;
end;

procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); cdecl;
begin
end;

exports
  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload',
  Java_com_pascal_jnitest_AndroidJNITest_stringFromJNI name 'Java_com_pascal_jnitest_AndroidJNITest_stringFromJNI',
  Java_com_pascal_jnitest_AndroidJNITest_intFromJNI name 'Java_com_pascal_jnitest_AndroidJNITest_intFromJNI';

begin
end.
