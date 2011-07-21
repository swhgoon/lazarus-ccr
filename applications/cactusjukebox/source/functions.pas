
{

  some helper functions/procedures

  written by Sebastian Kraft
  sebastian_kraft@gmx.de

  This software is free under the GNU Public License

  (c)2005-2008
}

Unit functions;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, crt, math, config;



//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function crc32(path: String): longint;

Function crc32_math(path: String): int64;

Function DirectoryIsEmpty(Directory: String): Boolean;

Function EraseDirectory(Directory: String): Boolean; //delete directory and all subdirectories/files in it

Function UTF8toLatin1(utfstring: ansistring): ansistring;

Function Latin1toUTF8(latin1string: ansistring): ansistring;

Function rmZeroChar(s: ansistring): ansistring;

Function FileCopy(Const FromFile, ToFile: String): boolean;

Function FreeSpaceOnDAP: int64;

Function ByteToFmtString(bytes: int64; d1, d2: byte): string;
// converts i.e. 1024 to 1,0 KB
// d1, d2 sets amount of digits before and after ','

Function SecondsToFmtStr(seconds: longint): string;//converts integer to mm:ss time format

Function MSecondsToFmtStr(MSeconds: longint): string;

function MakeValidFilename(Filename: String): string;

procedure BubbleSort(var Items: TStrings);

function IntTodB(i, ref: longint):integer;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


Implementation

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function crc32(path: String): longint;
//creates an very, very basic checksum to identify files

Var fhandle: THandle;
  buf: array [0..63] Of word;
  z: byte;
  i, eofile: longint;
  l: longint;
Begin
 {$Q-}
  fhandle := sysutils.fileopen(path, fmOpenRead);
  l := 0;
  i := 0;
  z := 0;
  eofile := 0;
  While (eofile<>-1) And (i<256) Do
    Begin
      eofile := FileRead(fhandle, buf, sizeof(buf));
      If (eofile<>-1) Then For z:=0 To high(buf) Do begin
                            L := L+buf[z];
                        end;
      inc(i);
    End;
  FileClose(fhandle);
  result := l;
 {$Q+}
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function crc32_math(path: String): int64;
//creates an very, very basic checksum to identify files

Var fhandle: THandle;
  buf: array [0..63] Of int64;
  z: byte;
  i, eofile: longint;
  l: int64;
Begin
  fhandle := sysutils.fileopen(path, fmOpenRead);
  l := 0;
  i := 0;
  z := 0;
  eofile := 0;
  While (eofile<>-1) And (i<256) Do
    Begin
      eofile := FileRead(fhandle, buf, high(buf));
      l := l+sumInt(buf);
      inc(i);
    End;
  FileClose(fhandle);
  result := l;

End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function EraseDirectory(Directory: String): Boolean;

Var Srec: TSearchRec;
Begin
  result := false;
  If DirectoryExists(Directory)Then
    Begin
      Try
        FindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, Srec);
        Repeat
          Begin
            If (Srec.Name <> '.') And (Srec.Name <> '..') Then
              DeleteFile(Directory+DirectorySeparator+Srec.Name);
          End;
        Until FindNext(Srec)<>0;
        FindClose(Srec);
        result := RemoveDir(Directory);
      Except
        result := false;
      End;
    End;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function UTF8toLatin1(utfstring: ansistring): ansistring;

Var i: integer;
  tmps: string;
  utf16: boolean;
Begin
  i := 0;
  tmps := '';
  utf16 := false;

  If length(utfstring)>0 Then
    Begin
      Repeat
        Begin
          inc(i);
          Case byte(utfstring[i]) Of 
            $ff: If byte(utfstring[i+1])=$fe Then utf16 := true;
            $c3:
                 Begin
                   Delete(utfstring, i, 1);
                   utfstring[i] := char(byte(utfstring[i])+64);
                 End;
            $c2:
                 Begin
                   Delete(utfstring, i, 1);
                   dec(i);
                 End;
          End;
        End;
      Until (i>=length(utfstring)-1) Or utf16;
      //if utf16 detected
      If utf16 Then
        Begin
          i := i+2;
          writeln('utf16');
          Repeat
            Begin
              inc(i);
              If byte(utfstring[i])<>0 Then tmps := tmps+utfstring[i];
            End;
          Until (i>=length(utfstring));
        End;
    End;

  If Not utf16 Then result := utfstring
  Else Result := tmps;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function rmZeroChar(s: ansistring): ansistring;

Var i: integer;
Begin
  i := 0;
  If s<>'' Then
    Begin
      Repeat
        Begin
          inc(i);
          If byte(s[i])=0 Then
            Begin
              Delete(s, i, 1);
              dec(i);
            End;
        End;
      Until i>=length(s)-1;
    End;
  Result := s;
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function Latin1toUTF8(latin1string: ansistring): ansistring;

Var i: integer;
  c: char;
  tmps: string;
  utf16: boolean;
Begin
  i := 0;
  utf16 := false;
  If length(latin1string)>0 Then
    Begin
      Repeat
        Begin
          inc(i);
          Case byte(latin1string[i]) Of 
            $ff: If byte(latin1string[i+1])=$fe Then utf16 := true;
            $00..$1f:
                      Begin
                        Delete(latin1string, i, 1);
                        dec(i);
                      End;
            $c0..$fd:
                      Begin
                        //c0..ff ist der gesamte wertebereich!!
                        If (byte(latin1string[i])=$c3) And (byte(latin1string[i+1])<$C0) Then inc(i)
                        Else
                          Begin
                            latin1string[i] := char(byte(latin1string[i])-64);
                            insert(char($c3), latin1string, i);
                            inc(i);
                          End;
                      End;

{     $a1..$bf:  begin
                        c:=latin1string[i];
                        insert(char($c2), latin1string, i);
//                        utfstring[i]:=char(byte(utfstring[i])+64);
                        inc(i);
                      end;}
          End;
        End;
      Until (i>=length(latin1string)-1) Or utf16;
      //if utf16 detected
      If utf16 Then
        Begin

//latin1string:=AnsiToUtf8(latin1string); may also work instead of following own utf16->utf8 routine
          inc(i);
          Repeat
            Begin
              inc(i);
              If byte(latin1string[i])>$1f Then
                If byte(latin1string[i])<$c0 Then
                  tmps := tmps+char(byte(latin1string[i]))
              Else
                tmps := tmps+char($c3)+char(byte(latin1string[i])-64);
            End;
          Until (i>=length(latin1string));
        End;
    End;
  If Not utf16 Then result := latin1string
  Else Result := tmps;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{   Function to copy a file FromFile -> ToFile    , mainly used while upload to player device}
Function FileCopy(Const FromFile, ToFile: String): boolean;

Var 
  FromF, ToF: file;
  NumRead, NumWritten: Word;
  Buf: array[1..4096] Of byte;
Begin
  Try
    AssignFile(FromF, FromFile);
    Reset(FromF, 1);              { Record size = 1 }
    AssignFile(ToF, ToFile);      { Open output file }
    Rewrite(ToF, 1);              { Record size = 1 }
    Repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(ToF, Buf, NumRead, NumWritten);
    Until (NumRead = 0) Or (NumWritten <> NumRead);
    CloseFile(FromF);
    CloseFile(ToF);
    result := true;
  Except
    result := false;
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function FreeSpaceOnDAP: int64;

Var tmps: string;
Begin
  tmps := GetCurrentDir;
  // get free memory on player, format string
  SetCurrentDir(CactusConfig.DAPPath);
  result := DiskFree(0);
  writeln('------>');
  writeln(DiskFree(0));
  SetCurrentDir(tmps);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function ByteToFmtString(bytes: int64; d1, d2: byte): string;

Var r: real;
  count: byte;
  comma, prefix, s1, s2: string;
  subzero:boolean;
Begin
  count := 0;
  if bytes>=0 then subzero:=false else subzero:=true;
  
  r := abs(bytes);
  While (r>=power(10, d1)) Do
    Begin
      r := r / 1024;
      inc(count);
    End;

  Case count Of 
    0: prefix := 'Byte';
    1: prefix := 'KB';
    2: prefix := 'MB';
    3: prefix := 'GB';
    4: prefix := 'TB';
    5: prefix := 'PB';
  End;

  str(round (r*power(10, d2)) , s2);

  If r >= 1 Then
    Begin
      s1 := copy(s2, 0, length(s2)-d2);
      s2 := copy(s2, length(s2)-d2+1, d2);
    End
  Else s1 := '0';

  If d2<>0 Then comma := ','
  Else
    Begin
      comma := '';
      s2 := '';
    End;
  if subzero=false then result := s1+comma+s2+' '+prefix else result := '- ' + s1+comma+s2+' '+prefix
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function SecondsToFmtStr(seconds: longint): string;

Var min, sec: longint;
  s, s2: string;
Begin
if seconds>0 then begin
  min := seconds Div 60;
  sec := seconds Mod 60;
  str(min, s);
  str(sec, s2);
  If min<10 Then s := '0'+s;
  If sec<10 Then s2 := '0'+s2;
  result := s+':'+s2;
 end else Result:='00:00';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function MSecondsToFmtStr(MSeconds: longint): string;
Begin
  if MSeconds>1000 then result := SecondsToFmtStr(MSeconds Div 1000)
        else Result:='00:00';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function DirectoryIsEmpty(Directory: String): Boolean;

Var 
  SeR: TSearchRec;
  i: Integer;
Begin
  Result := False;
  FindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, SeR);
  For i := 1 To 2 Do
    If (SeR.Name = '.') Or (SeR.Name = '..') Then
      Result := FindNext(SeR) <> 0;
  FindClose(SeR);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function MakeValidFilename(Filename: String): string;
var
  I: integer;
  { for long file names } // FIXME taken from code for win32 - list correct/complete??
  LongForbiddenChars  : set of Char = ['<', '>', '|', '"', '\', '/', ':', '*', '?'];
begin
  for I := 1 to Length(Filename) do
    if (Filename[I] in LongForbiddenChars) then
      Filename[I] := ' ';
  result := Filename;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure BubbleSort(var Items: TStrings);
var
  done: boolean;
  i, n: integer;
  Dummy: string;
begin
  n := Items.Count;

  repeat
    done := true;
    for i := 0 to n - 2 do
      if Items[i] > Items[i + 1] then
      begin
        Dummy := Items[i];
        Items[i] := Items[i + 1];
        Items[i + 1] := Dummy;

        done := false;
      end;
  until done;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function IntTodB(i, ref: longint): integer;
var dB: Real;
begin
  if i=0 then db:=0.001 else dB:=i;
  dB:= 20*log10(dB/ref);
  result:=round(dB);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

End.
