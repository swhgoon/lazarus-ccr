
{
  Unit for showing DebugOutput

  written by Sebastian Kraft
  sebastian_kraft@gmx.de

  This software is free under the GNU Public License

  (c)2007
}

Unit debug;

{$mode objfpc}{$H+}

Interface

Uses 
SysUtils;

Var CVerbosityLevel: Integer;
  //Current verbosity level.


{ Verbosity 0     -> absolutely no output
  Verbosity 1     -> standard output
  Verbosity 1..9  -> more specific output }

  //DebugOut checks verbosity level and only outputs string s when it
  //fits debug level.
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function DebugOutLn(s: String; level: integer): boolean;
Function DebugOut(s: String; level: integer): boolean;
Function DebugOutLn(s: integer; level: integer): boolean;
Function DebugOut(s: integer; level: integer): boolean;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Implementation
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function DebugOutLn(s: String; level: integer): boolean;
Begin
  If (CVerbosityLevel>0) And (CVerbosityLevel>=level) Then
    Begin
      writeln(s);
    End;
End;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function DebugOut(s: String; level: integer): boolean;
Begin
  If (CVerbosityLevel>0) And (CVerbosityLevel>=level) Then
    Begin
      write(s);
    End;
End;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function DebugOutLn(s: integer; level: integer): boolean;
Begin
  If (CVerbosityLevel>0) And (CVerbosityLevel>=level) Then
    Begin
      writeln(s);
    End;
End;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Function DebugOut(s: integer; level: integer): boolean;
Begin
  If (CVerbosityLevel>0) And (CVerbosityLevel>=level) Then
    Begin
      write(s);
    End;
End;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
End.
