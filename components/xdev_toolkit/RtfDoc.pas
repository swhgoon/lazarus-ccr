unit RtfDoc;

{
  Class for creating RTF document.

  Author:    Phil Hess.
  Copyright: Copyright (C) 2007 Phil Hess. All rights reserved.
  License:   Modified LGPL. This means you can link your code to this
             compiled unit (statically in a standalone executable or 
             dynamically in a library) without releasing your code. Only
             changes to this unit need to be made publicly available.
}

interface

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF} 

uses 
  SysUtils,
  RtfPars;
  
type
  TRtfDoc = class(TObject)
  private
    FParser : TRTFParser;
    procedure DoGroup;
    procedure DoCtrl;
    procedure DoText;
{$IFDEF FPC}
    procedure HandleError(s : ShortString);
{$ELSE}  {Delphi}
    procedure HandleError(s : string);
{$ENDIF}
  protected
    FFileName : string;
    FFileVar  : TextFile;
  public
    constructor Create;
    destructor Destroy; override;
    property Parser : TRTFParser read FParser;
    property FileName : string read FFileName;
    procedure Start(const FileName : string); virtual;
    procedure Done; virtual;
    procedure OutDefaultFontTable(DefFont : Integer); virtual;
    procedure OutToken(      AClass : Integer;
                             Major  : Integer;
                             Minor  : Integer;
                             Param  : Integer;
                       const Text   : string); virtual;
    procedure OutCtrl(Major : Integer;
                      Minor : Integer;
                      Param : Integer); virtual;
    procedure OutText(const Text : string); virtual;
  end;
  

implementation

constructor TRtfDoc.Create;
begin
  inherited Create;
  FParser := TRTFParser.Create(nil);
end;


destructor TRtfDoc.Destroy;
begin
  Parser.Free;
  inherited Destroy;
end;


procedure TRtfDoc.Start(const FileName : string);
var
  CurYear   : Word;
  CurMonth  : Word;
  CurDay    : Word;
  CurHour   : Word;
  CurMinute : Word;
  CurSecond : Word;
  CurSec100 : Word;
begin
  FFileName := FileName;
  AssignFile(FFileVar, FFileName);
  Rewrite(FFileVar);

  Parser.ResetParser;

  Parser.ClassCallbacks[rtfGroup] := DoGroup;
  Parser.ClassCallbacks[rtfText] := DoText;
  Parser.ClassCallbacks[rtfControl] := DoCtrl;
  Parser.DestinationCallbacks[rtfFontTbl] := nil;
  Parser.DestinationCallbacks[rtfColorTbl] := nil;
  Parser.DestinationCallbacks[rtfInfo] := nil;
  Parser.OnRTFError := HandleError;

  OutToken(rtfGroup, rtfBeginGroup, -1, rtfNoParam, '');
  OutCtrl(rtfVersion, -1, 1);
  OutCtrl(rtfCharSet, rtfAnsiCharSet, rtfNoParam);

   {Output document creation date and time}
  DecodeDate(Now, CurYear, CurMonth, CurDay);
  DecodeTime(Now, CurHour, CurMinute, CurSecond, CurSec100);
  OutToken(rtfGroup, rtfBeginGroup, -1, rtfNoParam, '');
  OutCtrl(rtfDestination, rtfInfo, rtfNoParam); 
  OutToken(rtfGroup, rtfBeginGroup, -1, rtfNoParam, '');
  OutCtrl(rtfSpecialChar, rtfICreateTime, rtfNoParam);
  OutCtrl(rtfSpecialChar, rtfIYear, CurYear);
  OutCtrl(rtfSpecialChar, rtfIMonth, CurMonth);
  OutCtrl(rtfSpecialChar, rtfIDay, CurDay);
  OutCtrl(rtfSpecialChar, rtfIHour, CurHour);
  OutCtrl(rtfSpecialChar, rtfIMinute, CurMinute);
  OutToken(rtfGroup, rtfEndGroup, -1, rtfNoParam, '');
  OutToken(rtfGroup, rtfEndGroup, -1, rtfNoParam, '');
  WriteLn(FFileVar);

end;  {TRtfDoc.Start}


procedure TRtfDoc.OutDefaultFontTable(DefFont : Integer);
begin
   {Output default font number}
  OutCtrl(rtfDefFont, -1, DefFont);
   {Output font table}
  OutToken(rtfGroup, rtfBeginGroup, -1, rtfNoParam, '');
  OutCtrl(rtfDestination, rtfFontTbl, rtfNoParam);
  OutToken(rtfGroup, rtfBeginGroup, -1, rtfNoParam, '');
  OutCtrl(rtfCharAttr, rtfFontNum, 0);
  OutToken(rtfControl, rtfFontFamily, rtfFFModern, rtfNoParam, 
           'Courier New;');
  OutToken(rtfGroup, rtfEndGroup, -1, rtfNoParam, '');
  OutToken(rtfGroup, rtfBeginGroup, -1, rtfNoParam, '');
  OutCtrl(rtfCharAttr, rtfFontNum, 1);
  OutToken(rtfControl, rtfFontFamily, rtfFFRoman, rtfNoParam, 
           'Times New Roman;');
  OutToken(rtfGroup, rtfEndGroup, -1, rtfNoParam, '');
  OutToken(rtfGroup, rtfBeginGroup, -1, rtfNoParam, '');
  OutCtrl(rtfCharAttr, rtfFontNum, 2);
  OutToken(rtfControl, rtfFontFamily, rtfFFSwiss, rtfNoParam, 
           'Arial;');
  OutToken(rtfGroup, rtfEndGroup, -1, rtfNoParam, '');
  OutToken(rtfGroup, rtfEndGroup, -1, rtfNoParam, '');
  WriteLn(FFileVar);
end;  {TRtfDoc.OutDefaultFontTable}


procedure TRtfDoc.Done;
begin
  WriteLn(FFileVar);
  OutToken(rtfGroup, rtfEndGroup, -1, rtfNoParam, '');
  CloseFile(FFileVar);
end;


procedure TRtfDoc.OutToken(      AClass : Integer;
                                 Major  : Integer;
                                 Minor  : Integer;
                                 Param  : Integer;
                           const Text   : string);
begin
  Parser.SetToken(AClass, Major, Minor, Param, Text);
  Parser.RouteToken;
  if Text <> '' then
    Write(FFileVar, Text);
end;


procedure TRtfDoc.OutCtrl(Major : Integer;
                          Minor : Integer;
                          Param : Integer);
begin
  OutToken(rtfControl, Major, Minor, Param, '');
end;


procedure TRtfDoc.OutText(const Text : string);
var
  CharNum : Integer;
begin
  for CharNum := 1 to Length(Text) do    
    OutToken(rtfText, Ord(Text[CharNum]), 0, rtfNoParam, '');
end;


procedure TRtfDoc.DoGroup;
begin
  if Parser.rtfMajor = rtfBeginGroup then
    Write(FFileVar, '{')
  else
    Write(FFileVar, '}');
end;


procedure TRtfDoc.DoCtrl;
var
  RtfIdx : Integer;
begin
  if (Parser.rtfMajor = rtfSpecialChar) and
     (Parser.rtfMinor = rtfPar) then
    WriteLn(FFileVar);  {Make RTF file more human readable}
  RtfIdx := 0;
  while rtfKey[RtfIdx].rtfKStr <> '' do
    begin
    if (Parser.rtfMajor = rtfKey[RtfIdx].rtfKMajor) and
       (Parser.rtfMinor = rtfKey[RtfIdx].rtfKMinor) then
      begin
      Write(FFileVar, '\');
      Write(FFileVar, rtfKey[RtfIdx].rtfKStr);
      if Parser.rtfParam <> rtfNoParam then
        Write(FFileVar, IntToStr(Parser.rtfParam));
      if rtfKey[RtfIdx].rtfKStr <> '*' then
        Write(FFileVar, ' ');
      Exit;
      end;
    Inc(RtfIdx);  
    end;
end;  {TRtfDoc.DoCtrl}


procedure TRtfDoc.DoText;
var
  AChar : Char;
begin
   {rtfMajor contains the character ASCII code,
     so just output it for now, preceded by \
     if special char.}
  AChar := Chr(Parser.rtfMajor);
  case AChar of
    '\' : Write(FFileVar, '\\');
    '{' : Write(FFileVar, '\{');
    '}' : Write(FFileVar, '\}');
    else
      begin
      if AChar > #127 then  {8-bit ANSI character?}
        Write(FFileVar, '\''' + IntToHex(Ord(AChar), 2))  {Encode using 7-bit chars}
      else  {7-bit ANSI character}         
        Write(FFileVar, AChar);
      end;
    end;
end;  {TRtfDoc.DoText}


{$IFDEF FPC}
procedure TRtfDoc.HandleError(s : ShortString);
begin
  WriteLn(StdErr, s);
end;
{$ELSE}  {Delphi}
procedure TRtfDoc.HandleError(s : string);
begin
  WriteLn(ErrOutput, S);
end;
{$ENDIF}



end.

