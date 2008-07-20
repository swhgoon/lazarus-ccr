program CvtHelp;

{

  Input files: HTML file created from WinHelp .rtf file by saving it
    in Word's "Web Page, Filtered" format. Also looks for WinHelp .hpj
    file with same name as HTML input file.

  Output file: HTML file that can be used with LCL apps that use
   HelpUtil unit's THelpUtilManager class.
   
  Author:    Phil Hess.
  Copyright: Copyright (C) 2007 Phil Hess. All rights reserved.
  License:   Modified LGPL.
}

{$IFDEF FPC}
 {$MODE Delphi}
{$ELSE}
 {$APPTYPE CONSOLE}
{$ENDIF} 
{$R+,Q+}

uses
  SysUtils,
  Classes;
  
const
  ProgramName    = 'CvtHelp';
  ProgramVersion = '0.01';
  
var
  OldFileName : string;
  HpjFileName : string;
  NewFileName : string;
  OldFileVar  : TextFile;
  HpjFileVar  : TextFile;
  NewFileVar  : TextFile;
{$IFNDEF FPC}
  MatchFound  : TFilenameCaseMatch;
{$ENDIF}
  MapSection  : TStringList;
  TitleStr    : string;
  CopyrightStr: string;
  Footnotes   : TStringList;
  InStr       : string;
  FnStr       : string;
  FnPos       : Integer;
  FnRef       : string;
  TopicStr    : string;
  TopicMapped : Boolean;
  BuildStr    : string;
  TopicPos    : Integer;
  FootIdx     : Integer;
  LinkPos     : Integer;
  NextLinkPos : Integer;
  UnlinkPos   : Integer;
       
begin

  if ParamCount < 2 then  {List program useage?}
    begin
    WriteLn(ProgramName, ', version ', ProgramVersion,
            ' - converts WinHelp/RTF-based HTML file to help HTML file.');
    WriteLn('Usage: ', ProgramName, ' infile outfile');
    Halt;
    end;

   {Get name of HTML file to convert from command line}
  OldFileName := ParamStr(1);
{$IFNDEF FPC}
  OldFileName := ExpandFileNameCase(OldFileName, MatchFound);
{$ELSE}
  OldFileName := ExpandFileName(OldFileName);
{$ENDIF}
  
  HpjFileName := ChangeFileExt(OldFileName, '.hpj');

   {Get name of HTML file to create from command line}
  NewFileName := ParamStr(2);
{$IFNDEF FPC}
  NewFileName := ExpandFileNameCase(NewFileName, MatchFound);
{$ELSE}
  NewFileName := ExpandFileName(NewFileName);
{$ENDIF}
  
  if (OldFileName = NewFileName) or
     (HpjFileName = NewFileName) then
    begin
    WriteLn('Can''t use input file name for output file');
    Halt;
    end;

   {Open input HTML file}
  AssignFile(OldFileVar, OldFileName);
  try
    Reset(OldFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t open input file ', OldFileName);
      Halt;
      end;
    end;

   {Open WinHelp project file}
  AssignFile(HpjFileVar, HpjFileName);
  try
    Reset(HpjFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t open WinHelp project file ', HpjFileName);
      Halt;
      end;
    end;

  MapSection := TStringList.Create;
  while not Eof(HpjFileVar) do
    begin
    ReadLn(HpjFileVar, InStr);
    if CompareText(InStr, '[OPTIONS]') = 0 then
      begin
      repeat
        ReadLn(HpjFileVar, InStr);
        if CompareText(Copy(InStr, 1, 6), 'TITLE=') = 0 then
          TitleStr := Copy(InStr, 7, MaxInt)
        else if CompareText(Copy(InStr, 1, 10), 'COPYRIGHT=') = 0 then
          CopyrightStr := Copy(InStr, 11, MaxInt);
      until Copy(InStr, 1, 1) = '[';
      end
    else if CompareText(InStr, '[MAP]') = 0 then
      begin
      repeat
        ReadLn(HpjFileVar, InStr);
        if (InStr <> '') and (Copy(InStr, 1, 1) <> '[') then
          MapSection.Add(Copy(InStr, 1, Pos(' ', InStr)-1) + '=' +
                         Trim(Copy(InStr, Pos(' ', InStr), MaxInt)));
      until Eof(HpjFileVar) or (Copy(InStr, 1, 1) = '[');
      end;
    end;
  CloseFile(HpjFileVar);

   {Create output HTML file}
  AssignFile(NewFileVar, NewFileName);
  try 
    Rewrite(NewFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t create output file ', NewFileName);
      Halt;
      end;
    end;

  Footnotes := TStringList.Create;

   {Read through entire file, saving footnote topic references and names}
  while not Eof(OldFileVar) do  
    begin
    ReadLn(OldFileVar, InStr);
    if CompareText(InStr, '<div><br clear=all>') = 0 then {Found footnotes?}
      begin
      while not Eof(OldFileVar) do
        begin
        ReadLn(OldFileVar, InStr);
        if Pos('MsoFootnoteText', InStr) > 0 then  {Found a footnote?}
          begin
          FnStr := InStr;
          repeat
            ReadLn(OldFileVar, InStr);
            if InStr <> '' then
              FnStr := FnStr + ' ' + InStr;
          until InStr = '';
          TopicStr := Copy(FnStr, Pos('</a>', FnStr) + 5, MaxInt);
          TopicStr := Copy(TopicStr, 1, Length(TopicStr)-4);
          if Pos('_', TopicStr) > 0 then
            begin
            FnPos := Pos('name=', FnStr) + 6;
            FnRef := '';
            repeat
              FnRef := FnRef + FnStr[FnPos];
              Inc(FnPos);
            until FnStr[FnPos] = '"';
            Footnotes.Add(FnRef + '=' + TopicStr);
            end;
          end;
        end;
      end;
    end;

  Reset(OldFileVar);
   {Process input file}
  while not Eof(OldFileVar) do  
    begin
    ReadLn(OldFileVar, InStr);
    if CompareText(Copy(InStr, 1, 7), '<title>') = 0 then  {Found title?}
      begin  {Replace with title from .hpj file}
      WriteLn(NewFileVar, '<title>', TitleStr, '</title>');
       {Include AppleTitle line so file can be registered as help book on OS X.}
      Write(NewFileVar, '<meta name="AppleTitle" content="');
      if CompareText(Copy(TitleStr, Length(TitleStr)-4, 5), ' Help') = 0 then
        Write(NewFileVar, Copy(TitleStr, 1, Length(TitleStr)-5)) {Don't include}
      else
        Write(NewFileVar, TitleStr);
      WriteLn(NewFileVar, '">');
       {Include copyright statement from .hpj file}
      WriteLn(NewFileVar, '<!--');
      WriteLn(NewFileVar, CopyrightStr);
      WriteLn(NewFileVar, '-->');
      end                                                

    else if (Pos('class=Topictitle', InStr) > 0) or
            (Pos('class=Topic>', InStr) > 0) then  {Found a topic?}
      begin
       {Get footnote number _ftnXX}
      FnPos := Pos('#_', InStr) + 1;
      FnRef := '';
      repeat
        FnRef := FnRef + InStr[FnPos];
        Inc(FnPos);
      until InStr[FnPos] = '"';
       {Insert anchor}
      TopicStr := Footnotes.Values[FnRef];
      if TopicStr <> '' then
        begin
        TopicMapped := False;
        if MapSection.Values[TopicStr] <> '' then
          begin
          WriteLn(NewFileVar, '<a name="', MapSection.Values[TopicStr],
                  '"></a>');
          TopicMapped := True;
          end;
        if not TopicMapped then  {No mapping in project file for topic?}
          begin  {Just use topic name in anchor}
          Write(NewFileVar, '<a name="', TopicStr, '"></a>');
          end;
        end;
       {Save part of 1st topic line}
      if Pos('class=Topictitle', InStr) > 0 then
        Write(NewFileVar, Copy(InStr, 1, Pos('class=Topictitle>', InStr)+16))
      else
        Write(NewFileVar, Copy(InStr, 1, Pos('class=Topic>', InStr)+11));
      BuildStr := InStr;
      repeat  {Get rest of topic lines}
        ReadLn(OldFileVar, InStr);
        if InStr <> '' then
          BuildStr := BuildStr + ' ' + InStr; 
      until InStr = '';
      TopicPos := Length(BuildStr);
      repeat
        Dec(TopicPos);
      until (TopicPos = 0) or (BuildStr[TopicPos] = '>');
      WriteLn(NewFileVar, Copy(BuildStr, TopicPos+2, MaxInt));
      end

    else if CompareText(InStr, '<div><br clear=all>') = 0 then {Found footnotes?}
      begin
      repeat  {Skip over footnotes}
        ReadLn(OldFileVar, InStr);
      until CompareText(InStr, '</body>') = 0;
      WriteLn(NewFileVar, InStr);
      end

    else  {Found normal line}
      begin  {See if it contains link to topic}
      LinkPos := Pos('<u>', InStr);
      if LinkPos > 0 then  {Line contains link?}
        begin
        BuildStr := InStr;
        repeat  {Link may span lines, so get rest of paragraph}
          ReadLn(OldFileVar, InStr);
          if InStr <> '' then
            BuildStr := BuildStr + ' ' + InStr;
        until InStr = '';
        InStr := BuildStr;
        while LinkPos > 0 do
          begin
          NextLinkPos := Pos('<u>', Copy(InStr, LinkPos+1, MaxInt));
          if NextLinkPos = 0 then
            NextLinkPos := Length(InStr) + 1
          else
            Inc(NextLinkPos, LinkPos); 
          BuildStr := '';
          for FootIdx := 0 to Footnotes.Count - 1 do
            begin
            TopicStr := Footnotes.ValueFromIndex[FootIdx];
            if (Pos(TopicStr, InStr) > LinkPos) and
               (Pos(TopicStr, InStr) < NextLinkPos) then
              begin
              BuildStr := Copy(InStr, 1, LinkPos-1) + '<a href="#';
              if MapSection.Values[TopicStr] <> '' then
                BuildStr := BuildStr + MapSection.Values[TopicStr]
              else
                BuildStr := BuildStr + TopicStr;
              UnlinkPos := Pos('</u>', InStr);
              BuildStr := BuildStr + '">' +
                          Copy(InStr, LinkPos+3, UnlinkPos-LinkPos-3) + '</a>' +
                          Copy(InStr, UnlinkPos+4, MaxInt);  
              InStr := BuildStr;
              LinkPos := Pos('<u>', InStr);
              Break;
              end;
            end;
          if BuildStr = '' then
            Break;
          end;
        WriteLn(NewFileVar, InStr);
        end
      else
        WriteLn(NewFileVar, InStr);
      end;
    end;  

  MapSection.Free;
  Footnotes.Free;
  CloseFile(OldFileVar);
  CloseFile(NewFileVar);

end.

