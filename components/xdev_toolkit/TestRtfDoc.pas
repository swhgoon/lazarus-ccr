program TestRtfDoc;

{
  Test program for RtfDoc unit.
}

{$IFDEF FPC}
 {$MODE Delphi}
{$ELSE}
 {$APPTYPE CONSOLE}
{$ENDIF} 
{$R+,Q+}

uses
  SysUtils,
  RtfPars,  {Free Pascal unit with TRtfParser class and rtf constants}
  RtfDoc;   {Descendant class used in this program}
  
begin

  with TRtfDoc.Create do  {Create TRtfDoc object}
    begin
    try
  
      try
        Start('test.rtf');  {Create RTF file}
      except
        on EInOutError do  {File read-only or some other I/O error}
          begin
          WriteLn('Can''t create file');
          Exit;
          end;
        end;

      OutDefaultFontTable(2);  {Select font 2 (Arial) as default}  

      OutCtrl(rtfParAttr, rtfQuadCenter, rtfNoParam);  {Center line}
      OutCtrl(rtfCharAttr, rtfBold, 1);  {Turn on bolding} 
      OutText('Hello');  {Output some text}
      OutCtrl(rtfCharAttr, rtfBold, 0);  {Turn off bolding}
      OutText(' there!');  {Output some more text}
      OutCtrl(rtfSpecialChar, rtfPar, rtfNoParam);  {End of paragraph}

      Done;  {Close RTF file}
  
    finally
      Free;  {Free TRtfDoc object}
      end;
    end;

end.

