{ TRTFView a component to view documents in RTF format.

  Copyright (C) 2007 Jesus Reyes Aguilar jesusrmx@yahoo.com.mx

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit RTFView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, RichView,
  RVStyle,
  // RTFPars is a fpc/fcl provided unit.
  // Some changes that are needed for this component were fixed in
  // fpc 2.1.1 revision 6507. For older systems UsePre211RTFPars conditional
  // could be used.
  {$ifdef UsePre211RtfPars}
  RTFParsPre211
  {$else}
  RTFPars
  {$endif}
  ;

{.$define Debug}
{.$define DumpText}
{.$define DumpRtf}

type
  TTextAttributes=class
    Color: TColor;
    FontName: TFontName;
    Style: TFontStyles;
    Size: Integer;
  end;

  PStyleRec=^TStyleRec;
  TStyleRec=record
    FontDef: Integer;
    Font,FontSize: Integer;
    FontStyles: TFontStyles;
    ForeColor,BackColor: Integer;
    Centered: boolean;
    StyleChanged: boolean;
    LastStyle: Integer;
    NoText: boolean;
    GrpText: boolean;
    // aditional info
    OptionalDest: boolean;
  end;

  
  { TRTFView }

  TRTFView = class(TCustomRichView)
  private
    FCurStyle: TStyleRec;

    FParrafoParcial: boolean;
    FGroupLevel: Integer;
    FStyleGroupChanged: boolean;

    FStack: array of TStyleRec;
    FNoText: boolean;
    FGroupText: boolean;
    FPendingLine: boolean;
    FExpandingStyleLevel:Integer;

    FParser : TRTFParser;
    FIdent: Integer;
    FCurText: string;
    FDefaultForeColor: TColor;
    FDefaultBackColor: TColor;
    
    procedure ClearCurStyle;
    Procedure DoDestination;
    procedure doSpecialchar;
    procedure doCharAttribute;
    procedure doParAttributes;
    procedure doDocAttribute;
    procedure doSectAttribute;
    procedure DoCtrl;
    procedure DoGroup;
    Procedure DoWrite;
    function  GetCheckPoints: TStringlist;
    procedure HandleError ( s : shortstring);
    function  ParValue: string;
    {$IFDEF Debug}
    procedure DumpFonts;
    procedure DumpColors;
    procedure DumpStyles;
    {$ENDIF}
    // rutinas para cargar los estilos
    function  FindStyle: Integer;
    procedure StyleModified;
    procedure EmitirInterParrafo;
    procedure PopStyle;
    procedure PushStyle;
    procedure MergePreviousLine(txt: string; center: boolean; Rv: Integer);
    // debug
    procedure MyWriteLn(msg: string); overload;
    procedure MyWriteLn(msg1, Msg2: string); overload;
    procedure DebugSt(St: TStyleRec; msg:string='');
    procedure MyWriteLn(Msg: string; aInt: Integer);

  protected
    function GetCredits: string; override;
    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(aRTFFile: string);
    property ChkPoints:TStringlist read GetCheckPoints;
    //property Style;
  private
    Flog: TextFile;
    FLogIdent: Integer;
    FWaitingFirstControlWord: boolean;
    procedure ReadHandler;
    
  published
    property Align;
    property AllowSelection;
    property Anchors;
    property BackgroundBitmap;
    property BackgroundStyle;
    property DefaultForeColor:TColor read FDefaultForeColor write FDefaultForeColor;
    property DefaultBackColor:TColor read FDefaultBackColor write FDefaultBackColor;
    property FirstJumpNo;
    property HelpContext;
    property LeftMargin;
    property MaxTextWidth;
    property MinTextWidth;
    property PopupMenu;
    property RightMargin;
    property SingleClick;
    property TabOrder;
    property TabStop;
    property Tracking;
    property Visible;
    property VScrollVisible;

    property OnClick;
    property OnJump;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResized;
    property OnRVDblClick;
    property OnRVMouseMove;
    property OnRVRightClick;
    property OnSaveComponentToFile;
    property OnSelect;
    property OnURLNeeded;
    property OnVScrolled;

    //property OnSaveComponentToFile;
    //property Delimiters;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RichView',[TRTFView]);
end;

{ Generic routines }

function SomeText(Msg,Txt: string): string;
begin
  if txt<>'' then begin
    result:=Msg+'"'+Copy(Txt, 1, 15)+'|-->"';
  end else
    result:='';
end;

procedure TRTFView.MyWriteLn(msg: string);
begin
  {$IFDEF DEBUG}
  WriteLn(StringOfChar(' ', FIdent),Msg)
  {$ENDIF}
end;

procedure TRTFView.MyWriteLn(msg1, Msg2: string);
begin
  {$IFDEF DEBUG}
  WriteLn(StringOfChar(' ', FIdent),Msg1, Msg2);
  {$ENDIF}
end;

procedure TRTFView.MyWriteLn(Msg: string; aInt: Integer);
begin
  {$IFDEF DEBUG}
  WriteLn(StringOfChar(' ', FIdent),Msg, aInt);
  {$ENDIF}
end;

procedure TRTFView.ReadHandler;
begin
  case FParser.rtfClass of
    rtfGroup:
      begin
        if FParser.rtfMajor=rtfBeginGroup then begin;
          {$ifdef Debug}
          Write(FLog, StringOfChar(' ', FLogIdent),'{');
          {$endif}
          Inc(FLogIdent, 2);
          FWaitingFirstControlWord:=True;
        end else
        if FParser.rtfMajor=rtfEndGroup then begin
          FWaitingFirstControlWord:=False;
          Dec(FLogIdent, 2);
          {$IFDEF DEBUG}
          WriteLn(FLog, StringOfChar(' ', FLogIdent), '}');
          {$ENDIF}
        end;
      end;
    rtfControl:
      begin
        if FWaitingFirstControlWord then begin
          {$IFDEF DEBUG}
          WriteLn(FLog, FParser.GetRtfText);
          {$ENDIF}
          FWaitingFirstControlWord:=False;
        end
        {$IFDEF DEBUG}
        else
          WriteLn(FLog, StringOfChar(' ', FLogIdent), FParser.GetRtfText);
        if FCurText<>'' then begin
          WriteLn(FLog, SomeText('',FCurText));
        end;
        {$ENDIF}
      end;
       {
    rtfText:
      begin
        WriteLn(FLog, StringOfChar(' ', FLogIdent), SomeText('',FCurText));
      end;
        }
    rtfUnknown:
      begin
        if FWaitingFirstControlWord then begin
          {$IFDEF DEBUG}
          WriteLn(FLog, '[',FParser.GetRtfText,']');
          //WriteLn(FLog, StringOfChar(' ', FLogIdent), FParser.GetRtfText);
          {$ENDIF}
          FWaitingFirstControlWord:=False;
        end else
          {$IFDEF DEBUG}
          WriteLn(FLog, StringOfChar(' ', FLogIdent), '[',FParser.GetRtfText,']');
          {$ENDIF}
      end;
  end;
end;

function NumEstilo(Num:Integer): string;
begin
  if Num=rtfBasedOnNone then
    result := 'none'
  else
    result := IntToStr(Num);
end;

function FStoS(Fs:TFontstyles): string;
begin
  result:='';
  if fsBold in Fs then result := Result +'B';
  if fsItalic in Fs then result := result + 'I';
  if fsUnderline in Fs then result:=result +'U';
  if fsStrikeout in fs then result := result + 'S';
  result:='['+result+']';
end;

procedure TRTFView.DebugSt(St: TStyleRec; msg:string='');
var
  s: string;
begin
  if msg<>'' then
    msg:=msg+': ';
  s := StringOfChar(' ',FIdent);
  WriteLn(s, msg,
    'Fn=',St.Font,' FnSz=',St.FontSize,' Clr=',St.ForeColor,
    ' St=',fstos(St.FontStyles),' Cen=',St.Centered,' SC=',St.StyleChanged,
    ' LS=',St.LastStyle,' Nt=',St.NoText);
end;

function RGB(R,G,B: byte): TColor;
begin
  //result := (byte(b) shl 16) or (byte(g) shl 8) or byte(r);
  result := StringToColor('$'+IntToHex(b,2)+IntToHex(g,2)+IntToHex(r,2));
end;

function rtfColorToColor(C: PRtfColor; DefColor:TColor): TColor;
begin
  if (C^.rtfCBlue=-1)and(C^.rtfCGreen=-1)and(C^.rtfCRed=-1) then
    result := DefColor
  else
    result := Rgb(C^.rtfCRed, C^.rtfCGreen, C^.rtfCBlue);
end;


{ TRTFView }

procedure TRTFView.ClearCurStyle;
begin
  FCurStyle.Font       := FCurStyle.FontDef;
  FCurStyle.FontSize   := 12;
  FCurStyle.FontStyles := [];
  FCurStyle.ForeColor  := -1;
  FCurStyle.BackColor  := -1;
  FCurStyle.Centered   := False;
end;

procedure TRTFView.DoDestination;
begin
  FParser.skipgroup;
  MyWriteLn('DoDestination Group Skipped', FIdent);
end;

procedure TRTFView.doSpecialchar;
var
  RV: Integer;
  S: String;
  Li: TLineInfo;
  i: Integer;
begin
  case FParser.RTFMinor of
    rtfCurHeadPage          : MyWriteLn('rtfCurHeadPage');
    rtfCurFNote             : MyWriteLn('rtfCurFNote');
    rtfCurHeadPict          : MyWriteLn('rtfCurHeadPict');
    rtfCurHeadDate          : MyWriteLn('rtfCurHeadDate');
    rtfCurHeadTime          : MyWriteLn('rtfCurHeadTime');
    rtfFormula              : MyWriteLn('rtfFormula');
    rtfNoBrkSpace           : MyWriteLn('rtfNoBrkSpace');
    rtfNoReqHyphen          : MyWriteLn('rtfNoReqHyphen');
    rtfNoBrkHyphen          : MyWriteLn('rtfNoBrkHyphen');
    rtfPage                 : MyWriteLn('rtfPage');
    rtfLine:
      begin
        MyWriteLn('<--|  NOTED rtfLine');
        FPendingLine:=True;
        if FNoText then begin
          MyWriteLn('      Saltandose el texto');
          exit;
        end;
        RV := FindStyle;
        if FCurStyle.Centered then
          AddCenterLine(FCurText, Rv)
        else begin
          if FCurText='' then
            AddFromNewLine(' ', Rv)
          else begin
            if FParrafoParcial then
              Add(FCurText, RV)
            else
              AddFromNewLine(FCurText, Rv);
          end;
        end;
        {$IFDEF DUMPTEXT}
        WriteLn;
        {$ENDIF}
        FParrafoParcial:=False;
        FCurStyle.LastStyle:=RV;
        FCurStyle.StyleChanged:=False;
        FCurText := '';
      end;
    rtfpar:
      begin
        MyWriteLn('<--|  NOTED rtfpar');
        if FNoText then begin
          MyWriteLn('      Saltandose el texto');
          exit;
        end;
        RV := FindStyle;
        if FCurStyle.Centered then begin
          // if FPendingLine  then merge previous line with
          // this
          if FPendingLine then
            MergePreviousLine(FCurText, True, Rv)
          else
            AddCenterLine(FCurText, Rv)
        end else begin
          if FCurText='' then
            AddFromNewLine(' ', Rv)
          else begin
            if FParrafoParcial then
              Add(FCurText, RV)
            else
              AddFromNewLine(FCurText, Rv);
          end;
        end;
        {$IFDEF DUMPTEXT}
        WriteLn;
        {$ENDIF}
        FPendingLine:=False;
        FParrafoParcial:=False;
        FCurStyle.LastStyle:=RV;
        FCurStyle.StyleChanged:=False;
        FCurText := '';
      end;
    rtfSect                 : MyWriteLn('rtfSect');
    rtfTab                  : MyWriteLn('rtfTab');
    rtfCell                 : MyWriteLn('rtfCell');
    rtfRow                  : MyWriteLn('rtfRow');
    rtfCurAnnot             : MyWriteLn('rtfCurAnnot');
    rtfAnnotation           : MyWriteLn('rtfAnnotation');
    rtfAnnotID              : MyWriteLn('rtfAnnotID');
    rtfCurAnnotRef          : MyWriteLn('rtfCurAnnotRef');
    rtfFNoteSep             : MyWriteLn('rtfFNoteSep');
    rtfFNoteCont            : MyWriteLn('rtfFNoteCont');
    rtfColumn               : MyWriteLn('rtfColumn');
    rtfOptDest:
      begin
        MyWriteLn('NOTED rtfOptDest');
        if FExpandingStyleLevel=0 then
          FNoText := True;
        //Fparser.SkipGroup;
      end;
    rtfIIntVersion          : MyWriteLn('rtfIIntVersion');
    rtfICreateTime          : MyWriteLn('rtfICreateTime');
    rtfIRevisionTime        : MyWriteLn('rtfIRevisionTime');
    rtfIPrintTime           : MyWriteLn('rtfIPrintTime');
    rtfIBackupTime          : MyWriteLn('rtfIBackupTime');
    rtfIEditTime            : MyWriteLn('rtfIEditTime');
    rtfIYear                : MyWriteLn('rtfIYear');
    rtfIMonth               : MyWriteLn('rtfIMonth');
    rtfIDay                 : MyWriteLn('rtfIDay');
    rtfIHour                : MyWriteLn('rtfIHour');
    rtfIMinute              : MyWriteLn('rtfIMinute');
    rtfINPages              : MyWriteLn('rtfINPages');
    rtfINWords              : MyWriteLn('rtfINWords');
    rtfINChars              : MyWriteLn('rtfINChars');
    rtfIIntID               : MyWriteLn('rtfIIntID');
    else
      MyWriteLn('rtfSpecialChar, ??? rtfMinor=',FParser.rtfMinor);
  end;
end;

procedure TRTFView.doCharAttribute;
  function ParColor: string;
  var
    c: PRTFColor;
  begin
    c := FParser.Colors[Fparser.rtfParam];
    result := ' Color '+IntToStr(Fparser.rtfParam)+' =';
    if c=nil then
      result := Result + 'nil'
    else
      with c^ do
        result := Result + '['+SysUtils.Format('R=%d G=%d B=%d',
          [rtfCRed,rtfCGreen,rtfCBlue])+']';
  end;
  function Fuente: string;
  var
    f: PRTFFont;
  begin
    f := FParser.Fonts[FParser.rtfParam];
    result := ' Fuente '+IntToStr(Fparser.rtfParam)+' =';
    if f=nil then
      result := result + 'nil'
    else
      result := result + '['+SysUtils.Format('Name=%s Family=%d',
        [f^.rtfFName, f^.rtfFFamily])+']';
  end;
  function FontSize: string;
  begin
    result := ' Size='+IntToStr(Fparser.rtfParam);
  end;
  function CheckStyle(Fs: TFontStyle): boolean;
  var
    oldbool,Newbool: boolean;
  begin
    OldBool := Fs in FCurStyle.FontStyles;
    if FParser.rtfParam=rtfNoParam then
      NewBool := true
    else
      Newbool := FParser.rtfParam > 0;
    result := NewBool<>OldBool;
    if NewBool then
      Include(FCurStyle.FontStyles, Fs)
    else
      Exclude(FCurStyle.FontStyles, Fs);
    if Result then
      StyleModified;
  end;
begin
  case Fparser.RTFMinor of
    rtfPlain:
      begin
        StyleModified;
        FCurStyle.FontStyles := [];
        MyWriteLn('NOTED rtfPlain');
      end;
    rtfBold:
      begin
        CheckStyle(fsBold);
        MyWriteLn('NOTED rtfBold', ParValue);
      end;
    rtfItalic:
      begin
        CheckStyle(fsItalic);
        MyWriteLn('NOTED rtfItalic', ParValue);
      end;
    rtfStrikeThru:
      begin
        StyleModified;
        Include(FCurStyle.FontStyles, fsStrikeOut);
        MyWriteLn('NOTED rtfStrikeThru');
      end;
    rtfOutline              : MyWriteLn('rtfOutline');
    rtfShadow               : MyWriteLn('rtfShadow');
    rtfSmallCaps            : MyWriteLn('rtfSmallCaps');
    rtfAllCaps              : MyWriteLn('rtfAllCaps');
    rtfInvisible            : MyWriteLn('rtfInvisible');
    rtfFontNum:
      begin
        StyleModified;
        FCurStyle.Font := FParser.rtfParam;
        MyWriteLn('NOTED rtfFontNum', Fuente);
      end;
    rtfFontSize:
      begin
        StyleModified;
        MyWriteLn('NOTED rtfFontSize', FontSize);
        FCurStyle.FontSize := FParser.rtfParam div 2;
      end;
    rtfExpand               : MyWriteLn('rtfExpand');
    rtfUnderline:
      begin
        StyleModified;
        Include(FCurStyle.FontStyles, fsUnderline);
        MyWriteLn('NOTED rtfUnderline');
      end;
    rtfWUnderline:
      begin
        StyleModified;
        Include(FCurStyle.FontStyles, fsUnderline);
        MyWriteLn('TWEAK rtfWUnderline');
      end;
    rtfDUnderline:
      begin
        StyleModified;
        Include(FCurStyle.FontStyles, fsUnderline);
        MyWriteLn('TWEAK rtfDUnderline');
      end;
    rtfDbUnderline:
      begin
        StyleModified;
        Include(FCurStyle.FontStyles, fsUnderline);
        MyWriteLn('TWEAK rtfDbUnderline');
      end;
    rtfNoUnderline:
      begin
        StyleModified;
        Exclude(FCurStyle.FontStyles, fsUnderline);
        MyWriteLn('NOTED rtfNoUnderline');
      end;
    rtfSuperScript          : MyWriteLn('rtfSuperScript');
    rtfSubScript            : MyWriteLn('rtfSubScript');
    rtfRevised              : MyWriteLn('rtfRevised');
    rtfForeColor:
      begin
        FCurStyle.ForeColor := FParser.rtfParam;
        MyWriteLn('NOTED rtfForeColor', ParColor);
      end;
    rtfBackColor:
      begin
        FCurStyle.BackColor := FParser.rtfParam;
        MyWriteLn('NOTED rtfBackColor', ParColor);
      end;
    rtfGray                 : MyWriteLn('rtfGray');
  end;
end;

procedure TRTFView.doParAttributes;
var
  n: Integer;
begin
  case Fparser.RTFMinor of
    rtfStyleNum             :
      begin
        n := Fparser.rtfParam;
        MyWriteLn('NOTED rtfStyleNum Value=',NumEstilo(n));
        MyWriteLn('INI Expandiendo estilo FNoText='+BoolToStr(FnoText));
        Inc(FExpandingStyleLevel);
        Inc(FIdent, 3);
        Fparser.ExpandStyle(n);
        Dec(FExpandingStyleLevel);
        Dec(FIdent, 3);
        MyWriteLn('FIN Expandiendo estilo '+NumEstilo(n),
          ' FNoText='+BoolToStr(FnoText));
      end;
    rtfParDef:
      begin
        StyleModified;
        ClearCurStyle;
        MyWriteLn('NOTED rtfParDef');
      end;
    rtfQuadCenter:
      begin
        StyleModified;
        FCurStyle.Centered := True;
        MyWriteLn('NOTED rtfQuadCenter');
      end;
    rtfQuadLeft             : MyWriteLn('rtfQuadLeft');
    rtfQuadRight            : MyWriteLn('rtfQuadRight');
    rtfQuadJust             : MyWriteLn('rtfQuadJust');
    rtfFirstIndent          : MyWriteLn('rtfFirstIndent');
    rtfLeftIndent           : MyWriteLn('rtfLeftIndent', ParValue);
    rtfRightIndent          : MyWriteLn('rtfRightIndent', ParValue);
    rtfSpaceBefore          : MyWriteLn('rtfSpaceBefore', ParValue);
    rtfSpaceAfter           : MyWriteLn('rtfSpaceAfter', ParValue);
    rtfSpaceBetween         : MyWriteLn('rtfSpaceBetween', ParValue);
    rtfKeepNext             : MyWriteLn('rtfKeepNext');
    else
      MyWriteLn('rtfParAttr, ??? rtfMinor=',FParser.rtfMinor);
  end;
end;

procedure TRTFView.doDocAttribute;
begin
  case FParser.RTFMinor of
    rtfPaperWidth           : MyWriteLn('rtfPaperWidth', ParValue);
    rtfPaperHeight          : MyWriteLn('rtfPaperHeight', ParValue);
    rtfLeftMargin           : MyWriteLn('rtfLeftMargin', ParValue);
    rtfRightMargin          : MyWriteLn('rtfRightMargin', ParValue);
    rtfTopMargin            : MyWriteLn('rtfTopMargin', ParValue);
    rtfBottomMargin         : MyWriteLn('rtfBottomMargin', ParValue);
    rtfFacingPage           : MyWriteLn('rtfFacingPage');
    rtfGutterWid            : MyWriteLn('rtfGutterWid');
    rtfDefTab               : MyWriteLn('rtfDefTab');
    rtfWidowCtrl            : MyWriteLn('rtfWidowCtrl');
    rtfHyphHotZone          : MyWriteLn('rtfHyphHotZone');
    rtfFNoteEndSect         : MyWriteLn('rtfFNoteEndSect');
    rtfFNoteEndDoc          : MyWriteLn('rtfFNoteEndDoc');
    rtfFNoteText            : MyWriteLn('rtfFNoteText');
    rtfFNoteBottom          : MyWriteLn('rtfFNoteBottom');
    rtfFNoteStart           : MyWriteLn('rtfFNoteStart');
    rtfFNoteRestart         : MyWriteLn('rtfFNoteRestart');
    rtfPageStart            : MyWriteLn('rtfPageStart');
    rtfLineStart            : MyWriteLn('rtfLineStart');
    rtfLandscape            : MyWriteLn('rtfLandscape');
    rtfFracWidth            : MyWriteLn('rtfFracWidth');
    rtfNextFile             : MyWriteLn('rtfNextFile');
    rtfTemplate             : MyWriteLn('rtfTemplate');
    rtfMakeBackup           : MyWriteLn('rtfMakeBackup');
    rtfRTFDefault           : MyWriteLn('rtfRTFDefault');
    rtfRevisions            : MyWriteLn('rtfRevisions');
    rtfMirrorMargin         : MyWriteLn('rtfMirrorMargin');
    rtfRevDisplay           : MyWriteLn('rtfRevDisplay');
    rtfRevBar               : MyWriteLn('rtfRevBar');
    else
      MyWriteLn('rtfDocAttr, ??? rtfMinor=',FParser.rtfMinor);
  end;
end;

procedure TRTFView.doSectAttribute;
begin
  case FParser.RTFMinor of
    rtfSectDef              : MyWriteLn('rtfSectDef');
    rtfNoBreak              : MyWriteLn('rtfNoBreak');
    rtfColBreak             : MyWriteLn('rtfColBreak');
    rtfPageBreak            : MyWriteLn('rtfPageBreak');
    rtfEvenBreak            : MyWriteLn('rtfEvenBreak');
    rtfOddBreak             : MyWriteLn('rtfOddBreak');
    rtfPageStarts           : MyWriteLn('rtfPageStarts');
    rtfPageCont             : MyWriteLn('rtfPageCont');
    rtfPageRestart          : MyWriteLn('rtfPageRestart');
    rtfPageDecimal          : MyWriteLn('rtfPageDecimal');
    rtfPageURoman           : MyWriteLn('rtfPageURoman');
    rtfPageLRoman           : MyWriteLn('rtfPageLRoman');
    rtfPageULetter          : MyWriteLn('rtfPageULetter');
    rtfPageLLetter          : MyWriteLn('rtfPageLLetter');
    rtfPageNumLeft          : MyWriteLn('rtfPageNumLeft');
    rtfPageNumTop           : MyWriteLn('rtfPageNumTop');
    rtfHeaderY              : MyWriteLn('rtfHeaderY');
    rtfFooterY              : MyWriteLn('rtfFooterY');
    rtfLineModulus          : MyWriteLn('rtfLineModulus');
    rtfLineDist             : MyWriteLn('rtfLineDist');
    rtfLineStarts           : MyWriteLn('rtfLineStarts');
    rtfLineRestart          : MyWriteLn('rtfLineRestart');
    rtfLineRestartPg        : MyWriteLn('rtfLineRestartPg');
    rtfLineCont             : MyWriteLn('rtfLineCont');
    rtfTopVAlign            : MyWriteLn('rtfTopVAlign');
    rtfBottomVAlign         : MyWriteLn('rtfBottomVAlign');
    rtfCenterVAlign         : MyWriteLn('rtfCenterVAlign');
    rtfJustVAlign           : MyWriteLn('rtfJustVAlign');
    rtfColumns              : MyWriteLn('rtfColumns');
    rtfColumnSpace          : MyWriteLn('rtfColumnSpace');
    rtfColumnLine           : MyWriteLn('rtfColumnLine');
    rtfENoteHere            : MyWriteLn('rtfENoteHere');
    rtfTitleSpecial         : MyWriteLn('rtfTitleSpecial');
    else
      MyWriteLn('rtfSectAttr, ??? rtfMinor=',FParser.rtfMinor);
  end;
end;

procedure TRTFView.doctrl;
begin
  case Fparser.rtfmajor of
    rtfdestination         : dodestination;
    rtfspecialchar         : dospecialchar;
    rtfcharattr            : doCharAttribute;
    rtfparattr             : doParAttributes;
    rtfPosAttr:
      begin
        case Fparser.rtfMinor of
          rtfPosXCenter: MyWriteLn('rtfPosXCenter');
        else
          MyWriteLn('rtfPosAttr, ??? rtfMinor=',FParser.rtfMinor);
        end;
      end;
    rtfVersion:
      MyWriteLn('rtfversion ', ParValue);
    rtfDefFont:
      begin
        FCurStyle.FontDef := Fparser.rtfParam;
        MyWriteLn('NOTED rtfdeffont', ParValue);
      end;
    rtfCharSet:
      begin
        case FParser.rtfMinor of
          rtfMacCharSet:  MyWriteLn('rtfcharset Mac');
          rtfAnsiCharSet: MyWriteLn('rtfcharset Ansi');
          rtfPcCharSet:   MyWriteLn('rtfcharset PC');
          rtfPcaCharSet:  MyWriteLn('rtfcharset PCA');
          else
            MyWriteLn('rtfcharset  Desconocido');
        end;
      end;
    rtfStyleAttr:
      case FParser.rtfMinor of
        rtfBasedOn: MyWriteLn('rtfStyleAttr Basado en ', ParValue);
        rtfNext: MyWriteLn('rtfStyleAttr Siguiente ', ParValue);
      end;

    rtfDocAttr: doDocAttribute;
    rtfSectAttr: doSectAttribute;

    else
      MyWriteLn('doCtrl, ??? rtfMajor=',FParser.rtfMajor);
  end;
end;

procedure TRTFView.doGroup;
var
  rv: integer;
  s: string;
begin
  case FParser.RTFMajor of
    rtfBeginGroup:
      begin
        if FGroupText and (FCurText<>'') then begin
          MyWriteLn('*** DOGROUP__BeginGroup', SomeText(' WillWrite=',FCurText));
          EmitirInterParrafo;
        end;
        MyWriteLn('INI GROUP FNoText='+BoolToStr(FNoText));
        Inc(FGroupLevel);
        Inc(FIdent,3);
        PushStyle;
        FGroupText:=False;
      end;
    rtfEndGroup:
      begin
        //
        if FGroupText and (FCurText<>'') then begin
          // emitir el texto del grupo actual
          MyWriteLn('*** DOGROUP__EndGroup'{ FCurText="',FCurText+'"'});
          EmitirInterParrafo;
        end;
        PopStyle;
        Dec(FIdent,3);
        Dec(FGroupLevel);
        MyWriteLn('FIN GROUP FNoText='+BoolToStr(FNoText));
      end;
  end;
end;

procedure TRTFView.Dowrite;
var
  c: Char;
  n: Integer;
begin
  c := chr(FParser.RTFMajor);

  if FNoText  then begin
    MyWriteLn('DoWrite: Skipping '+c);
    exit;
  end;

  // checar si el estilo ha cambiado en medio de
  // un parrafo
  if FCurStyle.StyleChanged and (FCurText<>'') then
    EmitirInterParrafo;

  FCurText := FCurText + c;
  FGroupText := True;
  {$IFDEF DumpText}
  Write(c);
  {$ENDIF}


  // check for changes in style
  FCurStyle.StyleChanged := False;
end;

function TRTFView.GetCheckPoints: TStringlist;
begin
  result := CheckPoints;
end;

procedure TRTFView.handleerror(s: shortstring);
begin
  MyWriteLn ('ERROR: ', s);
end;

function TRTFView.ParValue: string;
begin
  if FParser.rtfParam=rtfNoParam then
    result := ' <NoParam>'
  else
    result := ' Value='+IntToStr(Fparser.rtfParam);
end;

{$IFDEF Debug}
procedure TRTFView.DumpFonts;
var
  F: PRtfFont;
  i,k: Integer;
begin
  WriteLn('FontDump ------');
  k:=0;
  for i:=0 to 100 do begin
    F := FParser.Fonts[i];
    if F=nil then
      continue;
    inc(k);
    WriteLn('  ',i,' Num=', F^.rtfFNum,' Name=', F^.rtfFName,
      ' Family=', F^.rtfFFamily);
  end;
  if k=0 then
    WriteLn('  -- NO FONTS FOUND --');
end;

procedure TRTFView.DumpColors;
var
  C: PRTFColor;
  i,k: Integer;
begin
  WriteLn('ColorDump -----');
  k:=0;
  for i:=0 to 100 do begin
    C := FParser.Colors[i];
    if C =nil then
      continue;
    inc(k);
    WriteLn('  ',i:2,' Num=',C^.rtfCNum:2,' R=',C^.rtfCRed:3,
      ' G=',C^.rtfCGreen:3,' B=',C^.rtfCBlue:3);
  end;
  if k=0 then
    WriteLn('  -- NO COLORS FOUND --');

end;

procedure TRTFView.DumpStyles;
var
  S: PRTFSTyle;
  i,k: Integer;
begin
  WriteLn('StylesDump -----');
  k:=0;
  for i:=0 to 100 do begin
    S := FParser.Styles[i];
    if S=nil then
      continue;
    inc(k);
    WriteLn('  ',i,' Num=',S^.rtfSNum,' Name=',S^.rtfSName,' BasedOn=',
      NumEstilo(S^.rtfSBasedOn));
  end;
  if k=0 then
    WriteLn('  -- NO STYLES FOUND --');
end;
{$ENDIF}

function TRTFView.FindStyle: Integer;
  function TxFont(const Fname: String): string;
  var
    I, IndxOrg,IndxDest: Integer;
  const
    FontEqArray:array[1..3,1..2] of string =
      (
        ('Arial',                 'nimbus sans l'),//'Suse Sans'),//'Helvetica'),
        ('Times New Roman',       'Times'),
        ('Courier New',           'Courier [Adobe]')
      );
  begin
    {$IFDEF MSWINDOWS}
    IndxOrg:=2;
    IndxDest:=1;
    {$ELSE}
    IndxOrg:=1;
    IndxDest:=2;
    {$ENDIF}

    for i:=low(FontEqArray) to High(FontEqArray) do
      if CompareText(FName, FontEqArray[i, IndxOrg])=0 then begin
        result := FontEqArray[i, IndxDest];
        exit;
      end;

    result := FName;
  end;
var
  C: PRtfColor;
  F: PRtfFont;
  Clr: TColor;
  N: String;
  i: Integer;
begin
  // usando los valores actuales buscar un estilo
  // que tenga estos mismos datos
  {$IFDEF DumpRTF}
  MyWriteLn('======BUSCANDO ESTILO=======-');
  {$ENDIF}

  Clr := FDefaultForeColor;
  if FCurStyle.ForeColor=-1 then
    MyWriteLn('------ Color no ajustado, usando DefaultForeColor')
  else begin
    C := FParser.Colors[FCurStyle.ForeColor];
    if C<>nil then begin
      Clr := RtfColorToColor(C, Clr);
      MyWriteLn('------ Color ['+IntToStr(FCurStyle.ForeColor)+']=',
        ColorToString(Clr));
    end else
      MyWriteLn('------ Color ['+IntToStr(FCurStyle.ForeColor)+']',
        ' NO HAYADO usando Default');
  end;

  F := Fparser.Fonts[FCurStyle.Font];
  if F<>nil then
    N := F^.rtfFName
  else
    N := 'Times New Roman';

  N := TxFont(N);

  with Style do
  for i:=0 to TextStyles.Count-1 do begin
    if (FCurStyle.FontSize = TextStyles[i].Size) and
       (Clr = TextStyles[i].Color) and
       (FCurStyle.FontStyles= TextStyles[i].Style) and
       (CompareText(N,TextStyles[i].FontName)=0)
    then begin
      {$IFDEF DumpRTF}
      MyWriteLn('------ESTILO HALLADO ------');
      MyWriteLn('------ Indx   =', i);
      {$ENDIF}
      Result := i;
      exit;
    end;
  end;

  // aun no existe, darlo de alta
  result := Style.AddTextStyle;
  with Style.TextStyles[result] do begin
    Size := FCurStyle.FontSize;
    Style := FCurStyle.FontStyles;
    FontName := N;
    Color := Clr;
    {$IFDEF DumpRTF}
    MyWriteLn('------CREANDO ESTILO------');
    MyWriteLn('------ Indx   = ', result);
    MyWriteLn('------ Size   = ', Size);
    MyWriteLn('------ Name   = ', FontName);
    MyWriteLn('------ Color  = ', ColorToString(Clr));
    MyWriteLn('------ Bold   = ', BoolToStr(fsBold in Style));
    MyWriteLn('------ Italic = ', BoolToStr(fsItalic in Style));
    MyWriteLn('------ Under  = ', BoolToStr(fsUnderLine in Style));
    MyWriteLn('------ Strike = ', BoolToStr(fsStrikeOut in Style));
    {$ENDIF}
    //Color := clBlack;
  end;
end;

procedure TRTFView.StyleModified;
begin
  if FCurText<>'' then
    EmitirInterParrafo;
  FCurStyle.StyleChanged:=True;
end;

procedure TRTFView.EmitirInterParrafo;
var
  N: Integer;
begin
  if FNoText then begin
    MyWriteLn('      Saltandose el texto');
    exit;
  end;
  MyWriteLn('*** Emitiendo InterParrafo');
  N:=FindStyle;
  if FCurStyle.LastStyle<>N then
    FCurStyle.LastStyle := N;

  if FCurStyle.Centered then begin
    if FPendingLine then begin
      MergePreviousLine(FCurText,True, N);
      FPendingLine:=False;
    end else
      AddCenterLine(FCurText, N)
  end else begin
    if FParrafoParcial then
      if FPendingLine then begin
        MergePreviousLine(FCurText, False, N);
        FPendingLine:=False;
      end else
        Add(FCurText, N)
    else begin
      if FPendingLine then begin
        MergePreviousLine(FCurText, False, N);
        FPendingLine:=False;
      end else
        AddFromNewLine(FCurText, N);
    end;
  end;

  FParrafoParcial:=True;
  FCurText := '';
end;

procedure TRTFView.PopStyle;
var
  i:Integer;
begin
  i := Length(FStack)-1;
  {$IFDEF DEBUG}
  DebugSt(FStack[i],'PopStyle');
  {$ENDIF}
  FGroupText    := FStack[i].GrpText;
  FNoText       := FStack[i].NoText;
  FCurStyle.BackColor    := FStack[i].BackColor;
  FCurStyle.Centered     := FStack[i].Centered;
  FCurStyle.ForeColor    := FStack[i].ForeColor;
  FCurStyle.Font      := FStack[i].Font;
  FCurStyle.FontSize     := FStack[i].FontSize;
  FCurStyle.FontStyles   := FStack[i].FontStyles;
  FCurStyle.StyleChanged := FStack[i].StyleChanged;
  FCurStyle.LastStyle    := FStack[i].LastStyle;
  SetLength(FStack, i);
end;

procedure TRTFView.PushStyle;
var
  i: Integer;
begin
  i := Length(FStack);
  SetLength(FStack, i+1);
  FStack[i].BackColor    := FCurStyle.BackColor;
  FStack[i].Centered     := FCurStyle.Centered;
  FStack[i].ForeColor    := FCurStyle.ForeColor;
  FStack[i].Font         := FCurStyle.Font;
  FStack[i].FontSize     := FCurStyle.FontSize;
  FStack[i].FontStyles   := FCurStyle.FontStyles;
  FStack[i].StyleChanged := FCurStyle.StyleChanged;
  FStack[i].LastStyle    := FCurStyle.LastStyle;
  FStack[i].NoText       := FNoText;
  FStack[i].GrpText      := FGroupText;
  {$IFDEF DEBUG}
  DebugSt(FStack[i],'PushStyle');
  {$ENDIF}
end;

procedure TRTFView.MergePreviousLine(txt: string; center: boolean; Rv: Integer
  );
var
  i: Integer;
  Li: TLineInfo;
begin
  i := LineCount-1;
  Li := TLineInfo(lines.Objects[i]);
  MyWriteLn('******** Pending Line "', Lines[i]+'"');
  MyWriteLn('********     New Line "', txt+'"');
  Li.Center := Center;
  Li.StyleNo := Rv;
  Lines[i] := Txt;
end;

function TRTFView.GetCredits: string;
begin
  result := 'TRTFView based on '+ inherited GetCredits;
end;

constructor TRTFView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := TRVStyle.Create(nil);
  FDefaultForeColor:=clBlack;
  FDefaultBackColor:=clWindow;
end;

destructor TRTFView.Destroy;
begin
  Style.Free;
  inherited Destroy;
end;

procedure TRTFView.LoadFromFile(aRTFFile: string);
var
  Stream : TFilestream;
begin
  FCurText := '';
  Style.TextStyles.Clear;
  Clear;
  Stream:=TFileStream.Create(aRTFFile,fmopenread);
  FParser:=TRTFParser.Create(Stream);
  {$ifdef debug}
  AssignFile(FLog, 'rtf.txt');
  Rewrite(FLog);
  WriteLn(FLog, 'procesando: ', aRTFFile);
  {$endif}
  FLogIdent:=0;
  FWaitingFirstControlWord:=False;
  try
    FParser.classcallbacks[rtfText]:=@DoWrite;
    FParser.classcallbacks[rtfcontrol]:=@DoCtrl;
    FParser.classcallbacks[rtfGroup]:=@DoGroup;
    FParser.onRTFError:=@HandleError;
    FParser.SetReadHook(@ReadHandler);
    FParser.StartReading;
    {$IFDEF DEBUG}
    DumpFonts;
    DumpColors;
    DumpStyles;
    {$ENDIF}
  finally
    {$IFDEF DEBUG}
    CloseFile(FLog);
    {$ENDIF}
    Fparser.Free;
    Stream.free;
    Format;
    Invalidate;
  end;
end;

initialization

{$I rtfview.lrs}

end.
