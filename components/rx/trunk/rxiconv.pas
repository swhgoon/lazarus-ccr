(* 
  A.Voitov zprokuror(cyka)narod.ru 
  LAZARUS-FPC-LINUX codeset conversion routines 
  
  The goal is easy-and-on-fly *.lrs conversion from codeset used by developer to 
  user's system codeset without gettext, without separate message files etc. 
  
  Now I only have to set my codeset below (by default it's mine - UTF-8) - 
  DON'T FORGET ABOUT IT - 
  and call LocalizeForm('TFormClassName') right after {$I TFormClassName.lrs} - 
  see example below. 
  This way form resource file will be converted to system codeset when form is 
  creating. 

  For gtk/gnome there's only one conversion routine 
      function Localize(source:string):ansistring; 
  For gtk2 resource MUST, I guess, be converted to UTF-8 anyway (if it's not yet), 
  so I set current_codeset to UTF-8 with FORCE_UTF8 definition. Though if 
  developer's codeset is UTF-8 then no form resource conversion needed. 

  Localize is published function so it can be used again to convert string constants, 
  resource strings in most cases with no care about user's and developer's codeset. 
  But it's not enough sometime (file names, inifiles etc. can be wrong when gtk2 
  works in koi8-r locale). 
  That's why we've got some stuff th handle it without care. 
  1. First one is TIconv object (I'm not sure it's best way to do maybe it would class? 
     if so uncomment $DEFINE OBJ_IS_CLASS below) 
  2. Second are uiconv_xxx functions 
  They can be used to easy implement any valid conversions. 

  EXAMPLES: 
  1. Auto form conversion 
     - set my codeset below: {$DEFINE DC_UTF8} 
     - add LocalizeForm('TFormClassName') after {$I TFormClassName.lrs} 
     <CODE> 
       unit1; 
       [...] 
       initialization 
         {$I unit1.lrs} 
         LocalizeForm('TForm1'); 
       end. 
     </CODE> 
  2. TIconv object example: 
     object is local variable here, but it can be global (create at startup, 
     dead in the end) if it's always in-use. 
     <CODE> 
       function koi8r_to_utf8(S:string):string; 
       var iConverter:pIconv; 
       begin 
         iConverter:=NewIconv('CP1251', 'UTF-8'); 
         Result:=iConverter.iconv(S); 
         iConverter.Free; 
       end; 
     </CODE> 
  3. uiconv_xxx example - obvious. 
  
  APPENDIX 
  i. KNOWN DEVELOPER'S CODESET DEFENITIONS: 
   $DC_KOI8 (obvious) 
   $DC_UTF8 (obvious) - default 
   $DC_WIN  (CP1251)  - not tested yet 
  ii. GTK2 NOTES 
   As I found there's something wrong with some strings when gtk2 is used under 
   koi8-r locale. OpenDialog.FileName is stored in my inifile as koi8-r string 
   and then can't be loaded in MRU menu. 
   So I've got a couple of functions here for that case: str2gtk and gtk2str. 
   'str' means current codeset (locale) 
  iii. This unit tested with dc_utf8 and user's locale koi8-r only 
*) 

unit rxiconv; 
{* Developer's codeset. Must be set on design-time. Default is UTF8}
{.$DEFINE DC_KOI8} 
{$DEFINE DC_WIN}
{.$DEFINE DC_UTF8} //nothing defined so DC_UTF8 will be defined here 
{$IFNDEF DC_UTF8} 
    {$IFNDEF DC_KOI8} 
        {$IFNDEF DC_WIN} 
            {$DEFINE DC_UTF8} 
        {$ENDIF DC_WIN} 
    {$ENDIF DC_KOI8} 
{$ENDIF DC_UTF8} 

{* user acces to iconv functions} 
{$DEFINE USER_ICONV} 
{* TIconv object} 
{$DEFINE USE_OBJECT} 
{* Ticonv is class(TObject)} 
{$DEFINE OBJ_IS_CLASS} 

{* form-localization needed only with gtk1 or with gtk2 if developer's codeset 
is not utf-8. First turn it of.} 
{$UNDEF USE_LOCALIZE} 
{* if widgetset is GTK2 ($IFDEF LCLGtk2) and developer's codeset is not DC_UTF8 
   then we'd FORCE_UTF8 and turn on USE_LOCALIZE. And if widgetset is GTK1 then 
   we USE_LOCALIZE too.} 
{$IFDEF LCLGtk2} 
    {$IFNDEF DC_UTF8} 
         {$DEFINE USE_LOCALIZE} 
         {$DEFINE FORCE_UTF8} 
    {$ENDIF DC_UTF8} 
{$ELSE LCLGtk2} 
         {$DEFINE USE_LOCALIZE} 
{$ENDIF LCLGtk2} 

{$mode objfpc} 
interface 

{$IFNDEF WINDOWS}
uses
  {$IFDEF USE_LOCALIZE} LResources, Classes,{$ENDIF} initc, SysUtils; 

  {* returns current codeset} 
  function GetCodeset():ansistring; 
  {$IFDEF USE_LOCALIZE} 
  {* returns true if current codeset<>developer's codeset} 
  function InvalidCodeset():boolean; 
  {$ENDIF USE_LOCALIZE} 
  {* basic string conversion - enough for localization. 
     conversion from developer's codeset 2 user's codeset} 
  function Localize(source:ansistring):ansistring; 
  {with gtk2 converts utf8 to locale codeset and back} 
  {$IFDEF LCLGtk2} 
  function str2gtk(source:ansistring):ansistring; 
  function gtk2str(source:ansistring):ansistring; 
  {$ENDIF LCLGtk2} 
  {* public access 2 iconv} 
  {$IFDEF USER_ICONV} 
  function uiconv(ic_usr:pointer; source:ansistring):ansistring ; 
  function uiconv_open(ic_from, ic_to:ansistring):pointer ; 
  procedure uiconv_close(ic_usr:pointer) ; 
  {$ENDIF USER_ICONV} 
  {* form conversion} 
  procedure LocalizeForm(form_classname:ansistring); 
  procedure LocalizeAllForm;

{* iconv object/class} 
{$IFDEF USE_OBJECT} 
  type 
    {$IFDEF OBJ_IS_CLASS} 
    Ticonv=class(TObject) 
    {$ELSE OBJ_IS_CLASS} 
    pIconv=^Ticonv; 
    Ticonv=object 
    protected 
    {$ENDIF OBJ_IS_CLASS} 
      hIconv:pointer; 
    public 
      destructor Destroy;{$IFDEF OBJ_IS_CLASS}virtual;{$ENDIF OBJ_IS_CLASS} 
    {$IFNDEF OBJ_IS_CLASS} 
      procedure Free; 
    {$ENDIF OBJ_IS_CLASS} 
      constructor Create{$IFDEF OBJ_IS_CLASS}(cs_from, cs_to:ansistring){$ENDIF OBJ_IS_CLASS}; 
      function iconv(source:ansistring):ansistring; 
    end; 
    {$IFNDEF OBJ_IS_CLASS} 
    function NewIconv(cs_from, cs_to:ansistring):TIconv ; 
    {$ENDIF OBJ_IS_CLASS} 
{$ENDIF USE_OBJECT} 
{$ENDIF}
implementation 
{$IFNDEF WINDOWS}
uses dialogs;
{$linklib c} 

const 
  libiconvname='c'; 
  __LC_CTYPE = 0; 
  _NL_CTYPE_CLASS = (__LC_CTYPE shl 16); 
  _NL_CTYPE_CODESET_NAME = (_NL_CTYPE_CLASS)+14; 
  CODESET = _NL_CTYPE_CODESET_NAME; 
{developer's codeset names} 
{$IFDEF DC_KOI8} 
   DEV_CODESET='KOI8-R'; 
   DC_NAME='KOI'; 
   DC_NAME_EXT='R'; 
{$ENDIF DC_KOI8} 
{$IFDEF DC_UTF8} 
   DEV_CODESET='UTF-8'; 
   DC_NAME='UTF'; 
   DC_NAME_EXT='8'; 
{$ENDIF DC_UTF8} 
{$IFDEF DC_WIN} 
   DEV_CODESET='CP1251'; 
   DC_NAME='1251'; 
   DC_NAME_EXT='1251'; 
{$ENDIF DC_WIN} 

type 
  size_t   = cardinal; 
  pSize    = ^size_t; 
  psize_t  = pSize; 
  cInt     = longint; 
  piconv_t = ^iconv_t; 
  iconv_t  = pointer; 
  nl_item  = cint; 
  
var //iconv pointers 
  {$IFDEF LCLGtk2} 
  ic_str2gtk, ic_gtk2str, 
  {$ENDIF LCLGtk2} 
  ic_localize  : iconv_t; 

function nl_langinfo(__item:nl_item):pchar;cdecl;external libiconvname name 'nl_langinfo'; 
function iconv_open(__tocode:pchar; __fromcode:pchar):iconv_t;cdecl;external libiconvname name 'iconv_open'; 
function iconv(__cd:iconv_t; __inbuf:ppchar; __inbytesleft:psize_t; __outbuf:ppchar; __outbytesleft:psize_t):size_t;cdecl;external libiconvname name 'iconv'; 
function iconv_close(__cd:iconv_t):cint;cdecl;external libiconvname name 'iconv_close'; 

{* common procedures} 
function GetCodeset():ansistring; 
begin 
Result:= ansistring(nl_langinfo(CODESET)); 
end; 

function CodesetIs(CSNAME, CSEXT:ansistring):boolean ; 
var CS:ansistring; 
begin 
  CS:=UpperCase(GetCodeSet); 
  if ((pos(CSNAME,CS)<>0) and (pos(CSEXT,CS)<>0)) then Result:=true else Result:=false; 
end; 

{* main conversion procedure} 
function _iconv(hiconv:iconv_t; source:ansistring):ansistring; 
  const 
    ESysEILSEQ      = 84; 
    ESysE2BIG       = 7; 
  var 
    len:SizeInt; 
    outlength, 
    outoffset, 
    outleft : size_t; 
    srcpos, 
    destpos: pchar; 
    mynil : pchar; 
    my0 : size_t; 
begin 
    mynil:=nil; 
    my0:=0; 
    // extra space 
    len:=length(source); 
    outlength:=len*3+1;    //setlength(result,outlength); 
    Result:=StringOfChar(#0, outlength); 
    //outlength:=len+1; 
    srcpos:=pChar(source); 
    destpos:=pchar(result); 
    outleft:=outlength*2; 
    while iconv(hiconv,@srcpos,@len,@destpos,@outleft)=size_t(-1) do 
      begin 
        case fpgetCerrno of 
         ESysEILSEQ: 
            begin 
              { skip and set to '?' } 
              inc(srcpos); 
              pwidechar(destpos)^:='?'; 
              inc(destpos,2); 
              dec(outleft,2); 
              { reset } 
              iconv(hiconv,@mynil,@my0,@mynil,@my0); 
            end; 
          ESysE2BIG: 
            begin 
              outoffset:=destpos-pchar(result); 
              { extend } 
              setlength(result,outlength+len); 
              inc(outleft,len*2); 
              inc(outlength,len); 
              { string could have been moved } 
              destpos:=pchar(result)+outoffset; 
            end; 
          else 
            raise EConvertError.Create('iconv error '+IntToStr(fpgetCerrno)); 
        end; 
      end; 
    //setlength(result,length(result)-outleft div 2); // truncate string 
    Result:=TrimRight(Result);// not shure it always works right 
end; 

{$IFDEF USE_OBJECT} 
{* Ticonv *} 
{$IFNDEF OBJ_IS_CLASS} 
function _NewIconv(cs_from, cs_to:ansistring):pIconv ; 
begin 
  New( Result, Create); 
  Result^.hIconv:=Pointer(iconv_open(pChar(cs_to),  pChar(cs_from))); 
end; 

function NewIconv(cs_from, cs_to:ansistring):TIconv ; 
begin 
  Result:=_NewIconv(cs_from, cs_to)^; 
end; 

procedure Ticonv.Free(); 
begin 
  if @Self<>nil then Self.Destroy; 
end; 
{$ENDIF OBJ_IS_CLASS} 

constructor Ticonv.Create{$IFDEF OBJ_IS_CLASS}(cs_from, cs_to:ansistring){$ENDIF OBJ_IS_CLASS}; 
begin {$IFDEF OBJ_IS_CLASS} 
inherited Create; 
hIconv:=Pointer(iconv_open(pChar(cs_to),  pChar(cs_from))); 
{$ENDIF OBJ_IS_CLASS} 
end; 

destructor Ticonv.Destroy; 
begin 
   iconv_close(hIconv); 
   Inherited; 
end; 

function Ticonv.iconv(source:ansistring):ansistring; 
begin 
   Result:=_iconv(hIconv, source); 
end; 
{$ENDIF USE_OBJECT} 

{$IFDEF USER_ICONV} 
function uiconv(ic_usr:pointer; source:ansistring):ansistring ; 
begin 
  Result:=_iconv(iconv_t(ic_usr), pchar(source)); 
end; 

function uiconv_open(ic_from, ic_to:ansistring):pointer ; 
begin 
  Result:= iconv_open(pchar(ic_to), pchar(ic_from)); 
end; 

procedure uiconv_close(ic_usr:pointer) ; 
begin 
  iconv_close(iconv_t(ic_usr)); 
end; 
{$ENDIF USER_ICONV} 

function Localize(source:ansistring):ansistring; 
begin 
  Result:=_iconv(ic_localize, source); 
end; 

{$IFDEF LCLGtk2} 
function str2gtk(source:ansistring):ansistring; 
begin 
  Result:=_iconv(ic_str2gtk, source); 
end; 

function gtk2str(source:ansistring):ansistring; 
begin 
  Result:=_iconv(ic_gtk2str, source); 
end; 
{$ENDIF LCLGtk2} 

{* form localization - only if use_localize} 
{$IFDEF USE_LOCALIZE} 
function InvalidCodeset():boolean; 
begin 
Result:=False; 
{$IFNDEF FORCE_UTF8} Result:= not (CodesetIs(DC_NAME, DC_NAME_EXT));{$ENDIF FORCE_UTF8} 
end; 

{* converts form resource from developer's codeset to env codeset. 
   it's empty procedure if widgetSet is gtk2 and dev's codeset is utf8. 
   So USE_LOCCALIZE definition used } 
procedure LocalizeForm(form_classname:ansistring); 
    var res : TLResource; 
        S : ansistring; 
        RS, MS : TMemoryStream; 
begin 
    {$IFNDEF FORCE_UTF8} //always convert to utf8 
    if not InvalidCodeset then  exit; 
    {$ENDIF FORCE_UTF8} 
    {find resource} 
    res:=LazarusResources.Find(form_classname); 
    RS:=TMemoryStream.create; 
    MS:=TMemoryStream.create; 
    {read form 2 RS} 
    RS.Write(res.Value[1],length(res.Value)); 
    RS.Position:=0; 
    {convert 2 text} 
    LRSObjectBinaryToText( RS, MS); 
    MS.Position:=0; 
    {copy 2 string} 
    SetLength(S, MS.Size); 
    MS.Read(S[1], MS.Size); 
    {convert 2 ccs or utf8 - under gtk2} 
    S:=Localize(S); 
    {copy back to ms} 
    S:=Trim(S) + #0#0#0#0;  //doesn't work without it... 
    MS.Position:=0; 
    MS.Write(S[1],length(S)); 
    MS.Position:=0; 
    RS.SetSize(0); 
    {convert 2 binary RS} 
    LRSObjectTextToBinary(MS, RS); 
    RS.Position:=0; 
    SetLength(S, RS.Size); 
    {write 2 resource} 
    RS.Read(S[1],RS.Size); 
    res.Value:=S; 
    MS.Free;  RS.Free; 
end;

procedure LocalizeAllForm;
var
  i:integer;
begin
  for i:=0 to LazarusResources.Count - 1 do
  begin
    if LazarusResources.Items[i].ValueType = 'FORMDATA' then
      LocalizeForm(LazarusResources.Items[i].Name);
  end;
end;

{$ELSE USE_LOCALIZE} 
{* dummy proc for easy testing} 
procedure LocalizeForm(form_classname:ansistring);begin {*} end; 

procedure LocalizeAllForm;
begin
end;

{$ENDIF USE_LOCALIZE}

initialization 
{$IFNDEF FORCE_UTF8} 
  ic_localize := iconv_open(nl_langinfo(CODESET), DEV_CODESET); //main 
{$ELSE FORCE_UTF8} 
  ic_localize := iconv_open('UTF-8', DEV_CODESET); 
{$ENDIF FORCE_UTF8} 
{$IFDEF LCLGtk2} 
  ic_str2gtk:= iconv_open('UTF-8', nl_langinfo(CODESET)); 
  ic_gtk2str:= iconv_open(nl_langinfo(CODESET), 'UTF-8'); 
{$ENDIF LCLGtk2} 
finalization 
  iconv_close(ic_localize); 
{$IFDEF LCLGtk2} 
  iconv_close(ic_str2gtk); 
  iconv_close(ic_gtk2str); 
{$ENDIF LCLGtk2} 
{$ENDIF}
end.
