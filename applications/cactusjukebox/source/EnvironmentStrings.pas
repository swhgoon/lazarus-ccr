//******************************************************************************
//***                      COMMON FUNCTIONS                                  ***
//***                                                                        ***
//***        (c) Massimo Magnano 2000-2005                                   ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : EnvironmentStrings.pas
//
//  Description : Functions for expand String with Environment Variables inside.
//
//******************************************************************************
//  Exported Variables :
//         - All the System Variables like %SYSTEMROOT% , %TEMP% etc...
//
//         %SYSTEM-PATH%    Path to System Folder
//         %PROGRAM-PATH%   Application or Dll Path
//         %PROGRAM-EXE%    Application or Dll Full Name
//         %OS%             Operating System Type
//                               'WIN9X\'
//                               'WINNT\'
//                               'WIN32S\'
//                               'OTHER\'
//         %OS-MAJOR-VER%   Operating System Major Version Number
//         %OS-MINOR-VER%   Operating System Minor Version Number
//         %MAJOR-VER%      Application Major Version Number
//         %MINOR-VER%      Application Minor Version Number
//         %DATE-TIME%      Current Date-Time in the form yyyy-mm-dd-hh-nn-ss-z
//
//  For all Variables (except System Variables) the last char is always '\'
//
//  All Variables can be specified in the form :
//         %VarName:MaxLength%
//  In this Case the Var Value is Truncated (if Length>MaxLength)
//  or Expanded with spaces (if Length<MaxLength)   

unit EnvironmentStrings;

interface

uses Windows, SysUtils, ShlObj;

type
    TShellPaths = record
       VAR_NAME :String;
       nFolder  :Integer;
    end;

const
     MAX_Vars =5;

     VAR_SYSTEM_PATH  ='SYSTEM-PATH';
     VAR_PROGRAM_PATH ='PROGRAM-PATH';
     VAR_PROGRAM_EXE  ='PROGRAM-EXE';
     VAR_APPDATA      ='APPDATA';
     VAR_OS           ='OS';
     VAR_OS_MAJOR_VER ='OS-MAJOR-VER';
     VAR_OS_MINOR_VER ='OS-MINOR-VER';
     VAR_MAJOR_VER    ='MAJOR-VER';
     VAR_MINOR_VER    ='MINOR-VER';
     VAR_DATE_TIME    ='DATE-TIME';

     NumExcludedInStringASPARAM =5;
     Vars_ExcludedInStringASPARAM :array[0..NumExcludedInStringASPARAM-1] of String = (
        VAR_OS_MAJOR_VER,
        VAR_OS_MINOR_VER,
        VAR_MAJOR_VER,
        VAR_MINOR_VER,
        VAR_DATE_TIME
     );

     Vars :array[0..MAX_Vars-1] of String = (
        VAR_SYSTEM_PATH,
        VAR_PROGRAM_PATH,
        VAR_PROGRAM_EXE,
        VAR_APPDATA,
        VAR_OS
     );

     MAX_VAR_Shell =16;
     VAR_Shell : array [0..MAX_VAR_Shell-1] of TShellPaths =(
       (VAR_NAME :'DESKTOP_DIR'; nFolder :CSIDL_DESKTOPDIRECTORY),
       (VAR_NAME :'DESKTOP'; nFolder :CSIDL_DESKTOP),
       (VAR_NAME :'STARTMENU'; nFolder :CSIDL_STARTMENU),
       (VAR_NAME :'RECYCLEBIN'; nFolder :CSIDL_BITBUCKET),
       (VAR_NAME :'CONTROLPANEL'; nFolder :CSIDL_CONTROLS),
       (VAR_NAME :'MYCOMPUTER'; nFolder :CSIDL_DRIVES),
       (VAR_NAME :'FONTS'; nFolder :CSIDL_FONTS),
       (VAR_NAME :'NETHOOD'; nFolder :CSIDL_NETHOOD),
       (VAR_NAME :'NETWORK'; nFolder :CSIDL_NETWORK),
       (VAR_NAME :'PERSONAL'; nFolder :CSIDL_PERSONAL),
       (VAR_NAME :'PRINTERS'; nFolder :CSIDL_PRINTERS),
       (VAR_NAME :'PROGRAMS'; nFolder :CSIDL_PROGRAMS),
       (VAR_NAME :'RECENT'; nFolder :CSIDL_RECENT),
       (VAR_NAME :'SENDTO'; nFolder :CSIDL_SENDTO),
       (VAR_NAME :'STARTUP'; nFolder :CSIDL_STARTUP),
       (VAR_NAME :'TEMPLATES'; nFolder :CSIDL_TEMPLATES)
     );

     MAX_ExportedVars =(MAX_Vars+MAX_VAR_Shell+NumExcludedInStringASPARAM);

type
    TOnGetVariableFunction = function (Tag :TObject; Campo :String; var VarValue :String) :Boolean;

Var
   ExportedVars :array[0..MAX_ExportedVars-1] of String;
   MyVer        :String ='';
   MyMajorVer   :String ='';
   MyMinorVer   :String ='';
   MyLang       :String ='';
   ProgramPath  :String ='';
   ProgramEXE   :String ='';
   SystemPath   :String ='';

function ProcessPARAMString(Value: String; 
                            OnGetVariable :TOnGetVariableFunction =Nil;
                            OnGetVariableTag :TObject =Nil): String;

function StringASPARAM(Value: String; AddVars :array of string;
                       ProcessInternalVars :Boolean=False;
                       OnGetVariable :TOnGetVariableFunction =Nil;
                       OnGetVariableTag :TObject =Nil): String;

implementation

uses WindowsID, FileVer, Registry, ShellApi, ActiveX;

const
     APPDATA_PATH ='Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';


function GetApplicationDataPath :String;
Var
   Reg :TRegistry;

begin
     SetLength(Result, MAX_PATH);
     ExpandEnvironmentStrings('%APPDATA%', PChar(Result), MAX_PATH);
     Result :=PChar(Result);
     if (Result='') or not(DirectoryExists(Result)) then
     begin
          Reg :=TRegistry.Create;
          Reg.OpenKeyReadOnly(APPDATA_PATH);
          try
             Result :=Reg.ReadString('AppData');
          except
             Result :=ExtractFilePath(ParamStr(0));
          end;
          Reg.CloseKey;
          Reg.Free;
      end;
     if (Result<>'') then
     begin
          if (Result[Length(Result)]<>'\')
          then Result :=Result+'\';
      end;
end;


function ProcessPARAMString(Value: String;
                            OnGetVariable :TOnGetVariableFunction =Nil;
                            OnGetVariableTag :TObject =Nil): String;
Var
   auxStr,
   auxStr2,
   Campo             :String;
   index2,
   xpos1, xpos2,
   oldLength,
   toDel,
   maxChars           :Integer;
   Exist              :Boolean;
   LocalVarValue      :String;  //Non Posso Usare Result in GetStringProc, per problemi di stack

      function GetStringProc(Campo :String; var Exist :Boolean) :String;
      Var
         xStr       :String;
         i          :Integer;
         shellAlloc :IMAlloc;
         IDList     :PItemIDList;


      begin
        Exist :=False;

        if Assigned(OnGetVariable)                                         //new
        then Exist :=OnGetVariable(OnGetVariableTag, Campo, LocalVarValue);//new

        if Exist
        then Result :=LocalVarValue
        else begin
           if (Campo=VAR_SYSTEM_PATH)
           then begin
                     Result :=SystemPath;
                     Exist :=(SystemPath<>'');
                end
           else
           if (Campo=VAR_PROGRAM_PATH)
           then begin
                     Result :=ProgramPath;
                     Exist :=(ProgramPath<>'');
                end
           else
           if (Campo=VAR_PROGRAM_EXE)
           then begin
                     Result :=ProgramEXE;
                     Exist :=(ProgramEXE<>'');
                end
           else
           if (Campo=VAR_APPDATA)
           then begin
                     Result :=GetApplicationDataPath;
                     Exist :=(Result<>'');
                end
           else
           if (Campo=VAR_OS)
           then begin
                     Exist :=Win32_IsValidInfos;
                     if Exist then
                     Case Win32Platform of
                     VER_PLATFORM_WIN32_WINDOWS : Result :='WIN9X\';
                     VER_PLATFORM_WIN32_NT      : Result :='WINNT\';
                     VER_PLATFORM_WIN32s        : Result :='WIN32S\';
                     else                         Result :='OTHER\';
                     end;
                end
           else
           if (Campo=VAR_OS_MAJOR_VER)
           then begin
                     Exist :=Win32_IsValidInfos;
                     if Exist
                     then Result :=IntToStr(Win32MajorVersion)+'\';
                end
           else
           if (Campo=VAR_OS_MINOR_VER)
           then begin
                     Exist :=Win32_IsValidInfos;
                     if Exist
                     then Result :=IntToStr(Win32MinorVersion)+'\';
                end
           else
           if (Campo=VAR_MAJOR_VER)
           then begin
                     Exist :=(MyMajorVer<>'');
                     if Exist
                     then Result :=MyMajorVer+'\';
                end
           else
           if (Campo=VAR_MINOR_VER)
           then begin
                     Exist :=(MyMinorVer<>'');
                     if Exist
                     then Result :=MyMinorVer+'\';
                end
           else
           if (Campo=VAR_DATE_TIME)
           then begin
                     Exist :=True;
                     DateTimeToString(Result, 'yyyy-mm-dd-hh-nn-ss-z', Now);
                end
           else
           begin
                for i:=0 to MAX_VAR_Shell-1 do
                begin
                     if (Campo=VAR_Shell[i].VAR_NAME)
                     then begin
                            if (SHGetMalloc(shellAlloc)=NO_ERROR)
                            then begin
                                    SHGetSpecialFolderLocation(GetDesktopWindow,
                                                  VAR_Shell[i].nFolder, IDList);
                                    if (IDList<>Nil) then
                                    begin
                                         SetLength(xStr, MAX_PATH);
                                         SHGetPathFromIDList(IDList, PChar(xStr));
                                         xStr :=PChar(xStr);
                                         shellAlloc.Free(IDList);
                                    end;
                                 end;
                            Exist := (xStr<>'');
                            Break;
                          end;
                end;

                if not(Exist) then
                begin
                     SetLength(xStr, MAX_PATH);
                     ExpandEnvironmentStrings(PChar('%'+Campo+'%'), PChar(xStr), MAX_PATH);
                     xStr :=PChar(xStr);
                     Exist:=(pos('%', xStr)<=0);  //Se non c'è % e' una variable di sistema
                end;
                if Exist
                then Result :=xStr;
           end;
           end;
      end;

begin
     auxStr :=Value;
     xpos1 :=Pos('%', auxStr);
     While (xpos1>0) do
     begin
          auxStr[xpos1] :=' ';
          toDel :=Pos('%', auxStr)-xpos1+1;
          auxStr2 :=Copy(auxStr, xpos1+1, toDel-2);
          xpos2 :=Pos(':', auxStr2);
          if (xpos2>0) then begin
                                 //E' stata specificata la lunghezza massima della
                                 //Variabile nella forma %VarName:LunghezzaMax%
                                 Campo :=Copy(auxStr2, 1, xpos2-1);
                                 maxChars :=StrToInt(Copy(auxStr2, xpos2+1, MaxInt))
                                end
                       else begin
                                 Campo :=auxStr2;
                                 maxChars :=-1;
                             end;

          Exist :=False;
          auxStr2 :=GetStringProc(Uppercase(Campo), Exist);
          if Exist then
          begin
               if (maxChars>0) then
               begin
                    oldLength :=Length(auxStr2);
                    SetLength(auxStr2, maxChars);
                    if (oldLength<maxChars) then
                    for index2 :=oldLength+1 to maxChars do
                               auxStr2[index2] :=' ';
                end;
               Delete(auxStr, xpos1, toDel);
               Insert(auxStr2, auxStr, xpos1);
           end
          else Delete(auxStr, xpos1, toDel);
          xpos1 :=Pos('%', auxStr);
     end;
     Result :=auxStr;
end;

function StringASPARAM(Value: String; AddVars :array of string;
                       ProcessInternalVars :Boolean=False;
                       OnGetVariable :TOnGetVariableFunction =Nil;
                       OnGetVariableTag :TObject =Nil): String;
Var
   i      :Integer;
   UValue :String;

   procedure TryReplace(VarValue :String; VarName :String; var xResult :String);
   Var
      UVarValue :String;
      LVarValue,
      xpos      :Integer;

   begin
        UVarValue := Uppercase(VarValue);
        LVarValue := Length(UVarValue);

        xpos :=Pos(UVarValue, UValue);
        while (xpos>0) do
        begin
             Delete(UValue, xpos, LVarValue);
             Delete(xResult, xpos, LVarValue);
             Insert(VarName, UValue, xpos);
             Insert(VarName, xResult, xpos);
             xpos :=Pos(UVarValue, UValue);
        end;
   end;

begin
     UValue :=Uppercase(Value);
     Result :=Value;

     for i:=0 to Length(AddVars)-1 do
       TryReplace(ProcessPARAMString(AddVars[i], OnGetVariable, OnGetVariableTag),
                  AddVars[i], Result);

     if ProcessInternalVars
     then for i:=0 to MAX_ExportedVars-1 do
          begin
               if (ExportedVars[i]=Vars_ExcludedInStringASPARAM[0])
               then Break;//Exclude All Versions numbers from replace (ex. MyDir 1\ maybe replaced with "MyDir %OS-MINOR-VER%")

               TryReplace(ProcessPARAMString('%'+ExportedVars[i]+'%', OnGetVariable, OnGetVariableTag),
                          '%'+ExportedVars[i]+'%', Result);
          end;
end;

procedure CalcValues;
Var
   i :Integer;

begin
     SetLength(ProgramEXE, MAX_PATH);
     GetModuleFileName(HInstance, PChar(ProgramEXE), MAX_PATH);
     ProgramEXE :=PChar(ProgramEXE);

     ProgramPath :=ExtractFilePath(ProgramEXE);
     if (ProgramPath<>'') and (ProgramPath[Length(ProgramPath)]<>'\')
     then ProgramPath :=ProgramPath+'\';

     SetLength(SystemPath, MAX_PATH);
     GetSystemDirectory(PChar(SystemPath), MAX_PATH);
     SystemPath :=PChar(SystemPath);
     if (SystemPath<>'') and (SystemPath[Length(SystemPath)]<>'\')
     then SystemPath :=SystemPath+'\';

     MyVer :=GetFileVerLang(ParamStr(0), MyLang);
     if (MyVer<>'')
     then begin
               MyMajorVer :=Copy(MyVer, 1, Pos('.', MyVer)-1);
               MyMinorVer :=Copy(MyVer, Length(MyMajorVer)+1, Length(MyVer));
          end;

     for i:=0 to MAX_Vars-1 do
     begin
          ExportedVars[i] :=Vars[i];
     end;

     for i:=0 to MAX_VAR_Shell-1 do
     begin
          ExportedVars[i+MAX_Vars] :=VAR_Shell[i].VAR_NAME;
     end;

     //All Variables from this point are excluded in StringASPARAM
     for i :=0 to NumExcludedInStringASPARAM-1 do
     begin
          ExportedVars[i+MAX_Vars+MAX_VAR_Shell] :=Vars_ExcludedInStringASPARAM[i];
     end;
end;


initialization
   CalcValues;


end.
