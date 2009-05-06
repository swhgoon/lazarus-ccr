(*
  this file is a part of audio components suite.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de

$Log: acs_file.pas,v $
Revision 1.11  2006/08/03 17:31:09  z0m3ie
*** empty log message ***

Revision 1.10  2006/07/09 16:40:34  z0m3ie
*** empty log message ***

Revision 1.9  2006/07/07 15:51:19  z0m3ie
*** empty log message ***

Revision 1.8  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.3  2006/01/01 18:46:40  z0m3ie
*** empty log message ***

Revision 1.2  2005/12/26 17:31:38  z0m3ie
fixed some problems in acs_dsfiles
fixed some problems in acs_vorbis
reworked all buffers

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.10  2005/12/18 17:01:54  z0m3ie
delphi compatibility

Revision 1.9  2005/12/04 16:54:33  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.8  2005/10/05 20:26:36  z0m3ie
Linux changes

Revision 1.7  2005/10/02 16:51:46  z0m3ie
*** empty log message ***

Revision 1.6  2005/09/15 20:59:38  z0m3ie
start translate the documentation in the source for pasdoc

Revision 1.5  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.4  2005/09/13 20:14:25  z0m3ie
driver handling classes (basic audio class)

Revision 1.3  2005/09/13 04:37:30  z0m3ie
*** empty log message ***

Revision 1.2  2005/09/13 04:04:50  z0m3ie
First release without Components for Fileformats
only TFileIn and TFileOut are Visible
*)

{
@abstract(this unit introduces the base classes for acs)
@author(Christian Ulrich (2005))

this unit introduces basic fileformat support
}

unit acs_file;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Classes, ACS_Classes, Dialogs, SysUtils, ACS_Strings;

type
  TACSFileInClass = class of TACSCustomFileIn;
  TACSFileOutClass = class of TACSCustomFileOut;

  TACSFileCapTyp = (fcLoad,fcSave);
  TACSFileCapTyps = set of TACSFileCapTyp;

   { TACSFileFormat }

  TACSFormatClass = class of TComponent;

   TACSFileFormat = class
   public
     FileClass       : TACSFormatClass;
     Extension       : String;
     Description     : String;
   end;

   { To this List all Filefomats must be added,
     use initialization section of your format units to add your format to acs
     so the user must only add your unit to the uses clausle to have support for
     your fileformat.
   }

   { tacsfileformatslist }

   tacsfileformatslist = class (tlist)
   public
     destructor Destroy; override;
     procedure Add(const Ext, Desc: String; AClass: TACSFormatClass);
     function FindExt(ext : string;Typs : TACSFileCapTyps) : TACSFormatClass;
     function FindFromFileName(const fileName : String;Typs : TACSFileCapTyps) : TACSFormatClass;
     procedure Remove(AClass: TACSFormatClass);
     procedure BuildFilterStrings(var descriptions: String;Typs : TACSFileCapTyps);
   end;

  { This class is an wrapper for all fileformats
  }

  { TFileIn }

  TACSFileIn = CLASS(TACSCustomFileIn)
  private
    FEndSample: Integer;
    FFileName: string;
    FInput : TACSCustomFileIn;
    FDialog : TOpenDialog;
    FLoop: Boolean;
    FStartSample: Integer;
    FTotalSamples: Integer;
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    function GetTime : Integer;
    function GetValid : Boolean;
    function GetTotalTime : real; override;
    procedure Reset; override;
    procedure SetFileName(const AValue : String);
    function GetSize : Integer;
    function GetPosition : Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure Open;

    procedure Flush; override;
    procedure Init; override;
    function Seek(SampleNum : Integer) : Boolean; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer;override;
    function SetStartTime(Minutes, Seconds : Integer) : Boolean;
    function SetEndTime(Minutes, Seconds : Integer) : Boolean;
    procedure Jump(Offs : real);
    property Time : Integer read GetTime;
    property TotalSamples : Integer read FTotalSamples;
    property Valid : Boolean read GetValid;
    property Size : Integer read GetSize;
    property Position : Integer read GetPosition;
  published
    property EndSample : Integer read FEndSample write FEndSample;
    property FileName : string read FFileName write SetFileName;
    property Loop : Boolean read FLoop write FLoop;
    property StartSample : Integer read FStartSample write FStartSample;
  end;


  { This class is an wrapper for all fileformats
  }

  { TFileOut }

  TACSFileOut = class(TComponent)
  private
    FBufferSize: Integer;
    FFileMode: TACSFileOutputMode;
    FFileName: string;
    FOnDone: TACSOutputDoneEvent;
    FOnProgress: TACSOutputProgressEvent;
    FOnThreadException: TACSThreadExceptionEvent;
    FOutput : TACSCustomFileOut;
    FDialog : TSaveDialog;
    FInput : TACSCustomInput;
{$IFDEF LINUX}
    FAccessMask : Integer;
{$ENDIF}
    function GetDelay: Integer;
    function GetPriority: TTPriority;
    function GetProgress: real;
    function GetStatus: TACSOutputStatus;
    function GetTE: Integer;
    procedure SetDelay(const AValue: Integer);
    procedure SetPriority(const AValue: TTPriority);

    procedure ThreadException(Sender : TComponent;E : Exception);
    procedure OutputDone(Sender : TComponent);
    procedure OutputProgress(Sender : TComponent);
  protected
    FBaseChannel: Integer;
    procedure SetInput(vInput : TACSCustomInput);
    procedure Done;
    function DoOutput(Abort : Boolean):Boolean;
    procedure Prepare;
    procedure SetFileMode(aMode : TACSFileOutputMode); virtual;
    procedure SetFileName(const AValue: string);
  public
    destructor Destroy;override;
    procedure Open;
    property Buffersize : Integer read FBufferSize write FBufferSize;
    procedure Pause;virtual;
    procedure Resume;virtual;
    procedure Run;
    procedure Stop;
    property Delay : Integer read GetDelay write SetDelay;
    property ThreadPriority :  TTPriority read GetPriority write SetPriority;
    property Progress : real read GetProgress;
    property Status : TACSOutputStatus read GetStatus;
    property TimeElapsed : Integer read GetTE;
{$IFDEF LINUX}
    property AccessMask : Integer read FAccessMask write FAccessMask;
{$ENDIF}
  published
    property FileMode : TACSFileOutputMode read FFileMode write SetFileMode;
    property FileName : string read FFileName write SetFileName;
    property Input : TACSCustomInput read FInput write SetInput;
    property OnDone : TACSOutputDoneEvent read FOnDone write FOndone;
    property OnProgress : TACSOutputProgressEvent read FOnProgress write FOnProgress;
    property OnThreadException : TACSThreadExceptionEvent read FOnThreadException write FOnThreadException;
  end;

var
  FileFormats : TACSFileFormatsList;

implementation

{ TFileIn }

function TACSFileIn.GetBPS: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FInput.BitsPerSample;
end;

function TACSFileIn.GetCh: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FInput.Channels;
end;

function TACSFileIn.GetSR: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FInput.SampleRate;
end;

function TACSFileIn.GetTime: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FInput.Time;
end;

function TACSFileIn.GetValid: Boolean;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FInput.Valid;
end;

function TACSFileIn.GetTotalTime: real;
begin
  if not Assigned(FInput) then
    exit;
  Result := FInput.TotalTime;
end;

procedure TACSFileIn.Reset;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
   FInput.Reset;
end;

procedure TACSFileIn.SetFileName(const AValue: string);
begin
  FFileName := AValue;
  if Assigned(FInput) then
    FInput.Free;
  FInput := nil;
  if AValue = '' then
    exit;
  FInput := TACSFileInClass(FileFormats.FindFromFileName(AValue,[fcLoad])).Create(nil);
  if Assigned(FInput) then
    FInput.FileName := FFilename;
end;

function TACSFileIn.GetSize: Integer;
begin
  Result := 1;
  if Assigned(FInput) then
    Result := FInput.Size;
end;

function TACSFileIn.GetPosition: Integer;
begin
  Result := 0;
  if Assigned(FInput) then
    Result := FInput.Position;
end;

constructor TACSFileIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TACSFileIn.Destroy;
begin
  if Assigned(FInput) then
    FInput.Free;
  inherited Destroy;
end;

procedure TACSFileIn.Open;
var
  desc : string;
begin
  FDialog := TOpenDialog.Create(nil);
  FileFormats.BuildFilterStrings(desc,[fcLoad]);
  FDialog.Filter := desc;
  if FDialog.Execute then
    begin
      if Assigned(FInput) then
        FInput.Free;
      FInput := TACSFileInClass(FileFormats.FindFromFileName(FDialog.FileName,[fcLoad])).Create(nil);
      FFileName := FDialog.FileName;
      if Assigned(FInput) then
        FInput.FileName := FFilename;
    end;
  FDialog.Free;
end;

procedure TACSFileIn.Flush;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  FInput.Flush;
end;

procedure TACSFileIn.Init;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  FInput.Init;
end;

function TACSFileIn.Seek(SampleNum : Integer): Boolean;
begin
  if not Assigned(Finput) then
    EACSException.Create(strnoFileOpened);
  FInput.Seek(SampleNum);
end;

function TACSFileIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  Result:=FInput.GetData(Buffer, BufferSize);
end;

function TACSFileIn.SetStartTime(Minutes, Seconds: Integer): Boolean;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FInput.SetStartTime(Minutes,Seconds);
end;

function TACSFileIn.SetEndTime(Minutes, Seconds: Integer): Boolean;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FInput.SetEndTime(Minutes,Seconds);
end;

procedure TACSFileIn.Jump(Offs: real);
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strNoFileOpened);
  FInput.Jump(Offs);
end;

{ TACSFileOut }

procedure TACSFileOut.SetFileName(const AValue: string);
begin
  if FFileName=AValue then exit;
  if Assigned(FOutput) then
    FOutput.Free;
  FOutput := nil;
  FOutput := TACSFileOutClass(FileFormats.FindFromFileName(AValue,[fcSave])).Create(nil);
  if Assigned(FOutput) then
    begin
      FOutput.FileName:=AValue;
      foutput.FileMode := FFileMode; //GAK:20060731
      FOutput.Input := FInput;
      FOutput.OnDone := OutputDone;
      FOutput.OnProgress := OutputProgress;
      Foutput.OnThreadException := ThreadException;
      ffilename := avalue;//GAK:20060731
    end;
end;

procedure TACSFileOut.Done;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  FOutput.Done;
end;

function TACSFileOut.DoOutput(Abort: Boolean): Boolean;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FOutput.DoOutput(Abort);
end;

procedure TACSFileOut.Prepare;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  FOutput.Prepare;
end;

function TACSFileOut.GetDelay: Integer;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  result := FOutput.Delay;
end;

function TACSFileOut.GetPriority: TTPriority;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FOutput.ThreadPriority;
end;

function TACSFileOut.GetProgress: real;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FOutput.Progress;
end;

function TACSFileOut.GetStatus: TACSOutputStatus;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FOutput.Status;
end;

function TACSFileOut.GetTE: Integer;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  Result := FOutput.TimeElapsed;
end;

procedure TACSFileOut.SetDelay(const AValue: Integer);
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  FOutput.Delay := AValue;
end;

procedure TACSFileOut.SetPriority(const AValue: TTPriority);
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  FOutput.ThreadPriority := AValue;
end;

procedure TACSFileOut.ThreadException(Sender : TComponent;E: Exception);
begin
  if Assigned(OnThreadException) then
    OnThreadException(Sender,E);
end;

procedure TACSFileOut.OutputDone(Sender: TComponent);
begin
  if Assigned(OnDone) then
    OnDone(Sender);
end;

procedure TACSFileOut.OutputProgress(Sender: TComponent);
begin
  if Assigned(OnProgress) then
    OnProgress(Sender);
end;

procedure TACSFileOut.SetInput(vInput: TACSCustomInput);
begin
  FInput := vInput;
  if Assigned(FOutput) then
    FOutput.Input := FInput;
end;

procedure TACSFileOut.SetFileMode(aMode: TACSFileOutputMode);
begin
//GAK:20060731 changed whole of this method, as it was stopping component loading/creating
  if amode <> ffilemode then
  begin
    FFileMode := amode;
    if Assigned(FOutput) then  FOutput.FileMode := aMode;
  end;
end;

procedure TACSFileOut.Open;
var
  desc : string;

begin
  FDialog := TSaveDialog.Create(nil);
  FileFormats.BuildFilterStrings(desc,[fcSave]);
  FDialog.Filter := desc;
  if FDialog.Execute then
    begin
      FOutput := TACSFileOutClass(FileFormats.FindFromFileName(FDialog.FileName,[fcSave])).Create(nil);
      FileName := FDialog.FileName;
      foutput.FileMode := ffilemode;
      FOutput.Input := FInput;
      FInput := FInput;
      FOutput.OnDone := OutputDone;
      FOutput.OnProgress := OutputProgress;
      Foutput.OnThreadException := ThreadException;
    end;
  FDialog.Free;
end;

procedure TACSFileOut.Pause;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  FOutput.Pause;
end;

procedure TACSFileOut.Resume;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  FOutput.Resume;
end;

procedure TACSFileOut.Run;
begin
  if not Assigned(FOutput) then
    raise EACSException.Create(strNoFileOpened);
  FOutput.Run;
end;

procedure TACSFileOut.Stop;
begin
  if Assigned(FOutput) then
    FOutput.Stop;
end;

destructor TACSFileOut.Destroy;
begin
  if Assigned(FOutput) then
    FOutput.Free;
  inherited Destroy;
end;

{ TACSFileFormatsList }

destructor tacsfileformatslist.Destroy;
var
  i: integer;
begin
   for i:= 0 to Count-1 do
     TACSFileFormat(Items[i]).Free;
  inherited Destroy;
end;

procedure TACSFileFormatsList.Add(const Ext, Desc: String;AClass: TACSFormatClass);
var
   newRec : TACSFileFormat;
begin
   newRec:=TACSFileFormat.Create;
   with newRec do
     begin
       Extension:=LowerCase(Ext);
       FileClass:=AClass;
       Description:=Desc;
     end;
   inherited Add(newRec);
end;

function TACSFileFormatsList.FindExt(ext: string;Typs : TACSFileCapTyps): TACSFormatClass;
var
   i : Integer;
begin
   ext:=LowerCase(ext);
   for i:=Count-1 downto 0 do
     with TACSFileFormat(Items[I]) do
       begin
         if ((fcLoad in Typs) and (TACSFileFormat(Items[I]).FileClass.InheritsFrom(TACSCustomFileIn))) or ((fcSave in Typs) and (TACSFileFormat(Items[I]).FileClass.InheritsFrom(TACSCustomFileOut))) then
           if Extension=ext then
             begin
               Result:=TACSFileFormat(Items[I]).FileClass;
               Exit;
             end;
       end;
   Result:=nil;
end;

function TACSFileFormatsList.FindFromFileName(const fileName: String;Typs : TACSFileCapTyps): TACSFormatClass;
var
   ext : String;
begin
   ext:=ExtractFileExt(Filename);
   System.Delete(ext, 1, 1);
   Result:=FindExt(ext,Typs);
   if not Assigned(Result) then
      raise EACSException.CreateFmt(strUnknownExtension, [ext]);
end;

procedure TACSFileFormatsList.Remove(AClass: TACSFormatClass);
var
   i : Integer;
begin
   for i:=Count-1 downto 0 do begin
      if TACSFileFormat(Items[i]).FileClass.InheritsFrom(AClass) then
        begin
          TACSFileFormat(Items[i]).Free;
          Delete(i);
        end;
   end;
end;

procedure TACSFileFormatsList.BuildFilterStrings(var descriptions : String;Typs : TACSFileCapTyps);
var
   k, i : Integer;
   p : TACSFileFormat;
   filters : string;
begin
   descriptions:='';
   filters := '';
   k:=0;
   for i:=0 to Count-1 do
     begin
       p:=TACSFileFormat(Items[i]);
       if ((fcLoad in Typs) and (p.FileClass.InheritsFrom(TACSCustomFileIn))) or ((fcSave in Typs) and (p.FileClass.InheritsFrom(TACSCustomFileOut))) then
         with p do
           begin
             if k<>0 then
               begin
                 descriptions:=descriptions+'|';
                 filters := filters+';';
               end;
            descriptions:=descriptions+Description+' (*.'+Extension+')|'+'*.'+Extension;
            filters := filters+'*.'+Extension;
            Inc(k);
         end;
      end;
   descriptions := strAllFormats+'|'+filters+'|'+descriptions;
end;

initialization

  FileFormats := TACSFileFormatsList.Create;

finalization

  FileFormats.Free;

end.

