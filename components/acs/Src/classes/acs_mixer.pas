(*
  this file is a part of audio components suite v 2.4.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_mixer.pas,v $
Revision 1.2  2005/12/30 12:54:42  z0m3ie
some error checks

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.3  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.2  2005/10/02 16:51:46  z0m3ie
*** empty log message ***

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.11  2005/09/01 19:55:48  z0m3ie
again Delphi corrections

Revision 1.10  2005/08/31 20:30:39  z0m3ie
Mixer Channelname work now
minior corrections for Converters

Revision 1.9  2005/08/31 14:37:59  z0m3ie
*** empty log message ***

Revision 1.8  2005/08/31 14:33:16  z0m3ie
fixed delphi issue with TControlEntry

Revision 1.7  2005/08/30 22:10:55  z0m3ie
Mixer mostly completed

Revision 1.6  2005/08/29 21:46:43  z0m3ie
*** empty log message ***

Revision 1.5  2005/08/28 20:31:18  z0m3ie
linux restructuring for 2.4

Revision 1.4  2005/08/28 18:35:53  z0m3ie
created Delphi package for 2.4
more Mixer stuff
updated some things for Delphi

Revision 1.3  2005/08/26 17:12:56  z0m3ie
*** empty log message ***

Revision 1.2  2005/08/26 17:03:20  z0m3ie
begon to make acs resourcestring aware
more advanced tmixer for windows
restructured tmixer its better handleable now

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

Revision 1.2  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}


unit acs_mixer;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, ACS_Classes, ACS_Strings
  {$IFDEF MSWINDOWS}
  ,MMSystem,Windows,Dialogs
  ,Math
  {$ELSE}
  ,Soundcard, baseunix
  {$ENDIF}
  ;

type
  TACSMixerChannel = (mcUnknown,
                   mcVolume,
                   mcTreble,
                   mcBass,
                   mcSynth,
                   mcPCM,
                   mcSpeaker,
                   mcLine,
                   mcMic,
                   mcCD,
                   mcIMix,
                   mcAltPCM,
                   mcRecLev,
                   mcDigital,
                   mcMonitor,
                   mcHeadphone,
                   mcTelephone);

  {$IFDEF MSWINDOWS}
const
  FirstSource =    MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED - MIXERLINE_COMPONENTTYPE_SRC_FIRST ;
  LastSource =     MIXERLINE_COMPONENTTYPE_SRC_ANALOG    - MIXERLINE_COMPONENTTYPE_SRC_FIRST ;
  FirstDest  =     MIXERLINE_COMPONENTTYPE_DST_FIRST;
  LastDest   =     MIXERLINE_COMPONENTTYPE_DST_LAST;

type
  {$IFDEF LCL}
  TMixerLine = MIXERLINE;
  TMixerCaps = MIXERCAPS;
  TMixerControl = MIXERCONTROL;
  TMixerLineControls = MIXERLINECONTROLS;
  TMixerControlDetails = MIXERCONTROLDETAILS;
  {$ENDIF}

  TDataArray = ARRAY[FirstSource .. LastSource] OF MIXERCONTROLDETAILS_UNSIGNED;
  PDataArray = ^TDataArray;
  PControlEntry  = ^TControlEntry;
  TControlEntry = RECORD
    IsInited         : Boolean;
    CHandle          : Thandle;
    CDestination     : INTEGER;
    CID              : INTEGER;
    CName            : String[MIXER_SHORT_NAME_CHARS];
    CConnect         : INTEGER;
    CCControls       : INTEGER;
    CControlTyp      : INTEGER;
    CKanal           : INTEGER;
    CControl         : INTEGER;
    CComponentTyp    : DWORD;
    CMin, Cmax       : INTEGER;
    Cdetails         : TDataArray;
    CMultItems       : INTEGER;
    CcSteps          : DWORD;
  END;
  {$ENDIF}

  TACSMixerLevel = record
  case Word of
    1 :
    (
      Left, Right : Byte;
    );
    2 : (Main : Byte;);
  end;

  { TACSMixer }

  TACSMixer = class(TComponent)
  private
    FDevNum : Integer;
    FChannels : array of TACSMixerChannel;
    {$IFDEF LINUX}
    _mix_fd : Integer;
    FFileName : String;
    {$ELSE}
    FMixer : HMixer;
    FMixerCaps : TMixerCaps;
    FControls : array of TControlEntry;
    FMuteControls : array of TControlEntry;
    {$ENDIF}
    FMixerName : String;
    function GetRecSource : Integer;
    function GetVolume(vChannel : integer) : TACSMixerLevel;
    procedure SetVolume(vChannel : integer;  vLevel : TACSMixerLevel);
    procedure SetRecSource(vChannel : integer);
    procedure SetDevNum(Num : Integer);
    function GetChannel(Num : Integer) : TACSMixerChannel;
    function GetDevCount : Integer;
    function GetChannelCount : Integer;
    function GetChannelName(vChannel : Integer) : string;
    function GetMute(vChannel : integer) : Boolean;
    procedure SetMute(vChannel : integer;  Mute : Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsStereo(vChannel : Integer) : Boolean;
    function IsRecordable(vChannel : Integer) : Boolean;
    property Channel[vChannel : Integer] : TACSMixerChannel read GetChannel;
    property Level[vChannel : Integer] : TACSMixerLevel read GetVolume write SetVolume;
    property Mute[vChannels : Integer] : Boolean read GetMute write SetMute;
    property ChannelName[vChannel : Integer] : string read GetChannelName;
    property RecordSource : Integer read GetRecSource write SetRecSource;
    property DevCount : Integer read GetDevCount;
    property Channelcount : Integer read GetChannelCount;
  published
    property DevNum : Integer read FDevNum write SetDevNum stored True;
    property MixerName : String read FMixerName;
  end;

var
  MixersCount : Byte;

  function ChannelToStr(ch : TACSMixerChannel) : String;

implementation

{$I ACS_Mixer.inc}

  function ChannelToStr(ch : TACSMixerChannel) : String;
  begin
    case ch of
      mcVolume:  Result := strMixerVolume;
      mcTreble:  Result := strMixerTreble;
      mcBass:    Result := strMixerBass;
      mcSynth:   Result := strMixerSynth;
      mcPCM:     Result := strMixerPCM;
      mcSpeaker: Result := strMixerSpeaker;
      mcLine:    Result := strMixerLine;
      mcMic:     Result := strMixerMic;
      mcCD:      Result := strMixerCD;
      mcIMix:    Result := strMixerIMix;
      mcAltPCM:  Result := strMixerAlt;
      mcRecLev:  Result := strMixerRec;
      mcUnknown: Result := strMixerUnknown;
      else       Result := IntToStr(Integer(ch));
    end;
  end;

  constructor TACSMixer.Create;
  begin
    inherited Create(AOwner);
    if MixersCount > 0 then
    SetDevNum(0);
  end;

  function TACSMixer.GetChannel(Num: Integer): TACSMixerChannel;
  begin
    if (Num < 0) or (Num > (length(FChannels)-1)) then
      exit;
    Result := FChannels[Num];
  end;
  
  function TACSMixer.GetDevCount : Integer;
  begin
    Result := MixersCount;
  end;
  
  function TACSMixer.GetChannelCount : Integer;
  begin
    result := length(FChannels);
  end;
  
  function TACSMixer.GetChannelName(vChannel : Integer) : string;
  begin
    if (vChannel > -1) and (vChannel < ChannelCount) then
      Result := ChannelToStr(FChannels[vChannel]);
  end;

initialization
  MixersCount := CountMixers;
end.
