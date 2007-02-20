{
$Log: acs_mixer.inc,v $
Revision 1.3  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.1  2005/12/19 18:35:16  z0m3ie
*** empty log message ***

Revision 1.3  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.2  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.10  2005/08/31 20:30:40  z0m3ie
Mixer Channelname work now
minior corrections for Converters

Revision 1.9  2005/08/31 14:37:59  z0m3ie
*** empty log message ***

Revision 1.8  2005/08/30 22:10:55  z0m3ie
Mixer mostly completed

Revision 1.7  2005/08/29 22:50:33  z0m3ie
*** empty log message ***

Revision 1.6  2005/08/29 21:46:43  z0m3ie
*** empty log message ***

Revision 1.5  2005/08/28 20:33:10  z0m3ie
*** empty log message ***

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

}

function mixerSetControlDetails(x1: HMIXEROBJ; x2: PMIXERCONTROLDETAILS; x3: DWORD): MMRESULT; stdcall;
 external 'winmm.dll' name
 'mixerSetControlDetails';

  function GetChannelfromMask(Mask : DWORD) : TACSMixerChannel;
  begin
    case Mask of
    MIXERLINE_COMPONENTTYPE_DST_UNDEFINED  : Result := mcUnknown;
    MIXERLINE_COMPONENTTYPE_DST_DIGITAL    : Result := mcDigital;
    MIXERLINE_COMPONENTTYPE_DST_LINE       : Result := mcLine;
    MIXERLINE_COMPONENTTYPE_DST_MONITOR    : Result := mcMonitor;
    MIXERLINE_COMPONENTTYPE_DST_SPEAKERS   : Result := mcVolume;
    MIXERLINE_COMPONENTTYPE_DST_HEADPHONES : Result := mcHeadphone;
    MIXERLINE_COMPONENTTYPE_DST_TELEPHONE  : Result := mcTelephone;
    MIXERLINE_COMPONENTTYPE_DST_WAVEIN     : Result := mcPCM;
    MIXERLINE_COMPONENTTYPE_DST_VOICEIN    : Result := mcUnknown;
    MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED  : Result := mcUnknown;
    MIXERLINE_COMPONENTTYPE_SRC_DIGITAL    : Result := mcDigital;
    MIXERLINE_COMPONENTTYPE_SRC_LINE       : Result := mcLine;
    MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE : Result := mcMic;
    MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER: Result := mcSynth;
    MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC: Result := mcCD;
    MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE  : Result := mcTelephone;
    MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER  : Result := mcVolume;
    MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT    : Result := mcPCM;
    MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY  : Result := mcAltPCM;
    MIXERLINE_COMPONENTTYPE_SRC_ANALOG     : Result := mcUnknown;
    else
    end;
  end;

  procedure TACSMixer.SetDevNum(Num : Integer);
  type
    TData = array [0..3] of  MIXERCONTROLDETAILS_UNSIGNED;
    PData = ^TData;
  var
    destination,
    connection : Integer;
    data       : PData;
    pmxctrl    : PMixerControl;
    s : String;
    aLineInfo,
    aConnLineInfo : TMixerLine;
    error : Integer;

    procedure GetLineControls(mixLineInfo : TMixerLine);
    var
      j, k,
      datasize : Integer;
      aLineControl : TMixerLineControls;
      aControlDetails: TMixerControlDetails;
      amixControl : PMixerControl;
      aControl : PControlEntry;
    begin
      with aLineControl  do
        begin
          cbStruct  := SizeOf(TMixerLineControls);
          dwLineID  := mixLineInfo.dwLineID;
          cControls := mixLineInfo.cControls;
          cbmxctrl  := SizeOf(TMixerControl);
          GetMem(amixControl, SizeOf(TMixerControl) * mixLineInfo.cControls);
          pamxctrl  := amixControl;
        end;
      error := mixerGetLineControls(Num, @aLineControl, MIXER_GETLINECONTROLSF_ALL);
      pmxctrl := amixControl;
      for j := 0 TO aLineControl.cControls -1 do
        begin
          if  (pmxctrl^.dwControlType <> MIXERCONTROL_CONTROLTYPE_VOLUME)
          and (pmxctrl^.dwControlType <> MIXERCONTROL_CONTROLTYPE_MUTE) then
            continue;
          if (pmxctrl^.fdwControl and MIXERCONTROL_CONTROLF_UNIFORM) > 0 then
            aControlDetails.cChannels := 1
          else
            aControlDetails.cChannels :=  mixLineInfo.cChannels;
          if  (pmxctrl^.fdwControl AND MIXERCONTROL_CONTROLF_MULTIPLE) > 0 then
            begin
              aControlDetails.cMultipleItems := pmxctrl^.cMultipleItems;
              Getmem(data,pmxctrl^.cMultipleItems * SizeOf(MIXERCONTROLDETAILS_UNSIGNED));
              datasize := pmxctrl^.cMultipleItems;
            end
          else
            begin
              aControlDetails.cMultipleItems := 0;
              Getmem(data,  aControlDetails.cChannels *  SizeOf(MIXERCONTROLDETAILS_UNSIGNED));
              datasize :=  aControlDetails.cChannels;
            end;
          with aControlDetails do
            begin
              cbStruct    := sizeOf(TmixerControlDetails);
              dwControlID := pmxctrl^.dwControlID;
              cbDetails   := SizeOf(MIXERCONTROLDETAILS_UNSIGNED);
              paDetails   := data;
            end;
          error := mixerGetControlDetails(Num, @aControlDetails, MIXER_GETCONTROLDETAILSF_VALUE );
          if (pmxctrl^.dwControlType = MIXERCONTROL_CONTROLTYPE_MUTE) then
            begin
              setlength(FMuteControls,length(FControls)+1);
              aControl := @FMuteControls[length(FControls)-1];
            end
          else
            begin
              setlength(FControls,length(FControls)+1);
              aControl := @FControls[length(FControls)-1];
            end;
          setlength(FChannels,Max(length(FControls),length(FMuteControls)));
          with aControl^, pmxctrl^, aControlDetails do
            begin
              IsInited           := True;
              CDestination       := mixLineInfo.dwDestination;
              CName              := String(szShortname);
              CComponentTyp      := mixLineInfo.dwComponentType;
              CKanal             := cChannels;
              CID                := dwControlID;
              CConnect           := mixLineInfo.cConnections;
              CCControls         := mixLineInfo.cControls;
              CControl            := fdwControl;
              CControlTyp         := dwControlType;
              CMultItems          := cMultipleItems;
              CMax                := Bounds.lMaximum;
              CMin                := Bounds.lMinimum;
              CcSteps             := Metrics.cSteps;
              for k := 0 to datasize -1 do
                CDetails[k].dwValue  := data^[k].dwvalue;
            end;
          FChannels[length(FControls)-1] := GetChannelfromMask(aControl.CComponentTyp);
          Freemem(data);
          inc(pmxctrl);
        end;
      Freemem(amixControl);
    end;

  begin
    if Num in [0..MixersCount - 1] then // check [0..0] [0..-1]
      begin
        setlength(FChannels,0);
        setlength(FControls,0);
        setlength(FMuteControls,0);
        error := mixerGetDevCaps(Num, @FMixerCaps, sizeof(TMixerCaps));
        FMixer := Num;
        FMixerName := StrPas(FMixerCaps.szPName);
        error := mixerOpen(@Num, 0, 0, 0, MIXER_OBJECTF_MIXER);
        if error = MMSYSERR_NOERROR then
          begin
            for destination := 0 to FMixerCaps.cDestinations - 1 do
              begin
                aLineInfo.cbStruct      := SizeOf(TMixerLine);
                aLineInfo.dwDestination := destination;
                error := mixerGetLineInfo(Num, @aLineInfo, MIXER_GETLINEINFOF_DESTINATION);
                if aLineInfo.dwComponentType <> MIXERLINE_COMPONENTTYPE_DST_SPEAKERS then
                  continue;
                GetLineControls(aLineInfo);
                for connection := 0 TO aLineInfo.cConnections-1 do
                  begin
                    with aConnLineInfo do
                      begin
                        cbStruct      := SizeOf(TMixerLine);
                        dwDestination := destination;
                        dwSource      := connection;
                      end;
                    error := mixerGetLineInfo(Num, @aConnLineInfo, MIXER_GETLINEINFOF_SOURCE);
                     GetLineControls(aConnLineInfo);
                  end;
              end;
          end;
      end;
  end;

  function TACSMixer.GetVolume(vChannel : Integer) : TACSMixerLevel;
  type
    TData = array [0..3] of  MIXERCONTROLDETAILS_UNSIGNED;
    PData = ^TData;
  var
    data       : PData;
    aControldetails : TMixerControlDetails;
    datasize,k : Integer;
  begin
    if vChannel >= length(FControls) then
      exit;
    if FControls[vChannel].IsInited = False then
      exit;
    if (FControls[vChannel].CControl and MIXERCONTROL_CONTROLF_UNIFORM) > 0 then
      aControlDetails.cChannels := 1
    else
      aControlDetails.cChannels :=  FControls[vChannel].CKanal;
    if  (FControls[vChannel].CControl AND MIXERCONTROL_CONTROLF_MULTIPLE) > 0 then
      begin
        aControlDetails.cMultipleItems := FControls[vChannel].CMultItems;
        Getmem(data, FControls[vChannel].CMultItems * SizeOf(MIXERCONTROLDETAILS_UNSIGNED));
        datasize := FControls[vChannel].CMultItems;
      end
    else
      begin
        aControlDetails.cMultipleItems := 0;
        Getmem(data,  aControlDetails.cChannels *  SizeOf(MIXERCONTROLDETAILS_UNSIGNED));
        datasize :=  aControlDetails.cChannels;
      end;
    with aControlDetails do
      begin
        cbStruct       := SizeOf(TMixerControlDetails);
        dwControlID    := FControls[vChannel].CID;
        cChannels      := FControls[vChannel].CKanal;
        cMultipleItems := FControls[vChannel].CMultItems;
        cbDetails      := sizeof(MIXERCONTROLDETAILS_Signed);
        padetails      := data;
      end;
    mixerGetControlDetails(FMixer, @aControlDetails,MIXER_GETCONTROLDETAILSF_VALUE );
    with FControls[vChannel] do
      begin
        for k := 0 to datasize -1 do
          CDetails[k].dwValue  := data^[k].dwvalue;
      end;
    Freemem(data);
    if IsStereo(vChannel) then
      begin
        Result.Left := round((FControls[vChannel].CDetails[0].dwValue*255)/FControls[vChannel].Cmax);
        Result.Right := round((FControls[vChannel].CDetails[1].dwValue*255)/FControls[vChannel].Cmax);
      end
    else
      Result.Main := round((FControls[vChannel].CDetails[0].dwValue*255)/FControls[vChannel].Cmax);
  end;

  procedure TACSMixer.SetVolume(vChannel : Integer;  vLevel : TACSMixerLevel);
  var
    aControlDetails : TMixerControlDetails;
  begin
    if vChannel >= length(FControls) then
      exit;
    if IsStereo(vChannel) then
      begin
        FControls[vChannel].CDetails[0].dwValue := round((vLevel.Left*FControls[vChannel].CMax)/255);
        FControls[vChannel].CDetails[1].dwValue := round((vLevel.Right*FControls[vChannel].CMax)/255);
      end
    else
      FControls[vChannel].CDetails[0].dwValue := round((vLevel.Main*FControls[vChannel].CMax)/255);
    with aControlDetails do
      begin
        cbStruct       := SizeOf(TMixerControlDetails);
        dwControlID    := FControls[vChannel].CID;
        cChannels      := FControls[vChannel].CKanal;
        cMultipleItems := 0;
        cbDetails      := sizeof(MIXERCONTROLDETAILS_Signed);
        padetails      := @FControls[vChannel].CDetails;
      end;
    mixerSetControlDetails(FMixer, @aControlDetails,MIXER_SETCONTROLDETAILSF_Value);
  end;

  function TACSMixer.IsStereo(vChannel : Integer) : Boolean;
  begin
    if vChannel >= length(FControls) then
      exit;
    Result := not (FControls[vChannel].CKanal = 1);
  end;

  function TACSMixer.GetMute(vChannel : integer) : Boolean;
  type
    TData = array [0..3] of  MIXERCONTROLDETAILS_UNSIGNED;
    PData = ^TData;
  var
    data       : PData;
    aControldetails : TMixerControlDetails;
    datasize,k : Integer;
  begin
    if vChannel >= length(FMuteControls) then
      exit;
    if FMuteControls[vChannel].IsInited = False then
      exit;
    if (FMuteControls[vChannel].CControl and MIXERCONTROL_CONTROLF_UNIFORM) > 0 then
      aControlDetails.cChannels := 1
    else
      aControlDetails.cChannels :=  FMuteControls[vChannel].CKanal;
    if  (FMuteControls[vChannel].CControl AND MIXERCONTROL_CONTROLF_MULTIPLE) > 0 then
      begin
        aControlDetails.cMultipleItems := FMuteControls[vChannel].CMultItems;
        Getmem(data, FMuteControls[vChannel].CMultItems * SizeOf(MIXERCONTROLDETAILS_UNSIGNED));
        datasize := FMuteControls[vChannel].CMultItems;
      end
    else
      begin
        aControlDetails.cMultipleItems := 0;
        Getmem(data,  aControlDetails.cChannels *  SizeOf(MIXERCONTROLDETAILS_UNSIGNED));
        datasize :=  aControlDetails.cChannels;
      end;
    with aControlDetails do
      begin
        cbStruct       := SizeOf(TMixerControlDetails);
        dwControlID    := FMuteControls[vChannel].CID;
        cChannels      := FMuteControls[vChannel].CKanal;
        cMultipleItems := FMuteControls[vChannel].CMultItems;
        cbDetails      := sizeof(MIXERCONTROLDETAILS_Signed);
        padetails      := data;
      end;
    mixerGetControlDetails(FMixer, @aControlDetails,MIXER_GETCONTROLDETAILSF_VALUE );
    with FMuteControls[vChannel] do
      begin
        for k := 0 to datasize -1 do
          CDetails[k].dwValue  := data^[k].dwvalue;
      end;
    Freemem(data);
    Result := (FMuteControls[vChannel].CDetails[0].dwValue = 1);
  end;
  
  procedure TACSMixer.SetMute(vChannel : integer;  Mute : Boolean);
  var
    aControlDetails : TMixerControlDetails;
  begin
    if vChannel >= length(FMuteControls) then
      exit;
    if FMuteControls[vChannel].IsInited = False then
      exit;
    if Mute then
      FMuteControls[vChannel].CDetails[0].dwValue := 1
    else
      FMuteControls[vChannel].CDetails[0].dwValue := 0;
    with aControlDetails do
      begin
        cbStruct       := SizeOf(TMixerControlDetails);
        dwControlID    := FMuteControls[vChannel].CID;
        cChannels      := FMuteControls[vChannel].CKanal;
        cMultipleItems := 0;
        cbDetails      := sizeof(MIXERCONTROLDETAILS_Signed);
        padetails      := @FMuteControls[vChannel].CDetails;
      end;
    mixerSetControlDetails(FMixer, @aControlDetails,MIXER_SETCONTROLDETAILSF_Value);
  end;

  function TACSMixer.IsRecordable(vChannel : Integer) : Boolean;
  begin
  end;

  procedure TACSMixer.SetRecSource(vChannel : Integer);
  begin
  end;

  function TACSMixer.GetRecSource : Integer;
  begin
  end;

  destructor TACSMixer.Destroy;
  begin
    Setlength(FControls,0);
    Setlength(FMuteControls,0);
    Setlength(FChannels,0);
    inherited Destroy;
  end;

  function CountMixers : Byte;
  begin
    Result := mixerGetNumDevs;
  end;

