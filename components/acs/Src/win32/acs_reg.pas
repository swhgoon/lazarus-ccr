(*
$Log: acs_reg.pas,v $
Revision 1.16  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.13  2005/12/19 18:37:41  z0m3ie
*** empty log message ***

Revision 1.11  2005/12/18 17:01:53  z0m3ie
delphi compatibility

Revision 1.10  2005/12/04 16:54:33  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.9  2005/11/27 16:50:33  z0m3ie
add ACS VolumeQuerry
make ACS_VolumeQuerry localizeable
some little errorfixes (buffersize for linuxdrivers was initially 0)
make TAudioIn workable

Revision 1.8  2005/10/02 16:51:58  z0m3ie
*** empty log message ***

Revision 1.7  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.6  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.5  2005/08/26 17:03:20  z0m3ie
begon to make acs resourcestring aware
more advanced tmixer for windows
restructured tmixer its better handleable now

Revision 1.4  2005/08/25 21:06:29  z0m3ie
Added TMultiMixer

Revision 1.3  2005/08/25 20:15:37  z0m3ie
Version 2.4 restructure
*)

unit acs_reg;

interface

uses
  Classes, ACS_Audio,
  ACS_CDROM, ACS_AudioMix, ACS_Converters, ACS_Misc, ACS_File, ACS_Filters,
  ACS_Streams, ACS_Indicator, ACS_Mixer,ACS_MultiMix,ACS_VolumeQuery
  {$IFDEF FPC}
  ,LResources
  {$ENDIF};

  procedure Register();

implementation

procedure Register();
begin
  RegisterComponents('Audio I/O', [TACSAudioIn, TACSAudioOut,TACSMixer,
  TACSCDIn,TACSInputList, TACSMemoryIn, TACSFileIn, TACSFileOut, TACSStreamIn, TACSStreamOut, TACSNULLOut]);
  RegisterComponents('Audio Processing', [TACSAudioMixer,TACSMultiMixer, TACSSampleConverter, TACSRateConverter,
  TACSMSConverter, TACSAudioProcessor, TACSBWFilter, TACSSincFilter, TACSSoundIndicator, TACSStereoBalance, TACSConvolver,TACSVolumeQuery]);
end;

initialization
{$IFDEF FPC}
{$i ..\resources\acs_reg.lrs}
{$ELSE}
{$R ..\resources\resource.dcr}
{$ENDIF}


end.
