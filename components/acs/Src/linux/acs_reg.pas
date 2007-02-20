(*
  This file is a part of Audio Components Suite v 2.2 (Kylix Edition).
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru

$Log: acs_reg.pas,v $
Revision 1.9  2006/07/04 17:12:44  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.7  2005/12/19 18:37:21  z0m3ie
*** empty log message ***

Revision 1.5  2005/12/04 16:54:33  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.4  2005/11/27 16:50:33  z0m3ie
add ACS VolumeQuerry
make ACS_VolumeQuerry localizeable
some little errorfixes (buffersize for linuxdrivers was initially 0)
make TAudioIn workable

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

  procedure Register;

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
{$r ..\resources\resource.dcr}
{$ENDIF}

end.
