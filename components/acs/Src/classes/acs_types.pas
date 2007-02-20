(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_types.pas,v $
Revision 1.6  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.2  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.2  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}

unit acs_types;

interface

type

  TACSBuffer16 = array[0..0] of SmallInt;
  PACSBuffer16 = ^TACSBuffer16;

  TACSBuffer8 = array[0..0] of Byte;
  PACSBuffer8 = ^TACSBuffer8;

  TACSStereoSample16 = packed record
    Left, Right : SmallInt;
  end;

  TACSStereoBuffer16 = array[0..0] of TACSStereoSample16;
  PACSStereoBuffer16 = ^TACSStereoBuffer16;

  TACSStereoSample8 = packed record
    Left, Right : Byte;
  end;

  TACSStereoBuffer8 = array[0..0] of TACSStereoSample8;
  PACSStereoBuffer8 = ^TACSStereoBuffer8;


  TACSComplex = packed record
    Re, Im : Double;
  end;

  PACSComplex = ^TACSComplex;

  TACSComplexArray = array[0..0] of TACSComplex;
  PACSComplexArray = ^TACSComplexArray;

  TACSDoubleArray = array[0..0] of Double;
  PACSDoubleArray = ^TACSDoubleArray;

  TACSStereoSampleD = record
    Left : Double;
    Right : Double;
  end;

  TACSStereoBufferD = array[0..0] of TACSStereoSampleD;
  PACSStereoBufferD = ^TACSStereoBufferD;

const

  Pi = 3.14159265359;
  TwoPi = 6.28318530718;
  HalfPi = 1.57079632679;

implementation

end.
