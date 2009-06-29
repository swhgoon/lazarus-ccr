{
    This file is part of the Web Service Toolkit
    Copyright (c) 2009 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}

unit wst_consts;

interface

resourcestring
  SERR_InvalidArrayLength          = 'Invalid array length : %d.';
  SERR_InvalidCollectionLength     = 'Invalid collection length : %d.';
  SERR_InvalidHourOffetValue       = '"%d" is not a valid hour offset value.';
  SERR_InvalidMinuteOffetValue     = '"%d" is not a valid minute offset value.';
  SERR_InvalidParameter            = 'Invalid parameter : "%s".';
  SERR_NoReaderProc                = 'No reader proc for that type, Prop : "(%s : %s)".';
  SERR_NoSerializerFoThisType      = 'No serializer for this type : "%s".';
  SERR_ParamaterNotFound           = 'Parameter non found : "%s".';
  SERR_SerializerInitializationException = 'Unable to initialize the serializer of that type : "%s".';

implementation

end.

