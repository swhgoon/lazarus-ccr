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

const
  sWST_SIGNATURE = 'WST_METADATA_0.6';

resourcestring
  SERR_CannotMakeInternalSymbolName  ='Unable to make an internal symbol Name from "%s".';
  SERR_CannotResolveNamespace        = 'Unable to resolve this namespace : "%s".';
  SERR_DataFilterNotFound            = 'Data Filter not found : "%s".';
  SERR_DuplicateBindingName          = 'Duplicated binding : "%s".';
  SERR_ExpectingRemotableObjectClass = 'Expecting remotable object class but found "%s".';
  SERR_FailedTransportRequest        = '%s Request to %s failed.';
  SERR_HeaderNotUnderstood         = 'Header "%s" not Understood.';
  SERR_IllegalChar                 = 'Illegal character for that encoding : "%s".';
  SERR_IndexOutOfBound             = 'Index out of bound : %d.';
  SERR_IncompleteParamTypeRegistration  = 'Incomplete type registration for the type of this parameter : "%s".';
  SERR_InnerScopeMustBeSimpleType       = 'Inner Scope value must be a "simple type" value.';
  SERR_InsupportedOperation        = 'Insupported operation : "%s".';
  SERR_InvalidArrayBounds          = 'Invalid array bounds.';
  SERR_InvalidArrayLength          = 'Invalid array length : %d.';
  SERR_InvalidCollectionLength     = 'Invalid collection length : %d.';
  SERR_InvalidDataTypeInContext    = 'Invalid data type in this context : "%s".';
  SERR_InvalidEncodedData          = 'Invalid encoded data.';
  SERR_InvalidHourOffetValue       = '"%d" is not a valid hour offset value.';
  SERR_InvalidMinuteOffetValue     = '"%d" is not a valid minute offset value.';
  SERR_InvalidEmbeddedScopeOperation    = 'Invalid opération on scope, their are no embedded scope.';
  SERR_InvalidParameter            = 'Invalid parameter : "%s".';
  SERR_InvalidPropertyValue        = 'Invalid property ("%s") value : "%s".';
  SERR_InvalidParameterProc        = 'Invalid parameter : "%s"; Procedure = "%s".';
  SERR_InvalidParameters           = 'Invalid parameters.';
  SERR_InvalidPoolParametersArgs   = 'Invalid pool arguments Min = %d; Max = %d .';
  SERR_IsNotAFieldOf               = '"%s" is not a field of "%s".';
  SERR_NodeNotFoundByID            = 'Node not found with this ID in the document : "%s".';
  SERR_NoHandlerForThatVerb        = 'No handler for that verb : "%s".';
  SERR_NoReaderProc                = 'No reader proc for that type, Prop : "(%s : %s)".';
  SERR_NoScope                     = 'There is no scope.';  
  SERR_NoSerializerFoThisType      = 'No serializer for this type : "%s".';
  SERRE_ObjectCreationTimeOut      = 'Unable to create the object : Timeout expired.';
  SERR_OperationNotAllowedOnActivePool = 'Operation not allowed on an active pool.';
  SERR_ParamaterNotFound           = 'Parameter non found : "%s".';
  SERR_RecordExtendedRttiNotFound  = 'Record extended RTTI informations not found in type registry : "%s".';
  SERR_RootObjectCannotBeNIL       = 'The root object cannot be NIL.';
  SERR_SerializerInitializationException = 'Unable to initialize the serializer of that type : "%s".';
  SERR_ServiceNotFound                   = 'Service not found : "%s".';
  SERR_ScopeNotFound                     = 'Scope not found : "%s".';
  SERR_TypeNotRegistered                 = 'Type not registered : "%s".';
  SERR_UnexpectedEndOfData               = 'Unexpected end of data.';
  SERR_UnknownProperty                   = 'Unknown property : "%s".';
  
implementation

end.

