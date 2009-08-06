{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit rxdconst;

interface


resourcestring

{ RxDBCtrl }

  SLocalDatabase         = 'Невозможно произвести эту операцию с локальной базой данных';
  SRetryLogin            = 'Вы хотите повторить попытку соединения с базой данных?';
  SExprNotBoolean        = 'Field ''%s'' is not boolean';
  SExprBadNullTest       = 'NULL-values enabled in ''='' и ''<>''';
  SExprBadField          = 'Field ''%s'' not used in filter expression';
  SCaptureFilter         = 'Элементы управления захвачены фильтром';
  SNotCaptureFilter      = 'Элементы управления должны быть захвачены фильтром';
  SInactiveData          = 'inactive';
  SBrowseData            = 'browse';
  SEditData              = 'editing';
  SInsertData            = 'append';
  SSetKeyData            = 'find';
  SCalcFieldsData        = 'calc';
  SRegistration          = 'Register';
  SAppTitleLabel         = 'Application "%s"';
  SHintLabel             = 'Enter you user name and password';
  SUserNameLabel         = '&User name:';
  SPasswordLabel         = '&Password:';
  SMore1                 = '&More >>';
  SMore2                 = '&Less <<';
  SInvalidUserName       = 'User name or password not valid';
  SChangePassword        = 'Change password';
  SOldPasswordLabel      = '&Old password:';
  SNewPasswordLabel      = '&New password:';
  SConfirmPasswordLabel  = '&Confirm:';
  SPasswordChanged       = 'Password changed';
  SPasswordNotChanged    = 'Password not changed';
  SPasswordsMismatch     = 'New password and confirmation not equal';
  SDBExceptCaption       = 'Error in DB engine';
  SServerErrorLabel        = 'Server error';
  SErrorMsgLabel           = 'Error message';
  SNextButton              = '&Next';
  SPrevButton              = '&Prior';
  SExprIncorrect           = 'Error in filter expression';
  SExprTermination         = 'Error in filter end';
  SExprNameError           = 'Error in filed name';
  SExprStringError         = 'Error in string const';
  SExprInvalidChar         = 'Error symbol in expression: ''%s''';
  SExprNoRParen            = 'Ожидалось '')'', а встречено: %s';
  SExprExpected            = 'Ожидалось выражение, а встречено %s';
  SExprBadCompare          = 'Операции сравнения требуют наличия поля и константы';
  SConfirmSave             = 'Data changed. Save?';
  SDatabaseName            = 'Database loocked: %s';
  SUnlockCaption           = 'Unloock';
  SUnlockHint              = 'Enter you password';
  SDeleteMultipleRecords   = 'Delete all selected records?';

  SPropDefByLookup         = 'PropDefByLookup';
  SDataSourceFixed         = 'SDataSourceFixed';
  SCircularDataLink        = 'SCircularDataLink';
  sRxAscendign             = 'Ascendente';//'Ascendente');
  sRxDescending            = 'Descendente';//'Descendente');


  SDeleteRecordQuestion    = 'Delete record?';
  SFieldTypeMismatch       = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SInvalidDate             = 'Invalid Date';
  SFieldRequired           = 'Field ''%s'' must have a value';
  SNotEditing              = 'Dataset not in edit or insert mode';
  SUnknownFieldType        = 'SUnknownFieldType %s';
  SFieldReadOnly           = 'SFieldReadOnly %s';

  //RXDBgrid
  sRxDBGridFind            = 'Find data';  ////  'Buscar                              Ctrl+F';
  sRxDBGridFilter          = 'Filter data';//'Filtrar                               Ctrl+T';
  sRxDBGridFilterSimple    = 'Filter in table';//      Ctrl+E';  'Filtrar en Encabezado      Ctrl+E';
  sRxDBGridFilterClear     = 'Clear filter';//                      Ctrl+Q';'Quitar Filtro                      Ctrl+Q';
  sRxDBGridSortByColumns   = 'Sort data for collumns';//     Ctrl+C';'Ordenar por Columnas     Ctrl+C';
  sRxDBGridSelectColumns   = 'Select visible collumns';//      Ctrl+W';'Seleccionar Columnas      Ctrl+W';
  sRxDBGridEmptiFilter     = '(Empty)';

  //RxDBGrid filter form
  sRxFilterFormSelectExp   = 'Enter filter expression for data in table:';
  sRxFilterFormOnField     = 'On field :';
  sRxFilterFormOperaion    = 'Operation :';
  sRxFilterFormCondition   = 'Condition :';
  sRxFilterFormOperand     = 'Operand :';
  sRxFilterFormEnd         = 'end.';
  sRxFilterFormClear       = 'Clear filter';
  sRxFilterFormCancel      = 'Cancel';
  sRxFilterFormApply       = 'Apply';
  sRxFilterFormCaption     = 'Filter conditions';

  //TrxSortByForm
  sRxSortByFormCaption     = 'Sort on field';
  sRxSortByFormAllFields   = '&Fields in dataset:';
  sRxSortByFormSortFields  = '&Selected fields:';
  sRxSortByFormSortOrder   = 'Select field for sort data:';
  sRxSortByFormAddField    = '&Add field';
  sRxSortByFormRemoveField = '&Remove';
  sRxSortByFormMoveUpField = '&Up';
  sRxSortByFormMoveDnField = '&Down';

  //TRxMemoryData
  SMemNoRecords            = 'No data found';
  SInvalidFields           = 'No fields defined';

  //TrxDBGridFindForm
  sRxDbGridFindCaption     = 'Find data';
  sRxDbGridFindText        = 'Text to find';
  sRxDbGridFindOnField     = 'Find on field';
  sRxDbGridFindCaseSens    = 'Case sensetive';
  sRxDbGridFindPartial     = 'Partial key';
  sRxDbGridFindDirecion    = 'Direction';
  sRxDbGridFindRangeAll    = 'All';
  sRxDbGridFindRangeForw   = 'Forward';
  sRxDbGridFindRangeBack   = 'Backward';

  //TrxDBGridColumsForm
  sRxDbGridSelColCaption   = 'Grid columns';
  sRxDbGridSelColHint1     = 'Move selected column up';
  sRxDbGridSelColHint2     = 'Move selected column down';

const
  { The following strings should not be localized }
  sAction       = '.Action';
  sCount        = '.Count';
  sVisible      = '.Visible';
  sItem         = '.Item';
  sWidth        = '.Width';
  sTop          = '.Top';
  sVersion      = '.Version';
  sLeft         = '.Left';
  sShowHint     = '.ShowHint';
  sShowCaption  = '.ShowCaption';
  sToolBarStyle = '.ToolBarStyle';
  sButtonAllign = '.ButtonAllign';
  sOptions      = '.Options';
  sCaption      = '.Caption';
  
implementation


end.
