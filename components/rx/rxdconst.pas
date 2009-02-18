{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit rxdconst;

{ RX Data aware controls constants }
{
  Reserved range
  from MaxExtStrID - 86
  to   MaxExtStrID - 134
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

resourcestring
(*
{ DBLists }

  SLocalDatabase          = MaxExtStrID - 86;

{ DBUtils }

  SRetryLogin             = MaxExtStrID - 87;

{ DBFilter }

  SExprNotBoolean         = MaxExtStrID - 88;
  SExprBadNullTest        = MaxExtStrID - 89;
  SExprBadField           = MaxExtStrID - 90;
  SCaptureFilter          = MaxExtStrID - 91;
  SNotCaptureFilter       = MaxExtStrID - 92;

{ RxDBCtrl }

  SInactiveData           = MaxExtStrID - 93;
  SBrowseData             = MaxExtStrID - 94;
  SEditData               = MaxExtStrID - 95;
  SInsertData             = MaxExtStrID - 96;
  SSetKeyData             = MaxExtStrID - 97;
  SCalcFieldsData         = MaxExtStrID - 98;

{ LoginDlg }

  SRegistration           = MaxExtStrID - 99;
  SAppTitleLabel          = MaxExtStrID - 100;
  SHintLabel              = MaxExtStrID - 101;
  SUserNameLabel          = MaxExtStrID - 102;
  SPasswordLabel          = MaxExtStrID - 103;
  SInvalidUserName        = MaxExtStrID - 104;

{ ChPswDlg }

  SChangePassword         = MaxExtStrID - 105;
  SOldPasswordLabel       = MaxExtStrID - 106;
  SNewPasswordLabel       = MaxExtStrID - 107;
  SConfirmPasswordLabel   = MaxExtStrID - 108;
  SPasswordChanged        = MaxExtStrID - 109;
  SPasswordNotChanged     = MaxExtStrID - 110;
  SPasswordsMismatch      = MaxExtStrID - 111;

{ DBExcpt }

  SDBExceptCaption        = MaxExtStrID - 112;
  SBDEErrorLabel          = MaxExtStrID - 113;
  SServerErrorLabel       = MaxExtStrID - 114;
  SErrorMsgLabel          = MaxExtStrID - 115;
  SNextButton             = MaxExtStrID - 116;
  SPrevButton             = MaxExtStrID - 117;

{ DBFilter expression parser }

  SExprIncorrect          = MaxExtStrID - 118;
  SExprTermination        = MaxExtStrID - 119;
  SExprNameError          = MaxExtStrID - 120;
  SExprStringError        = MaxExtStrID - 121;
  SExprInvalidChar        = MaxExtStrID - 122;
  SExprNoRParen           = MaxExtStrID - 123;
  SExprExpected           = MaxExtStrID - 124;
  SExprBadCompare         = MaxExtStrID - 125;

{ DBUtils }

  SConfirmSave            = MaxExtStrID - 126;
  SDatabaseName           = MaxExtStrID - 127;  

{ LoginDlg }
  
  SUnlockCaption          = MaxExtStrID - 128;
  SUnlockHint             = MaxExtStrID - 129;

{ RxDBCtrl }

  SDeleteMultipleRecords  = MaxExtStrID - 130;*)

  SLocalDatabase = 'Невозможно произвести эту операцию с локальной базой данных';
  SRetryLogin = 'Вы хотите повторить попытку соединения с базой данных?';
  SExprNotBoolean = 'Поле ''%s'' не является полем логического типа';
  SExprBadNullTest = 'NULL-значения допустимы только в операциях ''='' и ''<>''';
  SExprBadField = 'Поле ''%s'' не может быть использовано в выражении фильтра';
  SCaptureFilter = 'Элементы управления захвачены фильтром';
  SNotCaptureFilter = 'Элементы управления должны быть захвачены фильтром';
  SInactiveData = 'неактивно';
  SBrowseData = 'просмотр';
  SEditData = 'редактирование';
  SInsertData = 'добавление';
  SSetKeyData = 'поиск';
  SCalcFieldsData = 'подсчет';
  SRegistration = 'Регистрация';
  SAppTitleLabel = 'Программа "%s"';
  SHintLabel = 'Введите Ваше пользовательское имя и пароль';
  SUserNameLabel = '&Имя пользователя:';
  SPasswordLabel = '&Пароль:';
  SMore1         = '&Больше >>';
  SMore2         = '&Меньше <<';
  SInvalidUserName = 'Неверное имя пользователя или пароль';
  SChangePassword = 'Смена пароля';
  SOldPasswordLabel = '&Старый пароль:';
  SNewPasswordLabel = '&Новый пароль:';
  SConfirmPasswordLabel = '&Подтверждение:';
  SPasswordChanged = 'Пароль сменен';
  SPasswordNotChanged = 'Пароль не сменен';
  SPasswordsMismatch = 'Новый пароль и подтверждение не совпадают';
  SDBExceptCaption = 'Ошибка процессора БД';
  SBDEErrorLabel = 'Ошибка BDE';
  SServerErrorLabel = 'Ошибка сервера';
  SErrorMsgLabel = 'Сообщение об ошибке';
  SNextButton = '&Дальше';
  SPrevButton = '&Назад';
  SExprIncorrect = 'Некорректно сформулировано выражение фильтра';
  SExprTermination = 'Неверное завершение выражения фильтра';
  SExprNameError = 'Невозможно определить завершение имени поля';
  SExprStringError = 'Невозможно определить завершение строковой константы';
  SExprInvalidChar = 'Неверный символ в выражении фильтра: ''%s''';
  SExprNoRParen = 'Ожидалось '')'', а встречено: %s';
  SExprExpected = 'Ожидалось выражение, а встречено %s';
  SExprBadCompare = 'Операции сравнения требуют наличия поля и константы';
  SConfirmSave = 'Данные были изменены. Сохранять?';
  SDatabaseName = 'База данных: %s';
  SUnlockCaption = 'Разблокирование';
  SUnlockHint = 'Введите ваш пароль';
  SDeleteMultipleRecords = 'Удалить все выбранные записи?';

  SPropDefByLookup = 'PropDefByLookup';
  SDataSourceFixed = 'SDataSourceFixed';
  SCircularDataLink = 'SCircularDataLink';


  SDeleteRecordQuestion = 'Delete record?';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SInvalidDate       = 'Invalid Date';
  SFieldRequired = 'Field ''%s'' must have a value';
  SNotEditing = 'Dataset not in edit or insert mode';
  SUnknownFieldType = 'SUnknownFieldType %s';
  SFieldReadOnly = 'SFieldReadOnly %s';

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
