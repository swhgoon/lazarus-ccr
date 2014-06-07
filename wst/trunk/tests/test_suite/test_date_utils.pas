{   This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_date_utils;

interface
uses
  SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  test_suite_utils, date_utils;

type

  { TTest_DateUtils }

  TTest_DateUtils = class(TWstBaseTest)
  protected
{$IFDEF FPC}
    class procedure CheckEquals(expected, actual: TTimeRec; msg: string = ''); overload;
{$ENDIF FPC}
{$IFDEF WST_DELPHI}
    procedure CheckEquals(expected, actual: TTimeRec; msg: string = ''); overload;
{$ENDIF WST_DELPHI}
  published
    procedure xsd_TryStrToDate_date_only();
    procedure xsd_TryStrToDate_date_timezone_1();
    procedure xsd_TryStrToDate_date_timezone_2();
    procedure xsd_TryStrToDate_date_time();
    procedure xsd_TryStrToDate_date_time_fractional_second();
    procedure xsd_TryStrToDate_date_time_fractional_second_2();
    procedure xsd_TryStrToDate_date_bad_separator();
    procedure xsd_TryStrToDate_date_time_bad_separator();
    procedure xsd_TryStrToDate_date_time_timezone_z();
    procedure xsd_TryStrToDate_date_time_timezone_zero();
    procedure xsd_TryStrToDate_date_time_timezone_1();
    procedure xsd_TryStrToDate_date_time_timezone_2();

    procedure xsd_DateTimeToStr_1();
    procedure xsd_DateTimeToStr_2();
    procedure xsd_DateTimeToStr_fractional_second_1();
    procedure xsd_DateTimeToStr_fractional_second_2();
    procedure xsd_DateTimeToStr_timezone_1();

    procedure xsd_TimeToStr_1();
    procedure xsd_TimeToStr_zero();
    procedure xsd_TimeToStr_fractional_second_1();
    procedure xsd_TryStrToTime_hour_only();
    procedure xsd_TryStrToTime_hour_minute_only();
    procedure xsd_TryStrToTime_hour_minute_second_only();
    procedure xsd_TryStrToTime_all_fields();
    procedure xsd_TryStrToTime_time_timezone_1();
    procedure xsd_TryStrToTime_time_timezone_2();
    procedure xsd_TryStrToTime_time_timezone_3();
    procedure xsd_TryStrToTime_time_timezone_4();
  end;

implementation

{ TTest_DateUtils }

procedure TTest_DateUtils.xsd_DateTimeToStr_1();
const
  sDATE_1 = '1976-10-12T23:34:56Z';
  sDATE_2 = '0987-06-12T20:34:56Z';
var
  d : TDateTimeRec;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(1976,10,12) + EncodeTime(23,34,56,0);
  CheckEquals(sDATE_1, xsd_DateTimeToStr(d,xdkDateTime));

  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(987,06,12) - EncodeTime(20,34,56,0);
  CheckEquals(sDATE_2, xsd_DateTimeToStr(d,xdkDateTime));
end;

procedure TTest_DateUtils.xsd_DateTimeToStr_2();
const
  sDATE_1 = '1976-10-12T23:34:56Z';
  sDATE_2 = '0987-06-12T20:34:56Z';
var
  d : TDateTime;
begin
  d := EncodeDate(1976,10,12) + EncodeTime(23,34,56,0);
  CheckEquals(sDATE_1, xsd_DateTimeToStr(d,xdkDateTime));

  d := EncodeDate(987,06,12) - EncodeTime(20,34,56,0);
  CheckEquals(sDATE_2, xsd_DateTimeToStr(d,xdkDateTime));
end;

procedure TTest_DateUtils.xsd_DateTimeToStr_fractional_second_1();
const
  sDATE_1 = '1976-10-12T23:34:56.007Z';
  sDATE_2 = '1976-10-12T23:34:56.078Z';
  sDATE_3 = '1976-10-12T23:34:56.789Z';
var
  d : TDateTimeRec;
begin
  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(1976,10,12) + EncodeTime(23,34,56,7);
  CheckEquals(sDATE_1, xsd_DateTimeToStr(d,xdkDateTime));

  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(1976,10,12) + EncodeTime(23,34,56,78);
  CheckEquals(sDATE_2, xsd_DateTimeToStr(d,xdkDateTime));

  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(1976,10,12) + EncodeTime(23,34,56,789);
  CheckEquals(sDATE_3, xsd_DateTimeToStr(d,xdkDateTime));
end;

procedure TTest_DateUtils.xsd_DateTimeToStr_fractional_second_2();
const
  sDATE_1 = '1976-10-12T23:34:56.007Z';
  sDATE_2 = '1976-10-12T23:34:56.078Z';
  sDATE_3 = '1976-10-12T23:34:56.789Z';
var
  d : TDateTime;
begin
  FillChar(d,SizeOf(d),#0);
  d := EncodeDate(1976,10,12) + EncodeTime(23,34,56,7);
  CheckEquals(sDATE_1, xsd_DateTimeToStr(d,xdkDateTime));

  FillChar(d,SizeOf(d),#0);
  d := EncodeDate(1976,10,12) + EncodeTime(23,34,56,78);
  CheckEquals(sDATE_2, xsd_DateTimeToStr(d,xdkDateTime));

  FillChar(d,SizeOf(d),#0);
  d := EncodeDate(1976,10,12) + EncodeTime(23,34,56,789);
  CheckEquals(sDATE_3, xsd_DateTimeToStr(d,xdkDateTime));
end;

procedure TTest_DateUtils.xsd_DateTimeToStr_timezone_1();
  //2002-10-10T12:00:00+05:00 is 2002-10-10T07:00:00Z
var
  d : TDateTimeRec;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(2002,10,10) + EncodeTime(12,0,0,0);
  d.HourOffset := 5;
  CheckEquals('2002-10-10T07:00:00Z', xsd_DateTimeToStr(d,xdkDateTime));
end;

procedure TTest_DateUtils.xsd_TimeToStr_1();
const
  sVALUE_1 = '01:23:45Z';
  sVALUE_2 = '12:34:56Z';
  sVALUE_3 = '20:34:56Z';
var
  d : TTimeRec;
begin
  //hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  d.Hour := 1;
  d.Minute := 23;
  d.Second := 45;
  d.MilliSecond := 0;
  d.HourOffset := 0;
  d.MinuteOffset := 0;
  CheckEquals(sVALUE_1, xsd_TimeToStr(d));

  d.Hour := 12;
  d.Minute := 34;
  d.Second := 56;
  d.MilliSecond := 0;
  d.HourOffset := 0;
  d.MinuteOffset := 0;
  CheckEquals(sVALUE_2, xsd_TimeToStr(d));

  d.Hour := 20;
  d.Minute := 34;
  d.Second := 56;
  d.MilliSecond := 0;
  d.HourOffset := 0;
  d.MinuteOffset := 0;
  CheckEquals(sVALUE_3, xsd_TimeToStr(d));
end;

procedure TTest_DateUtils.xsd_TimeToStr_zero();
const
  sVALUE_1 = '00:00:00Z';
var
  d : TTimeRec;
begin
  //hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  d.Hour := 0;
  d.Minute := 0;
  d.Second := 0;
  d.MilliSecond := 0;
  d.HourOffset := 0;
  d.MinuteOffset := 0;
  CheckEquals(sVALUE_1, xsd_TimeToStr(d));
end;

procedure TTest_DateUtils.xsd_TimeToStr_fractional_second_1();
const
  sVALUE_1 = '23:34:56.007Z';
  sVALUE_2 = '23:34:56.078Z';
  sVALUE_3 = '23:34:56.789Z';
var
  d : TTimeRec;
begin
  d.Hour := 23;
  d.Minute := 34;
  d.Second := 56;
  d.MilliSecond := 7;
  d.HourOffset := 0;
  d.MinuteOffset := 0;
  CheckEquals(sVALUE_1, xsd_TimeToStr(d));

  d.Hour := 23;
  d.Minute := 34;
  d.Second := 56;
  d.MilliSecond := 78;
  d.HourOffset := 0;
  d.MinuteOffset := 0;
  CheckEquals(sVALUE_2, xsd_TimeToStr(d));

  d.Hour := 23;
  d.Minute := 34;
  d.Second := 56;
  d.MilliSecond := 789;
  d.HourOffset := 0;
  d.MinuteOffset := 0;
  CheckEquals(sVALUE_3, xsd_TimeToStr(d));
end;

procedure TTest_DateUtils.xsd_TryStrToTime_hour_only();
var
  s : string;
  d : TTimeRec;
begin
  //hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '01';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(1,0,0,0),d);
  s := '05';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(5,0,0,0),d);
  s := '12';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(12,0,0,0),d);
  s := '13';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(13,0,0,0),d);
  s := '20';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(20,0,0,0),d);
end;

procedure TTest_DateUtils.xsd_TryStrToTime_hour_minute_only();
var
  s : string;
  d : TTimeRec;
begin
  //hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '01:00';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(1,0,0,0),d);
  s := '05:12';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(5,12,0,0),d);
  s := '12:55';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(12,55,0,0),d);
  s := '13:45';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(13,45,0,0),d);
  s := '20:34';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(20,34,0,0),d);
end;

procedure TTest_DateUtils.xsd_TryStrToTime_hour_minute_second_only();
var
  s : string;
  d : TTimeRec;
begin
  //hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '01:00:00';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(1,0,0,0),d);
  s := '05:12:34';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(5,12,34,0),d);
  s := '12:55:56';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(12,55,56,0),d);
  s := '13:45:41';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(13,45,41,0),d);
  s := '20:34:12';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(20,34,12,0),d);
end;

procedure TTest_DateUtils.xsd_TryStrToTime_all_fields();
var
  s : string;
  d : TTimeRec;
begin
  //hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '01:00:00.00';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(1,0,0,0),d);
  s := '05:12:34.001';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(5,12,34,1),d);
  s := '12:55:56.012';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(12,55,56,12),d);
  s := '13:45:41.123';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(13,45,41,123),d);
  s := '20:34:12.998';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(xsd_EncodeTime(20,34,12,998),d);
end;

procedure TTest_DateUtils.xsd_TryStrToTime_time_timezone_1();
var
  s : string;
  d : TTimeRec;
begin
  s := '23:34:56+12:34';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(23,d.Hour,'Hour');
    CheckEquals(34,d.Minute,'Minute');
    CheckEquals(56,d.Second,'Second');
    CheckEquals(0,d.MilliSecond,'MilliSecond');
  CheckEquals(12,d.HourOffset,'HourOffset');
  CheckEquals(34,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToTime_time_timezone_2();
var
  s : string;
  d : TTimeRec;
begin
  s := '23:34:56-01:23';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(23,d.Hour,'Hour');
    CheckEquals(34,d.Minute,'Minute');
    CheckEquals(56,d.Second,'Second');
    CheckEquals(0,d.MilliSecond,'MilliSecond');
  CheckEquals(-1,d.HourOffset,'HourOffset');
  CheckEquals(-23,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToTime_time_timezone_3();
var
  s : string;
  d : TTimeRec;
begin
  s := '23:34:56.78+12:34';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(23,d.Hour,'Hour');
    CheckEquals(34,d.Minute,'Minute');
    CheckEquals(56,d.Second,'Second');
    CheckEquals(78,d.MilliSecond,'MilliSecond');
  CheckEquals(12,d.HourOffset,'HourOffset');
  CheckEquals(34,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToTime_time_timezone_4();
var
  s : string;
  d : TTimeRec;
begin
  s := '23:34:56.789-01:23';
  Check(xsd_TryStrToTime(s,d));
    CheckEquals(23,d.Hour,'Hour');
    CheckEquals(34,d.Minute,'Minute');
    CheckEquals(56,d.Second,'Second');
    CheckEquals(789,d.MilliSecond,'MilliSecond');
  CheckEquals(-1,d.HourOffset,'HourOffset');
  CheckEquals(-23,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_bad_separator();
const
  DATE_STR = '1976;10;12';
var
  d : TDateTimeRec;
begin
  CheckEquals(False,xsd_TryStrToDate(DATE_STR,d,xdkDateTime),Format('"%s" is not a valid date.',[DATE_STR]));
end;

{$IFDEF FPC}
class procedure TTest_DateUtils.CheckEquals(expected, actual: TTimeRec; msg: string);
begin
  CheckEquals(expected.Hour,actual.Hour,msg + ', Hour');
  CheckEquals(expected.Minute,actual.Minute,msg + ', Minute');
  CheckEquals(expected.Second,actual.Second,msg + ', Second');
  CheckEquals(expected.MilliSecond,actual.MilliSecond,msg + ', MilliSecond');
  CheckEquals(expected.HourOffset,actual.HourOffset,msg + ', HourOffset');
  CheckEquals(expected.MinuteOffset,actual.MinuteOffset,msg + ', MinuteOffset');
end;
{$ENDIF FPC}

{$IFDEF WST_DELPHI}
procedure TTest_DateUtils.CheckEquals(expected, actual: TTimeRec; msg: string);
begin
  CheckEquals(expected.Hour,actual.Hour,msg + ', Hour');
  CheckEquals(expected.Minute,actual.Minute,msg + ', Minute');
  CheckEquals(expected.Second,actual.Second,msg + ', Second');
  CheckEquals(expected.MilliSecond,actual.MilliSecond,msg + ', MilliSecond');
  CheckEquals(expected.HourOffset,actual.HourOffset,msg + ', HourOffset');
  CheckEquals(expected.MinuteOffset,actual.MinuteOffset,msg + ', MinuteOffset');
end;
{$ENDIF WST_DELPHI}

procedure TTest_DateUtils.xsd_TryStrToDate_date_only();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12';
  d := xsd_StrToDate(s,xdkDateTime);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(1976,y,'Year');
      CheckEquals(10,m,'Month');
      CheckEquals(12,dy,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(0,hh,'Hour');
      CheckEquals(0,mn,'Minute');
      CheckEquals(0,ss,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');

  d := xsd_StrToDate(s,xdkDate);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(1976,y,'Year');
      CheckEquals(10,m,'Month');
      CheckEquals(12,dy,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(0,hh,'Hour');
      CheckEquals(0,mn,'Minute');
      CheckEquals(0,ss,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_timezone_1();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12+11:34';
  d := xsd_StrToDate(s,xdkDate);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(1976,y,'Year');
      CheckEquals(10,m,'Month');
      CheckEquals(12,dy,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(0,hh,'Hour');
      CheckEquals(0,mn,'Minute');
      CheckEquals(0,ss,'Second');
    CheckEquals(11,d.HourOffset,'HourOffset');
    CheckEquals(34,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_timezone_2();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12-11:34';
  d := xsd_StrToDate(s,xdkDate);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(1976,y,'Year');
      CheckEquals(10,m,'Month');
      CheckEquals(12,dy,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(0,hh,'Hour');
      CheckEquals(0,mn,'Minute');
      CheckEquals(0,ss,'Second');
    CheckEquals(-11,d.HourOffset,'HourOffset');
    CheckEquals(-34,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56';
  d := xsd_StrToDate(s,xdkDateTime);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(1976,y,'Year');
      CheckEquals(10,m,'Month');
      CheckEquals(12,dy,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(23,hh,'Hour');
      CheckEquals(34,mn,'Minute');
      CheckEquals(56,ss,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_bad_separator();
const
  DATE_STR = '1976-10-12T23/34:56';
var
  d : TDateTimeRec;
begin
  CheckEquals(False,xsd_TryStrToDate(DATE_STR,d,xdkDateTime),Format('"%s" is not a valid date.',[DATE_STR]));
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_fractional_second();

  procedure do_check(
    const AString : string;
    const AY, AM, ADY : Word;
    const AHH, AMN, ASS, ASSSS : Word
  );
  var
    d : TDateTimeRec;
    y,m,dy : Word;
    hh,mn,ss, ssss : Word;
  begin
    d := xsd_StrToDate(AString,xdkDateTime);
      DecodeDate(d.Date,y,m,dy);
        CheckEquals(AY,y,'Year');
        CheckEquals(AM,m,'Month');
        CheckEquals(ADY,dy,'Day');
      DecodeTime(d.Date,hh,mn,ss,ssss);
        CheckEquals(AHH,hh,'Hour');
        CheckEquals(AMN,mn,'Minute');
        CheckEquals(ASS,ss,'Second');
        CheckEquals(ASSSS,ssss,'MiliSecond');
      CheckEquals(0,d.HourOffset,'HourOffset');
      CheckEquals(0,d.MinuteOffset,'MinuteOffset');
  end;

begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  do_check('1976-10-12T23:34:56.7', 1976,10,12,  23,34,56,700);
  do_check('1976-10-12T23:34:56.07', 1976,10,12,  23,34,56,70);
  do_check('1976-10-12T23:34:56.007', 1976,10,12,  23,34,56,7);
  do_check('1976-10-12T23:34:56.789', 1976,10,12,  23,34,56,789);
  do_check('1976-10-12T23:34:56.78', 1976,10,12,  23,34,56,780);
  do_check('1976-10-12T23:34:56.078', 1976,10,12,  23,34,56,78);
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_fractional_second_2();

  procedure do_check(
    const AString : string;
    const AY, AM, ADY : Word;
    const AHH, AMN, ASS, ASSSS : Word
  );
  var
    d : TDateTimeRec;
    y,m,dy : Word;
    hh,mn,ss, ssss : Word;
  begin
    d := xsd_StrToDate(AString,xdkDateTime);
      DecodeDate(d.Date,y,m,dy);
        CheckEquals(AY,y,'Year');
        CheckEquals(AM,m,'Month');
        CheckEquals(ADY,dy,'Day');
      DecodeTime(d.Date,hh,mn,ss,ssss);
        CheckEquals(AHH,hh,'Hour');
        CheckEquals(AMN,mn,'Minute');
        CheckEquals(ASS,ss,'Second');
        CheckEquals(ASSSS,ssss,'MiliSecond');
      CheckEquals(0,d.HourOffset,'HourOffset');
      CheckEquals(0,d.MinuteOffset,'MinuteOffset');
  end;

begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  do_check('1976-10-12T23:34:56.123456789', 1976,10,12,  23,34,56,123);
  do_check('2010-01-04T23:43:21.569358Z', 2010,01,04,  23,43,21,569);
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_timezone_1();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56+12:34';
  d := xsd_StrToDate(s,xdkDateTime);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(12,d.HourOffset,'HourOffset');
    CheckEquals(34,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_timezone_2();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56-01:23';
  d := xsd_StrToDate(s,xdkDateTime);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(-1,d.HourOffset,'HourOffset');
    CheckEquals(-23,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_timezone_z();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56Z';
  d := xsd_StrToDate(s,xdkDateTime);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_timezone_zero();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56+00:00';
  d := xsd_StrToDate(s,xdkDateTime);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');

  s := '1976-10-12T23:34:56-00:00';
  d := xsd_StrToDate(s,xdkDateTime);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');
end;

initialization
  RegisterTest('Support',TTest_DateUtils.Suite);

end.
