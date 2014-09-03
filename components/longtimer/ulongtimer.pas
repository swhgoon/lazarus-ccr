unit uLongTimer;
{ TlongTimer

  Based on TIdleTimer component
  1. Set the Interval type
  2. For all Interval Types, you can set the Hour
  3. The OnTimer event will only be fired at the specified intervals
  4. The underlying interval is 30 minutes (when idle)

  Copyright (C)2014 minesadorada@charcodelvalle.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, ExtCtrls,DateUtils,Dialogs,AboutLongTimerunit;

type
  TIntervalType = (lt1Daily,lt2Weekly,lt3Monthly);
  TSampleInterval = (lt1Everyminute,lt2Every5minutes,lt3Every10Minutes,lt4Every30Minutes,lt5Every45Minutes);
  TDay = (lt1Monday,lt2Tuesday,lt3Wednesday,lt4Thursday,lt5Friday,lt6Saturday,lt7Sunday);
  TLongTimer = class(TAboutLongTimer)
  private
    { Private declarations }
   fCurrentDateTime,fLastFiredDateTime:TDateTime;
   fIntervalType:TIntervalType;
   fHour,fDay,fDate:Word;
   fTday:TDay;
   fHourDone,fDayDone,fDateDone:Boolean;
   fSampleInterval:TSampleInterval;
  protected
    { Protected declarations }
   procedure DoOnIdle(Sender: TObject; var Done: Boolean); override;
   procedure DoOnIdleEnd(Sender: TObject); override;
   procedure DoOnTimer;override;
   Procedure SetDay(aDay:TDay);
   Procedure SetDailyHour(aHour:Word);
   Procedure SetMonthlyDate(ADate:Word);
   procedure SetSampleInterval(ASampleInterval:TSampleInterval);
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
  published
    { Published declarations }
    // Default=false
    property AutoEnabled;
    // Same as TIdleTimer
    property AutoStartEvent;
    // Same as TIdleTimer
    property AutoEndEvent;
    // Same as TIdleTimer
    property Enabled;
    // This is fired only at the Long Intervals you set
    property OnTimer;
    // Same as TIdleTimer
    property OnStartTimer;
    // Same as TIdleTimer
    property OnStopTimer;
    // If Weekly or Monthly you can also set the Daily24Hour property
    property IntervalType:TIntervalType read fIntervalType write fIntervalType default lt1Daily;
    // Smaller = more accurate, larger = less CPU time
    property SampleInterval:TSampleInterval read fSampleInterval write SetSampleInterval default lt3Every10Minutes;
    // 0=Midnight, 4=4am, 16=4pm etc.
    Property Daily24Hour:Word read fHour write SetDailyHour;
    // You can also set the Hour as well as the Weekday
    Property WeeklyDay:TDay read fTDay write SetDay;
    // You can also set the hour as well as the date
    property MonthlyDate:Word read fDate write SetMonthlyDate default 1;
    // Until the first Timer event, this will be the component's creation time
    Property LastFired:TDateTime read fLastFiredDateTime;
  end;

procedure Register;

implementation
CONST
  C_OneMinute = 60000;
procedure Register;
begin
  RegisterComponents('System',[TLongTimer]);
  {$I longtimer_icon.lrs}
end;
constructor TLongTimer.Create(TheOwner: TComponent);
Var sz:String;
begin
  inherited;
  // Set About dialog properties
  AboutBoxComponentName:='TLongTimer';
  AboutBoxTitle:='TLongTimer Component';
  // AboutBoxWidth (integer)
  AboutBoxHeight:=380;
  sz:='LongTimer is a descendent of TIdleTimer' + LineEnding;
  sz+='and shares its properties and methods.' + LineEnding + LineEnding;
  sz+='Additional properties affect when the' + LineEnding;
  sz+='OnTimer event is fired.' + LineEnding + LineEnding;
  sz+='With LongTimer, the OnTimer event' + LineEnding;
  sz+='will be fired only ONCE - every time' + LineEnding;
  sz+='the interval that you set is reached.';
  AboutBoxDescription:=sz;
  // AboutBoxBackgroundColor (TColor, like clWhite)
  // AboutBoxFontName (string)
  // AboutBoxFontSize (integer)
  AboutBoxVersion:='0.0.1';
  AboutBoxAuthorname:='Gordon Bamber';
  // AboutBoxOrganisation (string)
  AboutBoxAuthorEmail:='minesadorada@charcodelvalle.com';
  AboutBoxLicenseType:='LGPL';// (string e.g. 'GPL', ModifiedGPL' etc

  fHourDone:=false;
  fDayDone:=False;
  fDateDone:=False;
  fCurrentDateTime:=Now;
  fLastFiredDateTime:=Now;
  fDate:=1;
  fSampleInterval:=lt3Every10Minutes;
  Interval:=10 * C_OneMinute;
  fIntervalType:=lt1Daily;
end;
procedure TLongTimer.DoOnIdle(Sender: TObject; var Done: Boolean);
begin
   // Do nothing special here
   inherited;
end;
procedure TLongTimer.DoOnIdleEnd(Sender: TObject);
begin
   // Do nothing special here
   inherited;
end;
procedure TLongTimer.DoOnTimer;
// Only allow this event to fire ONCE if datetime matches the interval set
Var
  cDay,cD,cM,cY,cH,cMi,cS,cms:Word;
  lDay,lD,lM,lY,lH,lMi,lS,lms:Word;
  fTempDate:Word;
begin
   // Split Current date into parts
   fCurrentDateTime:=Now;
   DecodeDate(fCurrentDateTime,cY,cM,cD);
   DecodeTime(fCurrentDateTime,cH,cMi,cS,cmS);
   cDay:=DayOfTheMonth(fCurrentDateTime);

   DecodeDate(fLastFiredDateTime,lY,lM,lD);
   DecodeTime(fLastFiredDateTime,lH,lMi,lS,lmS);
   lDay:=DayOfTheMonth(fLastFiredDateTime);

   // New hour?
   If (fIntervalType = lt1Daily) then
     If (cH <> lH) then fHourDone:=FALSE;
   // New Day?
   If (fIntervalType = lt2Weekly) then
     If (cDay <> lDay) then
       begin
       fDayDone:=FALSE;
       fHourDone:=FALSE;
       end;
   // New Date?
   If (fIntervalType = lt3Monthly) then
     If (cD <> lD) then
       begin
       fDateDone:=FALSE;
       fHourDone:=FALSE;
       end;


   // Only proceed further at specified interval in specified hour - else exit
   If (fIntervalType = lt1Daily) and ((fHourDone = TRUE) OR (cH <> fHour)) then Exit;
   If (fIntervalType = lt2Weekly) and ((fDayDone = TRUE) OR (cH <> fHour)) then Exit;
   If (fIntervalType = lt3Monthly) and ((fDateDone = TRUE)  OR (cH <> fHour)) then Exit;

   // Fire the OnTimer event for the user
   inherited; // Do whatever the user wants done
   fLastFiredDateTime:=Now;// Record the DateTime the OnTimer was fired

   // Now make sure it doesn't fire more than once when resampled

   // Deal with Months where fDate has been set to an invalid date
   // (i.e. 31st February)
   // Simply temporarily decrement the fDate until it is valid
   fTempDate:=fDate;
   If (fIntervalType = lt3Monthly) then
      While NOT IsValidDate(cY,cM,fTempDate) do Dec(fTempDate);

   // If ltDaily, then fDayDone and fDateDone are always FALSE
   If (fIntervalType = lt1Daily) and (cH = fHour) then
     begin
     fHourDone := TRUE;
     end;

   // If ltWeekly, then fHourDone and fDateDone are always FALSE
   // Set only if on Correct Weekday and at specified hour
   If (fIntervalType = lt2Weekly)
   and ((cDay = fDay)
   and (ch = fHour))
   then
   begin
     fDayDone := TRUE;
     fHourDone := TRUE;
   end;

   // If ltMonthly, then fDayDone and fHourDone are always FALSE
   // Set only if Correct day of month and at specified hour
   If (fIntervalType = lt3Monthly) and
   ((cD = fTempDate)
   and (ch = fHour))
   then
     begin
       fDateDone := TRUE;
       fHourDone := TRUE;
     end;
end;
procedure TLongTimer.SetSampleInterval(ASampleInterval:TSampleInterval);
Var TimerEnabled:Boolean;
Begin
    If ASampleInterval = fSampleInterval then exit;
    // Temporarily disable running timer?
    TimerEnabled:=Enabled;
    Enabled:=False;
    Case ASampleInterval of
       lt1Everyminute:Interval:=C_OneMinute;
       lt2Every5minutes:Interval:=5 * C_OneMinute;
       lt3Every10Minutes:Interval:=10 * C_OneMinute;
       lt4Every30Minutes:Interval:=30 * C_OneMinute;
       lt5Every45Minutes:Interval:=45 * C_OneMinute;
    end;
    Enabled:=TimerEnabled;
end;

Procedure TLongTimer.SetDay(aDay:TDay);
Var TimerEnabled:Boolean;
begin
   If ADay = fTDay then Exit;
   // Temporarily disable running timer?
   TimerEnabled:=Enabled;
   Enabled:=False;
   fTDay:=aDay;
   fDay:=Ord(aDay)+1;
   Enabled:=TimerEnabled;
 {
  // ISO day numbers.
  DayMonday    = 1;
  DayTuesday   = 2;
  DayWednesday = 3;
  DayThursday  = 4;
  DayFriday    = 5;
  DaySaturday  = 6;
  DaySunday    = 7;
}
end;
Procedure TLongTimer.SetDailyHour(aHour:Word);
Var TimerEnabled:Boolean;
begin
  If fHour=aHour then Exit;
  // Temporarily disable running timer?
  TimerEnabled:=Enabled;
  Enabled:=False;
  If (aHour >= 0) AND (aHour <=24) then fHour:=aHour;
  Enabled:=TimerEnabled;
end;

Procedure TLongTimer.SetMonthlyDate(ADate:Word);
Var TimerEnabled:Boolean;
begin
   If ADate=fDate then Exit;
   // Temporarily disable running timer?
   TimerEnabled:=Enabled;
   Enabled:=False;
   If (fDate > 0) and (fDate < 32) then fDate:=ADate;
   // Invalid dates like 31st Feb are dealt with in DoOnTimer
   // e.g. 31 stands for the last day in any month (inc Feb in a Leap Year)
   Enabled:=TimerEnabled;
end;

end.
