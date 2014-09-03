unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  Buttons, StdCtrls, uLongTimer;

{ Tmainform }
type
  Tmainform = class(TForm)
    cmb_Daily24Hour: TComboBox;
    cmb_IntervalType: TComboBox;
    cmb_weekordate: TComboBox;
    cmd_Close: TBitBtn;
    cmd_StopTimer: TButton;
    cmd_StartTimer: TButton;
    cmb_SampleInterval: TComboBox;
    crp_SetTimer: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LongTimer1: TLongTimer;
    memo_ReportTimerEvent: TMemo;
    procedure cmb_SampleIntervalChange(Sender: TObject);
    procedure cmd_StopTimerClick(Sender: TObject);
    procedure cmd_StartTimerClick(Sender: TObject);
    procedure cmb_Daily24HourChange(Sender: TObject);
    procedure cmb_IntervalTypeChange(Sender: TObject);
    procedure cmb_weekordateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LongTimer1StartTimer(Sender: TObject);
    procedure LongTimer1StopTimer(Sender: TObject);
    procedure LongTimer1Timer(Sender: TObject);
  private
    { private declarations }
    procedure PopulateWeekOrDate(Const i:Integer);

  public
    { public declarations }
  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }

procedure Tmainform.LongTimer1Timer(Sender: TObject);
begin
  // memo_ReportTimerEvent.Lines.Add('LastFired at ' + FormatDateTime('hh:nn:ss', LongTimer1.LastFired));
  memo_ReportTimerEvent.Lines.Add('Fired at ' + FormatDateTime('hh:nn:ss', Now));
end;

procedure Tmainform.FormCreate(Sender: TObject);
Var i:Integer;
begin
  Caption:=Application.Title;
  memo_ReportTimerEvent.Clear;
  cmb_Daily24Hour.Clear;
  cmb_Daily24Hour.Items.Add('Midnight');
  For i:=1 to 23 do
   cmb_Daily24Hour.Items.Add(Format('%2.d:00',[i]));
  LongTimer1.IntervalType:=lt1Daily;
  LongTimer1.Daily24Hour:=0;
  LongTimer1.Enabled:=FALSE;
  cmb_Daily24Hour.ItemIndex:=0;
  cmb_SampleInterval.Clear;
  cmb_SampleInterval.Items.Add('Every minute');
  cmb_SampleInterval.Items.Add('Every 5 minutes');
  cmb_SampleInterval.Items.Add('Every 10 minutes');
  cmb_SampleInterval.Items.Add('Every 30 minutes');
  cmb_SampleInterval.Items.Add('Every 45 minutes');
  memo_ReportTimerEvent.Lines.Add('Timer initially disabled');
  cmb_SampleInterval.ItemIndex:=2;
end;

procedure Tmainform.LongTimer1StartTimer(Sender: TObject);
begin
//    memo_ReportTimerEvent.Lines.Add('Timer running');
end;

procedure Tmainform.LongTimer1StopTimer(Sender: TObject);
begin
//    memo_ReportTimerEvent.Lines.Add('Timer stopped');

end;

procedure Tmainform.cmd_StopTimerClick(Sender: TObject);
begin
  LongTimer1.Enabled:=FALSE;
  memo_ReportTimerEvent.Lines.Add('Timer disabled');
end;

procedure Tmainform.cmb_SampleIntervalChange(Sender: TObject);
begin
  LongTimer1.SampleInterval:=TSampleInterval(cmb_SampleInterval.ItemIndex);
end;

procedure Tmainform.cmd_StartTimerClick(Sender: TObject);
begin
    LongTimer1.Enabled:=TRUE;
    memo_ReportTimerEvent.Lines.Add('Timer enabled');

end;

procedure Tmainform.cmb_Daily24HourChange(Sender: TObject);
begin
  LongTimer1.Daily24Hour:=cmb_Daily24Hour.ItemIndex;
end;
procedure Tmainform.PopulateWeekOrDate(Const i:Integer);
// 0=Weekly, 1=Monthly
Var iMonthDay:Integer;

begin
  cmb_weekordate.Clear;
   Case i of
   0:begin
      cmb_weekordate.Items.Add('Monday');
      cmb_weekordate.Items.Add('Tuesday');
      cmb_weekordate.Items.Add('Wednesday');
      cmb_weekordate.Items.Add('Thursday');
      cmb_weekordate.Items.Add('Friday');
      cmb_weekordate.Items.Add('Saturday');
      cmb_weekordate.Items.Add('Sunday');
     end;
   1:begin
       For iMonthDay := 1 to 31 do
        cmb_weekordate.Items.Add(Format('%2.d',[iMonthDay]));
     end;
   end;
   cmb_weekordate.ItemIndex:=0;
end;

procedure Tmainform.cmb_IntervalTypeChange(Sender: TObject);
begin
  cmd_StopTimer.Click;
  Case cmb_IntervalType.ItemIndex of
  0:
  begin
  LongTimer1.IntervalType:=lt1Daily;
  cmb_weekordate.Enabled:=FALSE;
  end;
  1:
  begin
    LongTimer1.IntervalType:=lt2Weekly;
    PopulateWeekOrDate(0);
    cmb_weekordate.Enabled:=TRUE;
  end;
  2:
  begin
    LongTimer1.IntervalType:=lt3Monthly;
    PopulateWeekOrDate(1);
    cmb_weekordate.Enabled:=TRUE;
  end;
  end;
end;

procedure Tmainform.cmb_weekordateChange(Sender: TObject);
begin
  Case LongTimer1.IntervalType of
     lt2Weekly:LongTimer1.WeeklyDay:=TDay(cmb_weekordate.ItemIndex);
     lt3Monthly:LongTimer1.MonthlyDate:=(cmb_weekordate.ItemIndex+1);
  end;
end;

end.

