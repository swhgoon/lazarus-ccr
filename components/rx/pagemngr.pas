unit pagemngr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls;

const
  pageNull = -1;
  DefStatusMessage = 'Step %d from %d';
  
type
  TPageOwner = TPageControl;
  TCheckPageEnabled = function (APageIndex:integer): Boolean of object;
  TPageManagerOption = (pmoSetFormCaption, pmoSetInfoControl);
  TPageManagerOptions = set of TPageManagerOption;
  { TPageManager }

  TPageManager = class(TComponent)
  private
    FNextBtn: TControl;
    FOnCheckPageEnabled: TCheckPageEnabled;
    FOnPageChanged: TNotifyEvent;
    FOptions: TPageManagerOptions;
    FPageOwner: TPageOwner;
    FPriorBtn: TControl;
    FSaveBtnNextClick: TNotifyEvent;
    FSaveBtnPriorClick: TNotifyEvent;
    FStatusControl: TControl;
    FStatusMessage: string;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    procedure SetNextBtn(const AValue: TControl);
    procedure SetOnCheckPageEnabled(const AValue: TCheckPageEnabled);
    procedure SetOptions(const AValue: TPageManagerOptions);
    procedure SetPageIndex(const AValue: Integer);
    procedure SetPageOwner(const AValue: TPageOwner);
    procedure SetPriorBtn(const AValue: TControl);
    procedure BtnClickNext(Sender: TObject);
    procedure BtnClickPrior(Sender: TObject);
    procedure SetStatusControl(const AValue: TControl);
    procedure SetStatusMessage(const AValue: string);
    procedure SyncBtnNextClick(Sync: Boolean);
    procedure SyncBtnPriorClick(Sync: Boolean);
  protected
    function GetPriorPageIndex(Page: Integer): Integer; virtual;
    function GetNextPageIndex(Page: Integer): Integer; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); virtual;
    procedure CheckBtnEnabled;
    procedure NextPage;
    procedure PriorPage;
    procedure PageChanged;virtual;
    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
  published
    property PageOwner: TPageOwner read FPageOwner write SetPageOwner;
    property NextBtn: TControl read FNextBtn write SetNextBtn;
    property PriorBtn: TControl read FPriorBtn write SetPriorBtn;
    property OnCheckPageEnabled:TCheckPageEnabled read FOnCheckPageEnabled write SetOnCheckPageEnabled;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
    property Options:TPageManagerOptions read FOptions write SetOptions default [];
    property StatusControl:TControl read FStatusControl write SetStatusControl;
    property StatusMessage:string read FStatusMessage write SetStatusMessage;
  end;

implementation

{ TPageManager }

procedure TPageManager.SetNextBtn(const AValue: TControl);
begin
  if FNextBtn=AValue then exit;
  SyncBtnNextClick(false);
  FNextBtn:=AValue;
  SyncBtnNextClick(true);
end;

function TPageManager.GetPageCount: Integer;
begin
  if Assigned(FPageOwner) then
    Result := FPageOwner.PageCount
  else
    Result := 0;
end;

function TPageManager.GetPageIndex: Integer;
begin
  if Assigned(PageOwner) then Result := PageOwner.ActivePageIndex
  else Result := pageNull;
end;

procedure TPageManager.SetOnCheckPageEnabled(const AValue: TCheckPageEnabled);
begin
  if FOnCheckPageEnabled=AValue then exit;
  FOnCheckPageEnabled:=AValue;
end;

procedure TPageManager.SetOptions(const AValue: TPageManagerOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TPageManager.SetPageIndex(const AValue: Integer);
begin
  if Assigned(FPageOwner) and (FPageOwner.ActivePageIndex <> AValue) then
  begin
    FPageOwner.ActivePageIndex:=AValue;
    PageChanged;
  end;
end;

procedure TPageManager.SetPageOwner(const AValue: TPageOwner);
begin
  if FPageOwner=AValue then exit;
  FPageOwner:=AValue;
end;

procedure TPageManager.SetPriorBtn(const AValue: TControl);
begin
  if FPriorBtn=AValue then exit;
  SyncBtnPriorClick(false);
  FPriorBtn:=AValue;
  SyncBtnPriorClick(true);
end;

procedure TPageManager.BtnClickNext(Sender: TObject);
begin
  if Assigned(FPageOwner) then
  begin
    FPageOwner.ActivePageIndex:=GetNextPageIndex(FPageOwner.ActivePageIndex);
    PageChanged;
  end;
end;

procedure TPageManager.BtnClickPrior(Sender: TObject);
begin
  if Assigned(FPageOwner) then
  begin
    FPageOwner.ActivePageIndex:=GetPriorPageIndex(FPageOwner.ActivePageIndex);
    PageChanged;
  end;
end;

procedure TPageManager.SetStatusControl(const AValue: TControl);
begin
  if FStatusControl=AValue then exit;
  FStatusControl:=AValue;
end;

procedure TPageManager.SetStatusMessage(const AValue: string);
begin
  if FStatusMessage=AValue then exit;
  FStatusMessage:=AValue;
end;

procedure TPageManager.SyncBtnNextClick(Sync: Boolean);
begin
  if Assigned(FNextBtn) and not (csDesigning in ComponentState) then
  begin
    if Sync then
    begin
      FSaveBtnNextClick := FNextBtn.OnClick;
      FNextBtn.OnClick := @BtnClickNext;
    end
    else
    begin
      FNextBtn.OnClick := FSaveBtnNextClick;
      FSaveBtnNextClick := nil;
    end;
  end;
end;

procedure TPageManager.SyncBtnPriorClick(Sync: Boolean);
begin
  if Assigned(FPriorBtn) and not (csDesigning in ComponentState) then
  begin
    if Sync then
    begin
      FSaveBtnPriorClick := FPriorBtn.OnClick;
      FPriorBtn.OnClick := @BtnClickPrior;
    end
    else
    begin
      FPriorBtn.OnClick := FSaveBtnPriorClick;
      FSaveBtnPriorClick := nil;
    end;
  end;
end;

function TPageManager.GetPriorPageIndex(Page: Integer): Integer;
begin
  Result:=Page;
  while Page > 0 do
  begin
    Dec(Page);
    if Assigned(FOnCheckPageEnabled) then
    begin
      if FOnCheckPageEnabled(Page) then
        break
      else
      if Page = 0 then
        exit;
    end
    else
      break;
  end;
  Result:=Page;
end;

function TPageManager.GetNextPageIndex(Page: Integer): Integer;
begin
  Result:=Page;
  if not Assigned(FPageOwner) then exit;
  while Page < FPageOwner.PageCount-1  do
  begin
    Inc(Page);
    if Assigned(FOnCheckPageEnabled) then
    begin
      if FOnCheckPageEnabled(Page) then
        break
      else
      if Page = FPageOwner.PageCount then
        exit;
    end
    else
      break;
  end;
  Result:=Page;
end;

procedure TPageManager.PageChanged;
var
  S:string;
begin
  if Assigned(OnPageChanged) then
    OnPageChanged(Self);
  if FStatusMessage <> '' then
  begin
    S:=Format(FStatusMessage, [PageIndex+1, PageCount]);
    if (pmoSetFormCaption in Options) and Assigned(Owner) and (Owner is TCustomForm) then
      TCustomForm(Owner).Caption:=S;
    if (pmoSetInfoControl in Options) and Assigned(FStatusControl) then
      FStatusControl.Caption:=S;
  end;
  CheckBtnEnabled;
end;

procedure TPageManager.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
    SyncBtnNextClick(true);
    SyncBtnPriorClick(true);
    PageChanged;
  end;
end;

constructor TPageManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatusMessage:=DefStatusMessage;
end;

procedure TPageManager.CheckBtnEnabled;
var
  P:integer;
begin
  P:=PageIndex;
  FNextBtn.Enabled:=GetNextPageIndex(P)>P;
  FPriorBtn.Enabled:=GetPriorPageIndex(P)<P;
end;

procedure TPageManager.NextPage;
begin
  BtnClickNext(nil);
end;

procedure TPageManager.PriorPage;
begin
  BtnClickPrior(nil);
end;

end.
