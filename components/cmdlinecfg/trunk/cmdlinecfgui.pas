unit cmdlinecfgui;

interface

uses
  Classes, SysUtils,
  cmdlinecfg;

type
  { TCmdLineLayoutInfo }

  // Section names are assumed to be . separated.
  // Anything after to . is expected to be a "sub section" of the section.

  { TLayoutSection }

  TLayoutElementType = (letSwitch, letSection);

  TLayoutSection = class(TObject)
    //level     : integer; // number of "dots" in the name
  public
    fName       : string;
    fElementType: TLayoutElementType;
  public
    Display     : string;
    GUIHint     : string;
    Elements    : array of TLayoutSection;
    ElemCount   : integer;
    function AddElement(const AName: string; AElementType: TLayoutElementType): TLayoutSection;
    destructor Destroy; override;
    property Name: string read fName;
    property ElementType: TLayoutElementType read fElementType;
  end;

  TCmdLineLayoutInfo = class(TObject)
  private
    fSections: TStringList;
    fValidOrder: Boolean;
    function DoGetSection(const SectName: String; Forced: Boolean = true): TLayoutSection;
  public
    constructor Create;
    destructor Destroy; override;
    function AddSection(const Section: string): TLayoutSection;
    function GetSection(const Section: string): TLayoutSection;
    //function GetSwitches(const Section: string; Dst: TStrings): Boolean;
  end;

  { TCmdLineUIControl }

  TCmdLineUIControl = class(TObject)
  private
    FValueChanged: TNotifyEvent;
  protected
    procedure ValueChanged; virtual;
  public
    procedure Init(cfg: TCmdLineCfg; layout: TCmdLineLayoutInfo); virtual; abstract;
    procedure SetValues(list: TList {of TCmdLineOptionValue}); virtual; abstract;
    procedure Serialize(list: TList {of TCmdLineOptionValue}); virtual; abstract;
    property OnValueChanged: TNotifyEvent read FValueChanged write fValueChanged;
  end;

implementation

{ TLayoutSection }

function TLayoutSection.AddElement(const AName: string; AElementType: TLayoutElementType): TLayoutSection;
begin
  if ElemCount = length(Elements) then begin
    if ElemCount=0 then SetLength(Elements, 2)
    else SetLength(Elements, ElemCount*2);
  end;
  Result:=TLayoutSection.Create;
  Result.fName:=AName;
  Result.fElementType:=AElementType;
  Result.Display:=Aname;
  Elements[ElemCount]:=Result;
  inc(ElemCount);
end;

destructor TLayoutSection.Destroy;
var
  i : integer;
begin
  for i:=0 to ElemCount-1 do Elements[i].Free;
  inherited Destroy;
end;

{ TCmdLineLayoutInfo }

function TCmdLineLayoutInfo.DoGetSection(const SectName: String; Forced: Boolean): TLayoutSection;
var
   i : integer;
begin
  i:=fSections.IndexOf(SectName);
  if (i<0) and Forced then begin
    Result:=TLayoutSection.Create;
    fSections.AddObject(SectName, Result);
    fValidOrder:=false; // a new section has been added, it might ruin the order
  end else if (i<0) and not Forced then begin
    Result:=nil;
  end else
    Result:=TLayoutSection(fSections.Objects[i]);
end;

constructor TCmdLineLayoutInfo.Create;
begin
  fSections:=TStringList.Create;
  fSections.OwnsObjects:=true;
  AddSection('');
end;

destructor TCmdLineLayoutInfo.Destroy;
begin
  fSections.Clear; // need to call clear explicitly, since FREE doesn't free objects (even if owned)
  fSections.Free;
  inherited Destroy;
end;

{procedure TCmdLineLayoutInfo.AddSwitch(const Section: string;
  const SwitchOrName: string);
begin
  GetSection(Section).fswitches.Add(SwitchOrName);
end;}

function TCmdLineLayoutInfo.AddSection(const Section: string): TLayoutSection;
begin
  Result:=DoGetSection(Section, true);
end;

function TCmdLineLayoutInfo.GetSection(const Section: string): TLayoutSection;
begin
  Result:=DoGetSection(Section, false);
end;

{function TCmdLineLayoutInfo.GetSections(Dst: TStrings): Boolean;
var
  i : Integer;
begin
  if not fValidOrder then begin
    SortSections;
    fValidOrder:=true;
  end;
  Dst.BeginUpdate;
  try
    for i:=0 to fSections.Count-1 do
      Dst.Add(fSections[i]);
  finally
    Dst.EndUpdate;
  end;
  Result:=True;
end;}

{function TCmdLineLayoutInfo.GetSwitches(const Section: string; Dst: TStrings): Boolean;
var
  sct : TLayoutSection;
begin
  sct:=GetSection(Section);
  Result:=Assigned(Sct);
  if not Result then Exit;
  Dst.AddStrings(sct.fswitches);
end;}

{ TCmdLineUIControl }

procedure TCmdLineUIControl.ValueChanged;
begin
  if Assigned(fValueChanged) then fValueChanged(Self);
end;

end.
