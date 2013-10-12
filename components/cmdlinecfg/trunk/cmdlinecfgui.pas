unit cmdlinecfgui;

interface

uses
  Classes, SysUtils, contnrs, cmdlinecfg;

type
  { TCmdLineLayoutInfo }

  // Section names are assumed to be . separated.
  // Anything after to . is expected to be a "sub section" of the section.

  { TLayoutSection }

  TLayoutElementType = (letSwitch, letSection);
  TLayoutElementTypes = set of TLayoutElementType;

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
    constructor Create(const AName: string = ''; AElementType: TLayoutElementType = letSection);
    destructor Destroy; override;
    property Name: string read fName;
    property ElementType: TLayoutElementType read fElementType;
  end;

  TCmdLineLayoutInfo = class(TObject)
  public
    RootElement : TLayoutSection;
    constructor Create;
    destructor Destroy; override;
  end;

  { TCmdLineUIControl }

  TCmdLineUIControl = class(TObject)
  private
    FValueChanged: TNotifyEvent;
  protected
    procedure ValueChanged; virtual;
  public
    procedure Init(cfg: TCmdLineCfg; layout: TCmdLineLayoutInfo; const ASection : string = ''); virtual; abstract;
    procedure SetValues(list: TList {of TCmdLineOptionValue}); virtual; abstract;
    procedure Serialize(list: TList {of TCmdLineOptionValue}); virtual; abstract;
    property OnValueChanged: TNotifyEvent read FValueChanged write fValueChanged;
  end;

function LayoutFindElement(aparent: TLayoutSection; const Name: string; LookFor: TLayoutElementTypes = [letSection] ): TLayoutSection;
procedure LayoutEnumElement(aparent: TLayoutSection; list: TList; LookFor: TLayoutElementTypes = [letSection] );
procedure LayoutGetUnused(cmd: TCmdLineCfg; layout: TLayoutSection; list: TList);

implementation

procedure LayoutGetSwitches(root: TLayoutSection; hash: TFPHashObjectList);
var
  sct : TList;
  i   : Integer;
  j   : Integer;
  el  : TLayoutSection;
  sel : TLayoutSection;
begin
  sct:=TList.Create;
  try
    sct.Add(root);
    j:=0;
    while j<sct.Count do begin
      el:=TLayoutSection(sct[j]);
      for i:=0 to el.ElemCount-1 do begin
        sel:=el.Elements[i];
        if sel.ElementType = letSection then
          sct.Add(sel)
        else begin
          hash.Add(sel.Name, sel);
        end;
      end;
      inc(j);
    end;
  finally
    sct.Free;
  end;
end;

procedure LayoutEnumElement(aparent: TLayoutSection; list: TList;
  LookFor: TLayoutElementTypes);
var
  i : integer;
begin
  if not Assigned(list) or not Assigned(aparent) or (LookFor = []) then Exit;
  for i:=0 to aparent.ElemCount-1 do begin
    if aparent.Elements[i].ElementType in LookFor then
      list.Add(aparent.Elements[i]);
  end;
end;

procedure LayoutGetUnused(cmd: TCmdLineCfg; layout: TLayoutSection; list: TList);
var
  i     : Integer;
  hash  : TFPHashObjectList;
  opt   : TCmdLineCfgOption;
begin
  if not Assigned(cmd) or not Assigned(layout) or not Assigned(list) then Exit;

  hash := TFPHashObjectList.Create(false);
  try
    LayoutGetSwitches(layout, hash);
    for i:=0 to cmd.Options.Count-1 do begin
      opt:=TCmdLineCfgOption(cmd.Options[i]);
      if not Assigned(hash.Find(opt.Name)) then begin
        list.Add(opt);
      end;
    end;
  finally
    hash.Free;
  end;
end;

function LayoutFindElement(aparent: TLayoutSection; const Name: string; LookFor: TLayoutElementTypes = [letSection]): TLayoutSection;
var
  i : integer;
  nm : string;
begin
  Result:=nil;
  if not Assigned(aparent) or (LookFor = []) then Exit;
  nm:=AnsiLowerCase(Name);
  for i:=0 to aparent.ElemCount-1 do
    if (aparent.Elements[i].fElementType in LookFor) and (AnsiLowerCase(aparent.Elements[i].Name)=nm) then
      Result:=aparent.Elements[i];
end;

{ TLayoutSection }

function TLayoutSection.AddElement(const AName: string; AElementType: TLayoutElementType): TLayoutSection;
begin
  if ElemCount = length(Elements) then begin
    if ElemCount=0 then SetLength(Elements, 2)
    else SetLength(Elements, ElemCount*2);
  end;
  Result:=TLayoutSection.Create(AName, AElementType);
  Result.Display:=Aname;
  Elements[ElemCount]:=Result;
  inc(ElemCount);
end;

constructor TLayoutSection.Create(const AName: string;
  AElementType: TLayoutElementType);
begin
  inherited Create;
  fName:=AName;
  fElementType:=AElementType;
end;

destructor TLayoutSection.Destroy;
var
  i : integer;
begin
  for i:=0 to ElemCount-1 do Elements[i].Free;
  inherited Destroy;
end;

{ TCmdLineLayoutInfo }
{
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
 }
constructor TCmdLineLayoutInfo.Create;
begin
  RootElement:=TLayoutSection.Create;
  RootElement.fName:='';
  RootElement.fElementType:=letSection;
end;

destructor TCmdLineLayoutInfo.Destroy;
begin
  RootElement.Free;
  inherited Destroy;
end;

{procedure TCmdLineLayoutInfo.AddSwitch(const Section: string;
  const SwitchOrName: string);
begin
  GetSection(Section).fswitches.Add(SwitchOrName);
end;}
{
function TCmdLineLayoutInfo.AddSection(const Section: string): TLayoutSection;
begin
  Result:=DoGetSection(Section, true);
end;

function TCmdLineLayoutInfo.GetSection(const Section: string): TLayoutSection;
begin
  Result:=DoGetSection(Section, false);
end;
}
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
