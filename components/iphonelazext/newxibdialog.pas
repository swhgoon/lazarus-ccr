unit newXibDialog;

{$mode objfpc}{$H+}

interface

uses
  MacOSAll,
  Types,Classes,SysUtils,FileUtil,LResources,Forms,Controls,Graphics,Dialogs,StdCtrls,LCLProc,
  Grids,
  //todo: use LCL file loading and drawing, instead of OSX
  CarbonGDIObjects, CarbonProc;

type

  { TnewXibForm }

  TnewXibForm = class(TForm)
    Button1:TButton;
    Button2:TButton;
    DrawGrid1:TDrawGrid;
    Edit1:TEdit;
    Label1:TLabel;
    Memo1:TMemo;
    procedure Button2Click(Sender:TObject);
    procedure DrawGrid1DrawCell(Sender:TObject;aCol,aRow:Integer;aRect:TRect;
      aState:TGridDrawState);
    procedure DrawGrid1SelectCell(Sender:TObject;aCol,aRow:Integer;var CanSelect
      :Boolean);
    procedure DrawGrid1Selection(Sender:TObject;aCol,aRow:Integer);
    procedure Edit1KeyPress(Sender:TObject;var Key:char);
    procedure FormCreate(Sender:TObject);
    procedure FormDestroy(Sender:TObject);
  private
    { private declarations }
    SelectedXib : String;
    Items       : TList;
    CustomName  : Boolean;
  public
    { public declarations }
    procedure AddTemplate(const AName, SourceXibFile, ADescr, IconFile: AnsiString);
    function Execute(var FileName, SourceXibFile: AnsiString): Boolean;
  end;

var
  newXibForm: TnewXibForm;

implementation

{$R *.lfm}

type

  { TXibItem }

  TXibItem = class(TObject)
    image       : TBitmap;
    sourcefile  : AnsiString;
    descr       : AnsiString;
    name        : AnsiString;
    constructor Create(const AName, ASourceFile, ADescr, IconFileName: AnsiString);
    destructor Destroy; override;
  end;

{ TXibItem }

constructor TXibItem.Create(const AName,ASourceFile,ADescr, IconFileName:AnsiString);
var
  url   : CFURLRef;
  data  : CGImageSourceRef;
  cf    : CFStringRef;
  img   : CGImageRef;
  ctx   : CGContextRef;
  space : CGColorSpaceRef;
  r     : CGRect;
  w :  WideString;
begin
  inherited Create;

  w:=UTF8Decode(AName);
  if w<>'' then w[1]:=WideUpperCase(w[1])[1];
  name:=UTF8Encode(w);

  sourcefile:=ASourceFile;
  descr:=ADescr;

  if IconFileName<>'' then begin
    CreateCFString(IconFileName, cf);
    url:=CFURLCreateWithFileSystemPathRelativeToBase(kCFAllocatorDefault , cf,
      kCFURLPOSIXPathStyle, false, nil);
    data:=CGImageSourceCreateWithURL(url, nil);
    img:=CGImageSourceCreateImageAtIndex(data, 0, nil);

    image:=TBitmap.Create;
    image.PixelFormat:=pf32bit;
    image.SetSize( 64, 64);
    image.BeginUpdate;
    space:=CGColorSpaceCreateDeviceRGB;
    ctx:=CGBitmapContextCreate(image.RawImage.Data, image.Width, image.Height, 8,
      image.Width*4, space, kCGImageAlphaPremultipliedFirst);

    r.origin.x:=0; r.origin.y:=0;
    r.size.width:=image.Width; r.size.height:=image.Height;
    CGColorSpaceRelease(space);
    CGContextDrawImage(ctx, r, img);
    CGContextRelease(ctx);
    image.EndUpdate;
    CFRelease(img);
    CFRelease(data);
    CFRelease(url);
  end;
end;

destructor TXibItem.Destroy;
begin
  image.Free;
  inherited Destroy;
end;

{ TnewXibForm }

procedure TnewXibForm.FormCreate(Sender:TObject);
begin
  Items:=TList.Create;
end;

procedure TnewXibForm.DrawGrid1DrawCell(Sender:TObject;aCol,aRow:Integer;aRect:
  TRect;aState:TGridDrawState);
var
  info  : TXibItem;
  sz    : TSize;
  x,y   : Integer;
  idx : integer;
begin
  idx:=DrawGrid1.ColCount*aRow+aCol;
  if (idx>=0) and (idx<Items.Count) then
    info:=TXibItem(Items[idx])
  else
    info:=nil;
  if not Assigned(info) then Exit;

  if gdSelected in aState then begin
    DrawGrid1.Canvas.Brush.Color:=clHighlight;
    DrawGrid1.Canvas.Font.Color:=clHighlightText;
  end else begin
    DrawGrid1.Canvas.Brush.Color:=clWindow;
    DrawGrid1.Canvas.Font.Color:=clWindowText;
  end;

  DrawGrid1.Canvas.FillRect(aRect);

  if Assigned(info.image) then
    DrawGrid1.Canvas.Draw(aRect.Left+4, aRect.Top, info.image);

  sz:=DrawGrid1.Canvas.TextExtent(info.name);
  x:=aRect.Left+ ((aRect.Right-aRect.Left) - sz.cx) div 2;
  y:=aRect.Top + info.image.Height+4;
  DrawGrid1.Canvas.TextOut(x, y, info.name);
end;

procedure TnewXibForm.Button2Click(Sender:TObject);
begin

end;

procedure TnewXibForm.DrawGrid1SelectCell(Sender:TObject;aCol,aRow:Integer;var
  CanSelect:Boolean);
var
  idx : integer;
begin
  idx:=DrawGrid1.ColCount*aRow+aCol;
  CanSelect:=(idx>=0) and (idx<Items.Count);
  if CanSelect then begin
    Memo1.Text:=TXibItem(Items[idx]).descr;
    if not CustomName then
      Edit1.Text:=TXibItem(Items[idx]).name;
    SelectedXib:=TXibItem(Items[idx]).sourcefile;
  end else begin
    Memo1.Text:='';
    SelectedXib:='';
  end;
  Button2.Enabled:=CanSelect;
end;

procedure TnewXibForm.DrawGrid1Selection(Sender:TObject;aCol,aRow:Integer);
begin

end;

procedure TnewXibForm.Edit1KeyPress(Sender:TObject;var Key:char);
begin
  CustomName:=True;
end;

procedure TnewXibForm.FormDestroy(Sender:TObject);
var
  i : Integer;
begin
  for i:=0 to Items.Count-1 do TObject(Items[i]).Free;
  Items.Free;
end;

procedure TnewXibForm.AddTemplate(const AName,SourceXibFile,ADescr, IconFile:AnsiString);
begin
  Items.Add( TXibItem.Create(AName, SourceXibFile, ADescr, IconFile));
end;

function TnewXibForm.Execute(var FileName,SourceXibFile:AnsiString):Boolean;
var
  w,h : integer;
begin
  if Items.Count=0 then begin
    Result:=False;
    Exit;
  end;
  w:=DrawGrid1.ClientWidth div DrawGrid1.DefaultColWidth;
  if w=0 then w:=1;
  DrawGrid1.ColCount:=w;
  h:=Items.Count div w;
  if h=0 then h:=1;
  DrawGrid1.RowCount:=h;
  CustomName:=False;
  Result:=ShowModal = mrOK;
  if Result then begin
    FileName:=Edit1.Text;
    SourceXibFile:=SelectedXib;
  end;
end;

end.

