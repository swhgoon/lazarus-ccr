unit appdelegate_iphoneu;

{$modeswitch ObjectiveC1}

interface

uses
  Classes,
  BufDataset,
  iPhoneAll,
  SysUtils,
  TableData;

type
  TTableViewDelegate = objcclass;

  { TAppDelegate_iPhone }

  TAppDelegate_iPhone = objcclass(NSObject, UIApplicationDelegateProtocol)
    UISearchBar1: UISearchBar;
    UITableView1: UITableView;
    UIWindow1: UIWindow;
    procedure applicationDidFinishLaunching(application: UIApplication); message 'applicationDidFinishLaunching:';
  private
    objectListTableViewDelegate: TTableViewDelegate;
  public
    procedure dealloc; override;
  end;

  { TTableViewDelegate }

  TTableViewDelegate = objcclass(NSObject, UISearchBarDelegateProtocol, UITableViewDataSourceProtocol)
  private
    FDataStrings: TStrings;
    FData: TBufDataset;
    FTableView: UITableView;
    procedure DatasetToStringlist; message 'DatasetToStringlist';
  public
    function initWithData(ATableView: UITableView): TTableViewDelegate; message 'initWithData:';
    function tableView_numberOfRowsInSection(tableView: UITableView; section: NSInteger): NSInteger; message 'tableView:numberOfRowsInSection:';
    function tableView_cellForRowAtIndexPath(tableView: UITableView; indexPath: NSIndexPath): UITableViewCell; message 'tableView:cellForRowAtIndexPath:';
    procedure searchBarSearchButtonClicked(searchBar: UISearchBar); message 'searchBarSearchButtonClicked:';
    procedure dealloc; override;
  end;

implementation

{ TTableViewDelegate }

procedure TTableViewDelegate.DatasetToStringlist;
begin
  FDataStrings.Clear;
  FData.First;
  while not FData.EOF do
    begin
    FDataStrings.Append(FData.FieldByName('name').asstring);
    FData.Next;
    end;
end;

function TTableViewDelegate.initWithData(ATableView: UITableView
  ): TTableViewDelegate;
var
  AnAlertView: UIAlertView;
begin
  result := init;
  FData:= CreateDataset;
  FDataStrings := TStringList.Create;
  FTableView := ATableView;
  DatasetToStringlist;
end;

function TTableViewDelegate.tableView_numberOfRowsInSection(
  tableView: UITableView; section: NSInteger): NSInteger;
begin
  result := FDataStrings.Count;
end;

function TTableViewDelegate.tableView_cellForRowAtIndexPath(
  tableView: UITableView; indexPath: NSIndexPath): UITableViewCell;
var
  s: nsstring;
begin
  result := tableview.dequeueReusableCellWithIdentifier(NSSTR('DefTableItem'));
  if not assigned(result) then
    result := UITableViewCell.alloc.initWithStyle_reuseIdentifier(UITableViewStylePlain,NSSTR('DefTableItem'));
  s := NSString.alloc.initWithUTF8String(pchar(FDataStrings[indexPath.row]));
  result.textLabel.setText(s);
end;

procedure TTableViewDelegate.searchBarSearchButtonClicked(searchBar: UISearchBar);
var
  AnAlertView: UIAlertView;
begin
  FData.Filter:='name="*'+searchBar.text.cString+'*"';
  FData.Filtered:=true;
  DatasetToStringlist;
  FTableView.reloadData;
end;

procedure TTableViewDelegate.dealloc;
begin
  FData.Free;
end;

procedure TAppDelegate_iPhone.applicationDidFinishLaunching(
  application: UIApplication);
begin
  objectListTableViewDelegate:=TTableViewDelegate.alloc.initWithData(UITableView1);
  UITableView1.setDataSource(objectListTableViewDelegate);
  UISearchBar1.setDelegate(objectListTableViewDelegate);
end;

procedure TAppDelegate_iPhone.dealloc;
begin
  objectListTableViewDelegate.release;
  UISearchBar1.release;
  UITableView1.release;
  UIWindow1.release;
  inherited dealloc;
end;

{$FakeResource *.xib}

end.

