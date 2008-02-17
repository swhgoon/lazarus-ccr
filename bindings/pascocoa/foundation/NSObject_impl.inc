
constructor NSObject.Create;
begin

end;

destructor NSObject.Destroy;
begin
  objc_msgSend(Handle, sel_registerName(PChar(Str_release)), []);
end;

