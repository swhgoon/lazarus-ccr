procedure add_examples;
begin
  with Installer do
    {$include examples/fpmake.pp}
end;

procedure add_src;
begin
  with Installer do
    {$include src/fpmake.pp}
end;

procedure add_utils;
begin
  with Installer do
    {$include utils/fpmake.pp}
end;

