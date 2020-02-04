entity procinter is
end;

architecture arch of procinter is
  procedure proc (procedure proc2 (v : natural)) is
  begin
    null;
  end proc;
begin
end arch;
