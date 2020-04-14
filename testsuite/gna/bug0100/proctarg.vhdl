entity proctarg is
end proctarg;

architecture behav of proctarg is
  procedure proc (n : natural) is
  begin
    proc := true;
  end proc;

  procedure proc (n : boolean) is
  begin
    proc <= true;
  end proc;
begin
end;
