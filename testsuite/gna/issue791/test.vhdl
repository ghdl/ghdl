entity test is
end test;

architecture behav of test is
  procedure proc is
  begin
    null;
  end proc;

  package pkg is new work.generic_pkg
                   generic map (proc);
begin
  process
  begin
    wait;
  end process;
end behav;
