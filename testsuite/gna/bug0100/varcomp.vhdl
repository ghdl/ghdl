entity varcomp is
end;

architecture behav of varcomp is
  component abuf is
  end component;
begin
  process
  begin
    (v, y) := abuf;

    x := ;
  end process;
end behav;
