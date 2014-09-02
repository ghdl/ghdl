entity repro is
  
end repro;

architecture behav of repro is

begin  -- behav

  process
    variable v : integer := 523;
    variable a : integer := 2;
  begin
    assert false report integer'image(v)(a);
    wait;
  end process;
end behav;
