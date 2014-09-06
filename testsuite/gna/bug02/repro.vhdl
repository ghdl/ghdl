entity repro is
  
end repro;

architecture behav of repro is
  function inc (a : integer) return integer is
  begin
    return a + 1;
  end inc;

  function inc (a : time) return time is
  begin
    return a + 1 ns;
  end inc;

  procedure inc (a : inout integer) is
  begin
    a := inc (a);
  end inc;
begin  -- behav

  process
    variable a : integer := 2;
  begin
    inc (a);
    assert a = 3 report "bad value of a";
    wait;
  end process;
end behav;
