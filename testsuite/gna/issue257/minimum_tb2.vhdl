entity minimum_tb2 is
end minimum_tb2;

architecture tb of minimum_tb2 is
begin
  process
    type natural_vector is array(natural range<>) of natural;
    constant A : natural_vector(1 downto 0) := (4, 6);
    variable b : natural_vector (0 to 1);
  begin
    -- Using the two-argument MINIMUM function -> analyzes, elaborates and runs
    report "MIN(a,b): "&integer'image(MINIMUM(a(1), a(0)));

    -- Using a MINIMUM over an array argument -> analyzes but crashes elaboration
    report "MIN(arr): "&integer'image(MINIMUM(a));

    b := a;
    report "MIN(arr): "&integer'image(MINIMUM(b));
  
    wait;  -- forever
  end process;
end tb;
