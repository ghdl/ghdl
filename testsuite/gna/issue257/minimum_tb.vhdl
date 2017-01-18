entity minimum_tb is
end minimum_tb;

architecture tb of minimum_tb is
begin
  process
    type natural_vector is array(natural range<>) of natural;
    constant A : natural_vector(1 downto 0) := (4, 6);
  begin
    -- Using the two-argument MINIMUM function -> analyzes, elaborates and runs
    report "MIN(a,b): "&integer'image(MINIMUM(a(1), a(0)));

    -- Using a MINIMUM over an array argument -> analyzes but crashes elaboration
    report "MIN(arr): "&integer'image(MINIMUM(a));
  
    wait;  -- forever
  end process;
end tb;
