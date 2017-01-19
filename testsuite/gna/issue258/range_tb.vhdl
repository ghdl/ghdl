entity range_tb is
end range_tb;

architecture tb of range_tb is
begin
  process
    variable i : integer;
  begin

    -- Behaves like 32-bit arithmetic with modular truncation
    i := integer'low;
    report integer'image(i);
    i := integer'low-1;
    report integer'image(i);

    -- Produces the error "overflow detected"
    i := integer'low;
    report integer'image(i);
    i := i-1;
    report integer'image(i);

    wait; -- forever
  end process;
end tb;
