entity range_tb1 is
end range_tb1;

architecture tb of range_tb1 is
begin
  process
    variable i : integer;
  begin

    -- Behaves like 32-bit arithmetic with modular truncation
    i := integer'low;
    report integer'image(i);
    i := integer'low-1;
    report integer'image(i);

    wait; -- forever
  end process;
end tb;
