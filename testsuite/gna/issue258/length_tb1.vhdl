entity length_tb1 is
end length_tb1;

architecture tb of length_tb1 is
begin
  process
    variable s : string (1 to 4);
    constant c : string := "hello";
  begin

    -- Behaves like 32-bit arithmetic with modular truncation
    s := c;

    wait; -- forever
  end process;
end tb;
