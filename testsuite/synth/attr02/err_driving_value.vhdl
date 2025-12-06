entity err_driving_value is
  port (i : bit; o1, o : out bit);
end;

architecture behav of err_driving_value is
begin
  process (i)
  begin
    o <= o1'driving_value;
    o1 <= not i;
  end process;
end;
