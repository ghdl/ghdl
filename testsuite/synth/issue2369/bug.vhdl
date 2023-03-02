entity bug is
  port (clk, x, y : in bit; z : out bit);
end entity bug;

architecture RTL of bug is
begin

process (clk)
begin
  if rising_edge(clk) then
    z <= x when y else unaffected;
  end if;
end process;

end architecture RTL;
