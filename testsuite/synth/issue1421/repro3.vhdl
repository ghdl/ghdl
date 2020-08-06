entity repro3 is
  port (clk : bit;
        v : bit_vector (1 downto 0);
        res : out bit_vector(1 downto 0));
end;

architecture behav of repro3 is
begin
  process (clk)
  begin
    if clk'event and clk = '1' then
      res <= v;
    end if;
    res (0) <= '0';
  end process;
end behav;
