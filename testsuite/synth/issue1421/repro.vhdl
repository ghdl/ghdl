entity repro is
  port (clk : bit;
        rst : bit;
        v : bit_vector (3 downto 0);
        res : out bit_vector(3 downto 0));
end;

architecture behav of repro is
begin
  process (clk)
  begin
    if clk'event and clk = '1' then
      res <= v;
      if rst = '1' then
        res <= "0000";
      end if;
    end if;
    res (2) <= '0';
  end process;
end behav;
      
