entity topa is
end topa;

architecture behav of topa is
  signal clk : bit;
  signal v : bit_vector (31 downto 0);
  signal done : boolean := false;
begin
  dut : entity work.enta
    port map (clk => clk,
              data => v);

  process
  begin
    clk <= '0';
    wait for 10 ns;
    clk <= '1';
    wait for 10 ns;
    if done then
      wait;
    end if;
  end process;

  process
  begin
    v <= x"12345678";
    wait for 40 ns;
    v <= x"00000000";
    wait for 80 ns;
    done <= true;
    wait;
  end process;
end behav;
