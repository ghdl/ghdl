entity topb is
end topb;

architecture behav of topb is
  signal clk : bit;
  signal v : natural;
  signal done : boolean := false;
begin
  dut : entity work.entb
    port map (clk => clk,
              val => v);

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
    v <= 2;
    wait for 40 ns;
    v <= 4;
    wait for 80 ns;
    done <= true;
    wait;
  end process;
end behav;
