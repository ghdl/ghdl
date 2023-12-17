library ieee;
use ieee.std_logic_1164.all;

entity tb is
end tb;

architecture behav of tb is
  constant delay : natural := 2;
  signal clk : std_logic := '0';
  signal ni, no : natural;
  signal eos : boolean := false;
begin
  dut: entity work.generic_fixed_delay
    generic map (
      num_delay_cycle => delay,
      data_type_t => natural
      )
    port map (
      clk => clk,
      input => ni,
      output => no
      );

  process
  begin
    clk <= not clk;
    if eos then
      wait;
    else
      wait for 5 ns;
    end if;
  end process;
  
  process
  begin
    for i in 1 to delay loop
      ni <= i;
      wait for rising_edge(clk);
    end loop;

    for i in delay + 1 to 10 loop
      ni <= i;
      wait for rising_edge(clk);
      assert no = i - delay;
    end loop;
    wait;
  end process;
end behav;
