library ieee;
use ieee.std_logic_1164.all;

entity tb is
end tb;

architecture behav of tb is
  constant depth : natural := 2;
  signal clk : std_logic := '0';
  signal ni, no : natural;
  signal eos : boolean := false;
begin
  dut: entity work.delay
    generic map (
      delay_type => natural,
      delay_depth => depth)
    port map (
      clk => clk,
      ce => '1',
      depth => depth,
      d => ni,
      q => no
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
    for i in 1 to depth loop
      ni <= i;
      wait until rising_edge(clk);
    end loop;

    for i in depth + 1 to 10 loop
      ni <= i;
      wait until rising_edge(clk);
      assert no = i - depth severity failure;
    end loop;

    eos <= True;
    wait;
  end process;
end behav;
