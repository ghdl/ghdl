entity tb_alias01 is
end tb_alias01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_alias01 is
  signal i : std_logic_vector(7 downto 0);
  signal o : std_logic;
begin
  dut: entity work.alias01
    port map (i, o);

  process
  begin
    i <= "11000011";
    wait for 1 ns;
    assert o = '1' severity failure;

    i <= "11100011";
    wait for 1 ns;
    assert o = '0' severity failure;

    wait;
  end process;
end behav;
