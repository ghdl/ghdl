entity tb_slice02 is
end tb_slice02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_slice02 is
  signal di : std_logic_vector (7 downto 0);
  signal do : std_logic_vector (3 downto 0);
begin
  dut: entity work.slice02
    port map (di, do);

  process
  begin
    di <= b"11_10_01_00";
    wait for 1 ns;
    assert do = b"10_11" severity error;

    wait;
  end process;
end behav;
