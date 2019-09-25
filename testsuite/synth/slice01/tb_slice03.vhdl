entity tb_slice03 is
end tb_slice03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_slice03 is
  signal di : std_logic_vector (7 downto 0);
  signal do : std_logic_vector (3 downto 0);
begin
  dut: entity work.slice03
    port map (di, do);

  process
  begin
    di <= x"12";
    wait for 1 ns;
    assert do = x"1" severity error;
    
    di <= x"e5";
    wait for 1 ns;
    assert do = x"e" severity error;
    
    wait;
  end process;
end behav;
