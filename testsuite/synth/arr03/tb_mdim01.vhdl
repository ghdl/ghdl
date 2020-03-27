entity tb_mdim01 is
end tb_mdim01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_mdim01 is
  signal a : std_logic_vector (3 downto 0);
  signal b : std_logic_vector (3 downto 0);
  signal c : std_logic_vector (3 downto 0);
begin
  dut: entity work.mdim01
    port map (a, b, c);

  process
    constant av : std_logic_vector := b"1101";
    constant bv : std_logic_vector := b"0111";
    constant cv : std_logic_vector := b"0011";
    constant zv : std_logic_vector := b"0111";
  begin
    a <= "1111";
    b <= "0000";
    wait for 1 ns;
    assert c = "1100" severity failure;
    
    wait;
  end process;
end behav;
