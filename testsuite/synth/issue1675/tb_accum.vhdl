entity tb_accum is
end tb_accum;

library ieee;
use ieee.std_logic_1164.all;

use work.pkg.all;

architecture behav of tb_accum is
  signal clk : std_logic;
  signal bi : bus_rec_out_t;
  signal res : std_logic_vector(15 downto 0);
begin
  dut: entity work.accum
    port map (clk, bi, res);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    bi <= (dat => x"00", stb => '0', rst => '1');
    pulse;
    assert res = x"0000" severity failure;
    wait;
  end process;
end behav;
