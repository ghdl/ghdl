entity tb_top is
end tb_top;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_top is
  signal ch : integer range 0 to 7;
  signal din : unsigned(7 downto 0);
  signal dout : unsigned(7 downto 0);
begin
  dut: entity work.top
    port map (ch, din, dout);

  process
  begin
    report "test shift by 0 + 1";
    
    ch <= 0;
    din <= x"e7";
    wait for 1 ns;
    assert dout = x"73" severity failure;

    report "test shift by 3 + 1";
    
    ch <= 3;
    din <= x"7e";
    wait for 1 ns;
    assert dout = x"07" severity failure;

    report "test shift by 7 + 1";
    
    ch <= 7;
    din <= x"9b";
    wait for 1 ns;
    assert dout = x"00" severity failure;

    wait;
  end process;
end behav;
