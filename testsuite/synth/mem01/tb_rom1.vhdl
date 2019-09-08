entity tb_rom1 is
end tb_rom1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_rom1 is
  signal addr : std_logic_vector(3 downto 0);
  signal dat : std_logic_vector(7 downto 0);
begin
  dut: entity work.rom1
    port map (addr, dat);

  process
  begin
    addr <= "0000";
    wait for 1 ns;
    assert dat = x"00" severity failure;
    
    addr <= "0101";
    wait for 1 ns;
    assert dat = x"41" severity failure;
    
    addr <= "1100";
    wait for 1 ns;
    assert dat = x"fc" severity failure;
    
    addr <= "1011";
    wait for 1 ns;
    assert dat = x"fb" severity failure;
    
    addr <= "0010";
    wait for 1 ns;
    assert dat = x"02" severity failure;
    
    wait;
  end process;
end behav;
