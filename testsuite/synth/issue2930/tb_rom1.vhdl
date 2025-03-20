entity tb_rom1 is
end tb_rom1;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_pkg.all;

architecture behav of tb_rom1 is
  signal addr : std_logic_vector(3 downto 0);
  signal dat : std_logic_vector(31 downto 0);
begin
  dut: entity work.rom1
    port map (addr, dat);

  process
  begin
    for i in mem1'range loop
      addr <= std_logic_vector(to_unsigned(i, 4));
      wait for 1 ns;
      assert dat = mem1(i) severity failure;
    end loop;

    wait;
  end process;
end behav;
