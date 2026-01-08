entity tb_rom02 is
end tb_rom02;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_rom02 is
  signal addr, paddr : std_logic_vector(3 downto 0);
  signal dat : std_logic;
begin
  dut: entity work.rom02
    port map (addr => addr, paddr => paddr, dat => dat);

  process
    procedure check_digit(ref : std_logic_vector(0 to 14)) is
    begin
      for i in ref'range loop
        paddr <= std_logic_vector(to_unsigned(i, 4));
        wait for 1 ns;
        assert dat = ref (i) severity failure;
      end loop;
    end check_digit;
  begin
    addr <= "0000";
    check_digit (b"111_101_101_101_111");
    
    addr <= "0010";
    check_digit(b"111_001_111_100_111");

    addr <= "0111";
    check_digit(b"111_001_001_001_001");

    wait;
  end process;
end behav;
