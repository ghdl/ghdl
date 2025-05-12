library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom_log is
  port (clk    : in  std_logic;
        rddata : out std_logic_vector(7 downto 0));
end rom_log;

architecture synth of rom_log is
	constant MEM_SIZE : natural := 8;

	type rom_type is array (0 to MEM_SIZE-1) of std_logic_vector(7 downto 0);

	constant rom : rom_type := (
          x"00", x"01", x"02", x"03",
          x"04", "XXXX0101", "XXXX0110", "XXXX0111"
	);

	signal read_index : integer range 0 to MEM_SIZE - 1 := 0;
begin
	rddata <= rom(read_index);

	process (CLK)
	begin
          if rising_edge(CLK) then
            read_index <= read_index + 1;
          end if;
	end process;
end architecture;
