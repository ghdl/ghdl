library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom_ub32 is
  port (clk    : in  std_logic;
        rddata : out std_logic_vector(7 downto 0));
end rom_ub32;

architecture synth of rom_ub32 is
	constant MEM_SIZE : natural := 4;

	type rom_type is array (0 to MEM_SIZE-1) of integer range 0 to 255;

	constant rom : rom_type := (
		0, 1, 2, 3
	);

	signal read_index : integer range 0 to MEM_SIZE - 1 := 0;
begin
	rddata <= std_logic_vector(to_unsigned(rom(read_index), 8));

	process (CLK)
	begin
          if rising_edge(CLK) then
            read_index <= read_index + 1;
          end if;
	end process;
end architecture;
