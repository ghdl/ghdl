library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sipo is
	port (clk       : in std_logic;
              in_data   : in std_logic;
              out_data  : out std_logic_vector(1 downto 0));
end entity;

architecture rtl of sipo is
	signal mem  : std_logic_vector(1 downto 0);
	signal ctr  : unsigned(0 downto 0);
begin
	process (clk) begin
		if rising_edge(clk) then
--			This indirection doesn't synthesize correctly.
			mem(to_integer(ctr)) <= in_data;

			ctr(0) <= not ctr(0);
		end if;
	end process;

	out_data <= mem;
end architecture rtl;
