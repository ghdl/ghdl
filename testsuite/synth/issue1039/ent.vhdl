library ieee;
use ieee.std_logic_1164.all,
    ieee.numeric_std.all;

entity ent is
	port (
		clk     : in std_logic;
		addr    : in std_logic_vector(2 downto 0);
		wr_data : in std_logic_vector(3 downto 0);
		rd_data : out std_logic_vector(3 downto 0)
	);
end ent;

architecture a of ent is
	type ram_type is array (0 to 7) of std_logic_vector(3 downto 0);
	signal ram : ram_type := (others => (others => '0'));
begin
	process(clk)
	begin
		if rising_edge(clk) then
			ram(to_integer(unsigned(addr))) <= wr_data;
			rd_data <= ram(to_integer(unsigned(addr)));
		end if;
	end process;
end;
