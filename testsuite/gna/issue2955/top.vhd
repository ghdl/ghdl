library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.types_pkg.all;

entity top is
end entity;

architecture a of top is
	signal s_in  : signed_vector(0 to 3)(7 downto 0);
	signal s_out : signed_vector(0 to 3)(7 downto 0);
begin
	u_dummy : entity work.dummy
		generic map (g_bit_width => 8, g_num_chnl => 4)
		port map (i_data => s_in, i_data2 => s_in, o_data => s_out);
	process
	begin
		s_in <= (others => (others => '0'));
		wait for 10 ns;
		std.env.finish;
	end process;
end architecture;
