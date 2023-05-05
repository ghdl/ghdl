
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

use     work.my_pkg.all;

entity my_ent is
	port(
		In_Header : in std_logic_vector
	);
begin
end entity;

architecture rtl of my_ent is
	constant HEADER_BYTES   : positive := In_Header'length / 8;
	signal In_Header_intern   : t_slvv_8 (HEADER_BYTES - 1 downto 0) := (others => (others => 'Z'));
	
begin
	assert false report"my_ent:: In_Header'left=" & integer'image(In_Header'left) & ", In_Header'right=" & integer'image(In_Header'right) severity note;


	In_Header_intern <= to_slvv_8(In_Header);

end architecture;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use     work.my_pkg.all;

entity my_tb is
end entity;

architecture Harness of my_tb is
	constant Meta_Constant : T_SLVV_8 (7 downto 0) := (0 => x"01", 7 => x"77", others => x"11");
	constant Meta_Constant_slv : std_logic_vector := to_slv(Meta_Constant);
begin


	process
	begin
		wait;
	end process;
	
		assert false report "Meta_Constant'left=" & integer'image(Meta_Constant'left) & ", Meta_Constant'right=" & integer'image(Meta_Constant'right) severity note;
		assert false report "Meta_Constant'element'left=" & integer'image(Meta_Constant'element'left) & ", Meta_Constant'element'right=" & integer'image(Meta_Constant'element'right) severity note;
		assert false report "Meta_Constant_slv'left=" & integer'image(Meta_Constant_slv'left) & ", Meta_Constant_slv'right=" & integer'image(Meta_Constant_slv'right) severity note;
		
	
	DUT : entity work.my_ent
	port map(
		In_Header => to_slv(Meta_Constant) --This creates the error
		-- In_Header => Meta_Constant_slv     --This works
	);
end architecture;
