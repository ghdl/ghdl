library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;


entity my_entity is
end entity;

architecture rtl of my_entity is
begin
	assert false report "Dummy." severity Note;

end architecture;
