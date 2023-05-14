
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

package my_pkg is
    type T_Stream_M2S is record
        Valid : std_logic;
        Data  : std_logic_vector;
    end record;

    type T_Stream_M2S_VECTOR is array(natural range <>) of T_Stream_M2S;

end package;

library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

use     work.my_pkg.all;

entity my_entity is
  generic (DATA_BITS : natural := 8);
end entity;

architecture rtl of my_entity is
		signal StmMux_In_M2S     : T_Stream_M2S_Vector(0 to 1)(Data(DATA_BITS -1 downto 0));
		signal StmMux_In_M2S_Elem1      : StmMux_In_M2S'element;    --Works
		signal StmMux_In_M2S_Elem2      : StmMux_In_M2S(0)'subtype; --Broken
begin

end architecture;
