library ieee;
use ieee.std_logic_1164.all;

package ghdl_bug is

type uncon_rec_t is record
	slv : std_logic_vector;
end record;
type uncon_rec_arr_t is array (0 to 1) of uncon_rec_t;

end ghdl_bug;
