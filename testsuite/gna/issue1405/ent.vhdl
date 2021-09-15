library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
end entity ent;

architecture beh of ent is
  type t_slv_array is array (natural range <>) of std_logic_vector;
  function test
    return t_slv_array is
    variable v_num_bytes   : integer := 2;
    variable v_byte_array  : t_slv_array(0 to v_num_bytes-1)(7 downto 0);
  begin
    return v_byte_array(0 to v_num_bytes-1);
  end function;
begin
end architecture beh;
