library ieee;
use ieee.std_logic_1164.all;

package mypkg is
  -- Unresolved element, unconstrained at the type-declaration site (no
  -- range on std_ulogic_vector): Add_Index's element RTI kind is
  -- Ghdl_Rtik_Type_Array, which used to hit "internal error: add_index(2)".
  type t_sulv_vector is array (natural range <>) of std_ulogic_vector;
end package mypkg;

library ieee;
use ieee.std_logic_1164.all;
use work.mypkg.all;

entity myentity is
  port (
    sig_i : in t_sulv_vector(0 to 1)(4 downto 0) := ("00011", "01001")
  );
end entity myentity;

architecture arch of myentity is
begin
end architecture arch;
