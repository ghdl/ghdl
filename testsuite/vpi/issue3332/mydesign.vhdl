library ieee;
use ieee.std_logic_1164.all;

package mypkg is
  -- Resolved element, unconstrained at the type-declaration site (no
  -- range on std_logic_vector): the element RTI kind is
  -- Ghdl_Rtik_Subtype_Unbounded_Array, which used to make
  -- vpi_handle_by_index silently return NULL (no crash, no diagnostic).
  type t_slv_vector is array (natural range <>) of std_logic_vector;
end package mypkg;

library ieee;
use ieee.std_logic_1164.all;
use work.mypkg.all;

entity myentity is
  port (
    sig_o : out t_slv_vector(0 to 1)(4 downto 0) := ("00101", "10101")
  );
end entity myentity;

architecture arch of myentity is
begin
end architecture arch;
