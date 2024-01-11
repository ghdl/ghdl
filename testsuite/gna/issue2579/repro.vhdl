library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
end;

architecture behav of repro is
  type    t_slv_array   is array (natural range <>) of std_logic_vector;
  type t_data is record
    val_1    : t_slv_array(0 to 4)(7 downto 0);
    val_2    : t_slv_array(0 to 0)(7 downto 0);
    val_3    : t_slv_array(0 to 3)(7 downto 0);
  end record;
 constant C_DEFAULT : t_data := (
    others => (others => x"FF")
    );
begin
end behav;
