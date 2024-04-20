library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ex2 is
end entity;

architecture rtl of ex2 is
begin

  p_main : process
    variable v_exponent : integer := 31;
    variable v_integer  : integer;
  begin
    report "Using to_string:" & to_string(integer'right);
    report "Using 'image:" & integer'image(integer'right);
    v_integer := integer'right;

    report "Using 'image:" & integer'image(2 ** v_exponent - 1);

    wait;
  end process p_main;

end architecture rtl;
