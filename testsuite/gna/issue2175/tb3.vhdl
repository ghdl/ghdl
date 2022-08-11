library ieee;
use ieee.std_logic_1164.all;

package pkg2 is

  type sulv_vector is array (natural range <>) of std_ulogic_vector;

  subtype NULL_RANGE is natural range 0 downto 1;
  constant NULL_SULV : std_ulogic_vector(NULL_RANGE) := (others => '0');
  constant NULL_SULV_VECTOR : sulv_vector(NULL_RANGE)(NULL_RANGE) := (others => NULL_SULV);

  function repeat(
    val : std_ulogic_vector;
    m   : natural
  ) return sulv_vector;
end package;

package body pkg2 is
  function repeat(
    val : std_ulogic_vector;
    m   : natural
  ) return sulv_vector
  is
    constant result : sulv_vector(m downto 1)(val'range) := (others => val);
  begin
    return result;
  end function;

end package body;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg2.all;

entity tb3 is
end;

architecture behav of tb3 is
begin
  process
    constant c : sulv_vector := repeat ("0101", 2);
  begin
    assert c(2) = x"5";
    assert c(1) = "0101";
    wait;
  end process;
end behav;
