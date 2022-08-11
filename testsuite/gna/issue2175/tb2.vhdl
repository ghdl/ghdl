library ieee;
use ieee.std_logic_1164.all;

entity tb2 is
end;

architecture behav of tb2 is
  type sulv_vector is array (natural range <>) of std_ulogic_vector;

  function repeat(
    val : std_ulogic_vector;
    m   : natural
  ) return sulv_vector
  is
    constant result : sulv_vector(m downto 1)(val'range) := (others => val);
  begin
    return result;
  end function;
begin
  process
    constant c : sulv_vector := repeat ("0101", 2);
  begin
    assert c(2) = x"5";
    assert c(1) = "0101";
    wait;
  end process;
end behav;
