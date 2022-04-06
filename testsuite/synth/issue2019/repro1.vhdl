library ieee;
use ieee.std_logic_1164.all;

entity ent is
  port (p : out std_ulogic_vector(3 downto 0));
end ent;


architecture a of ent is
  type my_logic_vector is array (natural range <>) of std_ulogic;

  function to_stdlogic (value : std_ulogic) return std_ulogic is
  begin
    return '0';
  end function;

  function to_stdlogic (value : my_logic_vector) return std_ulogic_vector is
    variable tmp : std_ulogic_vector(value'range);
  begin
    tmp(0) := to_stdlogic(value(0));
    --return tmp; -- uncommenting fixes it
  end function;

  signal s : my_logic_vector(3 downto 0);

begin
  p <= to_stdlogic(s);
end architecture;
