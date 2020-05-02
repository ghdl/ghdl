library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro4 is
end ;

architecture beh of repro4 is
  type str_acc is access string;
  type bv_acc is access bit_Vector;

  function f return str_acc is
  begin
    return new String'("abc");
  end f;

  function f return bv_acc is
  begin
    return new bit_vector'("001");
  end f;
  
begin
  process
    variable foo, bar : std_logic;
  begin
    f.all := "010";
    wait;
  end process;
end architecture;
