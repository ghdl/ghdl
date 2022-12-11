library ieee;
use ieee.std_logic_1164.all;

entity tb4 is
end;

architecture behav of tb4 is
  function repeat(val : std_ulogic; n : natural) return std_ulogic_vector
  is
    variable res : std_ulogic_vector(n - 1 downto 0);
  begin
    --  No aggregate use.
    for i in res'range loop
      res (i) := val;
    end loop;
    return res;
  end function;

begin
  process
    constant c : std_ulogic_vector (3 downto 0) := ('1', repeat ('0', 3));
  begin
    assert c = "1000" severity failure;
    wait;
  end process;
end behav;
