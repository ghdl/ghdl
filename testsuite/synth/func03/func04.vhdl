library ieee;
use ieee.std_logic_1164.all;

package func04_pkg is
  generic (function key (a: std_logic_vector) return std_logic_vector);

  function key2 (q : std_logic_vector) return std_logic_vector;
end func04_pkg;

package body func04_pkg is
  function key2 (q : std_logic_vector) return std_logic_vector is
  begin
    return key(key(q));
  end key2;
end func04_pkg;

library ieee;
use ieee.std_logic_1164.all;

entity func04 is
  port (d : std_logic_vector(3 downto 0);
        q : out std_logic_vector (3 downto 0));
end;

architecture behav of func04 is
  subtype slv4 is std_logic_vector(3 downto 0);
  function key (a: slv4) return slv4 is
  begin
    return (a(1), a(0), a(3), a(2));
  end key;

  package my_pkg is new work.func04_pkg
    generic map (key => key);
begin
  q <= my_pkg.key2(d);
end behav;

