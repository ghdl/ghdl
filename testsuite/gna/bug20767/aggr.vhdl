package pkg1 is
  type int_arr is array (natural range <>) of integer;
end pkg1;

use work.pkg1.all;

package pkg2 is
  function func (a : int_arr) return natural;
end pkg2;
  
package body pkg2 is
  function func (a : int_arr) return natural is
  begin
    return a'length;
  end func;
end pkg2;

entity tb is
end tb;

use work.pkg2.all;

architecture behav of tb is
begin
  process
    constant c : natural := func (a => (1, 2, 3));
  begin
    wait;
  end process;
end behav;
