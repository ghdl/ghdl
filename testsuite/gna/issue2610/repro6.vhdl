--  Alias of a record element of an external name, in a process placed
--  before the instance.
package repro6_pkg is
  type rec is record
    a : bit;
    b : bit;
  end record;
end repro6_pkg;

use work.repro6_pkg.all;

entity repro6_leaf is
end repro6_leaf;

architecture rtl of repro6_leaf is
  signal s : rec := ('0', '1');
begin
end rtl;

use work.repro6_pkg.all;

entity repro6 is
end repro6;

architecture tb of repro6 is
begin
  process
    alias s is << signal .repro6.g(0).u.s : rec >>;
    alias b is s.b;
  begin
    wait for 1 ns;
    assert b = '1' severity failure;
    report "repro6 ok";
    wait;
  end process;

  g : for i in 0 to 0 generate
    u : entity work.repro6_leaf;
  end generate;
end tb;
