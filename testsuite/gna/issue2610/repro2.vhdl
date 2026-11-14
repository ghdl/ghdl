--  A shared variable of a protected type, through an external name.
package repro2_pkg is
  type counter_t is protected
    procedure inc;
    impure function get return integer;
  end protected;
end package;

package body repro2_pkg is
  type counter_t is protected body
    variable n : integer := 0;
    procedure inc is
    begin
      n := n + 1;
    end procedure;
    impure function get return integer is
    begin
      return n;
    end function;
  end protected body;
end package body;

use work.repro2_pkg.all;

entity repro2_leaf is
end repro2_leaf;

architecture rtl of repro2_leaf is
  shared variable sv : counter_t;
begin
end rtl;

use work.repro2_pkg.all;

entity repro2 is
end repro2;

architecture tb of repro2 is
begin
  u0 : entity work.repro2_leaf;

  process
    alias sv is << variable .repro2.u0.sv : counter_t >>;
  begin
    sv.inc;
    sv.inc;
    assert sv.get = 2 report "sv" severity failure;
    report "repro2 ok";
    wait;
  end process;
end tb;
