package repro_pkg is
  type rec is record
    a : bit;
    b : bit;
  end record;
end repro_pkg;

use work.repro_pkg.all;

entity repro_leaf is
end repro_leaf;

architecture rtl of repro_leaf is
  signal s : rec := ('0', '1');
begin
end rtl;

use work.repro_pkg.all;

entity repro is
end repro;

architecture tb of repro is
begin
  --  The process is elaborated before the instance.
  process
    alias s is << signal .repro.g(0).u.s : rec >>;
    alias b is s.b;
  begin
    wait for 1 ns;
    assert b = '1' severity failure;
    report "done";
    wait;
  end process;

  g : for i in 0 to 0 generate
    u : entity work.repro_leaf;
  end generate;
end tb;
