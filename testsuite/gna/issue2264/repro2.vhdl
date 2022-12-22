package repro2_gpkg is
  generic (len : natural);

  subtype my_nat is natural range 0 to len;

  type my_rec is record
    v : integer;
  end record;

  constant my_cst : my_rec := (v => len);
end;

package repro2_gpkgpkg is
  generic (package pkg is new work.repro2_gpkg generic map (<>));

  use pkg.all;
  constant my2_cst : my_rec := (v => len + 1);
end;

package repro2_pkg10 is new work.repro2_gpkg generic map (len => 10);

package repro2_pkgpkg10 is new work.repro2_gpkgpkg generic map (pkg => work.repro2_pkg10);


entity repro2 is
end;

use work.repro2_pkg10.all;
use work.repro2_pkgpkg10.all;

architecture behav of repro2 is
begin
  process
    variable v : my_rec;
  begin
    assert my2_cst.v = my_cst.v + 1 severity failure;
    wait;
  end process;
end behav;
