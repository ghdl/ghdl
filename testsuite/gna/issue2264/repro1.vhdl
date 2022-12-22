package repro1_gpkg is
  generic (len : natural);

  subtype my_nat is natural range 0 to len;

  type my_rec is record
    v : integer;
  end record;

  constant my_cst : my_rec := (v => len);
end;

package repro1_pkg4 is new work.repro1_gpkg generic map (len => 4);
package repro1_pkg10 is new work.repro1_gpkg generic map (len => 10);


entity repro1 is
end;

use work.repro1_pkg10.all;

architecture behav of repro1 is
begin
  process
    variable v : my_rec;
  begin
    assert my_cst.v = 10 severity failure;
    --  Error.
    v := work.repro1_pkg4.my_cst;
    wait;
  end process;
end behav;
