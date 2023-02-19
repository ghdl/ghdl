package repro2_pkg is
  function get return natural;
end;

package body repro2_pkg is
  function get1 return natural is
  begin
    report "get1 called";
    return 123;
  end get1;
  
  constant c : natural := get1;

  function get return natural is
  begin
    report "get called, return: " & natural'image (c);
    return c;
  end get;
end repro2_pkg;

package repro2_gpkg is
  generic (type t);

  function get return natural;
end;

package body repro2_gpkg is
  function get return natural is
  begin
    return work.repro2_pkg.get;
  end get;
end;

entity repro2 is
end;

architecture behav of repro2 is
  package inst is new work.repro2_gpkg generic map (t => bit);
begin
  assert inst.get > 120 and inst.get < 130 severity failure;
end;
  
