package repro1_gpkg is
  generic (type t);

  function get return natural;
end;


package body repro1_gpkg is
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
end;

entity repro1 is
end;

architecture behav of repro1 is
  package inst is new work.repro1_gpkg generic map (t => bit);
begin
  assert inst.get > 120 and inst.get < 130 severity failure;
end;
  
