package gen1 is
  generic (v : natural);

  function get return natural;
end gen1;

package body gen1 is
  function get return natural is
  begin
    return v;
  end get;
end gen1;

package gen2 is
  generic (package pkg is new work.gen1 generic map (<>));

  function get2 return natural;
end gen2;

package body gen2 is

  function get2 return natural is
  begin
    return pkg.get;
  end get2;
end gen2;
