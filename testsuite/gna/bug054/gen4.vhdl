package gen1 is
  generic (v : natural := 5);

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
  use pkg.all;
  
  function get2 return natural is
  begin
    return get;
  end get2;
end gen2;

package pkg1 is new work.gen1;
package pkg2 is new work.gen2 generic map (work.pkg1);

entity tb is
end tb;

architecture behav of tb is
begin
  assert work.pkg2.get2 = 5;
end behav;
