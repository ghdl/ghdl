package pkg1 is
  generic (
    type atyp;
    function plus (a: atyp) return natural is <>);
  function wrap (a : atyp) return natural;
end pkg1;

package body pkg1 is
  function wrap (a : atyp) return natural is
  begin
    return plus (a);
  end wrap;
end pkg1;


entity tb_pkg1 is
end;

architecture behav of tb_pkg1 is
  function plus (a: bit_vector) return natural is
  begin
    return a'length;
  end plus;

  package my_pkg1 is new work.pkg1 generic map (atyp => bit_vector);

  constant c : natural := my_pkg1.wrap("0101");
begin
  assert c = 4 severity failure;
end behav;
