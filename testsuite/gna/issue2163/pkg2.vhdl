package pkg2 is
  generic (
    type atyp;
    function plus (a: atyp) return natural is <>);
  function wrap (a : atyp) return natural;
end pkg2;

package body pkg2 is
  function wrap (a : atyp) return natural is
  begin
    return plus (a);
  end wrap;
end pkg2;


entity tb_pkg2 is
end;

architecture behav of tb_pkg2 is
  constant plus : natural := 5;
  package my_pkg2 is new work.pkg2
    generic map (atyp => bit_vector, plus => plus);

  constant c : natural := my_pkg2.wrap("0101");
begin
  assert c = 4 severity failure;
end behav;
