package pkg3 is
  generic (
    type atyp;
    function plus (a: atyp) return natural is <>);
  function wrap (a : atyp) return natural;
end pkg3;

package body pkg3 is
  function wrap (a : atyp) return natural is
  begin
    return plus (a);
  end wrap;
end pkg3;


entity tb_pkg3 is
end;

architecture behav of tb_pkg3 is
  function plus (a: bit_vector) return natural is
  begin
    return a'length;
  end plus;

  package my_pkg3 is new work.pkg3 generic map (atyp => bit_vector,
                                                plus => open);

  constant c : natural := my_pkg3.wrap("0101");
begin
  assert c = 4 severity failure;
end behav;
