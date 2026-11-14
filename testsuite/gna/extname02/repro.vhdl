--  Indexed and slice names whose prefix is an external name.
package repro_pkg is
  type mem_t is array (0 to 3, 0 to 1) of bit;
end repro_pkg;

use work.repro_pkg.all;

entity repro_leaf is
end repro_leaf;

architecture rtl of repro_leaf is
  signal v : bit_vector (7 downto 0) := x"a5";
  signal m : mem_t := (others => (others => '0'));
  constant c : bit_vector (3 downto 0) := "0110";
begin
end rtl;

use work.repro_pkg.all;

entity repro is
end repro;

architecture tb of repro is
  signal mirror : bit;
begin
  u : entity work.repro_leaf;

  b : block
  begin
    --  In a concurrent statement.
    mirror <= << signal .repro.u.v : bit_vector (7 downto 0) >> (0);
  end block;

  process
  begin
    assert << signal .repro.u.v : bit_vector (7 downto 0) >> (7) = '1'
      severity failure;
    assert << signal .repro.u.v : bit_vector (7 downto 0) >> (3 downto 0)
      = x"5" severity failure;
    assert << constant .repro.u.c : bit_vector (3 downto 0) >> (1) = '1'
      severity failure;
    << signal .repro.u.v : bit_vector (7 downto 0) >> (1) <= force '1';
    << signal .repro.u.m : mem_t >> (2, 1) <= force '1';
    wait for 1 ns;
    assert << signal .repro.u.v : bit_vector (7 downto 0) >> = x"a7"
      severity failure;
    assert << signal .repro.u.m : mem_t >> (2, 1) = '1' severity failure;
    assert mirror = '1' severity failure;
    report "repro ok";
    wait;
  end process;
end tb;
