library ieee;
use ieee.std_logic_1164.all;
use ieee.fixed_pkg.all;

entity tf is
end entity tf;

architecture testbench of tf is
    signal sx : sfixed (3 downto 0)  := "1001";
    signal sy : sfixed (3 downto -3) := "0101110";
    signal sz : sfixed (3 downto -3);
    signal sci : std_ulogic := '1';
    signal sco : std_ulogic;
begin
  tt : process is
    variable x : sfixed (3 downto 0) := sx;
    variable y : sfixed (3 downto -3) := sy;
    variable z : sfixed (3 downto -3);
    variable ci : std_ulogic := sci;
    variable co : std_ulogic;
    constant eres : sfixed(3 downto -3) := b"1110_111";
  begin
    add_carry (L => x, R => y, c_in => ci, result => z, c_out => co);
    sz <= z;
    sco <= co;
    wait for 1 ns;
    assert z = eres;
    report to_bstring(z);

    x := b"1101";
    y := b"0111_000";
    ci := '1';
    add_carry (L => x, R => y, c_in => ci, result => z, c_out => co);
    wait for 1 ns;
    report "res=" & to_bstring(z) & ", co=" & to_string(co);
    wait;
  end process tt;
end architecture testbench;

