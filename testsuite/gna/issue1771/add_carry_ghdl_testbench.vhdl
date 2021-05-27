use std.textio.all;
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_textio.all;
use ieee.fixed_pkg.all;

entity add_carry_ghdl_testbench is
end entity add_carry_ghdl_testbench;

architecture testbench of add_carry_ghdl_testbench is
    signal sci : std_ulogic := '1';
    signal sx_6_0 : ufixed (6 downto 0) := "1101000";
    signal sx_3_0 : ufixed (3 downto 0) := "1101";
    signal sy_6_0 : ufixed (6 downto 0) := "0111000";
    signal sx_3_m3 : ufixed (3 downto -3) := "1101000";
    signal sy_3_m3 : ufixed (3 downto -3) := "0111000";
    signal sz_6_0 : ufixed (6 downto 0);
    signal sz_3_m3 : ufixed (3 downto -3);
    signal sz_3_0_m3 : ufixed (3 downto -3);
    signal sco_6_0 : std_ulogic;
    signal sco_3_m3 : std_ulogic;
    signal sco_3_0_m3 : std_ulogic;
begin
  test_add_carry_6_0 : process is
    variable x : ufixed (6 downto 0) := sx_6_0;
    variable y : ufixed (6 downto 0) := sy_6_0;
    variable z : ufixed (6 downto 0);
    variable ci : std_ulogic := sci;
    variable co : std_ulogic;
    variable msg : line;
  begin
    add_carry (L => x, R => y, c_in => ci, result => z, c_out => co);
    sz_6_0 <= z;
    sco_6_0 <= co;
    wait for 1 ns;
    write(msg, "sci=" & to_string(sci) & LF);
    write(msg, "sx_6_0=" & to_hstring(sx_6_0) & LF);
    write(msg, "sy_6_0=" & to_hstring(sy_6_0) & LF);
    write(msg, "sz_6_0=" & to_hstring(sz_6_0) & LF);
    write(msg, "sco_6_0=" & to_string(sco_6_0) & LF);
    writeline(output,msg);
    wait;
  end process test_add_carry_6_0;

    
  test_add_carry_3_m3 : process is
    variable x : ufixed (3 downto -3) := sx_3_m3;
    variable y : ufixed (3 downto -3) := sy_3_m3;
    variable z : ufixed (3 downto -3);
    variable ci : std_ulogic := sci;
    variable co : std_ulogic;
    variable msg : line;
  begin
    add_carry (L => x, R => y, c_in => ci, result => z, c_out => co);
    sz_3_m3 <= z;
    sco_3_m3 <= co;
    wait for 2 ns;
    write(msg, "sci=" & to_string(sci) & LF);
    write(msg, "sx_3_m3=" & to_hstring(sx_3_m3) & LF);
    write(msg, "sy_3_m3=" & to_hstring(sy_3_m3) & LF);
    write(msg, "sz_3_m3=" & to_hstring(sz_3_m3) & LF);
    write(msg, "sco_3_m3=" & to_string(sco_3_m3) & LF);
    writeline(output,msg);
    wait;
  end process test_add_carry_3_m3;
  
  test_add_carry_3_0_m3 : process is
    variable x : ufixed (3 downto  0) := sx_3_0;
    variable y : ufixed (3 downto -3) := sy_3_m3;
    variable z : ufixed (3 downto -3);
    variable ci : std_ulogic := sci;
    variable co : std_ulogic;
    variable msg : line;
  begin
    add_carry (L => x, R => y, c_in => ci, result => z, c_out => co);
    sz_3_0_m3 <= z;
    sco_3_0_m3 <= co;
    wait for 3 ns;
    write(msg, "sci=" & to_string(sci) & LF);
    write(msg, "sx_3_0=" & to_hstring(sx_3_0) & LF);
    write(msg, "sy_3_m3=" & to_hstring(sy_3_m3) & LF);
    write(msg, "sz_3_0_m3=" & to_hstring(sz_3_0_m3) & LF);
    write(msg, "sco_3_0_m3=" & to_string(sco_3_0_m3) & LF);
    writeline(output,msg);
    wait;
  end process test_add_carry_3_0_m3;
  
end architecture testbench;
