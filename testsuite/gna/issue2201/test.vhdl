-------------------------------------------------
--  declare signal with custom resolution funciton.
--  The signal is a vector of vector of integers
--  Apply the resolution function of integer element
--  to the array of array.


package composites is
  --  integer
  type mx_ivec is array (natural range <>) of integer;
  type mx_ivec_p is access mx_ivec;
  type mx_ivec_plst is array (natural range <>) of mx_ivec_p;
  type mx_ivec_plst_p is access mx_ivec_plst;
  type mx_ivec_arr is array (natural range <>) of mx_ivec;
end package;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.composites.all;
package reso_pkg is
  subtype rint is integer;
  type rint_vec IS ARRAY ( NATURAL RANGE <> ) OF rint;
  function areso (s : rint_vec) return rint;
  subtype res_rint is areso rint;
  subtype resob is (areso) mx_ivec;
  subtype resoba is ((areso)) mx_ivec_arr;
end package;

package body reso_pkg is
  function areso(s : rint_vec) return rint is
    variable rtn : rint := 0;
  begin
    IF (s'LENGTH = 1) THEN
      RETURN s(s'LOW);
    ELSE
      FOR i IN s'RANGE LOOP
        if s(i) = rint'left then
          if rtn < 0 then
            rtn := rint'left;
          else
            rtn := rtn + s(i);
          end if;
        elsif s(i) = rint'right then
          if rtn > 0 then
            rtn := rint'right;
          else
            rtn := rtn + s(i);
          end if;
        else
          rtn := rtn + s(i);
        end if;

      END LOOP;
    END IF;
    RETURN rtn;
  end function areso;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.composites.all;
use work.reso_pkg.all;

entity unc_subt2 is
begin
  process
  begin
    report "Test unc_subt2 starting: signal resolution.";
    wait;
  end process;
end entity unc_subt2;

architecture rtl of unc_subt2 is

  signal bv : bit_vector(7 downto 0) := "11000100";
  signal b : bit;
  signal clk : bit;
  signal tcnt : integer;

  signal a1 : res_rint;

  signal a11 : rint;
  signal a12 : rint;
  signal a13 : rint;

  signal arr1 : mx_ivec(7 downto 0);
  signal arr2 : mx_ivec(7 downto 0);
  signal arr3 : mx_ivec(7 downto 0);
  signal arr_r : resob(7 downto 0);

  signal arr_arr1 : mx_ivec_arr(3 downto 0)(7 downto 0);
  signal arr_arr2 : mx_ivec_arr(3 downto 0)(7 downto 0);
  signal arr_arr3 : mx_ivec_arr(3 downto 0)(7 downto 0);
  signal arra_r : resoba(3 downto 0)(7 downto 0);

begin
  clock: process
  begin
    wait for 1 ns;
    clk <= not clk;
  end process;

  proc1: process(clk)
    variable cnt : integer := 0;
  begin
    if clk = '1' then
      b <= '0';
      cnt := cnt + 1;
    else
      b <= not b;
      if cnt >= 50 then
        report "Test passed ..." severity failure;
      end if;
    end if;
    tcnt  <= cnt;
  end process;

  proca11: process
    variable v1 : rint := 0;
  begin
    wait on clk until clk = '1';
    v1 := v1 + 1;
    if v1 > 10 then
      v1 := 0;
    end if;
    for i in arr_arr1'range loop
      for j in arr_arr1(i)'range loop
        arr_arr1(i)(j) <= v1;
      end loop;
    end loop;
    for i in arr1'range loop
      arr1(i) <= v1;
    end loop;
    wait for 0 ps;
    arra_r <= arr_arr1;
    arr_r <= arr1;
    a11  <= v1;
    a1 <= v1;
  end process;

  proca12: process
    variable v1 : rint := -5;
  begin
    wait on clk until clk = '1';
    v1 := v1 + 1;
    if v1 > 5 then
      v1 := -5;
    end if;

    for i in arr_arr2'range loop
      for j in arr_arr2(i)'range loop
        arr_arr2(i)(j) <= v1;
      end loop;
    end loop;

    for i in arr2'range loop
      arr2(i) <= v1;
    end loop;
    wait for 0 ps;
    arra_r <= arr_arr2;
    arr_r <= arr2;
    a12 <= v1;
    a1 <= v1;
  end process;

  proca13: process
    variable v1 : rint := -10;
  begin
    wait on clk until clk = '1';
    v1 := v1 + 1;
    if v1 > 10 then
      v1 := -10;
    end if;
    for i in arr_arr3'range loop
      for j in arr_arr3(i)'range loop
        arr_arr3(i)(j) <= v1;
      end loop;
    end loop;

    for i in arr3'range loop
      arr3(i) <= v1;
    end loop;
    wait for 0 ps;
    arra_r <= arr_arr3;
    arr_r <= arr3;
    a13 <= v1;
    a1 <= v1;
  end process;

  process(clk)
    variable tint : integer;
  begin
    if clk'event and clk = '0' then
      assert a1 = a11 + a12 + a13
        report "ERROR:  Resolved signal not the value expected."  severity failure;


      for i in arr_r'range loop
        tint := arr_r(i);
        assert tint = a11 + a12 + a13
          report "ERROR:  Resolved signal not the value expected."  severity failure;
      end loop;

      for i in arra_r'range loop
        for j in arra_r(i)'range loop
          tint := arra_r(i)(j);
          assert tint = a11 + a12 + a13
            report "ERROR:  Resolved signal not the value expected."  severity failure;
        end loop;
      end loop;
    end if;
  end process;

end rtl;
