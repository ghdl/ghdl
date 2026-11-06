library ieee;
use ieee.std_logic_1164.all;

package recpkg is
  type inner_t is record
    flag : std_logic;
    cnt  : integer;
  end record;

  type rec_t is record
    bit_f : std_logic;
    vec_f : std_logic_vector (3 downto 0);
    int_f : integer;
    inner : inner_t;
  end record;

  type rec_arr_t is array (0 to 1) of inner_t;
end recpkg;

library ieee;
use ieee.std_logic_1164.all;
use work.recpkg.all;

entity myentity is
  port (
    rp : inout rec_t := ('1', "1010", 42, ('0', -7));
    ro : out rec_t
    );
end myentity;

architecture arch of myentity is
  --  Not driven: values are set through VPI.
  signal rs : rec_t := ('0', "0000", 1, ('1', 2));
  --  Driven by the process below.
  signal rc : rec_t;
  --  Arrays of records are not handled by VPI, but must not crash.
  signal ra : rec_arr_t;
begin
  ro <= ('1', "0011", 5, ('Z', 6));

  process
  begin
    wait for 5 ns;
    rc.int_f <= 5;
    wait for 1 ns;
    rc.inner.flag <= '1';
    rc.inner.cnt <= 77;
    wait for 1 ns;
    rc.vec_f <= "1111";

    --  Check the values written through VPI at 3 ns.
    if rs.int_f /= 99 then
      report "Error: rs.int_f is " & integer'image (rs.int_f) severity note;
    end if;
    if rs.vec_f /= "0101" then
      report "Error: rs.vec_f was not written" severity note;
    end if;
    if rs.inner.cnt /= -3 then
      report "Error: rs.inner.cnt is " & integer'image (rs.inner.cnt)
        severity note;
    end if;
    if rp.bit_f /= '0' then
      report "Error: rp.bit_f is " & std_logic'image (rp.bit_f)
        severity note;
    end if;
    wait for 1 ns;
    report "VHDL checks done" severity note;
    wait;
  end process;
end arch;
