--  Reduced from FailLinApprox.zip attached to the report: the call to
--  appy_textfile_content in psi_fix_lin_approx_sin18b_tb.vhd, which
--  associates one element of an array formal, "Data(0) => InData", where
--  the array's element subtype is unbounded.

library ieee;
use ieee.std_logic_1164.all;

package repro_pkg is
  --  Array of an unbounded element: the bounds are dynamic.
  type slv_array_t is array (natural range <>) of std_logic_vector;

  procedure drive (signal data : out slv_array_t);
end package;

package body repro_pkg is
  procedure drive (signal data : out slv_array_t) is
  begin
    data (0) <= (data (0)'range => '1');
  end procedure;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use work.repro_pkg.all;

entity repro is
end entity;

architecture a of repro is
  signal s : std_logic_vector (7 downto 0) := (others => '0');
begin
  process
  begin
    --  Individual association of an array element with dynamic bounds.
    drive (data (0) => s);
    wait for 1 ns;
    assert s = x"ff" report "bad value" severity failure;
    wait;
  end process;
end architecture;
