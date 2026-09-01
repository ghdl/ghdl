--  Same defect as counter.vhd, but with the package instantiated at
--  library level instead of inside the architecture.  It fails in a
--  different place -- Grt.Waves.Get_Signal_Number on a garbage signal
--  pointer -- which is the signature reported on issue #2416.

library ieee; use ieee.std_logic_1164.all;
package gp2 is
  generic (W : natural);
  subtype word is std_logic_vector (W - 1 downto 0);
end package;

package inst2 is new work.gp2 generic map (W => 4);

library ieee; use ieee.std_logic_1164.all;
entity top3 is end entity;
architecture behave of top3 is
  signal s : work.inst2.word := (others => '0');
begin
  s <= not s after 1 ns;
end architecture;
