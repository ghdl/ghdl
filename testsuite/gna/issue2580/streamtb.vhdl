library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package StreamPkg is
generic(
   type DAT
);
   type streamDataArrayType is array (natural range <>) of DAT;
end StreamPkg;

package body StreamPkg is
end package body StreamPkg;



library ieee;
use ieee.std_logic_1164.all;

entity StreamRegSlice is
   generic (
      type DAT
   );
   port (
      rxcDat_i : in  DAT;
      txcDat_o : out DAT
   );
end StreamRegSlice;

architecture rtl of StreamRegSlice is
begin
   txcDat_o <= rxcDat_i;
end rtl;


use std.env.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity StreamTb is
   generic (
      WIDTH : positive := 8
   );
   package StreamPkgDef is new work.StreamPkg generic map (DAT => std_logic_vector(WIDTH-1 downto 0));
   use StreamPkgDef.all;
end StreamTb;

architecture behavior of StreamTb is
-- subtype DAT is std_logic_vector(WIDTH-1 downto 0);
   signal txcDat : DAT;
   signal rxcDat : DAT;
begin

   -- DUT instance
   dut : entity work.StreamRegSlice
   generic map (
      DAT => DAT
   )
   port map (
      rxcDat_i => txcDat,
      txcDat_o => rxcDat
   );
--   rxcDat <= txcDat;

   p_test : process
   begin
      -- end simulation
      for i in 0 to 8-1 loop
         txcDat <= std_logic_vector(to_unsigned(i, WIDTH));
         wait for 10 ns;
      end loop;
      report "Simulation end";
      finish;
   end process p_test;

end behavior;
