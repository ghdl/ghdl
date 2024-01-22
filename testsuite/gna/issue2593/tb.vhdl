

library ieee;
use ieee.std_logic_1164.all;

entity mode_rtl is
   generic (
      type DAT_G
   );
   port (
      rxd_i : in  DAT_G;
      txd_o : out DAT_G
   );
end mode_rtl;

architecture rtl of mode_rtl is
begin
   txd_o <= rxd_i;
end rtl;


use std.env.all;

library ieee;
use ieee.std_logic_1164.all;

entity mode_tb is
   generic (
      MODE : string := "mode_x"
   );
end mode_tb;

architecture behavior of mode_tb is
   subtype DAT_G is std_logic_vector(8-1 downto 0);
   signal txd : DAT_G;
   signal rxd : DAT_G;
begin
   -- all MODE options must be of the same length to avoid tool errors
   gen_dut: case MODE generate
      when "mode_0" =>

         dut : entity work.mode_rtl
         generic map (
            DAT_G => DAT_G
         )
         port map (
            rxd_i => txd,
            txd_o => rxd
         );

      when others =>

         -- passthrough
         rxd <= txd;

   end generate gen_dut;

   p_test : process
   begin
      report "Simulation timeout." severity ERROR;
      finish;
   end process p_test;

end behavior;

