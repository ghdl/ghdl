use std.env.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity recursive_xor_tb is
   generic (
      WIDTH : positive := 8
   );
end recursive_xor_tb;

architecture behavior of recursive_xor_tb is
   constant T_C : time := 10.0 ns;
   signal xi : std_logic_vector(WIDTH-1 downto 0);
   signal xo : std_logic;
   signal cnt : integer;
begin

   dut: entity work.recursive_xor
   generic map (
      WIDTH => WIDTH
   )
   port map (
      xi => xi,
      xo => xo
   );

   p_test : process
   begin
      for i in 0 to 2**WIDTH-1 loop
         cnt <= i;
         xi <= std_logic_vector(to_unsigned(i, WIDTH));
         wait for T_C;
         assert (xo = xor xi) report "mixmatch at i=" & integer'image(i) severity ERROR;
         wait for T_C;
      end loop;
      -- end simulation
      wait for T_C;
      report "Simulation end.";
      finish;
   end process p_test;

end behavior;

