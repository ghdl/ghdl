library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity recursive_xor is
   generic (
      WIDTH : positive := 8
   );
   port (
      xi : in  std_logic_vector(WIDTH-1 downto 0);
      xo : out std_logic
   );
end recursive_xor;

architecture rtl of recursive_xor is
begin
   g_recursion: if (WIDTH = 2) generate

      xo <= xi(1) xor xi(0);

   else generate
      signal xt : std_logic_vector(1 downto 0);
   begin

      g_split : for i in 0 to 1 generate

         u_split: entity work.recursive_xor
         generic map (
            WIDTH => WIDTH/2
         )
         port map (
            xi => xi((i+1)*WIDTH/2-1 downto (i+0)*WIDTH/2),
            xo => xt(i)
         );

      end generate g_split;

      xo <= xt(1) xor xt(0);

   end generate g_recursion;
end rtl;
