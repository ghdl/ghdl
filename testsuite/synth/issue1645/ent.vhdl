library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
   generic (size : natural := 4);
   port (
      clk : in std_logic;
      r   : out std_logic
   );
end entity ent;

architecture rtl of ent is

   type ram_type is array (0 to size-1) of std_logic_vector(7 downto 0);
   subtype index_type is natural range ram_type'range;   -- This declaration leads to incorrect sign-extension
--   subtype index_type is natural range 0 to size-1;      -- This declaration leads to correct unsigned comparison.
   signal a : index_type := 0;

begin

   process (clk)
   begin
      if rising_edge(clk) then
         if a = index_type'high then
            a <= index_type'low;
         else
            a <= a + 1;
         end if;
      end if;
   end process;

   process (a)
   begin
      if a < 3 then
         r <= '1';
      else
         r <= '0';
      end if;
   end process;
end architecture rtl;
