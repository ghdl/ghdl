library ieee;
use ieee.std_logic_1164.all;

entity ent is
   port (
      clk    : in  std_logic;
      input  : in  std_logic;
      output : out std_logic
   );
end entity ent;

architecture rtl of ent is

   signal r : std_logic;

begin

   process (clk)
   begin
      if rising_edge(clk) then
         r <= input;
      end if;
   end process;

   output <= r;

end architecture rtl;

