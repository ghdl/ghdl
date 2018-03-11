library ieee;
   use ieee.std_logic_1164.all;

entity impure_ex is
port (
   clk : in  std_logic;
   arg : in  std_logic;
   res : out std_logic
);
end impure_ex;

architecture rtl of impure_ex is

   -- An impure function called from a combinatorial process with "all" 
   -- sensitivity triggers an exception.
   impure function foo return std_logic is
   begin
      return arg;
   end function foo;

   signal ns : std_logic;

begin

   --comb_process : process(res) -- this works
   comb_process : process(all)
   begin
      ns <= res XOR foo;
   end process;

   process(clk)
   begin
      if rising_edge(clk) then
         res <= ns;
      end if;
   end process;

end rtl;
