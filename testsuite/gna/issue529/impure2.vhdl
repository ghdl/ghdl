entity impure_ex2 is
port (
   clk : in  bit;
   arg : in  bit;
   res : out bit
);
end impure_ex2;

architecture rtl of impure_ex2 is

   -- An impure function called from a combinatorial process with "all" 
   -- sensitivity triggers an exception.
   impure function foo return bit is
   begin
      return arg;
   end function foo;

   signal ns : bit;

begin

   --comb_process : process(res) -- this works
   comb_process : process(all)
   begin
      ns <= res XOR foo;
   end process;
end rtl;
