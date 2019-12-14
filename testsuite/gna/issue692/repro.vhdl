entity repro is
end;

architecture behav of repro
is
   function test (l : natural) return boolean is
     variable v : bit_vector (l - 1 downto 0);
   begin
     assert v (l / 2) = '0';
     assert v (0) = '0';
     assert v (l - 1) = '0';
     return True;
   end test;
begin
   process
     variable res : boolean;
   begin
     res := test (128 * 1024);
     wait;
   end process;
end behav;
