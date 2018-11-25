entity repro is
end;

architecture behav of repro
is
   procedure test (l : natural) is
     variable v : bit_vector (l - 1 downto 0);
   begin
     assert v (l / 2) = '0';
     assert v (0) = '0';
     assert v (l - 1) = '0';
   end test;
begin
   process
   begin
     test (128 * 1024);
     wait;
   end process;
end behav;
