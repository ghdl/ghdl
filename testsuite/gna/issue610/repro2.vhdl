entity repro2 is
  generic (l : natural := 10);
end repro2;

architecture behav of repro2 is
begin
   process
     variable v : string (0 to l);
     alias a : string is v;
   begin
     v := (others => ' ');
     a := (others => 'x');
     wait;
   end process;
end behav;
