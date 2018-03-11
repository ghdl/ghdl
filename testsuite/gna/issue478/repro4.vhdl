entity repro4 is
end;

architecture behav of repro4 is
begin
   process
   begin
    "foo" (true, false);
   end process;
end;
