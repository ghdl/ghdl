entity inst2 is
end;

architecture behav of inst2 is
begin
   gen: if False b = 0 generate
     component cmp is
     end component;
   begin
   end generate;
end behav;
