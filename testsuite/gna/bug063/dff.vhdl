entity DFF is
     port (CLK, CLEAR, D : in bit;
           Q : out bit);
end;

architecture BEHAV of DFF is
begin
process (CLK, CLEAR)
     begin
           if (CLEAR = ‘1’) then
                Q <= ‘0’;
           elsif (CLK’event and CLK = ‘1’) then
                Q <= D;
           end if;
     end process;
end BEHAV;

