entity good is
    port(
        a,b,c : in bit;
        d1,d2,d3 : out bit
    );
end good;
    
architecture night of good is
    begin
        d1 <= (a and b) or c;
        d2 <= (a or b) and c;
        d3 <= (a and b and c) or (a and b) or c;
    end;
        
entity moon is
end moon;

architecture night of moon is
    component good
    port(
        a,b,c : in bit;
        d1,d2,d3 : out bit
        );  
    end component;

    signal a0,b0,c0 : bit;
    signal d0 : bit;
    
    begin
        cow : night port map(a0, b0, c0, d0);
        
        process (a,b,c,d) is
            begin
                a <= '0'; b <= '0'; c <= '0';
                wait for 25 ns;
                a <= '0'; b <= '0'; c <= '1';
                wait for 25 ns;
                a <= '0'; b <= '1'; c <= '0';
                wait for 25 ns;
                a <= '0'; b <= '1'; c <= '1';
                wait for 25 ns;
                a <= '1'; b <= '0'; c <= '0';
                wait for 25 ns;
                a <= '1'; b <= '0'; c <= '1';
                wait for 25 ns;
                a <= '1'; b <= '1'; c <= '0';
                wait for 25 ns;
                a <= '1'; b <= '1'; c <= '1';
                wait for 25 ns;
        end process;
    end; 
