entity mux4 is
        port ( a0, a1, a2, a3 : in bit;
               sel0, sel1 : in bit;
               y : out bit );
end entity mux4;

architecture behav of mux4 is
begin   
        with bit_vector'(sel0, sel1) select
                y <= a0 when "00",
                     a1 when "01",
                     a2 when "10",
                     a3 when "11";
end architecture behav;
