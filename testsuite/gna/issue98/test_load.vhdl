library ieee ;
use ieee.std_logic_1164.all;

entity test_load is

port(
    clk_i          : in    std_ulogic;
    rst_i          : in    std_ulogic;
    dat_i          : in    std_ulogic_vector(0 to 31);
    sot_in         : in    std_ulogic;
    dat_o          : out    std_ulogic_vector(0 to 2559)

    );
end test_load;

architecture RTL of test_load is

    signal w        :   std_ulogic_vector(0 to 2559);

begin

    process(clk_i)   
    begin
        if (clk_i'event and clk_i = '1') then
            w <= dat_i & w(0 to 2559-32);

        end if;
    end process;    
    dat_o <= w; 

end RTL; 
