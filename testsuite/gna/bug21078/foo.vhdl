library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo is
end entity;

architecture fum of foo is
    
    signal ia:      std_logic_vector(15 downto 0);
    signal clock:   std_logic:='0';
    signal cw:      unsigned(15 downto 0):=(others => '0');
begin
        
    -- counter and  cw are converted to UNSIGNED to use numeric_std "+"
    
    
CTR: process (clock)

    variable counter : unsigned (15 downto 0) :=  x"0000";
    variable op : std_logic_vector (7 downto 0);

    begin

        if clock'event and clock='1' then

            op := std_logic_vector(cw (15 downto 8));

        -- This if statement is misbehaving 
            if (op = x"20") then
                counter  := counter + cw (7 downto 0);
            end if;

            counter := counter + 2;

            ia <= std_logic_vector(counter);

        end if;

    end process;

CLK:    process
    begin
        if Now <= 200 ns then
            wait for 10 ns;
            clock <= not clock;
        else
            wait;
        end if;

    end process;

STIMULUS:
    process (clock)
    begin
        if clk'event and clock = '1' then
            if Now = 110 ns then
                cw <= X"200F";
            else
                cw <= (others => '0');
            end if;
        end if;
    end process;
    
end architecture;

    
    
