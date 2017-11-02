library ieee;
    use ieee.std_logic_1164.all;

entity top is
    port ( 
        clk : in std_logic;
        A : in std_logic_vector(4 downto 1);
        B : out std_logic_vector(4 downto 1)
    );
end top;

architecture rtl of top is
begin
    gen_pe : for i in 1 to 4 generate
        test : process (clk) is
        begin
            if (rising_edge(clk)) then 
                B(i) <= A(i);
            end if;
        end process test;
    end generate gen_pe;
end rtl;
