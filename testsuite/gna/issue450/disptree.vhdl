library ieee;
    use ieee.std_logic_1164.all;

entity disptree is
    port ( 
        clk : in std_logic;
        A : in std_logic_vector(5 downto 1);
        B : out std_logic_vector(5 downto 1)
    );
end disptree;

architecture rtl of disptree is
begin
    gen_for : for i in 1 to 4 generate
        test1: process (clk) is
        begin
            if (rising_edge(clk)) then 
                B(i) <= A(i);
            end if;
        end process test1;
    end generate gen_for;
    
    gen_if : if True generate
		test2: process (clk) is
        begin
            if (rising_edge(clk)) then 
                B(5) <= A(5);
            end if;
        end process test2;
	end generate gen_if;
end rtl;

