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
    signal C:  std_logic_vector(4 downto 1);
    signal D:  std_logic_vector(4 downto 1);
begin
    D <= C xor A;
    gen_pe : for i in 1 to 4 generate
        C(i) <= not A(i);
        test : process (clk) is
        begin
            if (rising_edge(clk)) then 
                B(i) <= D(3) and C(i);
            end if;
        end process test;
    end generate gen_pe;
end rtl;
