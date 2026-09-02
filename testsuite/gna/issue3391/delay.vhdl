library ieee;
    use ieee.std_logic_1164.all;

entity delay is
    generic (
        DCYCLES : natural := 1
    );
    port (
        clk  : in  std_logic;
        din  : in  natural;   -- unconstrained
        dout : out natural    -- unconstrained
    );
end entity delay;

architecture rtl of delay is
    type chain_t is array (0 to DCYCLES-1) of natural;
    signal chain : chain_t := (others => 0);
begin

    process(clk)
    begin
        if rising_edge(clk) then
            chain(0) <= din;
            for i in 1 to DCYCLES-1 loop
                chain(i) <= chain(i-1);
            end loop;
        end if;
    end process;

    dout <= din when DCYCLES = 0 else chain(DCYCLES-1);

end architecture rtl;
