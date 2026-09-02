library ieee;
    use ieee.std_logic_1164.all;

entity top is
end entity top;

architecture rtl of top is
    signal clk : std_logic := '0';
    signal a   : natural range 0 to 3 := 0;  -- constrained actual
    signal b   : natural;                    -- unconstrained (matches dout)
begin

    -- Positional association -- the elaborator canonicalizes this to a
    -- by-name association internally, so the bug triggers either way.
    dut : entity work.delay
        generic map (2)
        port map (clk, a, b);

    clk <= not clk after 5 ns;

    process
    begin
        wait for 100 ns;
        --std.env.stop;
    end process;

end architecture rtl;
