library ieee;
use ieee.std_logic_1164.all;

entity tb_ent is
end;

architecture a of tb_ent is
    signal a, enable, d_in, d_out : std_logic;
begin
    uut: entity work.ent
        port map (
            a => a,
            enable => enable,
            d_in => d_in,
            d_out => d_out
        );

    process
    begin
        a <= '0';
        enable <= '0';

        wait for 10 ns;
        assert d_out = '0';

        a <= '1';

        wait for 10 ns;
        assert d_out = '1' severity failure;

        enable <= '1';
        a <= 'Z';
        d_in <= '0';

        wait for 10 ns;
        assert a = '0' severity failure;

        d_in <= '1';

        wait for 10 ns;
        assert a = '1' severity failure;

        wait;
    end process;
end;
