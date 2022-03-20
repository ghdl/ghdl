entity tc4 is
    port (
        state   : in bit;
        o       : out bit_vector(3 downto 0)
        );
end entity tc4;

architecture behaviour of tc4 is
    signal misc_sel      : bit_vector(3 downto 0);
begin
    testcase_0: process(all)
    begin
        misc_sel <= "0000";

        if state = '0' then
          misc_sel <= "0111";
        else
          misc_sel(3) <= '1';
        end if;

        o <= misc_sel;
    end process;
end architecture behaviour;
