entity tb2 is
end entity;

architecture arch of tb2 is
    signal s: integer := 0;
    signal s2: integer := 0;
begin
    process is
    begin
        wait for 1 us;
        s2 <= 3;
        s <= 1;
        s <= 2 after 1 us;
        assert s = 0;
        wait on s;
        report "s = " & integer'image(s);
        assert s = 2 severity failure;
        assert now = 2 us severity failure;
        wait;
    end process;

end architecture;
