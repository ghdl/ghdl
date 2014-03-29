entity tb is
end entity;

architecture arch of tb is
    signal s: integer := 0;
begin
    process is
    begin
        wait for 1 us;
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
