entity tb3 is
end entity;

architecture arch of tb3 is
    signal s: integer := 0;
begin
    process is
    begin
        wait for 1 us;
        s <= 1;
        s <= reject 1 ns inertial 2 after 2 us;
        assert s = 0;
        wait on s;
        report "s = " & integer'image(s);
        assert s = 1 severity failure;
        assert now = 1 us severity failure;
        wait on s;
        report "s = " & integer'image(s);
        assert s = 2 severity failure;
        assert now = 3 us severity failure;
        wait;
    end process;

end architecture;
