entity bug02 is
end bug02;

architecture rtl of bug02 is
    FUNCTION weird (bw : integer range 2 to 32)
        RETURN INTEGER IS
    BEGIN
        RETURN -(2**(bw - 1));
    END weird;
begin
    process
        constant c : integer := weird (10);
    begin
        report natural'image(c);
        assert c = -512 severity failure;
        wait;
    end process;
end architecture;
