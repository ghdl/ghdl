entity ent2 is
end entity;

architecture test of ent2 is
begin

    b1: block is
        signal s1 : bit_vector(1 to 3);
        constant c1 : string := "hello";
    begin
        s1 <= "101";
    end block;

    p1: process is
    begin
        wait for 1 ns;
        assert << signal b1.s1 : bit_vector >> = "101";  -- OK
        assert << constant b1.c1 : string(1 to 5) >> = "hello";  -- OK
        assert << constant b1.c1 : string(1 to 6) >> = "hello";  -- Error
        wait;
    end process;

end architecture;
