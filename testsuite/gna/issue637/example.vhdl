entity example is
end entity;

architecture tb of example is
    type arrType is array (1 downto 0) of integer;
    type arrTypePtr is access arrType;
begin
    process (all)
        variable ptr : arrTypePtr;
    begin
        ptr := new arrType'(10, 5); -- works
        ptr.all(0) := 5; -- crash
        ptr(0) := 5; --crash
    end process;

end architecture;
