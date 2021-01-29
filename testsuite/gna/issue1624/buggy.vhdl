entity buggy is
begin
end entity buggy;

architecture a of buggy is
begin

p: process
    variable v: real;
begin
    -- Remove the next line and GHDL does not crash
    v := real((now / (1 ns)) * 1.0e-9);
    wait;
end process;

end architecture a;
