

package mwe_pkg is generic (N : natural);
end package;

entity mwe is
end entity;

architecture bhv of mwe is
begin
    process is
        package inst_pkg is new work.mwe_pkg generic map (N => 0);
    begin
        std.env.finish;
        wait;
    end process;
end architecture;

