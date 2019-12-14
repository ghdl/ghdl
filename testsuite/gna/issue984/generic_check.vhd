use work.types_pkg.all;
-- Workaround:
--use work.const_pkg.all;

entity generic_check is
    generic (
        i : generic_type
    );
end entity;

architecture a of generic_check is
begin
    process
    begin
        assert i(0) = 5 report "Should be 5, is " & integer'image(i(0))
           severity failure;
        wait;
    end process;
end architecture;
