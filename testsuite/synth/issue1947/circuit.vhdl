library ieee;
use ieee.std_logic_1164.all;
package some_generic_package is
    generic (
        some_param : natural
    );
    procedure some_proc (
        variable some_var : out std_logic
    );
end;
package body some_generic_package is
    procedure some_proc (
        variable some_var : out std_logic
    ) is
    begin
    end;
end package body;

library ieee;
use ieee.std_logic_1164.all;
entity circuit is
    port (
        clk : std_logic
    );
end;
package package_instance is new work.some_generic_package
    generic map(
        4
    );
use work.package_instance.all;
architecture rtl of circuit is
begin
    process
        variable some_var : std_logic;
    begin
        wait until clk;

        some_proc(some_var);

    end process;
end architecture;
