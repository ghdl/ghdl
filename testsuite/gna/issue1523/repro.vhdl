library ieee;
use ieee.std_logic_1164.all;

package package1 is
    type InnerRecordType is record
        inner2a : std_logic_vector(3 downto 0); -- NOTE: deleting me makes test pass
        inner2b : std_logic_vector;
    end record;
    
    type OuterRecordType is record
        inner1 : InnerRecordType;
    end record;
end package;

library ieee;
use ieee.std_logic_1164.all;

use work.package1.all;

entity entity1 is
    generic (WIDTH : integer);
    port (port1 : in OuterRecordType(inner1(inner2b(WIDTH-1 downto 0))));
end entity;

architecture arc of entity1 is
begin
    process is
    begin
        wait for 10 ns;
        assert port1.inner1.inner2b = x"ffff" severity failure;
        report "PASS";
        wait;
    end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

use work.package1.all;

entity ghdl_bug_repro is
end entity;

architecture arc of ghdl_bug_repro is
    constant WIDTH : integer := 16;
    
    signal sig1 : OuterRecordType(inner1(inner2b(WIDTH-1 downto 0)));
begin
    ENTITY1_INST : entity work.entity1 generic map (WIDTH => WIDTH) port map (port1 => sig1);
    sig1.inner1.inner2b <= x"ffff";
end architecture;
