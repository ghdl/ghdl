library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library work;

package memory is

    Generic(chip_addr : std_logic_vector(31 downto 0));

end memory;

package body memory is
end memory;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library work;

entity mem is
    Port(clk : IN std_logic);

    package d_memory is new work.memory
        generic map (chip_addr => x"00000000");
end mem;

architecture bh of mem is
begin
end bh;
