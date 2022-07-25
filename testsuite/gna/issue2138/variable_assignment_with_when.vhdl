library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity variable_assignment_with_when is
    port(
            x : in  std_logic_vector(7 downto 0);
            y : out std_logic_vector(3 downto 0)
        );
end entity;

architecture arch of variable_assignment_with_when is
begin

    process (all) is
        variable sum : natural;
    begin
        sum := 0;
        for i in x'range loop
            sum := sum + 1 when x(i) = '1';
        -- this works:
        -- if x(i) then
        --     sum := sum + 1;
        -- end if;
        end loop;
        y <= std_logic_vector(to_unsigned(sum, y'length));
    end process;

end architecture;
