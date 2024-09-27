library ieee;
use ieee.std_logic_1164.all;

entity handle is
    port (data_i : in std_ulogic_vector);
end;

architecture arch of handle is
begin
end;

library ieee;
use ieee.std_logic_1164.all;

entity top is
    port (count_i : in std_ulogic_vector(5 downto 0));
end;

architecture arch of top is
begin
    handle : entity work.handle port map (
        data_i(5 downto 0) => count_i
    );
end;
