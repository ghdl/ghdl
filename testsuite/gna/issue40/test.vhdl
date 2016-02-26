library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

entity a is
port
(
    foo : in std_ulogic_vector
);
end a;

architecture rtl of a is
begin
end rtl;

library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

entity b is
port
(
    bar     : inout std_logic_vector(7 downto 0)
);
end b;

architecture rtl of b is
begin

i_a: entity work.a
port map
(
    foo => std_ulogic_vector(bar)
);
end rtl;

