library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

entity a2 is
port
(
    foo : out std_ulogic_vector
);
end;

architecture rtl of a2 is
begin
end rtl;

library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

entity b2 is
port
(
    bar     : inout std_logic_vector(7 downto 0)
);
end;

architecture rtl of b2 is
begin

i_a: entity work.a2
port map
(
    std_logic_vector (foo) => bar
);
end rtl;

