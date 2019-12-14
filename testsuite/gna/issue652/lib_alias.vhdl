library ieee;
entity foo is
end entity;
architecture fum of foo is
    alias fee is ieee;  -- non-object alias
    use fee.std_logic_1164.all;
    signal s : std_logic;
begin
end architecture;
