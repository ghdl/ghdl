library ieee;
context ieee.ieee_std_context;


entity tb_direction_mismatch is
end entity tb_direction_mismatch;

architecture tb of tb_direction_mismatch is
    signal left : std_logic_vector(1 to 0);
begin
    uut : entity work.direction_mismatch
    port map (
        left => left
    );
end architecture;
