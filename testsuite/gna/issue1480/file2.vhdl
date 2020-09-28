entity e is
    port (clk : std_logic);
end entity;

architecture a of e is
begin
    assert rising_edge(clk);
end architecture;
