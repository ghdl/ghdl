entity top is end entity;

architecture behav of top is
    component black is
        port (a : out bit);
    end component;
begin
    inst : black port map (a => open);
end architecture;
