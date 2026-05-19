entity top2 is
  port (a, b : out bit);
end entity;

architecture behav of top2 is
    component black is
        port (a : out bit);
    end component;
begin
    inst1 : black port map (a => a);
    inst2 : black port map (a => b);
end architecture;
