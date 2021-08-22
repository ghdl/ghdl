entity repro_sub is
  generic (num_ports : integer);
  port (clocks : bit_vector(0 to num_ports - 1));
end entity;

architecture a of repro_sub is
begin
end architecture;


entity repro is
end entity;

architecture a of repro is
  signal clock : bit := '0';
begin
  repro_sub_inst : entity work.repro_sub
    generic map (num_ports => 4)
    port map (clocks => (others => clock));
end architecture;
