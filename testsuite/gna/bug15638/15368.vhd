entity bug is 
end entity;

architecture a of bug is
  component cmp is
    port(o :out bit_vector);
  end component;

signal o:bit_vector(4 downto 0);

begin
  i_exp: cmp port map(o);

  process(o)
  begin
     report "o event" severity note;
  end process;

end architecture;

entity cmp is
  port(o :out bit_vector);
end entity;

architecture a of cmp is
  signal big_o:bit_vector(255 downto 0);
  signal a:bit_vector(4 downto 0);
begin

  o <= big_o(a'range);

  big_o <= (others => '1') after 5 ns, (others => '0') after 10 ns;
  a     <= (others => '1') after 20 ns, (others => '0') after 30 ns;

end architecture; 
