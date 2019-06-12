entity notype2 is
end;

architecture behav of notype2 is
  signal s : out bit_vector(7 downto 0);
begin
  s (1) <= '0';
end behav;
