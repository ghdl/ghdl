entity repro1 is
  generic (c : bit_vector);
end entity;

architecture behaviour of repro1 is

  signal s : bit_vector(3 downto 0);

begin

  c <= s;

end architecture;
