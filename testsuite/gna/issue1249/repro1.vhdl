entity repro1 is
  generic (WIDTH : natural := 4);
end;

architecture behav of repro1 is
  type myarr is array(natural range <>) of bit_vector(WIDTH-1 downto 0);

  type myrec is record
    a: bit_vector(WIDTH-1 downto 0);
  end record;
begin
end;
