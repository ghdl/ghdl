entity repro is
  generic (depth : natural := 5);
   port (inp : bit := '0');
end entity;

architecture mult of repro is
   signal  s : bit;
begin
  gen: if depth > 0 generate
    inst : entity work.repro
      generic map (depth => depth - 1)
      port map(inp => s and inp);
  end generate;
end architecture mult;

