entity repro2 is
  generic (depth : natural := 7);
  port (foo: in boolean);
end entity;

architecture foo of repro2 is
    signal foo_int: boolean;
begin
  cond: if depth > 0 generate
    FUMBLE:
      entity work.repro2
        generic map (depth => depth - 1)
        port map (foo => foo_int);
  end generate;
end architecture;
