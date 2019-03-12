entity tb is

end entity tb;

architecture functional of tb is

begin  -- architecture functional

  dut : entity work.a
    port map (i => true);

end architecture functional;
