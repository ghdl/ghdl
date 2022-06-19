entity repro1 is
end;

architecture rtl of repro1 is
  signal vec : bit_vector(7 downto 0);
begin
  vec <= (3 downto 0 => "111", others => '0'); -- Associate a 3 bit element to a 4 bit slice
  process
  begin
    wait for 1 ns;
    report to_string(vec);
    wait;
  end process;
end architecture rtl;
