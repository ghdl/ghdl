entity repro is
end entity;

architecture a of repro is
  constant C_PATTERN_0 : bit_vector(31 downto 0) := (
     1 downto  0 => "01",
     3 downto  2 => "11",
     5 downto  4 => "01",
     7 downto  6 => "10",
    others =>       '0'
  );
begin
end architecture;
