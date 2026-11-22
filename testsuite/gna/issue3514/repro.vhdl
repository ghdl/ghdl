entity repro is end entity;

architecture a of repro is
  type ram_t is record
    adr : bit_vector;
  end record;
  type top_t is record
    r : ram_t(adr(7 downto 0));
    s : bit_vector(15 downto 0);
  end record;
  signal c : top_t;
begin
  c.s <= x"003c"; -- to_signed(60, 16);
  process begin
    wait for 1 ns;
    report "RESULT: s=" & to_hstring(c.s) & " (expected 003C)";
    assert c.s = x"003c" severity failure;
    wait;
  end process;
end architecture;
