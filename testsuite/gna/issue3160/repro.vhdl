entity repro is
end entity;

architecture a of repro is
  --  A record with unbounded elements.
  type rec_t is record
    x : bit_vector;
    y : bit_vector;
  end record;

  --  The aggregate names the elements in the reverse of their declaration
  --  order, and is the result of a function, so its bounds are built by
  --  the code generator rather than folded at analysis.
  function f (v : rec_t) return rec_t is
  begin
    return (y => v.y, x => v.x);
  end function;

  signal s : rec_t (x (3 downto 0), y (1 downto 0)) := (x => "1100", y => "10");
  signal r : rec_t (x (3 downto 0), y (1 downto 0));
begin
  r <= f (s);

  check : process
  begin
    wait for 1 ns;
    assert r.x = "1100" and r.y = "10" report "bad value" severity failure;
    wait;
  end process;
end architecture;
