--  External names in a block after a component instance (bound after the
--  declarations of the architecture are elaborated), alias of an element,
--  sensitivity, and external names in subprograms (absolute and relative).
entity repro5_leaf is
  generic (N : natural := 3);
end repro5_leaf;

architecture rtl of repro5_leaf is
  signal s : bit_vector (N - 1 downto 0) := (others => '1');
  signal t : integer := 5;
begin
end rtl;

entity repro5 is
end repro5;

architecture tb of repro5 is
  component repro5_leaf is
    generic (N : natural := 3);
  end component;

  impure function get_t return integer is
    alias t is << signal u.t : integer >>;
  begin
    return t;
  end function;
begin
  u : repro5_leaf;

  b : block
    alias s is << signal .repro5.u.s : bit_vector >>;
    alias s0 is s (0);
    signal cnt : natural := 0;
  begin
    process (s)
    begin
      cnt <= cnt + 1;
    end process;

    process
      procedure check (v : integer) is
        alias t is << signal ^.u.t : integer >>;
      begin
        assert t = v report "t" severity failure;
      end procedure;
    begin
      assert s'length = 3 report "length" severity failure;
      assert s0 = '1' report "s0" severity failure;
      check (5);
      assert get_t = 5 report "get_t" severity failure;
      s <= force "010";
      wait for 1 ns;
      assert cnt = 2 report "cnt" severity failure;
      report "repro5 ok";
      wait;
    end process;
  end block;
end tb;
