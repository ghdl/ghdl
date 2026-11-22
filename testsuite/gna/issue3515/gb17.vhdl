library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
entity gb17 is end entity;
architecture a of gb17 is
  function three return natural is begin return 3; end function;
  type a_t is record
    cnt : unsigned(7 downto 0);
    b   : unsigned;
  end record;
  type m_t is record
    u : std_logic_vector;
  end record;

  type top_t is record
    x : a_t(b(three-1 downto 0));
    y : m_t(u(30 downto 0));
    z : std_logic_vector(31 downto 0);
  end record;

begin
  process
    variable v : top_t;
  begin
    v.x.cnt := x"AE";
    v.z := (others => '1');
    report "RESULT x.cnt=" & to_hstring(v.x.cnt) & " (expected AE)";
    assert v.x.cnt = x"AE" severity failure;
    wait;
  end process;
end architecture;
