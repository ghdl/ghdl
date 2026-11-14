--  External names: component instance (bound after the process), for and
--  if generate statements, relative pathnames with ^, package pathnames,
--  constants, generics, ports, alias of a record element, driver, force and
--  release, sensitivity on an alias of an external name.
package repro1_pkg is
  type rec is record
    a : bit;
    v : bit_vector (3 downto 0);
  end record;

  signal psig : integer := 42;
  constant pconst : natural := 7;
end package;

use work.repro1_pkg.all;

entity repro1_leaf is
  generic (W : positive := 4; ID : natural := 0);
  port (clk : in bit; q : out bit_vector (W - 1 downto 0));
end repro1_leaf;

architecture rtl of repro1_leaf is
  signal r : rec := ('1', "1010");
  signal cnt_s : integer := ID;
  signal vec : bit_vector (W - 1 downto 0) := (others => '0');
  signal drv_me : bit := '0';
  constant K : integer := ID * 10;
begin
  q <= vec;
  vec (0) <= clk;

  blk : block
    signal inner : bit := '1';
  begin
  end block;
end rtl;

use work.repro1_pkg.all;

entity repro1 is
end repro1;

architecture tb of repro1 is
  component repro1_leaf is
    generic (W : positive := 4; ID : natural := 0);
    port (clk : in bit; q : out bit_vector (W - 1 downto 0));
  end component;

  signal clk : bit := '0';
  signal q0 : bit_vector (3 downto 0);
  type qarr is array (0 to 2) of bit_vector (7 downto 0);
  signal qs : qarr;
  signal mirror : bit;
  signal nevents : natural := 0;
begin
  --  Process before the instances.
  check : process
    alias r0 is << signal .repro1.u0.r : rec >>;
    alias vec2 is << signal g(2).u.vec : bit_vector >>;
    alias k1 is << constant g(1).u.K : integer >>;
    alias w2 is << constant g(2).u.W : positive >>;
    alias ifs is << signal ig.u.cnt_s : integer >>;
    alias ps is << signal @work.repro1_pkg.psig : integer >>;
    alias pc is << constant @work.repro1_pkg.pconst : natural >>;
    alias q0p is << signal u0.q : bit_vector (3 downto 0) >>;
    alias d is << signal .repro1.u0.drv_me : bit >>;
  begin
    assert r0.a = '1' report "r0.a" severity failure;
    assert r0.v = "1010" report "r0.v" severity failure;
    assert vec2'length = 8 report "vec2'length" severity failure;
    assert k1 = 10 report "k1" severity failure;
    assert w2 = 8 report "w2" severity failure;
    assert ifs = 5 report "ifs" severity failure;
    assert ps = 42 report "ps" severity failure;
    assert pc = 7 report "pc" severity failure;
    d <= '1';
    r0.v <= force "0110";
    clk <= '1';
    wait for 1 ns;
    assert d = '1' report "d" severity failure;
    assert r0.v = "0110" report "force" severity failure;
    assert q0p(0) = '1' report "q0p" severity failure;
    assert vec2(0) = '1' report "vec2(0)" severity failure;
    assert mirror = '1' report "mirror" severity failure;
    assert nevents = 1 report "nevents" severity failure;
    r0.v <= release;
    wait for 1 ns;
    assert r0.v = "1010" report "release" severity failure;
    report "repro1 ok";
    wait;
  end process;

  u0 : repro1_leaf port map (clk, q0);

  g : for i in 0 to 2 generate
    u : entity work.repro1_leaf generic map (W => 8, ID => i) port map (clk, qs (i));
  end generate;

  ig : if true generate
    u : entity work.repro1_leaf generic map (ID => 5) port map (clk => clk, q => open);
  end generate;

  b : block
    alias ext_vec0 is << signal .repro1.u0.vec : bit_vector (3 downto 0) >>;
  begin
    --  Concurrent statements using external names.
    mirror <= << signal .repro1.u0.blk.inner : bit >>;

    sens : process (ext_vec0)
    begin
      if ext_vec0 (0) = '1' then
        nevents <= nevents + 1;
      end if;
    end process;

    process
      alias rr is << signal ^.u0.r : rec >>;
      alias ra is rr.a;
      alias gk is << constant ^.g(0).u.K : integer >>;
    begin
      assert rr.v = "1010" report "rr" severity failure;
      assert ra = '1' report "ra" severity failure;
      assert gk = 0 report "gk" severity failure;
      report "b ok";
      wait;
    end process;
  end block;
end tb;
