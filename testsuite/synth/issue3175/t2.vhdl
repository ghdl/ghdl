library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity s is
  generic (
    a : integer;
    b : integer;
    c : integer -- := a / 8
  );
  port (
    clk : in std_logic;
    d : in integer range 32 to 255;
    e : in integer range 0 to c - 1;
    f : in integer range 0 to b - 1;
    g : out std_logic_vector(7 downto 0)
  );
end s;

architecture rtl of s is

  type a_t is array (0 to b - 1) of std_logic_vector(7 downto 0);
  type b_t is array (0 to c - 1) of a_t;
  type c_t is array (32 to 255) of b_t;

  signal i : c_t := (others => (others => (others => (others => '1'))));

begin

  ROM_PROC : process(clk)
  begin
    if rising_edge(clk) then
      g <= i(d)(e)(f);
    end if;
  end process;

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity t is
  port (
    clk : in std_logic;
    h : out std_logic_vector(7 downto 0)
  );
end t;

architecture str of t is

  constant a : integer := 16;
  constant b : integer := 3;
  constant c : integer := a / 8;  -- 4
  --  Size: 4 * 14B * 224 = 12544B = 100352b

  signal d : integer range 32 to 255;
  signal e : integer range 0 to c - 1;
  signal f : integer range 0 to b - 1;

begin

  d <= 127;
  e <= 1;
  f <= 2;

  S_INST : entity work.s(rtl)
  generic map (
    a => a,
    b => b,
    c => c
  )
  port map (
    clk => clk,
    d => d,
    e => e,
    f => f,
    g => h
  );

end architecture;

-- %23:$q{n31w8} := $dff{i29} (
--  .$clk{p33}: %8:$o{n17w1} := $posedge{i15} (
--     .$i{p14}: \clk{n11w1}),
--  .$d{p34}: %20:$o{n29w8} := $dyn_extract{i27} #($offset=0) (
--     .$v{p28}: \i:$o{n15w100352} := $isignal{i13} (
--         .$i{p11}: %6:$o{n2w100352},
--         .$init{p12}: %6:$o{n2w100352}),
--     .$i{p29}: %19:$o{n28w17} := $addidx{i26} (
--        .$i0{p26}: %18:$o{n27w7} := $memidx{i25} #($step=8, $max=13) (
--           .$i{p25}: %17:$o{n26w4} := $sub{i24} (
--               .$a{p23}: %16:$o{n25w4} := 4'uhd,
--               .$b{p24}: \f{n14w4})),
--        .$i1{p27}: %15:$o{n24w17} := $addidx{i22} (
--           .$i0{p21}: %14:$o{n23w9} := $memidx{i21} #($step=112, $max=3) (
--               .$i{p20}: %13:$o{n22w2} := $sub{i20} (
--                   .$a{p18}: %12:$o{n21w2} := 2'uh3,
--                   .$b{p19}: \e{n13w2})),
--           .$i1{p22}: %11:$o{n20w17} := $memidx{i18} #($step=448, $max=223) (
--               .$i{p17}: %10:$o{n19w8} := $sub{i17} (
--                   .$a{p15}: %9:$o{n18w8} := 8'uhff,
--                   .$b{p16}: \d{n12w8}))))))
-- \g := %23:$q{n31w8}
