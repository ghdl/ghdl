library ieee;
use ieee.std_logic_1164.all;

entity adder is
  port (
    clk : in std_logic;
    a   : in std_logic;
    b   : in std_logic;
    v   : in std_logic_vector(3 downto 0);
    sum : out std_logic
  );
end entity;

architecture arch of adder is
begin
  sum <= a xor b;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity wrap is
  port (
    clk : in std_logic;
    x   : in std_logic;
    y   : in std_logic;
    s   : out std_logic
  );
end entity;

architecture arch of wrap is
  signal vec : std_logic_vector(3 downto 0);
begin
  u_in : entity work.adder
    port map (clk => clk, a => x, b => y, v => vec, sum => s);
end architecture;

library ieee;
use ieee.std_logic_1164.all;

package types is
  subtype v1 is std_logic_vector(0 to 0);
  type rec1 is record
    f : std_logic;
    g : v1;
  end record;
  type outer is record
    sub : rec1;
    x   : std_logic;
  end record;
  type sa is array (natural range <>) of v1;
  subtype sa1 is sa(0 to 0);
  subtype sa2 is sa(0 to 1);
end package;

library ieee;
use ieee.std_logic_1164.all;
use work.types.all;

entity comp is
  port (
    r  : in rec1;
    r2 : out rec1;
    sp : in sa1;
    sw : in sa2;
    io : inout std_logic;
    bf : buffer std_logic
  );
end entity;

architecture arch of comp is
begin
  r2 <= r;
  bf <= r.f;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use work.types.all;

entity inner is
end entity;

architecture arch of inner is
  signal rs, rs2 : rec1;
  signal os, os2 : outer;
  signal sbig : sa2;
  signal ssm  : sa1;
  signal sio, sbf : std_logic;
begin
  --  Whole objects are resolved, parts are not.
  uc1 : entity work.comp
    port map (r => os.sub, r2 => os2.sub, sp => sbig(0 to 0), sw => sbig,
              io => sio, bf => sbf);

  uc2 : entity work.comp
    port map (r => rs, r2 => rs2, sp => ssm, sw => sbig,
              io => sio, bf => sbf);

  blk : block
    signal lrs : rec1;
    signal lsb : sa2;
    signal lio, lbf : std_logic;
  begin
    ub1 : entity work.comp
      port map (r => rs, r2 => lrs, sp => ssm, sw => lsb,
                io => lio, bf => sbf);
    ub2 : entity work.comp
      port map (r => lrs, r2 => rs2, sp => ssm, sw => sbig,
                io => sio, bf => lbf);
  end block;

  gen : for i in 0 to 1 generate
    signal lg : rec1;
    signal lio, lbf : std_logic;
  begin
    ug : entity work.comp
      port map (r => lg, r2 => rs2, sp => ssm, sw => sbig,
                io => sio, bf => lbf);
  end generate;

  ig : if true generate
    ui : entity work.comp
      port map (r => rs, r2 => rs2, sp => ssm, sw => sbig,
                io => sio, bf => sbf);
  end generate;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity myentity is
  port (
    top_in  : in std_logic;
    top_out : out std_logic
  );
end entity;

architecture arch of myentity is
  signal clk, x, y, s1, s2, s3 : std_logic;
  signal vec  : std_logic_vector(7 downto 0);
  signal nvec : std_logic_vector(3 downto 0);
begin
  clk <= top_in;
  x <= top_in;
  y <= not top_in;
  vec <= (others => top_in);
  nvec <= (others => y);

  u1 : entity work.adder
    port map (clk => clk, a => x, b => y, v => vec(3 downto 0), sum => s1);

  u2 : entity work.adder
    port map (clk => clk, a => x, b => not y, v => nvec, sum => s2);

  u3 : entity work.adder
    port map (clk => clk, a => x, b => y, v => vec(7 downto 4), sum => open);

  u_wrap : entity work.wrap
    port map (clk => clk, x => x, y => y, s => s3);

  u_inner : entity work.inner;

  top_out <= s1 xor s2 xor s3;
end architecture;
