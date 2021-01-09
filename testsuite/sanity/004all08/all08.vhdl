--  Files containing most (if not all) features of vhdl08.
--  Like a comment.

--  TODO: at specifications.

context ctxt_ieee is
  library ieee;
  use ieee.std_logic_1164.all;
end ctxt_ieee;

context work.ctxt_ieee;

package pkg is
  --  TODO: file param,
  procedure clear (v : out std_logic_vector);

  type my_enum is
    (lit_a, lit_b, lit_c, 'e');
  subtype my_enum_lit is my_enum range lit_b to 'e';
  pure function "+" (v : my_enum) return my_enum;

  type my_short is range -2**15 to 2**15 - 1;
  type DISTANCE is range 0 to 1E16 units
    -- primary unit:
    angstrom;
    -- metric lengths:
    nm = 10 angstrom;
    um = 1000 nm;
    mm = 1000 um;
    cm = 10 mm;       -- Large unit.
  end units;

  type my_float is range 0.0 to 1.0e20;

  type my_array1d is array (my_short range <>) of boolean;
  subtype my_array1d10 is my_array1d (1 to 10);

  type my_array2d is array (natural range <>, natural range <>) of boolean;
  subtype my_array2d_8x8 is my_array2d (1 to 8, 1 to 8);

  type d2c_type is array (0 to 9) of character;
  type chess_type is array (1 to 8, 1 to 8) of std_logic_vector;

  attribute user_attr : boolean;
  attribute user_attr of clear [std_logic_vector]: procedure is True;

  type cell;
  type cell_acc is access cell;
  type cell is record
    chain : cell_acc;
    val : natural;
    b0, b1 : bit;
  end record;

  procedure prepend (variable ch : inout cell_acc; val : natural);

  type text_file is file of string;

  function is_eof (file t : text_file; fake : boolean) return boolean;

  alias iseof is is_eof [text_file, boolean return boolean];

  type sharedcounter is protected
     procedure increment (n : natural);
     procedure decrement (constant n : natural);
     impure function get return natural;
  end protected;

  type my_urec is record
    l : std_ulogic;
    adr : std_ulogic_vector;
    dat : std_ulogic_vector;
  end record;

  subtype my_urec8 is my_urec (adr (1 downto 0), dat (7 downto 0));
  subtype my_slv is (resolved) std_ulogic_vector;
end pkg;

package body pkg is
  pure function "+" (v : my_enum) return my_enum is
  begin
    return v;
  end "+";

  procedure clear (v : out std_logic_vector) is
  begin
    v := (v'range => '0');
  end clear;

  procedure prepend (variable ch : inout cell_acc; val : natural)
  is
    variable res : cell_acc;
    variable len : natural;
  begin
    --  Check if already in the list.
    res := ch;
    while res /= null loop
      if res.val = val then
       return;
      end if;
      res := res.all.chain;
      null;
    end loop;

    len := 0;
    res := ch;
    L1: loop
       exit L1 when res = null;
       len := len + 1;
       res := res.chain;
       next when res.val = val;
    end loop;

    res := new cell'(chain => ch, val => val, b0 | b1 => '0');
    ch := res;
  end prepend;

  function is_eof (file t : text_file; fake : boolean) return boolean is
  begin
    s: if fake then
      return false;
    else
      return endfile (t);
    end if s;
  end is_eof;

  procedure check_is_eof parameter (filename : string)
  is
    file f : text_file open read_mode is filename;
    file f2, f3 : text_file;
  begin
    null;
  end check_is_eof;

  type sharedcounter is protected body
    variable val : natural := 0;
    procedure increment (n : natural) is
    begin
      val := val + n;
    end increment;

    procedure decrement (constant n : natural) is
    begin
      val := val - n;
    end procedure decrement;

    impure function get return natural is
    begin
      return val;
    end function get;
  end protected body;
end pkg;

package genpkg is
  generic (val : natural := 5;
           function plus (l, r : integer) return integer);
  procedure add (l : inout integer);
end genpkg;

package body genpkg is
  procedure add (l : inout integer) is
  begin
    l := plus (l, val);
  end add;
end genpkg;

package genpkg2 is
  generic (v : natural;
           type t1;
           package subgenpkg is new work.genpkg generic map (<>));
end genpkg2;

package my_adder_pkg is new work.genpkg generic map (val => open, plus => "+");

library ieee, work;
use ieee.std_logic_1164.all;

entity reg is
  generic (width : natural);
  port (clk : std_logic;
        signal rst_n : std_logic;
        d : in std_logic_vector (width - 1 downto 0);
        q : out std_logic_vector (width - 1 downto 0));
  subtype bus_type is std_logic_vector (width - 1 downto 0);
begin
  ass1: postponed assert width < 128 report "large width" severity warning;
end reg;

library ieee;
use work.pkg.sharedcounter, ieee.std_logic_1164.all;

architecture behav of reg
is
  shared variable counter : sharedcounter;
begin
  process (clk, rst_n)
  begin
    if rising_edge(clk) then
      if rst_n = '0' then
        q <= (others => '0');
        counter.increment (1);
      else
        q <= d;
      end if;
    end if;
  end process;
end behav;

configuration reg_conf1 of reg is
  for behav
  end for;
end reg_conf1;

library ieee, work;
use ieee.std_logic_1164.all;

entity check_zero is
  port (i0 : in std_logic);
end check_zero;

architecture behav of check_zero is
begin
  assert (i0 = '0');
end behav;

entity reg_tb is
  generic (conf : natural := 2);
end reg_tb;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

architecture behav of reg_tb is
  component reg is
    generic (width : natural);
    port (clk : std_logic;
          rst_n : std_logic;
          d : in std_logic_vector (width - 1 downto 0);
          q : out std_logic_vector (width - 1 downto 0));
  end component reg;

  component check_zero is
    port (i0 : in std_logic);
  end component check_zero;

  subtype data_type is std_logic_vector (31 downto 0);

  function get_vector (n : natural) return data_type is
  begin
    case n is
      when 0 =>
        return 32x"0000_0000";
      when 1 =>
        return 32d"1";
      when 2 =>
        return 32sb"1";
      when 3 | 4 =>
        return data_type'(x"3333_4444");
      when 5 to 7 =>
        return (0 to 5 => '1', 6 | 7 => '0', others => '1');
      when 8 =>
        return ('0', '1', '0', others => '0');
      when others =>
        return x"ffff_ffff";
    end case;
  end get_vector;

  signal clk : std_logic bus;
  signal rst_n : std_logic := '0';
  signal din, dout : data_type;

  signal s1 : std_logic;

  signal si : integer;
  signal si2 : integer;

  alias my_clk : std_logic is clk;

  group syn is (subtype, signal <>);
  group sig_syn : syn (data_type, clk, rst_n);

  type data_array_type is array (natural range <>) of data_type;

  constant zero : natural := 0;

  procedure disp_msg (msg : string) is
  begin
    if msg'left (1) /= 1 then
      report "strange start" severity note;
    elsif msg'length > 20 then
      report "long message";
    else
      report msg
        severity note;
    end if;
  end disp_msg;

  for cmpz0 : check_zero use entity work.check_zero port map (i0 => i0);
  for cmpz1 : check_zero use open;
  for others : check_zero use entity work.check_zero;
begin
  process
  begin
    clk <= '0', '1' after 10 ns;
    wait for 20 ns;
  end process;

  rst_n <= '0', '1' after 25 ns;

  si2 <= 1 when rst_n = '0' else
         2 when rst_n = '1';

  cmpz0 : check_zero port map (i0 => din (0));
  cmpz1 : check_zero port map (i0 => din (1));
  cmpz2 : check_zero port map (i0 => din (2));

  disp_msg ("start of design");

  process (all)
  begin
    s1 <= not rst_n;
    assert s1'driving and s1'driving_value /= '0';
  end process;

  si <= integer'(1) when clk = '0' else 2;

  assert si'event or si'active or si'last_value < 3;
  assert si'last_active < 10 ns and si'last_event < 10 ns;
  assert si'transaction = '0';

  postponed process is
  begin
    disp_msg (msg => "test is starting """ & reg_tb'simple_name & '"');

    for i in 1 to 10 loop
      din <= get_vector(i);
      wait on my_clk until rising_edge(my_clk);
    end loop;

    wait;
  end process;

  compute: process
    subtype byte_idx is natural range 0 to 7;
    variable v : integer;
    variable b1, b2, b3 : boolean;
    variable bv1, bv2 : bit_vector (byte_idx);
    variable d : distance;
  begin
    b2 := true;
    b1 := (b2 and b3) or b1;
    b3 := (b1 xor b2) nand b3;
    b2 := (b1 nor b2) xnor b3;

    assert byte_idx'left = 0 and byte_idx'low = 0;
    assert byte_idx'right = 7 and byte_idx'high = 7;
    assert byte_idx'ascending;

    assert boolean'pos(b1) = 1;

    bv1 := bv2 sll v;
    bv2 := (bv1 rol v) and 8x"f0";
    bv1 := not(bv2 sra (v rem 3));

    v := -2;
    v := ((3 * v) / 4) ** 2;
    v := (v + 4) - 1;
    v := natural (v mod 128);
    b1 := v >= 3;
    b2 := v /= 4;
    b3 := b2 or (v = 5);

    d := 1.5 cm when v >= 0 else mm;

    report "v = " & integer'image (v) severity note;
    wait;
  end process compute;

  cmp_reg : reg
    generic map (width => 32)
    port map (clk => clk,
      rst_n => rst_n,
      d => din,
      q => dout);

  blk1: block (clk)
    signal dout2 : data_type register;
    disconnect dout2 : data_type after 1 ns;

    signal dout3 : data_type;
    signal dout4 : data_type;

    for cmpz1_0, cmpz1_1 : check_zero use entity work.check_zero;
  begin
    assert dout (7 downto 0) = din (7 downto 0);
    assert dout'right = 0;

    cmpz1_0 : check_zero port map (i0 => din (0));
    cmpz1_1 : check_zero port map (i0 => din (1));

    dout2 <= guarded din;

    with dout(0) select
      dout4 <= din when '0',
              (others => '1') when others;

    g1: for i in 0 to 40 generate
       g2: if i <= data_type'left generate
         cmp: entity work.reg
            generic map (width => 1)
            port map (clk => clk,
                       rst_n => rst_n,
                       d(0) => din (i),
                       q(0) => dout3 (i));
      end generate g2;
    end generate g1;
  end block;

  blk2: block is
    generic (w : natural);
    generic map (w => 1);
    port (di : std_logic_vector (w - 1 downto 0);
          do : out std_logic_vector (w - 1 downto 0));
    port map (di => din (0 downto 0),
              do => dout (0 downto 0));

    for all : check_zero use entity work.check_zero;
  begin
    cmpz1_0 : check_zero port map (i0 => din (0));

    g4: case conf generate
      when g4_1: 1 | 2 =>
        begin
          cmp : configuration work.reg_conf1
            generic map (width => 1)
            port map (clk => clk,
                      rst_n => std_logic (rst_n),
                      d => di,
                      q => do);
        end g4_1;
      when others =>
    end generate g4;
  end block blk2;
end behav;

configuration cfg of reg_tb is
  for behav
    --  component configuration.
    for cmp_reg : reg
      use entity work.reg (behav);
    end for;
    for blk1
      for g1(1)
      end for;
      for g1(2 to 3)
        for g2
        end for;
      end for;
    end for;
  end for;
end cfg;
