library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

entity abs_square is
  port (
    reset : in std_logic;
    clk : in std_logic;

    din_i : in std_logic_vector(7 downto 0);
    din_q : in std_logic_vector(7 downto 0);
    din_valid : in std_logic;
    din_mark : in std_logic_vector;

    dout_i : out std_logic_vector(7 downto 0);
    dout_q : out std_logic_vector(7 downto 0);
    dout_valid : out std_logic;
    dout_pow_sq : out std_logic_vector(15 downto 0);
    dout_mark : out std_logic_vector
  );
end abs_square;

architecture rtl of abs_square is

  constant COMP_DELAY : natural := 3; -- wrong!

  signal din_i_r : std_logic_vector(7 downto 0);
  signal din_q_r : std_logic_vector(7 downto 0);

  signal din_i_rr : std_logic_vector(7 downto 0);
  signal din_q_rr : std_logic_vector(7 downto 0);
  signal din_i_sq_rr : signed(15 downto 0);

  signal din_i_rrr : std_logic_vector(7 downto 0);
  signal din_q_rrr : std_logic_vector(7 downto 0);
  signal din_i_sq_rrr : signed(15 downto 0);
  signal din_q_sq_rrr : signed(15 downto 0);

begin
  process (clk) begin
    if rising_edge(clk) then
      din_i_r <= din_i;
      din_q_r <= din_q;
    end if;
  end process;

  process (clk) begin
    if rising_edge(clk) then
      din_i_rr <= din_i_r;
      din_q_rr <= din_q_r;
      din_i_sq_rr <= signed(din_i_r) * signed(din_i_r);
    end if;
  end process;

  process (clk) begin
    if rising_edge(clk) then
      din_i_rrr <= din_i_rr;
      din_q_rrr <= din_q_rr;
      din_i_sq_rrr <= din_i_sq_rr;
      din_q_sq_rrr <= signed(din_q_rr) * signed(din_q_rr);
    end if;
  end process;

  process (clk) begin
    if rising_edge(clk) then
      dout_i <= din_i_rrr;
      dout_q <= din_q_rrr;
      dout_pow_sq <= std_logic_vector(din_i_sq_rrr + din_q_sq_rrr);
    end if;
  end process;

  dv : entity work.delay_vector
    generic map
    (
      DELAY => 4
    )
    port map
    (
      clk => clk,
      clken => '1',
      d => din_mark,
      q => dout_mark
    );

  db : entity work.delay_bit
    generic map
    (
      DELAY => 4
    )
    port map
    (
      clk => clk,
      clken => '1',
      d => din_valid,
      q => dout_valid
    );
end rtl;