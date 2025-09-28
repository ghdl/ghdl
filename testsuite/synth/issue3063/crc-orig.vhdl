-- Global package for array types and dimensions,
 library IEEE;
 use IEEE.std_logic_1164.all;
 use IEEE.numeric_std.all;
 use ieee.math_real.all;
 use ieee.fixed_pkg.all;
 use ieee.fixed_float_types.all;
 use ieee.float_pkg.all;
package global_types_pkg is
    type t_float_1d_vector is array (natural range <>) of float32;
    type t_float_2d_vector is array (natural range <>) of t_float_1d_vector;
    type t_slv_1d_vector is array (natural range <>) of std_logic_vector;
    type t_slv_2d_vector is array (natural range <>) of t_slv_1d_vector;
  constant C_LEN_CALCULATE_CRC_BITS_G_MAX : natural := 128;

  constant NAUF : UNRESOLVED_ufixed (0 downto 1) := (others => '0');
  constant NASF : UNRESOLVED_sfixed (0 downto 1) := (others => '0');
  constant NSLV : STD_ULOGIC_VECTOR (0 downto 1) := (others => '0');
  function to_sfixed (arg : UNRESOLVED_ufixed;
                      constant left_index     : INTEGER;  -- left index (high index)
                      constant right_index    : INTEGER                   := 0;  -- right index
                      constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
                      constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed;

  function to_sfixed (
    arg                     : UNRESOLVED_UNSIGNED;               -- signed
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed;

  function to_ufixed (arg : UNRESOLVED_sfixed) return UNRESOLVED_ufixed;

  function to_ufixed (arg : UNRESOLVED_sfixed;
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed;

  function fn_stdlogic_to_sfixed (arg1 : std_logic_vector;
                                  high : integer;
                                  low : integer
                                  )
    return UNRESOLVED_sfixed;

  function fn_stdlogic_to_ufixed (arg1 : std_logic_vector;
                                  high : integer;
                                  low : integer
               )
    return UNRESOLVED_ufixed;
end package global_types_pkg;
package body global_types_pkg is

  function cleanvec (
    arg : UNRESOLVED_sfixed)            -- input
    return UNRESOLVED_sfixed
  is
  begin  -- function cleanvec
    return arg;
  end function cleanvec;

  -- purpose: Fixes "downto" problem and resolves meta states
  function cleanvec (
    arg : UNRESOLVED_ufixed)            -- input
    return UNRESOLVED_ufixed
  is
  begin  -- function cleanvec
    return arg;
  end function cleanvec;

  function to_sfixed (arg : UNRESOLVED_ufixed;
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed is
    variable result : UNRESOLVED_sfixed (arg'high+1 downto arg'low);
  begin
    if arg'length < 1 then
      return NASF;
    end if;
    result (arg'high downto arg'low) := UNRESOLVED_sfixed(cleanvec(arg));
    result (arg'high+1)              := '0';
    return result;
  end function to_sfixed;

  function to_ufixed (arg : UNRESOLVED_sfixed;
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed is
    variable result : UNRESOLVED_ufixed (arg'high+1 downto arg'low);
  begin
    if arg'length < 1 then
      return NAUF;
    end if;
    result (arg'high downto arg'low) := UNRESOLVED_ufixed(cleanvec(arg));
    result (arg'high+1)              := '0';
    return result;
  end function to_ufixed;

  function to_ufixed (arg : UNRESOLVED_sfixed)
    return UNRESOLVED_ufixed is
    variable result : UNRESOLVED_ufixed (arg'high downto arg'low);
  begin
    result := UNRESOLVED_ufixed(cleanvec(arg));
    return result;
  end function to_ufixed;

  function to_sfixed (
    arg                     : UNRESOLVED_UNSIGNED;               -- signed
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed
  is
    constant ARG_LEFT : INTEGER := arg'length-1;
    alias XARG        : UNRESOLVED_UNSIGNED(ARG_LEFT downto 0) is arg;
    variable result   : UNRESOLVED_sfixed (left_index downto right_index);
  begin
    if arg'length < 1 or (left_index < right_index) then
      return NASF;
    end if;
    result := resize (arg            => UNRESOLVED_sfixed (XARG),
                      left_index     => left_index,
                      right_index    => right_index,
                      round_style    => round_style,
                      overflow_style => overflow_style);
    return result;
  end function to_sfixed;

  function fn_stdlogic_to_sfixed (arg1 : std_logic_vector;
                                  high : integer;
                                  low : integer
                                  )
    return UNRESOLVED_sfixed is
    variable result : UNRESOLVED_sfixed (high downto low);
  begin

    result := resize(to_sfixed(std_logic_vector(resize(signed(arg1),(high-low)+1)),high, low),high, low);
    return result;
  end function fn_stdlogic_to_sfixed;

  function fn_stdlogic_to_ufixed (arg1 : std_logic_vector;
                                  high : integer;
                                  low : integer
                                  )
    return UNRESOLVED_ufixed is
    variable result : UNRESOLVED_ufixed (high downto low);
  begin

    result := resize(to_ufixed(std_logic_vector(resize(signed(arg1),(high-low)+1)),high, low),high, low);
    return result;
  end function fn_stdlogic_to_ufixed;
end package body global_types_pkg;


library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.float_pkg.all;
  use ieee.math_real.all;
  use ieee.fixed_pkg.all;
  use ieee.fixed_float_types.all;

library work;
  use work.global_types_pkg.all;

entity calculate_crc_bits is
  generic (
    width_a     : integer := 100; -- Number of information bits (A)
    width_g_max : integer := 24;  -- Number of columns in G_max (CRC length L)
    width_p     : integer := 24   -- Width of output p; must equal WIDTH_G_max
  );
  port (
    clk          : in    std_logic;                                                                          -- Clock signal
    rst          : in    std_logic;                                                                          -- Reset signal
    input_valid  : in    std_logic;                                                                          -- Input valid signal
    output_valid : out   std_logic;                                                                          -- Output valid signal
    a            : in    std_logic_vector(width_a - 1 downto 0);                                             -- A information bits; a(0) maps to MATLAB a(1)
    g_max        : in    t_slv_1d_vector(0 to C_LEN_CALCULATE_CRC_BITS_G_MAX - 1)(width_g_max - 1 downto 0); -- Generator matrix rows
    p            : out   std_logic_vector(width_p - 1 downto 0)                                              -- CRC bits output
  );
end entity calculate_crc_bits;

architecture behavioral of calculate_crc_bits is

  constant num_rows   : integer := C_LEN_CALCULATE_CRC_BITS_G_MAX;
  constant a_len      : integer := width_a;
  constant l_len      : integer := width_p;
  constant row_offset : integer := num_rows - a_len;

begin

  assert (width_p = width_g_max)
    report "WIDTH_p must equal WIDTH_G_max for calculate_crc_bits"
    severity failure;
  assert (num_rows >= a_len)
    report "G_max must have at least WIDTH_a rows (C_LEN_CALCULATE_CRC_BITS_G_MAX >= WIDTH_a)"
    severity failure;

  crc_calc_proc : process (clk, rst) is

    variable p_calc            : std_logic_vector(width_p - 1 downto 0);
    variable parity_j          : std_logic;
    variable a_bit_i           : std_logic;
    variable g_bit_ij          : std_logic;
    variable p_hold            : std_logic_vector(width_p - 1 downto 0);
    variable output_valid_next : std_logic;

  begin

    if (rst = '1') then
      p            <= (others => '0');
      output_valid <= '0';
    elsif rising_edge(clk) then
      p_hold            := p;
      output_valid_next := '0';
      if (input_valid = '1') then
        p_calc := (others => '0');

        for j in 0 to l_len - 1 loop

          parity_j := '0';

          for i in 0 to a_len - 1 loop

            a_bit_i  := a(i);
            g_bit_ij := g_max(row_offset + i)(j);
            parity_j := parity_j xor (a_bit_i and g_bit_ij);

          end loop;

          p_calc(j) := parity_j;

        end loop;

        p_hold            := p_calc;
        output_valid_next := '1';
      end if;
      p            <= p_hold;
      output_valid <= output_valid_next;
    end if;

  end process crc_calc_proc;

end architecture behavioral;
