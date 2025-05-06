library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use ieee.fixed_pkg.all;
use ieee.fixed_float_types.all;

entity divider_nrd_pipe is
  generic (
    q_scale_dividend   : integer := 12;
    q_scale_divisor    : integer := 12;
    q_scale_div_result : integer := 12;
    round              : integer := 1;
    width_dividend     : integer := 16;
    width_divisor      : integer := 16;
    width_div_result   : integer := 16
  );
  port (
    clk          : in    std_logic; -- Clock signal
    rst          : in    std_logic; -- Reset signal
    input_valid  : in    std_logic; -- Input valid signal
    output_valid : out   std_logic; -- Output valid signal
    dividend     : in    std_logic_vector(width_dividend - 1 downto 0); -- Dividend signal
    divisor      : in    std_logic_vector(width_divisor - 1 downto 0); -- Divisor signal
    div_result   : out   std_logic_vector(width_div_result - 1 downto 0) -- Division result signal
  );
end entity divider_nrd_pipe;

architecture behavioral of divider_nrd_pipe is
  subtype dividend_t is sfixed(width_dividend - q_scale_dividend - 1 downto -q_scale_dividend);
  subtype divisor_t is sfixed(width_divisor - q_scale_divisor - 1 downto -q_scale_divisor);
  subtype quotient_t is sfixed(width_dividend - q_scale_dividend - 1 downto -q_scale_dividend);
  subtype remainder_t is sfixed(width_dividend - q_scale_dividend - 1 downto -q_scale_dividend);

  signal dividend_sign     : dividend_t;
  signal divisor_sign      : divisor_t;
  signal sign_swap         : std_logic;
  signal input_valid_sign  : std_logic;
  signal input_valid_pipe  : std_logic_vector(width_dividend - 1 downto 0);
  signal divisor_z         : sfixed(width_divisor - q_scale_divisor - 1 downto -q_scale_divisor);
  signal dividend_z        : sfixed(width_dividend - q_scale_dividend - 1 downto -q_scale_dividend);
  signal quotient          : quotient_t;
  signal remainder         : remainder_t;
  signal remainder_slv     : std_logic_vector(width_dividend - 1 downto 0);
  signal sign_swap_pipe    : std_logic_vector(width_dividend - 1 downto 0);
  signal div_result_tmp    : sfixed(width_div_result - q_scale_div_result - 1 downto -q_scale_div_result);
  signal input_valid_final : std_logic;
  signal sign_swap_final   : std_logic;
  signal quotient_rnd      : quotient_t;

begin
  process_label : process (clk, rst)
    variable dividend_slv : std_logic_vector(width_dividend - 1 downto 0);
    variable divisor_slv : std_logic_vector(width_divisor - 1 downto 0);
  begin
    if (rst = '1') then
      dividend_sign     <= (others => '0');
      divisor_sign      <= (others => '0');
      sign_swap         <= '0';
      input_valid_sign  <= '0';
      input_valid_pipe  <= (others => '0');
      divisor_z         <= (others => '0');
      dividend_z        <= (others => '0');
      quotient          <= (others => '0');
      remainder         <= (others => '0');
      remainder_slv     <= (others => '0');
      sign_swap_pipe    <= (others => '0');
      div_result_tmp    <= (others => '0');
      input_valid_final <= '0';
      sign_swap_final   <= '0';
      quotient_rnd      <= (others => '0');
      output_valid      <= '0';
      div_result        <= (others => '0');
    elsif rising_edge(clk) then
      if (input_valid = '1') then
        dividend_slv := dividend;
        divisor_slv := divisor;
        dividend_sign <= to_sfixed(signed(dividend_slv)); -- jacob mod
        divisor_sign  <= to_sfixed(signed(divisor_slv)); -- jacob mod

        if (signed(dividend_slv) < 0) then
          sign_swap <= '1';
        else
          sign_swap <= '0';
        end if;

        if (signed(divisor_slv) < 0) then
          sign_swap <= sign_swap xor '1';
        end if;

        input_valid_sign  <= '1';
        input_valid_pipe  <= (others => '0');
        divisor_z         <= (others => '0');
        dividend_z        <= (others => '0');
        quotient          <= (others => '0');
        remainder         <= (others => '0');
        remainder_slv     <= (others => '0');
        sign_swap_pipe    <= (others => '0');
        div_result_tmp    <= (others => '0');
        input_valid_final <= '0';
        sign_swap_final   <= '0';
        quotient_rnd      <= (others => '0');
      end if;

      for i in 0 to width_dividend - 1 loop
        if (i = width_dividend - 1) then
          input_valid_pipe(i) <= input_valid_sign;
          divisor_z         <= divisor_sign;
          dividend_z        <= dividend_sign;
          sign_swap_pipe(i) <= sign_swap;

          remainder_slv <= std_logic_vector(to_slv(remainder));
          dividend_slv := std_logic_vector(to_slv(dividend_sign));
          remainder_slv(i) <= dividend_slv(width_dividend - 1);

          if (remainder_slv(i) = '1') then
            remainder_slv(i) <= '0';
            quotient(i)  <= '1';
          else
            quotient(i) <= '0';
          end if;
        else
          input_valid_pipe(i) <= input_valid_pipe(i + 1);
          divisor_z         <= divisor_z;
          dividend_z        <= dividend_z;
          remainder_slv(i)  <= remainder_slv(i + 1);
          sign_swap_pipe(i) <= sign_swap_pipe(i + 1);

          remainder_slv(i) <= remainder_slv(i) or dividend_slv(width_dividend - 1);

          if (remainder_slv(i) = '1') then
            remainder_slv(i) <= '0';
            quotient(i)  <= '1';
          else
            quotient(i) <= '0';
          end if;
        end if;
      end loop;

      remainder <= to_sfixed(signed(remainder_slv)); -- jacob mod

      if (to_integer(quotient) > 2 ** (width_div_result - 1) - 1) then
        quotient_rnd <= to_sfixed(real(2 ** (width_div_result - 1) - 1), width_dividend - q_scale_dividend - 1, -q_scale_dividend, fixed_wrap, fixed_truncate, 16);
      else
        quotient_rnd <= quotient;
      end if;

      if (round = 1) then
        if (remainder_slv(0) = '1') then
          if (to_integer(quotient_rnd) < 2 ** (width_div_result - 1) - 1) then
            quotient_rnd <= to_sfixed(real(to_integer(quotient_rnd) + 1), width_dividend - q_scale_dividend - 1, -q_scale_dividend, fixed_wrap, fixed_truncate, 16);
          end if;
        end if;
      end if;

      if (sign_swap = '1') then
        div_result_tmp <= -quotient_rnd;
      else
        div_result_tmp <= quotient_rnd;
      end if;

      div_result   <= std_logic_vector(to_slv(div_result_tmp(width_div_result - 1 downto 0)));
      output_valid <= input_valid_final;
    end if;
  end process process_label;
end architecture behavioral;
