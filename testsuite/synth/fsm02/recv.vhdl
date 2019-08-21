library ieee;
use ieee.std_logic_1164.all;

entity recv is
  port (
    rst : std_logic;
    clk : std_logic;
    rx : std_logic;
    byte : out std_logic_vector (7 downto 0);
    b_err : out std_logic;
    b_en : out std_logic);
end recv;

architecture behav of recv
is
    type state_t is
         (s_wait, s0, s1, s2, s3, s4, s5, s6, s7, s_parity, s_stop);
    signal state: state_t;
    signal parity: std_logic;
    signal err : std_logic;
    signal en : std_logic;
begin
  process (clk) is
  begin
    if rising_edge(clk) then
      if rst = '1' then
        state <= s_wait;
        err <= '0';
        en <= '0';
      else
        en <= '0';
        case state is
          when s_wait =>
            if rx = '0' then
              state <= s0;
              err <= '0';
              parity <= '0';
            end if;
          when s0 =>
            byte (0) <= rx;
            parity <= parity xor rx;
            state <= s1;
          when s1 =>
            byte (1) <= rx;
            parity <= parity xor rx;
            state <= s2;
          when s2 =>
            byte (2) <= rx;
            parity <= parity xor rx;
            state <= s3;
          when s3 =>
            byte (3) <= rx;
            parity <= parity xor rx;
            state <= s4;
          when s4 =>
            byte (4) <= rx;
            parity <= parity xor rx;
            state <= s5;
          when s5 =>
            byte (5) <= rx;
            parity <= parity xor rx;
            state <= s6;
          when s6 =>
            byte (6) <= rx;
            parity <= parity xor rx;
            state <= s7;
          when s7 =>
            byte (7) <= rx;
            parity <= parity xor rx;
            state <= s_parity;
          when s_parity =>
            if rx /= parity then
              err <= '1';
            end if;
            state <= s_stop;
          when s_stop =>
            if rx /= '1' then
              err <= '1';
            end if;
            en <= '1';
            state <= s_wait;
        end case;
      end if;
    end if;
  end process;

    b_en <= en;
    b_err <= err;

    --psl default clock is rising_edge(clk);
    --psl restrict {rst;(not rst)[*]};

    assert rst = '1' or err /= '1' report "parity error" severity error;
end behav;
