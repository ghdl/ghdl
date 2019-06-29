library ieee;
use ieee.std_logic_1164.all;

entity fsm_4s is
  port (clk : std_logic;
        rst : std_logic;
        d : std_logic;
        done : out std_logic);
end fsm_4s;

architecture behav of fsm_4s is
  type state_t is (S0_1, S1_0, S2_0, S3_1);
  signal s : state_t;
begin
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        s <= S0_1;
        done <= '0';
      else
        --  Reset by default
        s <= S0_1;
        done <= '0';
        case s is
          when S0_1 =>
            if d = '1' then
              s <= S1_0;
            end if;
          when S1_0 =>
            if d = '0' then
              s <= S2_0;
            end if;
          when S2_0 =>
            if d = '0' then
              s <= S3_1;
            end if;
          when S3_1 =>
            if d = '1' then
              done <= '1';
            end if;
        end case;
      end if;
    end if;
  end process;
end behav;
