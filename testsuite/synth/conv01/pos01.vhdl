library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity pos01 is
  generic (g_en : boolean := True);
  port (clk : std_logic;
        rst : std_logic;
        en : std_logic;
        st : out std_logic_vector(1 downto 0));
end pos01;

architecture behav of pos01 is
  type t_state is (IDLE, WAIT1, WAIT2, DONE);
  signal s : t_state;
  constant c1 : integer := t_state'pos(WAIT2);
  constant c2 : integer := boolean'pos(g_en);
begin
  process (clk) is
  begin
    if rising_edge(clk) then
      if rst = '1' then
        s <= IDLE;
      else
        case s is
          when IDLE =>
            if en = '1' then
              s <= WAIT1;
            end if;
          when WAIT1 =>
            if en = '1' then
              s <= WAIT2;
            end if;
          when WAIT2 =>
            if en = '1' then
              s <= DONE;
            end if;
          when DONE =>
            null;
        end case;
      end if;
    end if;
  end process;

  st <= std_logic_vector(to_unsigned(t_state'pos(s), 2));
end behav;

            
