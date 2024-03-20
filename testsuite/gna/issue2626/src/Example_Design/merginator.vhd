library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity merginator is
   generic (
      G_DATA_SIZE : natural
   );
   port (
      clk_i      : in  std_logic;
      rst_i      : in  std_logic;

      s1_valid_i : in  std_logic;
      s1_ready_o : out std_logic;
      s1_data_i  : in  std_logic_vector(G_DATA_SIZE-1 downto 0);

      s2_valid_i : in  std_logic;
      s2_ready_o : out std_logic;
      s2_data_i  : in  std_logic_vector(G_DATA_SIZE-1 downto 0);

      m_valid_o  : out std_logic;
      m_ready_i  : in  std_logic;
      m_data_o   : out std_logic_vector(G_DATA_SIZE-1 downto 0)
   );
end entity merginator;

architecture synthesis of merginator is

   type state_t is (
      FIRST_ST,
      SECOND_ST
   );

   signal state     : state_t := FIRST_ST;
   signal new_state : state_t := FIRST_ST;

begin

   s1_ready_o <= m_ready_i when new_state = FIRST_ST  else '0';
   s2_ready_o <= m_ready_i when new_state = SECOND_ST else '0';

   m_valid_o <= s1_valid_i or s2_valid_i;
   m_data_o  <= s1_data_i when new_state = FIRST_ST else s2_data_i;

   p_newstate : process (all)
   begin
      new_state <= state;

      case state is
         when FIRST_ST =>
            if s1_valid_i = '0' and s2_valid_i = '1' then
               new_state <= SECOND_ST;
            end if;

         when SECOND_ST =>
            if s1_valid_i = '1' and s2_valid_i = '0' then
               new_state <= FIRST_ST;
            end if;
      end case;

      if rst_i = '1' then
         new_state <= FIRST_ST;
      end if;
   end process p_newstate;

   p_fsm : process (clk_i)
   begin
      if rising_edge(clk_i) then
         state <= new_state;
      end if;
   end process p_fsm;

end architecture synthesis;

