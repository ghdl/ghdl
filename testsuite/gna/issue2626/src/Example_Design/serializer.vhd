library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity serializer is
   generic (
      G_DATA_SIZE_IN  : natural;
      G_DATA_SIZE_OUT : natural
   );
   port (
      clk_i       : in  std_logic;
      rst_i       : in  std_logic;
      s_valid_i   : in  std_logic;
      s_ready_o   : out std_logic;
      s_data_i    : in  std_logic_vector(G_DATA_SIZE_IN-1 downto 0);

      m_valid_o   : out std_logic;
      m_ready_i   : in  std_logic;
      m_data_o    : out std_logic_vector(G_DATA_SIZE_OUT-1 downto 0)
   );
end entity serializer;

architecture synthesis of serializer is

   type state_t is (
      IDLE_ST,
      BUSY_ST
   );

   signal count : natural range 0 to G_DATA_SIZE_IN/G_DATA_SIZE_OUT-1;
   signal data  : std_logic_vector(G_DATA_SIZE_IN-1 downto 0);
   signal state : state_t := IDLE_ST;

begin

   s_ready_o <= '1' when state = IDLE_ST else '0';
   m_data_o <= data(G_DATA_SIZE_IN-1 downto G_DATA_SIZE_IN-G_DATA_SIZE_OUT);

   p_fsm : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if m_ready_i = '1' then
            m_valid_o <= '0';
         end if;

         case state is
            when IDLE_ST =>
               if s_valid_i = '1' then
                  count     <= G_DATA_SIZE_IN/G_DATA_SIZE_OUT-1;
                  data      <= s_data_i;
                  m_valid_o <= '1';
                  state     <= BUSY_ST;
               end if;

            when BUSY_ST =>
               if m_ready_i = '1' then
                  if count > 0 then
                     data(G_DATA_SIZE_IN-1 downto G_DATA_SIZE_OUT) <= data(G_DATA_SIZE_IN-G_DATA_SIZE_OUT-1 downto 0);
                     data(G_DATA_SIZE_OUT-1 downto 0) <= (others => '0');
                     m_valid_o <= '1';
                     count     <= count - 1;
                  else
                     state <= IDLE_ST;
                  end if;
               end if;
         end case;

         if rst_i = '1' then
            state <= IDLE_ST;
         end if;
      end if;
   end process p_fsm;

end architecture synthesis;

