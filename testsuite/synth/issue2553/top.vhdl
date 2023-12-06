library ieee;
use ieee.std_logic_1164.all;

entity Top is
   port (
      i_clk   : in std_logic;
      i_btns1 : in std_logic_vector(3 downto 0);
      i_btns2 : in std_logic_vector(1 downto 0)
   );
end entity Top;

architecture Rtl of Top is

   component Debounce is
      generic (
         DEBOUNCE_COUNT : integer := 33300;
         NUM_PINS       : integer :=     1
      );
      port (
         i_clk : in  std_logic;
         i_ins : in  std_logic_vector(NUM_PINS-1 downto 0);
         o_out : out std_logic_vector(NUM_PINS-1 downto 0)
      );
   end component Debounce;

   signal w_debounced : std_logic_vector(5 downto 0);

begin

   Debouncer: Debounce
   generic map (
      NUM_PINS => 6
   )
   port map (
      i_clk             => i_clk,
      i_ins(5 downto 2) => i_btns1,
      i_ins(1 downto 0) => i_btns2,
      o_out             => w_debounced
   );

end architecture Rtl;
