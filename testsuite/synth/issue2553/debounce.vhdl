library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Debounce is
   generic (
      DEBOUNCE_COUNT : integer := 33300;
      NUM_PINS       : integer :=     1
   );
   port (
      i_clk : in  std_logic;
      i_ins : in  std_logic_vector(NUM_PINS-1 downto 0);
      o_out : out std_logic_vector(NUM_PINS-1 downto 0)
   );
end entity Debounce;

architecture Rtl of Debounce is

   type count_t is array (NUM_PINS-1 downto 0) of integer;
   signal counter : count_t := (others => 0);
   signal state   : std_logic_vector(NUM_PINS-1 downto 0) := (others => '0');

begin

   Main : process (i_clk) is
   begin
      if rising_edge(i_clk) then
         for ii in (num_pins - 1) downto 0 loop
            if i_ins(ii) /= state(ii) and counter(ii) < DEBOUNCE_COUNT then
               counter(ii) <= counter(ii) + 1;
            elsif counter(ii) = DEBOUNCE_COUNT then
               state(ii) <= i_ins(ii);
               counter(ii) <= 0;
            else
               counter(ii) <= 0;
            end if;
            o_out(ii) <= state(ii);
         end loop;
      end if;
   end process;

end architecture Rtl;
