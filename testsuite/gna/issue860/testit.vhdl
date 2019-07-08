library ieee;
use     ieee.std_logic_1164.all;

entity testit is
   port (
       aclk : in std_logic;
       aresetn :in std_logic;

      d: in std_logic;
      q: out std_logic
);

end entity;

architecture rtl of testit is
       -- shouldn't GHDL at least generate a warn about these conflicting with entity inputs?
       -- Currrently, it doesn't warn then you get x's in simulation...
       signal aclk       :std_logic; 
       signal aresetn :std_logic; 
begin
   process(aclk, aresetn)
   begin
       if (aresetn = '0') then
          q <= '0';
      elsif (rising_edge(aclk)) then
          q <= d;
      end if;
   end process;

end architecture;

library ieee;
use     ieee.std_logic_1164.all;

entity testbench is

end entity;

architecture sim of testbench is
    signal aclk    :std_logic;
    signal aresetn :std_logic;
    signal d       :std_logic;
    signal q       :std_logic;
begin

aclk    <= '0';
aresetn <= '0';
d       <= '0';

testit: entity work.testit
    port map (
       aclk    => aclk,
       aresetn => aresetn,
      d        => d,
      q        => q
    );

end architecture;
