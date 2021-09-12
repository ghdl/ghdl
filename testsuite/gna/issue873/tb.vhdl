library ieee;
use ieee.std_logic_1164.all;

entity dummy is
    generic (
        LENGTH : natural);
    port (
        A : in std_logic_vector (LENGTH - 1 downto 0);
        Q : out std_logic);
end entity;

architecture rtl of dummy is

begin

    Q <= A(0);

end rtl;

library ieee;
use ieee.std_logic_1164.all;
 
entity tb is
end tb;
 
architecture rtl of tb is

    signal x : std_logic;

begin
    x <= '0';
 
    dummy_i: entity work.dummy
        generic map (
            LENGTH => 1)
        port map (
            A => std_logic_vector(x),
            Q => open);

end architecture;
