library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top_module is
    port (
        clk : in std_logic;
        X   : in std_logic_vector(15 downto 0);
        Y   : in std_logic_vector(15 downto 0);
        R   : out std_logic_vector(31 downto 0)
    );
end entity;

architecture arch of top_module is
    signal XX : signed(15 downto 0);
    signal YY : signed(15 downto 0);
    signal RR : signed(31 downto 0);
begin
    XX <= signed(X);
    YY <= signed(Y);
    RR <= XX * YY;
    R <= std_logic_vector(RR);
end architecture;
