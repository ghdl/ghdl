library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is end entity;

architecture a_test of test is
    signal a, b : std_logic;
    signal i1 : integer := 1;
    constant i2 : integer := 1;
    signal t1 : time := 1 ns;
    constant t2 : time := 1 ns;
begin
    process
        variable m : boolean;
        variable ip1 : integer := 1;
        constant ip2 : integer := 1;
        variable tp1 : time := 1 ns;
        constant tp2 : time := 1 ns;
    begin
        m := a'stable(1 ns);        --works ... literal
        m := a'stable(i1 * ns);     --works ... signal * unit
        m := a'stable(i2 * ns);     --works ... constant * unit
        m := a'stable(t1);          --works ... signal (time)
        m := a'stable(t2);          --works ... constant (time)
        m := a'stable(ip1 * ns);    --crashs ... LOCAL variable * unit
        m := a'stable(ip2 * ns);    --works ...  LOCAL constant * unit
        m := a'stable(tp1);         --crashs ... LOCAL variable (time)
        m := a'stable(tp2);         --crashs ... LOCAL constant (time)
        --
        m := a'quiet(1 ns);        --works
        m := a'quiet(i1 * ns);     --works
        m := a'quiet(i2 * ns);     --works
        m := a'quiet(t1);          --works
        m := a'quiet(t2);          --works
        m := a'quiet(ip1 * ns);    --crashs
        m := a'quiet(ip2 * ns);    --works
        m := a'quiet(tp1);         --crashs
        m := a'quiet(tp2);         --crashs
        --
        b <= a'delayed(1 ns);        --works
        b <= a'delayed(i1 * ns);     --works
        b <= a'delayed(i2 * ns);     --works
        b <= a'delayed(t1);          --works
        b <= a'delayed(t2);          --works
        b <= a'delayed(ip1 * ns);    --crashs
        b <= a'delayed(ip2 * ns);    --works
        b <= a'delayed(tp1);         --crashs
        b <= a'delayed(tp2);         --crashs
    end process;
end architecture;
