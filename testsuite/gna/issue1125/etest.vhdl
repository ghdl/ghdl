library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.numeric_std.all;

entity etest is
end;

architecture atest of etest is 

function foo(a : std_logic_vector) return unsigned is
variable v : unsigned(3 downto 0):=(others=>'0') ;
begin
    v := v + unsigned(a(1 downto 1));
    return v;
end;

begin

    test : process
    variable v : std_logic_vector(3 downto 0) ;
    variable w : std_logic_vector(0 to 3);
    variable x : unsigned(3 downto 0) ;
    begin
        x := foo(w);
        -- x := x + unsigned(w(0 downto 0)) ;
        wait;
    end process ; -- test

end;
