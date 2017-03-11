--  package bb is new work.b generic map ( X => 6);

use work.bb.all;

entity bbb is
end entity;

architecture a of bbb is

begin
    process
        variable n:  m (0 to 0);
    begin
        report "X = " & integer'image(X);
        report "m'left = " & integer'image(n(0)'left);
        wait;
    end process;
end architecture;
