library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port (foo : in  std_logic);
end entity issue;

architecture beh of issue is
    procedure check (arg : in std_logic) is
    begin
        assert (arg xor '1') = (arg and '0');
    end procedure;
begin
    check (foo);
    --assert (foo xor '1') = (foo and '0');
end architecture;
