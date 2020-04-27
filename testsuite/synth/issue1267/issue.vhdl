library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port (bar    : in  std_logic;
          foobar : out std_logic);
end issue;

architecture beh of issue is
    function foo (arg : std_logic) return std_logic is
    begin
        RET_PATH1:
        if arg = '1' then
            return '1';
        end if;

        -- null range intended, but not necessary to repro
        for i in 2 to 1 loop
            EXIT_LOOP:
            exit when true;
        end loop;

        RET_PATH2:
        return '0';
    end function;
begin
    foobar <= foo (bar);
end architecture;
