library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

package reproducer_pkg is

    -- Functions
    function MIN(LEFT, RIGHT: unsigned) return unsigned;
    function MIN(LEFT, RIGHT: integer)  return integer;

end reproducer_pkg;

package body reproducer_pkg is
    
    function MIN(LEFT, RIGHT: unsigned) return unsigned is
    begin
        if LEFT < RIGHT then
            return LEFT;
        else
            return RIGHT;
        end if;
    end;
    
    function MIN(LEFT, RIGHT: integer) return integer is
    begin
        if LEFT < RIGHT then
            return LEFT;
        else
            return RIGHT;
        end if;
    end;
    
end reproducer_pkg;    
library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

library work;
    use work.reproducer_pkg.all;

entity reproducer is
    port(
		inputA		: in  unsigned(7 downto 0);
		inputB		: in  unsigned(7 downto 0);
		inputC		: in  integer;
		inputD		: in  integer;
		OutputA		: out unsigned(7 downto 0);
		OutputB		: out integer
    );
end reproducer;

architecture rtl of reproducer is
begin

	OutputA <= min(inputA, inputB);
	OutputB <= min(inputC, inputD);
    
end rtl;
