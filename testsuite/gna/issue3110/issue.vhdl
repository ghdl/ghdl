package pkg is
    procedure gen_proc
        generic   (constant n : positive)
        parameter (constant v : bit_vector(n-1 downto 0));
end package pkg;

package body pkg is
    procedure gen_proc
        generic   (constant n : positive)
        parameter (constant v : bit_vector(n-1 downto 0))
    is begin
        for i in n-1 downto 0 loop
            assert(v(i) = '1');
        end loop;
    end procedure gen_proc;
end package body;

----

use work.pkg.all;

entity issue is begin end entity issue;

architecture arch of issue is
    constant n : positive := 2;
    procedure proc is new gen_proc generic map (n => n);
begin
    proc("11");
end architecture arch;
