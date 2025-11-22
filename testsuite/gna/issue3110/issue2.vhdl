entity issue2 is
begin
end entity issue2;

architecture arch of issue2 is
    procedure gen_proc
        generic   (constant n : positive)
        parameter (constant v : bit_vector(n-1 downto 0));

    procedure gen_proc
        generic   (constant n : positive)
        parameter (constant v : bit_vector(n-1 downto 0))
    is begin
        for i in n-1 downto 0 loop
            assert(v(i) = '1');
        end loop;
    end procedure gen_proc;

    constant n : positive := 2;
    procedure proc is new gen_proc generic map (n => n);
begin
    proc("11");
end architecture arch;
