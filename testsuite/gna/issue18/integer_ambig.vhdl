use work.fum.all;

entity overload_index_issue is
end entity overload_index_issue ;
architecture dumbit_down of overload_index_issue is
    shared variable fee: fie;
begin

    process
        variable Data : integer ;
    begin
        Data := fee.foo(120.0);
        wait;
    end process;
end architecture dumbit_down;
