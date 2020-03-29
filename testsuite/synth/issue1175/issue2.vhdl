entity issue2 is
    port (foo  : in  bit_vector(32-1 downto 0);
          bar  : out bit);
end issue2;

architecture rtl of issue2 is
        alias a_bar is foo(foo'high);
begin
    bar <= a_bar;
end architecture;
