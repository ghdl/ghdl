package b is
    generic ( X: natural := 4);
    type m is array (natural range <>) of bit_vector (X - 1 downto 0);
end package;
