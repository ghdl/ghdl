
package word_generic_pkg is
   generic (length: natural := 32);
subtype  word is bit_vector (length - 1 downto 0);
end package;
