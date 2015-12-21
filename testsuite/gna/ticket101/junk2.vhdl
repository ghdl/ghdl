package t_Junk2 is
    type ufixed is array (integer range <>) of bit;
    type t_MULTIPLIERS is array(15 downto -16) of ufixed(7 downto -8);
--    type t_MULTIPLIERS is array(15 downto 1) of ufixed(7 downto -1);    -- Will compile correctly
--    type t_MULTIPLIERS is array(15 downto -1) of ufixed(7 downto 1);    -- Will not compile correctly
end package t_Junk2;

