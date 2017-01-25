entity e is
end entity;

architecture a of e is
  type AT is access INTEGER;
  type FT is file of INTEGER;
begin
  process
    variable BV : bit_vector(0 to 1);
    variable V : AT;
    file F : FT;
  begin
    report BV'simple_name;               -- BV
--    report BV'subtype'simple_name;       -- _anon            [line 14]
    report BV'subtype'base'simple_name;  -- bit_vector
--    report BV'element'simple_name;       -- bit
    
    report V'simple_name;                -- V
    report V'subtype'simple_name;        -- V
    report V'subtype'base'simple_name;   -- AT
    
    report F'simple_name;                -- F
    report F'subtype'simple_name;        -- FT
    report F'subtype'base'simple_name;   -- FT
    
    wait;
  end process;
end architecture;
