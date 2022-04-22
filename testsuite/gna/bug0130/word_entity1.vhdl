
entity word_entity is
   generic (
       package word40 is new work.word_generic_pkg
       generic map (
           WIDTH => 40  -- not a generic in word_generic_pkg
       )
   );
end entity;

architecture foo of word_entity is
begin
end architecture;
