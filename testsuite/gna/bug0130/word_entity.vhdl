
entity word_entity is
   generic (
       package word40 is new work.word_generic_pkg
       generic map (
           LENGTH => 40
       )
   );
end entity;

architecture foo of word_entity is
begin
end architecture;

