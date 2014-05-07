entity assignment_to_an_aggregate is
end entity;
architecture example of assignment_to_an_aggregate is
    type vowel_type is (a, e, i, o, u);
    type consonant_type is (b, c, d, f, g);
    
    signal my_vowel: vowel_type;
    signal my_consonant: consonant_type;
begin

    (my_vowel, my_consonant) <= (a,b);

end;
