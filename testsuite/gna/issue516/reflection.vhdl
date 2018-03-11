package reflection is
  type ENUMERATION_VALUE_MIRROR_PT is protected
    impure function pos   return INTEGER;
    impure function image return STRING;
  end protected;
  type ENUMERATION_VALUE_MIRROR is access ENUMERATION_VALUE_MIRROR_PT;
	
  type ENUMERATION_SUBTYPE_MIRROR_PT is protected
    impure function enumeration_literal(literal_name : STRING)       return ENUMERATION_VALUE_MIRROR;
  end protected;
end package;
