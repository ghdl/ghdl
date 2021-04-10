package generic_list_mwe_pkg is
  generic (
    type g_element_t);

  type elementp_t is access g_element_t;

  type g_list is protected

    impure function pull return g_element_t;

  end protected;

end package generic_list_mwe_pkg;

package body generic_list_mwe_pkg is

  type g_list is protected body
    type list_obj_t;
    type listp_t is access list_obj_t;

    type list_obj_t is record
      element : elementp_t;             -- pointer to element                                                                                                                                               
      nxt     : listp_t;                -- pointer to next list_obj                                                                                                                                         
    end record;

    variable list : listp_t;

    impure function pull
      return g_element_t is
      variable v_list : listp_t;
      variable v_elementp : elementp_t;
      impure function delete_and_return
        return g_element_t is
        variable ret : v_elementp.all'subtype;
      begin  -- function delete_and_return                                                                                                                                                                  
        ret := v_elementp.all;    -- Create a constrained copy of last element in list                                                                                                                      
        list := list.nxt;
        deallocate(v_list.element);
        deallocate(v_list);
        return ret;
      end function delete_and_return;
    begin
      assert list/=null report "Tried to pull element from empty list" severity error;
      v_list     := list;
      v_elementp := list.element;
      return delete_and_return;
    end function;
  end protected body;
      
end generic_list_mwe_pkg;
