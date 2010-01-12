#break __gnat_raise
#break ada__exceptions__raise_exception
break __gnat_raise_nodefer_with_msg

define pt
call disp_tree.disp_tree ($arg0, 0, 0)
end

document pt
Print the structure of the iirs that is $arg0.
end

define prt
set lang c
print (iirs__iir *) $
set lang ada
end

define pl
call disp_tree_list ($arg0, 0, 0)
end

document pl
Print the list of iirs that is $arg0.
end

define plf
call disp_tree_list ($arg0, 0, 1)
end

document plf
Print flatly the list of iirs that is $arg0.
end

define ptf
call disp_tree_flat ($arg0, 0)
end

document ptf
Print the iirs that is $arg0.
end

define ptf1
call disp_tree ($, 0, 1)
end

document ptf1
Print the iirs that is $.
end

define pv
call disp_value ($)
end

document pv
Print the value that is $.
end

define ploc
call disp_iir_location ($arg0.all)
end

document ploc
Print the location for iir_acc $.
end

set lang ada
#break exception
