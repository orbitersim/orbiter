----
-- A list class that wraps a table
-- @classmod List
import insert,concat,remove from table

class List
    --- constructor passed a table `t`, which can be `nil`.
    new: (t) =>
        @ls = t or {}

    --- append to list.
    add: (item) =>
        insert @ls,item

    --- insert `item` at `idx`
    insert: (idx,item) =>
        insert @ls,idx,item

    --- remove item at `idx`
    remove: (idx) => remove @ls,idx

    --- length of list
    len: => #@ls

    --- string representation
    __tostring: => '['..(concat @ls,',')..']'

    --- return idx of first occurence of `item`
    find: (item) =>
        for i = 1,#@ls
            if @ls[i] == item then return i

    --- remove item by value
    remove_value: (item) =>
        idx = self\find item
        self\remove idx if idx

    --- remove a list of items
    remove_values: (items) =>
        for item in *items do self\remove_value item

    --- create a sublist of items indexed by a table `indexes`
    index_by: (indexes) =>
        List [@ls[idx] for idx in *indexes]

    --- make a copy of this list
    copy: => List [v for v in *@ls]

    --- append items from the table or list `list`
    extend: (list) =>
        other = if list.__class == List then list.ls else list
        for v in *other do self\add v
        self

    --- concatenate two lists, giving a new list
    __concat: (l1,l2) -> l1\copy!\extend l2

    --- an iterator over all items
    iter: =>
        i,t,n = 0,@ls,#@ls
        ->
            i += 1
            if i <= n then t[i]

return List
