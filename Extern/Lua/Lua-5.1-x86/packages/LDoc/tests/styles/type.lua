-----
-- module containing a class
-- @module type

----
-- Our class. Any function or table in this section belongs to `Bonzo`
-- @type Bonzo

----
-- make a new Bonzo.
-- @see Bonzo:dog
-- @string s name of Bonzo
function Bonzo.new(s)
end

-----
-- get a string representation.
-- works with `tostring`
function Bonzo:__tostring()
end

----
-- Another method.
function Bonzo:dog ()

end

----
-- Private method.
-- You need -a flag or 'all=true' to see these
-- @local
function Bonzo:cat ()

end


----
-- A subtable with fields.
-- @table Details
-- @string[readonly] name
-- @int[readonly] age

---
-- This is a simple field/property of the class.
-- @string[opt="Bilbo",readonly] frodo direct access to text
