local PWD = os.getenv("PWD")

local run
if not arg[1] then
  run = function (dir)
     local cmd = 'cd '..dir..' && lua '..PWD..'/ldoc.lua --testing . && diff -r doc cdocs'
     print(cmd)
     os.execute(cmd)
  end
elseif arg[1] == 'update' then
   run = function (dir)
     local cmd = 'cd '..dir..' && lua '..PWD..'/ldoc.lua --dir cdocs --testing .'
     print(cmd)
     os.execute(cmd)
   end
end

for _,d in ipairs{'tests','tests/example','tests/md-test'} do
   run(d)
end
