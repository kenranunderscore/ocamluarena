local m = {}

-- TODO: instead of a module with the 'meta' attribute, we could have a function
-- 'register' that would return the whole bot, that is, including event
-- handlers?
m.meta = { name = "Lloyd", color = { red = 20, green = 230, blue = 10 } }

function m.on_tick(n)
  me.log("got tick event: " .. n)
  if n < 100 then
    return { me.move(200), me.turn_right(0.02) }
  end
end

return m
