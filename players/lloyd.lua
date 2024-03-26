local m = {}

-- TODO: instead of a module with the 'meta' attribute, we could have a function
-- 'register' that would return the whole bot, that is, including event
-- handlers?
m.meta = { name = "Lloyd", color = { red = 20, green = 230, blue = 10 } }

function m.on_tick(n)
  if n % 50 == 0 then
    me.log("shooting")
    return { me.attack(2.1) }
  end

  if n < 100 then
    return { me.move(200), me.turn_right(0.02) }
  end
end

function m.on_enemy_seen(name, x, y)
  me.log("enemy " .. name .. " seen at (" .. x(", ") .. y .. ")")
end

return m
