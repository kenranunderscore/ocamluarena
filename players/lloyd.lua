local m = {}

-- TODO: instead of a module with the 'meta' attribute, we could have a function
-- 'register' that would return the whole bot, that is, including event
-- handlers?
m.meta = { name = "Lloyd", color = { red = 20, green = 230, blue = 10 } }

function m.on_tick(n)
  if n % 20 == 0 then
    return { me.move(1), me.turn_right(0.05), me.look_right(0.03), me.attack(n) }
  else
    return { me.move(1), me.turn_right(0.05), me.look_right(0.03), me.attack(4) }
  end
end

function m.on_enemy_seen(name, x, y) end

function m.on_attack_hit(name, x, y)
  me.log("enemy " .. name .. " HIT at (" .. x .. ", " .. y .. ")")
end

function m.on_hit_by(name)
  me.log("ouch! FUCK YOU, " .. name)
end

function m.on_death()
  me.log("das war's, ich bin tot")
end

return m
