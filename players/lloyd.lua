local m = {}

-- TODO: instead of a module with the 'meta' attribute, we could have a function
-- 'register' that would return the whole bot, that is, including event
-- handlers?
m.meta = { name = "Lloyd", color = { red = 20, green = 230, blue = 10 } }

function m.on_tick(n)
  if n % 12 == 0 then
    return {}
  end

  if n < 100 then
    return { me.move(2), me.turn_left(0.02) }
  end

  return { me.move(1), me.turn_left(0.05), me.look_left(0.03) }
end

function m.on_enemy_seen(name, x, y)
  me.log("enemy " .. name .. " seen at (" .. x .. ", " .. y .. ")")
end

function m.on_enemy_seen(name, x, y)
  me.log("enemy " .. name .. " seen at (" .. x .. ", " .. y .. ")")
end

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
