local m = {}

m.meta = { name = "Kai", color = { red = 255, green = 10, blue = 10 } }

function m.on_tick(n)
  if n % 10 == 0 then
    -- return { me.move(5), me.attack(n / 50), me.look_left(0.05) }
    return { me.move(5), me.look_left(0.05), me.attack(n / 50) }
  end
end

function m.on_enemy_seen(name, x, y)
  me.log("enemy " .. name .. " seen at (" .. x .. ", " .. y .. ")")
end

function m.on_attack_hit(name, x, y)
  me.log("enemy " .. name .. " HIT at (" .. x .. ", " .. y .. ")")
end

function m.on_hit_by(name)
  me.log("ouch! damn you, " .. name)
end

function m.on_death()
  me.log("oh noes, I'm dead")
end

return m
