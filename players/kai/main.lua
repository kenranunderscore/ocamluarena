local m = {}

function m.on_tick(n)
  if n % 10 == 0 then
    return { me.move_forward(5), me.turn_head_left(0.05), me.attack() }
  end
end

function m.on_enemy_seen(name, x, y)
  angle = math.atan2(y - me.y(), x - me.x()) + math.pi / 2
  return { me.attack(angle) }
end

function m.on_enemy_attack(name)
  me.log("HELP, I am being attacked by " .. name)
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
