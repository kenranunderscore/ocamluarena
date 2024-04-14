local m = {}

function m.on_round_started(n)
  me.log("Yay, new round new luck!")
end

function m.on_tick(n)
  if n % 20 == 0 then
    return { me.move_left(1), me.turn_right(0.05), me.look_right(0.03) }
  else
    return { me.move_backward(1), me.turn_right(0.05), me.look_right(0.03) }
  end
end

function m.on_enemy_seen(name, x, y)
  angle = math.atan2(y - me.y(), x - me.x()) + math.pi / 2
  return { me.attack(angle) }
end

function m.on_attack_hit(name, x, y)
  me.log("enemy " .. name .. " HIT at (" .. x .. ", " .. y .. ")")
end

function m.on_hit_by(name)
  me.log("ouch! *shakes fist at " .. name .. "*")
end

function m.on_death()
  me.log("that's it, I'm dead")
end

function m.on_round_won()
  me.log("Taste my lightning, suckerrrrrrs")
end

function m.on_round_over(winner)
  me.log("over")
  if winner == nil then
    me.log("haha, no one won")
  elseif winner ~= "Lloyd" then
    me.log("I'm not happy for you, " .. winner)
  end
end

return m
