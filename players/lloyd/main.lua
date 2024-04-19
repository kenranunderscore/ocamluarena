local m = {}

function m.on_round_started(n) end

local dir = 1

function m.on_tick(n)
  if me.head_turn_remaining() == 0 then
    dir = -dir
    return { me.turn_head(dir * math.pi) }
  end
end

function m.on_enemy_seen(name, x, y)
  if name == "Kai" then
    angle = math.atan2(y - me.y(), x - me.x()) + math.pi / 2
    a = utils.normalize_relative_angle(angle - me.heading())
    res = { me.turn(a) }
    if me.turn_remaining() < 0.05 then
      table.insert(res, me.attack())
    end
    return res
  end
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
