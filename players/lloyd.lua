local m = {}

-- TODO: instead of a module with the 'meta' attribute, we could have a function
-- 'register' that would return the whole bot, that is, including event
-- handlers?
m.meta = { name = "Lloyd", color = { red = 250, green = 30, blue = 10 } }

function log(msg)
  print("[" .. m.meta.name .. "] " .. msg)
end

function m.on_tick(n)
  log("got tick event: " .. n)
  if n < 1000 then
    return { me.move(50) }
  else
    return {}
  end
end

-- function m.on_kill_enemy(enemy_name)
--    return { me.stop() }
-- end

log("reporting in")

return m
