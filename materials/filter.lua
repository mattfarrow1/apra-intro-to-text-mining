function Header (el)
  -- I am using level 2 header for slides in the markdown below
  if el.level == 2 then
    el.attributes['background-image'] = '/slide templates/Slide7.png'
    el.attributes['background-size'] = 'contain'
  end
  return el
end